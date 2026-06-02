#' Assess possible excess-loss thresholds
#'
#' @description
#' Compare candidate thresholds for capped severity and large-loss pricing work.
#'
#' `assess_excess_threshold()` is a diagnostic helper. It does not choose a
#' threshold automatically. It shows how many claims and how much historical
#' claim cost sit above candidate thresholds, and how much pure premium would
#' remain after capping claims at each threshold.
#'
#' Use this before [calculate_excess_loss()] to understand the effect of the
#' threshold on the portfolio. The output is useful for tariff notes, pricing
#' reviews and governance discussions around capped severity models.
#'
#' @param data A `data.frame` with claim-level observations.
#' @param claim_amount Character string. Claim amount column.
#' @param thresholds Numeric vector of candidate thresholds.
#' @param exposure Optional character string. Exposure column. If supplied,
#'   pure premium before and after capping is calculated.
#' @param group Optional character string. Grouping column used to assess
#'   thresholds by segment.
#'
#' @return A `data.frame` with class `"excess_threshold_assessment"`.
#'
#' @author Martin Haringa
#'
#' @examples
#' claims <- data.frame(
#'   sector = rep(c("Industry", "Retail"), each = 5),
#'   claim_amount = c(1000, 25000, 120000, 50000, 175000,
#'                    2000, 40000, 90000, 150000, 300000),
#'   earned_exposure = rep(1, 10)
#' )
#'
#' thresholds <- assess_excess_threshold(
#'   data = claims,
#'   claim_amount = "claim_amount",
#'   thresholds = c(25000, 50000, 100000, 150000),
#'   exposure = "earned_exposure",
#'   group = "sector"
#' )
#'
#' autoplot(thresholds, y = "premium_impact")
#'
#' @export
assess_excess_threshold <- function(data,
                                    claim_amount,
                                    thresholds,
                                    exposure = NULL,
                                    group = NULL) {
  validate_assess_excess_threshold(data, claim_amount, thresholds, exposure, group)

  groups <- if (is.null(group)) {
    list(All = seq_len(nrow(data)))
  } else {
    split(seq_len(nrow(data)), as.character(data[[group]]))
  }

  out <- lapply(thresholds, function(threshold) {
    rows <- lapply(names(groups), function(g) {
      idx <- groups[[g]]
      amount <- data[[claim_amount]][idx]
      total_loss <- sum(amount)
      capped_loss <- sum(pmin(amount, threshold))
      excess_loss <- sum(pmax(amount - threshold, 0))
      exposure_sum <- if (is.null(exposure)) NA_real_ else sum(data[[exposure]][idx])
      ans <- data.frame(
        threshold = threshold,
        n_claims = length(amount),
        n_excess_claims = sum(amount > threshold),
        excess_loss = excess_loss,
        excess_loss_ratio = safe_ratio_excess(excess_loss, total_loss),
        pure_premium_before = safe_ratio_excess(total_loss, exposure_sum),
        pure_premium_after = safe_ratio_excess(capped_loss, exposure_sum),
        premium_impact = safe_ratio_excess(excess_loss, exposure_sum),
        total_loss = total_loss,
        capped_loss = capped_loss,
        stringsAsFactors = FALSE
      )
      if (!is.null(group)) {
        ans$group <- g
      }
      ans
    })
    do.call(rbind, rows)
  })

  out <- do.call(rbind, out)
  if (!is.null(group)) {
    out <- out[, c("threshold", "group", setdiff(names(out), c("threshold", "group"))),
               drop = FALSE]
  }
  row.names(out) <- NULL
  attr(out, "claim_amount") <- claim_amount
  attr(out, "exposure") <- exposure
  attr(out, "group") <- group
  class(out) <- c("excess_threshold_assessment", "data.frame")
  out
}

#' Decompose claim amounts into capped and excess parts
#'
#' Large claims can distort risk-factor relativities and make pricing models
#' unstable. `calculate_excess_loss()` separates each claim into a capped part
#' and an excess part above a selected threshold.
#'
#' The capped claim amount can be used to model the base premium, while the
#' excess component can be analysed, pooled or allocated separately. This allows
#' the impact of large individual claims to be controlled without ignoring the
#' associated cost.
#'
#' The function is deliberately deterministic. It does not perform smoothing,
#' credibility weighting, allocation or simulation. It simply decomposes each
#' observed claim into:
#'
#' \deqn{
#' claim\_amount =
#' capped\_claim\_amount +
#' excess\_claim\_amount
#' }
#'
#' where:
#'
#' \deqn{
#' excess\_claim\_amount =
#' max(claim\_amount - threshold, 0)
#' }
#'
#' and:
#'
#' \deqn{
#' capped\_claim\_amount =
#' min(claim\_amount, threshold)
#' }
#'
#' The resulting excess component can subsequently be allocated using
#' [allocate_excess_loss()] and added back to the technical premium using
#' [apply_excess_loading()].
#'
#' @details
#'
#' ## Typical pricing workflow
#'
#' A common workflow is:
#'
#' 1. Select an excess threshold.
#' 2. Split claims into capped and excess components.
#' 3. Model frequency and severity using capped claim amounts.
#' 4. Allocate the excess-loss burden separately.
#' 5. Add the resulting excess loading back to the technical premium.
#'
#' This approach reduces the influence of a small number of large claims on
#' risk-factor relativities while ensuring that the total cost of excess losses
#' remains reflected in the final premium.
#'
#' @param data A data.frame with claim-level observations.
#' @param claim_amount Character string. Claim amount column.
#' @param threshold Positive numeric scalar. Claims above this value contribute
#'   to the excess component. Claims below the threshold remain fully included
#'   in the capped claim amount.
#'
#' @return A data.frame with the original data and the columns
#'   `claim_amount`, `capped_claim_amount`, `excess_claim_amount` and
#'   `is_excess_claim`.
#'
#' @author Martin Haringa
#'
#' @examples
#' claims <- data.frame(
#'   claim_amount = c(1000, 120000, 30000)
#' )
#'
#' calculate_excess_loss(
#'   claims,
#'   claim_amount = "claim_amount",
#'   threshold = 100000
#' )
#'
#' @export
calculate_excess_loss <- function(data, claim_amount, threshold) {
  validate_calculate_excess_loss(data, claim_amount, threshold)
  amount <- data[[claim_amount]]
  out <- data
  out$claim_amount <- amount
  out$capped_claim_amount <- pmin(amount, threshold)
  out$excess_claim_amount <- pmax(amount - threshold, 0)
  out$is_excess_claim <- amount > threshold
  attr(out, "claim_amount_column") <- claim_amount
  attr(out, "threshold") <- threshold
  class(out) <- c("excess_loss_decomposition", "data.frame")
  out
}

#' Allocate excess loss to a pricing portfolio
#'
#' @description
#' Large claims can distort risk-factor relativities and create unstable
#' premiums. `allocate_excess_loss()` redistributes historical excess losses
#' across a portfolio in a controlled and transparent way.
#'
#' The function is typically used after [calculate_excess_loss()]. The base
#' premium can be modelled on capped claim amounts, while the excess part of
#' large claims is allocated back to the portfolio as an additional loading.
#'
#' @details
#'
#' ## Allocation methods
#'
#' The `allocation` argument determines how the excess burden is shared.
#'
#' - `"portfolio"`: excess losses are pooled across the entire portfolio and
#'   redistributed using the specified allocation weight. The excess burden is
#'   shared by all included risks regardless of their risk-factor level.
#'
#'   This provides the most stable excess loading and is often appropriate when
#'   excess losses are infrequent, highly volatile or considered a portfolio-
#'   wide risk rather than a risk-factor-specific characteristic.
#'
#' - `"risk_factor"`: excess losses are allocated separately for each
#'   risk-factor level. The excess burden observed within a group is spread
#'   across all risks in that group and is not shared with other groups.
#'
#'   This produces the strongest link between excess loadings and observed
#'   group experience, but can lead to volatile results when excess losses are
#'   rare.
#'
#' - `"partial"`: excess losses are allocated using a credibility-weighted
#'   combination of portfolio and risk-factor experience. Risk-factor levels
#'   with more credible experience receive excess loadings that more closely
#'   reflect their own observed excess-loss burden, while less credible groups
#'   are pooled more strongly towards the portfolio average.
#'
#'   This approach typically provides a good balance between pricing stability
#'   and risk differentiation and is therefore often the preferred choice in
#'   practical rating applications.
#'
#' ## Credibility weighting
#'
#' For `allocation = "partial"`, excess losses are allocated using a
#' credibility-weighted blend of portfolio and risk-factor experience.
#'
#' The allocated loading is calculated as:
#'
#' \deqn{
#' loading_g = Z_g \cdot loading_g^{risk\ factor} +
#'             (1 - Z_g) \cdot loading^{portfolio}
#' }
#'
#' where `Z_g` represents the credibility assigned to the risk-factor-level
#' experience.
#'
#' If `credibility` is supplied, the same credibility is applied to all
#' risk-factor levels.
#'
#' If `credibility = NULL`, credibility is determined separately for each
#' risk-factor level based on the selected `credibility_basis`:
#'
#' \deqn{
#' Z_g = \frac{n_g}{n_g + credibility\_threshold}
#' }
#'
#' where `n_g` is determined by `credibility_basis`:
#'
#' - `"claims"`: total number of claims in the risk-factor level.
#' - `"excess_claims"`: number of claims with positive excess loss.
#' - `"allocation_weight"`: total allocation weight in the risk-factor level.
#'
#' `credibility_threshold` represents the amount of experience required to
#' reach 50 percent credibility.
#'
#' Example:
#'
#' A sector with 20 claims and `credibility_threshold = 50` receives:
#'
#' \deqn{
#' Z = \frac{20}{20 + 50} = 0.29
#' }
#'
#' Therefore 29% of the excess loading is based on the sector's own excess-loss
#' experience and 71% is based on the portfolio-wide excess loading.
#'
#' For example, with `credibility_threshold = 50`, a group with:
#'
#' - 10 claims receives 17% credibility;
#' - 50 claims receives 50% credibility;
#' - 100 claims receives 67% credibility;
#' - 200 claims receives 80% credibility.
#'
#' The final credibility is scaled using `credibility_scale` and then capped
#' between 0 and 1:
#'
#' \deqn{
#' Z_g = min(1, max(0, Z_g \cdot credibility\_scale))
#' }
#'
#' Higher values of `credibility_threshold` or lower values of
#' `credibility_scale` pool more strongly towards the portfolio loading.
#'
#' ## Bootstrap allocation
#'
#' With `method = "observed"`, the function allocates the historically observed
#' excess loss.
#'
#' With `method = "bootstrap"`, the function repeatedly resamples observed
#' positive excess claim amounts. This provides a pragmatic estimate of
#' excess-loss volatility and the resulting uncertainty in excess loadings.
#'
#' The approach is intended as a practical pricing approximation rather than a
#' formal extreme value model.
#'
#' The bootstrap affects both the total excess burden and the distribution of
#' excess loss across risk-factor levels. Use `bootstrap_seed` to make bootstrap
#' results reproducible.
#'
#' ## Severity noise
#'
#' `severity_noise` can only be used with `method = "bootstrap"`.
#'
#' If `severity_noise = "none"`, bootstrap samples reuse the observed excess
#' claim amounts.
#'
#' If `severity_noise = "lognormal"`, sampled excess claims are multiplied by
#' lognormal noise. This is usually the most natural option for large claims,
#' because claim amounts remain positive and variation is multiplicative.
#'
#' If `severity_noise = "normal"`, additive normal noise is applied. This may
#' be useful for experimentation, but is generally less natural for large
#' positive claim amounts.
#'
#' `severity_noise_sd` controls the amount of additional severity variation. As
#' a rough guide:
#'
#' - `0.10` provides limited variation;
#' - `0.25` provides moderate variation;
#' - `0.50` provides substantial variation.
#'
#' ## Preserving the total excess loss
#'
#' If `preserve_total_excess = TRUE`, the final allocation is rescaled so that
#' the sum of allocated excess loss equals the total excess loss being
#' allocated.
#'
#' This ensures that credibility blending, bootstrap sampling or other
#' allocation choices do not unintentionally increase or decrease the total
#' excess burden.
#'
#' ## Typical pricing workflow
#'
#' A common workflow is:
#'
#' 1. Use [calculate_excess_loss()] to separate capped and excess losses.
#' 2. Model the base premium using capped claim amounts.
#' 3. Allocate the excess-loss burden using `allocate_excess_loss()`.
#' 4. Add the resulting excess loading back to the technical premium using
#'    [apply_excess_loading()].
#'
#' This approach prevents a small number of large claims from distorting
#' risk-factor relativities while still ensuring that the excess-loss burden is
#' reflected in the final premium.
#'
#' @param data A data.frame, typically the output of [calculate_excess_loss()].
#' @param excess_amount Character string. Column containing the excess claim
#'   amount to allocate.
#' @param allocation_weight Character string. Column used as allocation weight,
#'   typically exposure, premium, insured value or another earned unit.
#' @param risk_factor Optional character string. Risk-factor column used for
#'   `allocation = "risk_factor"` or `allocation = "partial"`.
#' @param allocation_subset Optional character string. Logical column indicating
#'   which rows participate in the allocation. If `NULL`, all rows are included.
#' @param allocation Character string. One of `"portfolio"`, `"risk_factor"` or
#'   `"partial"`.
#' @param credibility Optional numeric scalar between 0 and 1. Used directly
#'   when `allocation = "partial"`. If `NULL`, credibility is calculated from
#'   `credibility_basis` and `credibility_threshold`.
#' @param credibility_basis Character string. Experience basis used when
#'   `credibility = NULL`: `"claims"`, `"excess_claims"` or
#'   `"allocation_weight"`.
#' @param credibility_threshold Positive numeric scalar. Amount of experience
#'   required to reach 50 percent credibility.
#' @param credibility_scale Positive numeric scalar. Multiplies the derived or
#'   supplied credibility before it is capped between 0 and 1.
#' @param method Character string. Either `"observed"` or `"bootstrap"`.
#' @param n_bootstrap Positive whole number. Number of bootstrap samples.
#' @param bootstrap_seed Optional integer seed for reproducible bootstrap
#'   allocation.
#' @param severity_noise Character string. One of `"none"`, `"lognormal"` or
#'   `"normal"`.
#' @param severity_noise_sd Non-negative numeric scalar controlling severity
#'   variation in bootstrap samples.
#' @param preserve_total_excess Logical. If `TRUE`, the final allocation is
#'   rescaled so that the total allocated excess loss equals the total excess
#'   loss being allocated.
#'
#' @return An object of class `"excess_loss_allocation"`.
#'
#' @author Martin Haringa
#'
#' @examples
#' claims <- data.frame(
#'   sector = rep(c("Industry", "Retail"), each = 4),
#'   claim_amount = c(
#'     1000, 120000, 30000, 8000,
#'     2000, 150000, 40000, 6000
#'   ),
#'   earned_exposure = rep(1, 8)
#' )
#'
#' decomposed <- calculate_excess_loss(
#'   claims,
#'   claim_amount = "claim_amount",
#'   threshold = 100000
#' )
#'
#' # Pool all excess losses across the portfolio
#' portfolio_allocation <- allocate_excess_loss(
#'   decomposed,
#'   excess_amount = "excess_claim_amount",
#'   allocation_weight = "earned_exposure",
#'   allocation = "portfolio"
#' )
#'
#' # Allocate excess losses separately by sector
#' sector_allocation <- allocate_excess_loss(
#'   decomposed,
#'   excess_amount = "excess_claim_amount",
#'   allocation_weight = "earned_exposure",
#'   risk_factor = "sector",
#'   allocation = "risk_factor"
#' )
#'
#' # Blend sector and portfolio experience using credibility
#' partial_allocation <- allocate_excess_loss(
#'   decomposed,
#'   excess_amount = "excess_claim_amount",
#'   allocation_weight = "earned_exposure",
#'   risk_factor = "sector",
#'   allocation = "partial",
#'   credibility_basis = "claims",
#'   credibility_threshold = 50
#' )
#'
#' summary(partial_allocation)
#'
#' @export
allocate_excess_loss <- function(data,
                                 excess_amount,
                                 allocation_weight,
                                 risk_factor = NULL,
                                 allocation_subset = NULL,
                                 allocation = c("portfolio", "risk_factor", "partial"),
                                 credibility = NULL,
                                 credibility_basis = c("claims", "excess_claims", "allocation_weight"),
                                 credibility_threshold = 50,
                                 credibility_scale = 1,
                                 method = c("observed", "bootstrap"),
                                 n_bootstrap = 1000,
                                 bootstrap_seed = NULL,
                                 severity_noise = c("none", "lognormal", "normal"),
                                 severity_noise_sd = 0.25,
                                 preserve_total_excess = TRUE) {
  method <- match.arg(method)
  allocation <- match.arg(allocation)
  credibility_basis <- match.arg(credibility_basis)
  severity_noise <- match.arg(severity_noise)
  validate_allocate_excess_loss(
    data, excess_amount, allocation_weight, allocation_subset, risk_factor,
    method, allocation, credibility, credibility_basis, credibility_threshold,
    credibility_scale, n_bootstrap, bootstrap_seed, severity_noise,
    severity_noise_sd, preserve_total_excess
  )

  allocation_data <- prepare_allocation_data(
    data = data,
    excess_amount = excess_amount,
    allocation_weight = allocation_weight,
    allocation_subset = allocation_subset,
    risk_factor = risk_factor
  )
  groups <- summarize_allocation_groups(allocation_data)
  portfolio_loading <- safe_ratio_excess(
    sum(allocation_data$excess_amount[allocation_data$included]),
    sum(allocation_data$weight[allocation_data$included])
  )

  boot <- NULL
  if (identical(method, "bootstrap")) {
    if (!is.null(bootstrap_seed)) {
      withr_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      old_seed <- if (withr_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
      on.exit({
        if (withr_seed) {
          assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      }, add = TRUE)
      set.seed(bootstrap_seed)
    }
    boot <- bootstrap_excess_allocation(
      allocation_data = allocation_data,
      n_bootstrap = n_bootstrap,
      severity_noise = severity_noise,
      severity_noise_sd = severity_noise_sd
    )
    groups <- merge(groups, boot$group_summary, by = "group", all.x = TRUE,
                    sort = FALSE)
    groups$group_loading <- groups$bootstrap_loading_mean
    portfolio_loading <- boot$portfolio_loading
  }

  groups <- derive_final_loading(
    groups = groups,
    allocation = allocation,
    portfolio_loading = portfolio_loading,
    credibility = credibility,
    credibility_basis = credibility_basis,
    credibility_threshold = credibility_threshold,
    credibility_scale = credibility_scale
  )
  allocation_data <- merge(
    allocation_data,
    groups[, c("group", "group_loading", "portfolio_loading", "credibility",
               "allocated_loading"), drop = FALSE],
    by = "group",
    all.x = TRUE,
    sort = FALSE
  )
  allocation_data$allocated_loading[!allocation_data$included] <- 0
  allocation_data$allocated_excess_loss <- allocation_data$allocated_loading *
    allocation_data$weight
  target_excess_loss <- portfolio_loading *
    sum(allocation_data$weight[allocation_data$included])
  if (isTRUE(preserve_total_excess)) {
    allocation_data <- preserve_allocated_total(
      allocation_data = allocation_data,
      target_excess_loss = target_excess_loss
    )
  }

  groups <- summarize_allocated_groups(allocation_data, groups)
  structure(
    list(
      data = allocation_data,
      summary = groups,
      method = method,
      allocation = allocation,
      credibility_basis = credibility_basis,
      credibility_threshold = credibility_threshold,
      credibility_scale = credibility_scale,
      preserve_total_excess = preserve_total_excess,
      bootstrap = boot
    ),
    class = "excess_loss_allocation"
  )
}

#' Apply excess loading to a pricing portfolio
#'
#' Apply an allocated excess-loss loading to a portfolio data set.
#'
#' `apply_excess_loading()` is the final step in the excess-loss pricing workflow.
#' It does not cap claims, estimate excess losses or allocate the excess burden.
#' Instead, it takes the output of [allocate_excess_loss()] and adds the
#' allocated excess component back to the base premium or base rate.
#'
#' The function is typically used after the base premium has been modelled on
#' capped claim amounts. The excess loading then ensures that the cost of claims
#' above the selected threshold is still reflected in the final technical
#' premium.
#'
#' @details
#'
#' ## Premium output
#'
#' With `output = "premium"`, the function adds the allocated excess loss in
#' monetary terms to the base premium:
#'
#' \deqn{
#' loaded\_premium =
#' base\_premium + allocated\_excess\_loss
#' }
#'
#' `allocated_excess_loss` is the row-level monetary amount of excess loss
#' allocated to each risk.
#'
#' ## Rate output
#'
#' With `output = "rate"`, the function adds the allocated excess loading per
#' unit of weight to the base rate:
#'
#' \deqn{
#' loaded\_rate =
#' base\_rate + allocated\_loading
#' }
#'
#' Use this option when the base value represents a rate per exposure, premium
#' unit, insured value or other allocation weight.
#'
#' If the input column supplied through `base_premium` contains premium amounts
#' rather than rates, the function first converts the base premium to a rate:
#'
#' \deqn{
#' base\_rate =
#' \frac{base\_premium}{weight}
#' }
#'
#' ## Interpretation of allocation columns
#'
#' `allocated_excess_loss` represents the monetary excess-loss burden allocated
#' to a row.
#'
#' `allocated_loading` represents the excess loading per unit of allocation
#' weight.
#'
#' In other words:
#'
#' \deqn{
#' allocated\_excess\_loss =
#' allocated\_loading \cdot weight
#' }
#'
#' This distinction is important when moving between premium amounts and rates.
#'
#' ## Typical pricing workflow
#'
#' A common workflow is:
#'
#' 1. Use [calculate_excess_loss()] to separate capped and excess losses.
#' 2. Model the base premium using capped claim amounts.
#' 3. Allocate the excess-loss burden using [allocate_excess_loss()].
#' 4. Use `apply_excess_loading()` to add the allocated excess component back to
#'    the base premium or base rate.
#'
#' This produces a final technical premium that reflects both the modelled
#' capped loss cost and the separately allocated excess-loss burden.
#'
#' @param data A data.frame containing the base premium or base rate.
#' @param allocation An object returned by [allocate_excess_loss()].
#' @param base_premium Character string. Column containing the base premium
#'   amount or base rate before the excess loading is added.
#' @param allocated_excess_loss Optional character string. Column in
#'   `allocation$data` containing the allocated excess-loss amount in monetary
#'   terms. If `NULL`, `allocated_excess_loss` is used.
#' @param allocated_loading Optional character string. Column in
#'   `allocation$data` containing the allocated excess loading per unit of
#'   allocation weight. If `NULL`, `allocated_loading` is used.
#' @param weight Optional character string. Weight column used to convert between
#'   premium amounts and rates when `output = "rate"`.
#' @param output Character string. Use `"premium"` to return premium amounts or
#'   `"rate"` to return rates per unit of weight.
#'
#' @return A data.frame. With `output = "premium"`, the result contains
#'   `base_premium`, `allocated_excess_loss`, `allocated_loading`,
#'   `excess_loading` and `loaded_premium`. With `output = "rate"`, the result
#'   contains `base_rate`, `allocated_loading` and `loaded_rate`.
#'
#' @author Martin Haringa
#'
#' @examples
#' claims <- data.frame(
#'   sector = rep(c("Industry", "Retail"), each = 4),
#'   claim_amount = c(
#'     1000, 120000, 30000, 8000,
#'     2000, 150000, 40000, 6000
#'   ),
#'   earned_exposure = rep(1, 8)
#' )
#'
#' decomposed <- calculate_excess_loss(
#'   claims,
#'   claim_amount = "claim_amount",
#'   threshold = 100000
#' )
#'
#' decomposed$base_premium <- 500
#'
#' allocation <- allocate_excess_loss(
#'   decomposed,
#'   excess_amount = "excess_claim_amount",
#'   allocation_weight = "earned_exposure"
#' )
#'
#' apply_excess_loading(
#'   decomposed,
#'   allocation,
#'   base_premium = "base_premium"
#' )
#'
#' apply_excess_loading(
#'   decomposed,
#'   allocation,
#'   base_premium = "base_premium",
#'   weight = "earned_exposure",
#'   output = "rate"
#' )
#'
#' @export
apply_excess_loading <- function(data,
                                 allocation,
                                 base_premium = "base_premium",
                                 allocated_excess_loss = NULL,
                                 allocated_loading = NULL,
                                 weight = NULL,
                                 output = c("premium", "rate")) {
  output <- match.arg(output)
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!inherits(allocation, "excess_loss_allocation")) {
    stop("`allocation` must be returned by `allocate_excess_loss()`.",
         call. = FALSE)
  }
  if (nrow(data) != nrow(allocation$data)) {
    stop("`data` must have the same number of rows as the allocation data.",
         call. = FALSE)
  }
  validate_character_column(data, base_premium, "base_premium")
  if (!is.numeric(data[[base_premium]]) || any(is.na(data[[base_premium]]))) {
    stop("`base_premium` must refer to a numeric column without missing values.",
         call. = FALSE)
  }
  allocated_excess_loss <- allocated_excess_loss %||% "allocated_excess_loss"
  allocated_loading <- allocated_loading %||% "allocated_loading"
  validate_character_column(allocation$data, allocated_excess_loss,
                            "allocated_excess_loss")
  validate_character_column(allocation$data, allocated_loading,
                            "allocated_loading")
  out <- data
  if (identical(output, "premium")) {
    out$base_premium <- data[[base_premium]]
    out$allocated_excess_loss <- allocation$data[[allocated_excess_loss]]
    out$allocated_loading <- allocation$data[[allocated_loading]]
    out$excess_loading <- out$allocated_excess_loss
    out$loaded_premium <- out$base_premium + out$allocated_excess_loss
    return(out)
  }
  if (is.null(weight)) {
    stop("`weight` must be supplied when `output = 'rate'`.", call. = FALSE)
  }
  validate_character_column(data, weight, "weight")
  if (!is.numeric(data[[weight]]) || any(is.na(data[[weight]])) ||
      any(data[[weight]] <= 0)) {
    stop("`weight` must refer to a positive numeric column without missing values.",
         call. = FALSE)
  }
  out$base_rate <- data[[base_premium]] / data[[weight]]
  out$allocated_loading <- allocation$data[[allocated_loading]]
  out$loaded_rate <- out$base_rate + out$allocated_loading
  out
}

#' Summarise an excess threshold assessment
#'
#' @description
#' Return the key threshold diagnostics from an
#' `"excess_threshold_assessment"` object.
#'
#' @param object An object returned by [assess_excess_threshold()].
#' @param ... Unused.
#'
#' @return A `data.frame`.
#'
#' @author Martin Haringa
#' @keywords internal
#' @export
summary.excess_threshold_assessment <- function(object, ...) {
  .check_dots_empty(...)
  keep <- intersect(
    c(
      "threshold", "group", "n_excess_claims", "excess_loss",
      "excess_loss_ratio", "pure_premium_before", "pure_premium_after",
      "premium_impact"
    ),
    names(object)
  )
  out <- object[, keep, drop = FALSE]
  row.names(out) <- NULL
  out
}

#' Print an excess threshold assessment
#'
#' @description
#' Compact print method for objects returned by [assess_excess_threshold()].
#'
#' @param x An object returned by [assess_excess_threshold()].
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @author Martin Haringa
#' @keywords internal
#' @export
print.excess_threshold_assessment <- function(x, ...) {
  .check_dots_empty(...)
  cat("Excess threshold assessment\n")
  cat("Thresholds: ", length(unique(x$threshold)), "\n", sep = "")
  if ("group" %in% names(x)) {
    cat("Groups: ", length(unique(x$group)), "\n", sep = "")
  }
  cat("Threshold range: ",
      format(min(x$threshold), big.mark = ","), " - ",
      format(max(x$threshold), big.mark = ","), "\n", sep = "")
  invisible(x)
}

#' Summarise an excess-loss allocation
#'
#' @description
#' Return the allocation audit table from an object produced by
#' [allocate_excess_loss()].
#'
#' @param object An object returned by [allocate_excess_loss()].
#' @param compare_to_empirical Logical. If `TRUE`, keep columns with the
#'   empirical loss and empirical excess loss used for comparison.
#' @param ... Unused.
#'
#' @return A `data.frame`.
#'
#' @author Martin Haringa
#' @keywords internal
#' @export
summary.excess_loss_allocation <- function(object,
                                           compare_to_empirical = FALSE,
                                           ...) {
  .check_dots_empty(...)
  out <- object$summary
  if ("group_weight" %in% names(out)) {
    names(out)[names(out) == "group_weight"] <- "weight"
  }
  if (!isTRUE(compare_to_empirical)) {
    drop <- intersect(c("empirical_loss", "empirical_excess_loss"), names(out))
    out <- out[, setdiff(names(out), drop), drop = FALSE]
  }
  preferred <- c(
    "group", "weight", "n_claims", "n_excess_claims",
    "historical_excess_loss", "excess_loss_ratio",
    "credibility_basis", "credibility_experience", "credibility_threshold",
    "credibility",
    "group_loading", "portfolio_loading", "allocated_loading", "allocated_excess_loss",
    "empirical_loss", "empirical_excess_loss"
  )
  out <- out[, c(intersect(preferred, names(out)),
                 setdiff(names(out), preferred)), drop = FALSE]
  row.names(out) <- NULL
  out
}

#' Print an excess-loss allocation
#'
#' @description
#' Compact print method for objects returned by [allocate_excess_loss()].
#'
#' @param x An object returned by [allocate_excess_loss()].
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @author Martin Haringa
#' @keywords internal
#' @export
print.excess_loss_allocation <- function(x, ...) {
  .check_dots_empty(...)
  cat("Excess loss allocation\n")
  cat("Method: ", x$method, "\n", sep = "")
  cat("Allocation: ", x$allocation, "\n", sep = "")
  cat("Groups: ", nrow(x$summary), "\n", sep = "")
  cat("Allocated excess loss: ",
      format(round(sum(x$data$allocated_excess_loss)), big.mark = ","),
      "\n", sep = "")
  invisible(x)
}

#' Plot an excess threshold assessment
#'
#' @description
#' Visualise one diagnostic from an object returned by
#' [assess_excess_threshold()]. The plot helps compare how candidate thresholds
#' affect excess loss, excess claim counts or pure-premium impact.
#'
#' @param object An object returned by [assess_excess_threshold()].
#' @param y Character. Measure to plot on the y-axis.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#'
#' @author Martin Haringa
#' @export
autoplot.excess_threshold_assessment <- function(object,
                                                 y = c(
                                                   "premium_impact",
                                                   "excess_loss",
                                                   "n_excess_claims",
                                                   "excess_loss_ratio"
                                                 ),
                                                 ...) {
  y <- match.arg(y)
  .check_dots_empty(...)
  pal <- .plot_palette_ir()
  grid_theme <- .plot_grid_theme_ir()
  p <- ggplot2::ggplot(object, ggplot2::aes(x = .data[["threshold"]], y = .data[[y]]))
  if ("group" %in% names(object)) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(color = .data[["group"]]), linewidth = 0.6) +
      ggplot2::geom_point(ggplot2::aes(color = .data[["group"]]), size = 1.8)
  } else {
    p <- p +
      ggplot2::geom_line(color = pal$risk_premium, linewidth = 0.6) +
      ggplot2::geom_point(color = pal$risk_premium, size = 1.8)
  }
  p +
    ggplot2::labs(x = "Threshold", y = y, color = NULL) +
    ggplot2::theme_minimal() +
    grid_theme
}

#' Plot an excess-loss allocation
#'
#' @description
#' Visualise the allocated excess loading, allocated excess loss or credibility
#' by allocation group.
#'
#' @param object An object returned by [allocate_excess_loss()].
#' @param y Character. Measure to plot on the y-axis.
#' @param top_n Optional positive whole number. If supplied, only the largest
#'   `top_n` groups by `y` are shown.
#' @param show_labels Logical. If `TRUE`, add direct value labels to the bars.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#'
#' @author Martin Haringa
#' @export
autoplot.excess_loss_allocation <- function(object,
                                            y = c("allocated_loading",
                                                  "allocated_excess_loss",
                                                  "credibility"),
                                            top_n = NULL,
                                            show_labels = FALSE,
                                            ...) {
  y <- match.arg(y)
  .check_dots_empty(...)
  validate_top_n(top_n)
  if (!is.logical(show_labels) || length(show_labels) != 1L ||
      is.na(show_labels)) {
    stop("`show_labels` must be TRUE or FALSE.", call. = FALSE)
  }
  pal <- .plot_palette_ir()
  grid_theme <- .plot_grid_theme_ir()
  plot_data <- object$summary[order(object$summary[[y]], decreasing = TRUE), ,
                              drop = FALSE]
  if (!is.null(top_n)) {
    plot_data <- utils::head(plot_data, top_n)
  }
  plot_data$group <- stats::reorder(plot_data$group, plot_data[[y]])
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[["group"]], y = .data[[y]])) +
    ggplot2::geom_col(fill = pal$risk_premium, width = 0.65) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = y) +
    ggplot2::theme_minimal() +
    grid_theme
  if (isTRUE(show_labels)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = signif(.data[[y]], 3)),
      hjust = -0.15,
      size = 3
    )
  }
  p
}

validate_assess_excess_threshold <- function(data, claim_amount, thresholds,
                                             exposure, group) {
  validate_data_frame(data)
  validate_character_column(data, claim_amount, "claim_amount")
  if (!is.numeric(data[[claim_amount]]) || any(is.na(data[[claim_amount]])) ||
      any(data[[claim_amount]] < 0)) {
    stop("`claim_amount` must refer to a numeric column with non-negative values.",
         call. = FALSE)
  }
  if (!is.numeric(thresholds) || length(thresholds) < 1L ||
      any(!is.finite(thresholds)) || any(thresholds <= 0)) {
    stop("`thresholds` must be positive finite numbers.", call. = FALSE)
  }
  if (!is.null(exposure)) {
    validate_character_column(data, exposure, "exposure")
    if (!is.numeric(data[[exposure]]) || any(is.na(data[[exposure]])) ||
        any(data[[exposure]] < 0)) {
      stop("`exposure` must refer to a numeric column with non-negative values.",
           call. = FALSE)
    }
  }
  if (!is.null(group)) {
    validate_character_column(data, group, "group")
  }
}

validate_calculate_excess_loss <- function(data, claim_amount, threshold) {
  validate_data_frame(data)
  validate_character_column(data, claim_amount, "claim_amount")
  if (!is.numeric(data[[claim_amount]]) || any(is.na(data[[claim_amount]])) ||
      any(data[[claim_amount]] < 0)) {
    stop("`claim_amount` must refer to a numeric column with non-negative values.",
         call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      !is.finite(threshold) || threshold <= 0) {
    stop("`threshold` must be a single positive number.", call. = FALSE)
  }
}

validate_allocate_excess_loss <- function(data, excess_amount,
                                          allocation_weight,
                                          allocation_subset, risk_factor,
                                          method, allocation, credibility,
                                          credibility_basis,
                                          credibility_threshold,
                                          credibility_scale, n_bootstrap,
                                          bootstrap_seed, severity_noise,
                                          severity_noise_sd,
                                          preserve_total_excess) {
  validate_data_frame(data)
  validate_character_column(data, excess_amount, "excess_amount")
  validate_character_column(data, allocation_weight, "allocation_weight")
  if (!is.numeric(data[[excess_amount]]) || any(is.na(data[[excess_amount]])) ||
      any(data[[excess_amount]] < 0)) {
    stop("`excess_amount` must refer to a numeric column with non-negative values.",
         call. = FALSE)
  }
  if (!is.numeric(data[[allocation_weight]]) ||
      any(is.na(data[[allocation_weight]])) ||
      any(data[[allocation_weight]] < 0)) {
    stop("`allocation_weight` must refer to a numeric column with non-negative values.",
         call. = FALSE)
  }
  if (!is.null(allocation_subset)) {
    validate_character_column(data, allocation_subset, "allocation_subset")
    if (!is.logical(data[[allocation_subset]]) ||
        any(is.na(data[[allocation_subset]]))) {
      stop("`allocation_subset` must refer to a logical column without missing values.",
           call. = FALSE)
    }
  }
  if (!is.null(risk_factor)) {
    validate_character_column(data, risk_factor, "risk_factor")
  }
  if (!identical(allocation, "portfolio") && is.null(risk_factor)) {
    stop("`risk_factor` must be supplied when `allocation` is not 'portfolio'.",
         call. = FALSE)
  }
  if (!identical(severity_noise, "none") && !identical(method, "bootstrap")) {
    stop("`severity_noise` is only allowed when `method = 'bootstrap'`.",
         call. = FALSE)
  }
  if (!is.null(credibility) &&
      (!is.numeric(credibility) || length(credibility) != 1L ||
       !is.finite(credibility) || credibility < 0 || credibility > 1)) {
    stop("`credibility` must be NULL or a number between 0 and 1.",
         call. = FALSE)
  }
  if (!is.numeric(credibility_threshold) ||
      length(credibility_threshold) != 1L ||
      !is.finite(credibility_threshold) || credibility_threshold <= 0) {
    stop("`credibility_threshold` must be a single positive number.",
         call. = FALSE)
  }
  if (!is.numeric(credibility_scale) || length(credibility_scale) != 1L ||
      !is.finite(credibility_scale) || credibility_scale < 0) {
    stop("`credibility_scale` must be a single non-negative number.",
         call. = FALSE)
  }
  if (!is.numeric(n_bootstrap) || length(n_bootstrap) != 1L ||
      is.na(n_bootstrap) || n_bootstrap < 1 ||
      n_bootstrap != floor(n_bootstrap)) {
    stop("`n_bootstrap` must be a positive whole number.", call. = FALSE)
  }
  if (!is.null(bootstrap_seed) &&
      (!is.numeric(bootstrap_seed) || length(bootstrap_seed) != 1L ||
       is.na(bootstrap_seed) || bootstrap_seed != floor(bootstrap_seed))) {
    stop("`bootstrap_seed` must be NULL or a single whole number.",
         call. = FALSE)
  }
  if (!is.numeric(severity_noise_sd) || length(severity_noise_sd) != 1L ||
      !is.finite(severity_noise_sd) || severity_noise_sd < 0) {
    stop("`severity_noise_sd` must be a single non-negative number.",
         call. = FALSE)
  }
  if (!is.logical(preserve_total_excess) ||
      length(preserve_total_excess) != 1L ||
      is.na(preserve_total_excess)) {
    stop("`preserve_total_excess` must be TRUE or FALSE.", call. = FALSE)
  }
}

prepare_allocation_data <- function(data, excess_amount, allocation_weight,
                                    allocation_subset, risk_factor) {
  included <- if (is.null(allocation_subset)) {
    rep(TRUE, nrow(data))
  } else {
    data[[allocation_subset]]
  }
  out <- data.frame(
    row_id = seq_len(nrow(data)),
    excess_amount = data[[excess_amount]],
    claim_amount = if ("claim_amount" %in% names(data)) data[["claim_amount"]] else data[[excess_amount]],
    weight = data[[allocation_weight]],
    included = included,
    group = if (is.null(risk_factor)) "portfolio" else as.character(data[[risk_factor]]),
    stringsAsFactors = FALSE
  )
  if (any(out$included & out$weight <= 0)) {
    stop("`allocation_weight` must be positive for included rows.",
         call. = FALSE)
  }
  out
}

summarize_allocation_groups <- function(allocation_data) {
  groups <- split(allocation_data, allocation_data$group)
  out <- lapply(names(groups), function(g) {
    z <- groups[[g]]
    included <- z[z$included, , drop = FALSE]
    historical_excess <- sum(included$excess_amount)
    group_weight <- sum(included$weight)
    empirical_loss <- sum(included$claim_amount)
    data.frame(
      group = g,
      group_weight = group_weight,
      n_claims = nrow(included),
      n_excess_claims = sum(included$excess_amount > 0),
      historical_excess_loss = historical_excess,
      empirical_loss = empirical_loss,
      empirical_excess_loss = historical_excess,
      excess_loss_ratio = safe_ratio_excess(historical_excess, empirical_loss),
      group_loading = safe_ratio_excess(historical_excess, group_weight),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

derive_final_loading <- function(groups, allocation, portfolio_loading,
                                 credibility, credibility_basis,
                                 credibility_threshold, credibility_scale) {
  groups$portfolio_loading <- portfolio_loading
  groups$credibility_basis <- credibility_basis
  groups$credibility_experience <- credibility_experience(groups, credibility_basis)
  groups$credibility_threshold <- credibility_threshold
  if (identical(allocation, "portfolio")) {
    groups$credibility <- 0
    groups$allocated_loading <- portfolio_loading
    return(groups)
  }
  if (identical(allocation, "risk_factor")) {
    groups$credibility <- 1
    groups$allocated_loading <- groups$group_loading
    return(groups)
  }
  base_credibility <- if (is.null(credibility)) {
    automatic_excess_credibility(groups, credibility_basis, credibility_threshold)
  } else {
    credibility
  }
  groups$credibility <- pmin(1, pmax(0, base_credibility * credibility_scale))
  groups$allocated_loading <- groups$credibility * groups$group_loading +
    (1 - groups$credibility) * portfolio_loading
  groups
}

automatic_excess_credibility <- function(groups, credibility_basis,
                                         credibility_threshold) {
  n <- credibility_experience(groups, credibility_basis)
  safe_ratio_excess(n, n + credibility_threshold)
}

credibility_experience <- function(groups, credibility_basis) {
  if (identical(credibility_basis, "claims")) {
    return(groups$n_claims)
  }
  if (identical(credibility_basis, "excess_claims")) {
    return(groups$n_excess_claims)
  }
  groups$group_weight
}

bootstrap_excess_allocation <- function(allocation_data, n_bootstrap,
                                        severity_noise, severity_noise_sd) {
  included <- allocation_data[allocation_data$included, , drop = FALSE]
  tail <- included[included$excess_amount > 0, , drop = FALSE]
  if (nrow(tail) == 0) {
    stop("No positive excess amounts are available for bootstrap allocation.",
         call. = FALSE)
  }
  group_weights <- rowsum(included$weight, included$group, reorder = FALSE)
  total_weight <- sum(included$weight)
  boot_summaries <- replicate(n_bootstrap, {
    sampled <- tail[sample(seq_len(nrow(tail)), nrow(tail), replace = TRUE), ,
                    drop = FALSE]
    sampled$excess_amount <- apply_severity_noise(
      sampled$excess_amount,
      severity_noise = severity_noise,
      severity_noise_sd = severity_noise_sd
    )
    sampled_sum <- rowsum(sampled$excess_amount, sampled$group, reorder = FALSE)
    group_excess <- numeric(nrow(group_weights))
    names(group_excess) <- rownames(group_weights)
    group_excess[rownames(sampled_sum)] <- as.numeric(sampled_sum[, 1])
    data.frame(
      group = names(group_excess),
      group_loading = safe_ratio_excess(group_excess, as.numeric(group_weights[, 1])),
      total_loading = safe_ratio_excess(sum(group_excess), total_weight),
      stringsAsFactors = FALSE
    )
  }, simplify = FALSE)
  all <- do.call(rbind, boot_summaries)
  group_summary <- aggregate_bootstrap_summary(all)
  portfolio_loading <- mean(all$total_loading)
  list(group_summary = group_summary, portfolio_loading = portfolio_loading)
}

apply_severity_noise <- function(x, severity_noise, severity_noise_sd) {
  if (identical(severity_noise, "none") || severity_noise_sd == 0) {
    return(x)
  }
  if (identical(severity_noise, "lognormal")) {
    return(x * exp(stats::rnorm(length(x), mean = 0, sd = severity_noise_sd)))
  }
  pmax(x + stats::rnorm(length(x), mean = 0, sd = severity_noise_sd * stats::sd(x)), 0)
}

aggregate_bootstrap_summary <- function(x) {
  split_x <- split(x, x$group)
  out <- lapply(names(split_x), function(g) {
    z <- split_x[[g]]
    data.frame(
      group = g,
      bootstrap_loading_mean = mean(z$group_loading),
      bootstrap_loading_sd = stats::sd(z$group_loading),
      bootstrap_loading_p05 = stats::quantile(z$group_loading, 0.05, names = FALSE),
      bootstrap_loading_p50 = stats::quantile(z$group_loading, 0.50, names = FALSE),
      bootstrap_loading_p95 = stats::quantile(z$group_loading, 0.95, names = FALSE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

summarize_allocated_groups <- function(allocation_data, groups) {
  allocated <- rowsum(allocation_data$allocated_excess_loss,
                      allocation_data$group,
                      reorder = FALSE)
  groups$allocated_excess_loss <- as.numeric(allocated[groups$group, 1])
  groups
}

preserve_allocated_total <- function(allocation_data, target_excess_loss) {
  included <- allocation_data$included
  raw_total <- sum(allocation_data$allocated_excess_loss[included])
  if (is.na(target_excess_loss) || target_excess_loss < 0) {
    stop("The total excess loss being allocated is invalid.", call. = FALSE)
  }
  if (target_excess_loss == 0) {
    allocation_data$allocated_loading[included] <- 0
    allocation_data$allocated_excess_loss[included] <- 0
    return(allocation_data)
  }
  if (is.na(raw_total) || raw_total <= 0) {
    stop("Allocated excess loss is zero and cannot be rescaled.",
         call. = FALSE)
  }
  scaling_factor <- target_excess_loss / raw_total
  allocation_data$allocated_loading[included] <-
    allocation_data$allocated_loading[included] * scaling_factor
  allocation_data$allocated_excess_loss[included] <-
    allocation_data$allocated_excess_loss[included] * scaling_factor
  allocation_data
}

safe_ratio_excess <- function(num, den) {
  ifelse(is.na(den) | den <= 0, NA_real_, num / den)
}

validate_data_frame <- function(data) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
}

validate_character_column <- function(data, col, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a single non-empty character string.",
         call. = FALSE)
  }
  if (!col %in% names(data)) {
    stop("Column not found in `data`: ", col, call. = FALSE)
  }
}

validate_top_n <- function(top_n) {
  if (is.null(top_n)) {
    return(invisible(TRUE))
  }
  if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) ||
      top_n < 1 || top_n != floor(top_n)) {
    stop("`top_n` must be NULL or a positive whole number.", call. = FALSE)
  }
  invisible(TRUE)
}

.check_dots_empty <- function(...) {
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused argument(s): ", paste(names(dots), collapse = ", "),
         call. = FALSE)
  }
}
