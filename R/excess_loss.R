#' Assess possible excess-loss thresholds
#'
#' @description
#' Compare candidate thresholds for capped severity and large-loss pricing work.
#'
#' `assess_excess_threshold()` is a diagnostic helper. It does not choose a
#' threshold automatically. It shows how many claims, how many records contain
#' claim amounts above candidate thresholds, how much historical claim cost sits
#' above those thresholds, and how much risk premium would remain after capping
#' claims at each threshold.
#'
#' The function is intended for portfolio-level data as well as claim-level
#' data. Portfolio-level data can include policies without claims, for example
#' rows where `claim_count = 0` and the claim amount is zero. Use this before
#' [calculate_excess_loss()] to understand the effect of the threshold on the
#' portfolio. The output is useful for tariff notes, pricing reviews and
#' governance discussions around capped severity models.
#'
#' @param data A `data.frame` with portfolio-level or claim-level observations.
#'   Portfolio-level data can include policies without claims.
#' @param claim_amount Character string. Claim amount column.
#' @param thresholds Numeric vector of candidate thresholds.
#' @param exposure Optional character string. Exposure column. If supplied,
#'   risk premium before and after capping is calculated. The output column keeps
#'   this original name. If `NULL`, every record is counted as one exposure unit
#'   and the output contains an `exposure` column.
#' @param group Optional character string. Grouping column used to assess
#'   thresholds by segment. The output column keeps this original name. If
#'   `NULL`, no grouping column is added.
#' @param claim_count Optional character string. Claim-count column. If
#'   supplied, `n_claims` is calculated as the sum of this column. If `NULL`,
#'   records with `claim_amount > 0` are counted as one claim and records with
#'   `claim_amount == 0` as zero claims.
#'
#' @details
#' The output can be used for two common follow-up analyses. First, aggregate the
#' threshold assessment to portfolio level to calculate the average additional
#' risk premium required to finance the excess layer. Second, after selecting a
#' threshold, compare groups to see which parts of the portfolio benefit most
#' from the excess protection.
#'
#' @return A `data.frame` with class `"threshold_assessment"` and columns:
#' \describe{
#'   \item{group column}{The original grouping column, such as `sector`, if
#'   `group` is supplied. This is the first column when grouping is used.}
#'   \item{`threshold`}{The excess threshold being assessed. Thresholds are
#'   shown in the same order as supplied in the `thresholds` argument.}
#'   \item{exposure column}{The original exposure column, such as
#'   `policy_years`, if `exposure` is supplied. If `exposure = NULL`, this
#'   column is named `exposure` and counts records.}
#'   \item{`n_claims`}{Total number of claims, calculated from `claim_count` or
#'   inferred from `claim_amount > 0`.}
#'   \item{`n_excess_records`}{Number of records with
#'   `claim_amount > threshold`. This counts records, not individual claims.}
#'   \item{`total_loss`}{Total claim amount before applying the threshold.}
#'   \item{`capped_loss`}{Total claim amount retained below or at the
#'   threshold.}
#'   \item{`excess_loss`}{Total claim amount above the threshold.}
#'   \item{`pure_premium_before`}{Risk premium before capping:
#'   `total_loss / exposure`.}
#'   \item{`pure_premium_after`}{Risk premium after capping:
#'   `capped_loss / exposure`.}
#'   \item{`premium_reduction`}{`pure_premium_before - pure_premium_after`,
#'   equivalent to `excess_loss / exposure`. This is positive when applying the
#'   threshold reduces the retained risk premium.}
#'   \item{`premium_reduction_ratio`}{`premium_reduction /
#'   pure_premium_before`. This is between 0 and 1 when
#'   `pure_premium_before > 0`; if `pure_premium_before == 0`, it is defined as
#'   0.}
#' }
#'
#' @author Martin Haringa
#'
#' @examples
#' portfolio <- data.frame(
#'   policy_id = 1:10,
#'   sector = rep(c("Industry", "Retail"), each = 5),
#'   claim_count = c(
#'     0, 1, 1, 1, 1,
#'     0, 1, 1, 1, 1
#'   ),
#'   claim_amount = c(
#'     0, 25000, 120000, 50000, 175000,
#'     0, 40000, 90000, 150000, 300000
#'   ),
#'   policy_years = rep(1, 10)
#' )
#'
#' thresholds <- assess_excess_threshold(
#'   data = portfolio,
#'   claim_amount = "claim_amount",
#'   thresholds = c(25000, 50000, 100000, 150000),
#'   exposure = "policy_years",
#'   group = "sector",
#'   claim_count = "claim_count"
#' )
#'
#' thresholds
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   as_gt(thresholds)
#' }
#'
#' # Calculate the average additional risk premium required to finance
#' # the excess portion of the claims.
#' thresholds |>
#'   dplyr::summarise(
#'     policy_years = sum(policy_years),
#'     excess_loss = sum(excess_loss),
#'     capped_loss = sum(capped_loss),
#'     extra_risk_premium = excess_loss / policy_years,
#'     risk_premium_increase = excess_loss / capped_loss,
#'     .by = "threshold"
#'   )
#'
#' # After selecting a threshold, compare which groups benefit most
#' # from the excess protection.
#' selected_threshold <- thresholds |>
#'   dplyr::filter(threshold == 100000) |>
#'   dplyr::select(
#'     sector,
#'     threshold,
#'     policy_years,
#'     n_claims,
#'     n_excess_records,
#'     premium_reduction,
#'     premium_reduction_ratio
#'   ) |>
#'   dplyr::arrange(dplyr::desc(premium_reduction_ratio))
#'
#' selected_threshold
#'
#' # If claim_count is omitted, records with positive claim amounts are counted.
#' assess_excess_threshold(
#'   data = portfolio,
#'   claim_amount = "claim_amount",
#'   thresholds = 100000,
#'   exposure = "policy_years",
#'   group = "sector"
#' )
#'
#' @export
assess_excess_threshold <- function(data,
                                    claim_amount,
                                    thresholds,
                                    exposure = NULL,
                                    group = NULL,
                                    claim_count = NULL) {
  validate_assess_excess_threshold(
    data = data,
    claim_amount = claim_amount,
    thresholds = thresholds,
    exposure = exposure,
    group = group,
    claim_count = claim_count
  )

  groups <- if (is.null(group)) {
    list(All = seq_len(nrow(data)))
  } else {
    split(seq_len(nrow(data)), as.character(data[[group]]), drop = TRUE)
  }
  if (!is.null(group)) {
    groups <- groups[sort(names(groups))]
  }
  exposure_output <- exposure %||% "exposure"

  out <- lapply(names(groups), function(g) {
    rows <- lapply(thresholds, function(threshold) {
      idx <- groups[[g]]
      amount <- data[[claim_amount]][idx]
      total_loss <- sum(amount)
      capped_loss <- sum(pmin(amount, threshold))
      excess_loss <- sum(pmax(amount - threshold, 0))
      exposure_sum <- if (is.null(exposure)) {
        length(idx)
      } else {
        sum(data[[exposure]][idx])
      }
      n_claims <- if (is.null(claim_count)) {
        sum(amount > 0)
      } else {
        sum(data[[claim_count]][idx])
      }
      pure_premium_before <- safe_ratio_excess(total_loss, exposure_sum)
      pure_premium_after <- safe_ratio_excess(capped_loss, exposure_sum)
      premium_reduction <- pure_premium_before - pure_premium_after
      ans <- data.frame(
        threshold = threshold,
        n_claims = n_claims,
        n_excess_records = sum(amount > threshold),
        total_loss = total_loss,
        capped_loss = capped_loss,
        excess_loss = excess_loss,
        pure_premium_before = pure_premium_before,
        pure_premium_after = pure_premium_after,
        premium_reduction = premium_reduction,
        premium_reduction_ratio = safe_ratio_excess_zero(
          premium_reduction,
          pure_premium_before
        ),
        stringsAsFactors = FALSE
      )
      if (!is.null(group)) {
        ans[[group]] <- data[[group]][idx][1]
      }
      ans[[exposure_output]] <- exposure_sum
      ans
    })
    do.call(rbind, rows)
  })

  out <- do.call(rbind, out)
  preferred <- c(
    group,
    "threshold",
    exposure_output,
    "n_claims",
    "n_excess_records",
    "total_loss",
    "capped_loss",
    "excess_loss",
    "pure_premium_before",
    "pure_premium_after",
    "premium_reduction",
    "premium_reduction_ratio"
  )
  out <- out[, intersect(preferred, names(out)), drop = FALSE]
  row.names(out) <- NULL
  attr(out, "claim_amount") <- claim_amount
  attr(out, "exposure") <- exposure
  attr(out, "group") <- group
  attr(out, "claim_count") <- claim_count
  class(out) <- c("threshold_assessment", "data.frame")
  out
}

#' Decompose claim amounts into capped and excess parts
#'
#' Large claims can distort risk-factor relativities and make pricing models
#' unstable. `calculate_excess_loss()` separates each row in a portfolio into a
#' capped claim amount and an excess part above a selected threshold.
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
#' claim\_amount\_capped +
#' claim\_amount\_excess
#' }
#'
#' where:
#'
#' \deqn{
#' claim\_amount\_excess =
#' max(claim\_amount - threshold, 0)
#' }
#'
#' and:
#'
#' \deqn{
#' claim\_amount\_capped =
#' min(claim\_amount, threshold)
#' }
#'
#' The output column names are derived from the column supplied through
#' `claim_amount`. For example, if `claim_amount = "incurred_loss"`, the added
#' columns are `incurred_loss_capped`, `incurred_loss_excess` and
#' `incurred_loss_is_excess`.
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
#' @param data A data.frame with portfolio-level or claim-level observations.
#'   Portfolio-level data can include policies without claims, for example rows
#'   where `n_claims = 0` and the claim amount is zero.
#' @param claim_amount Character string. Claim amount column.
#' @param threshold Positive numeric scalar. Claims above this value contribute
#'   to the excess component. Claims below the threshold remain fully included
#'   in the capped claim amount.
#'
#' @return A data.frame with the original data and three added columns. The
#'   names are derived from `claim_amount`: `<claim_amount>_capped`,
#'   `<claim_amount>_excess` and `<claim_amount>_is_excess`.
#'
#' @author Martin Haringa
#'
#' @examples
#' portfolio <- data.frame(
#'   policy_id = 1:4,
#'   n_claims = c(0, 1, 1, 0),
#'   claim_amount = c(0, 120000, 30000, 0)
#' )
#'
#' calculate_excess_loss(
#'   portfolio,
#'   claim_amount = "claim_amount",
#'   threshold = 100000
#' )
#'
#' @export
calculate_excess_loss <- function(data, claim_amount, threshold) {
  validate_calculate_excess_loss(data, claim_amount, threshold)
  amount <- data[[claim_amount]]
  capped_col <- paste0(claim_amount, "_capped")
  excess_col <- paste0(claim_amount, "_excess")
  indicator_col <- paste0(claim_amount, "_is_excess")

  added_cols <- c(capped_col, excess_col, indicator_col)
  existing_cols <- intersect(added_cols, names(data))
  if (length(existing_cols) > 0) {
    stop(
      "Output column names already exist in `data`: ",
      paste(existing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  out <- data
  out[[capped_col]] <- pmin(amount, threshold)
  out[[excess_col]] <- pmax(amount - threshold, 0)
  out[[indicator_col]] <- amount > threshold
  attr(out, "claim_amount_column") <- claim_amount
  attr(out, "claim_amount_capped_column") <- capped_col
  attr(out, "claim_amount_excess_column") <- excess_col
  attr(out, "claim_amount_is_excess_column") <- indicator_col
  attr(out, "threshold") <- threshold
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
#'   For portfolio allocation, no risk-factor-level experience is used.
#'   Therefore, `risk_factor_credibility` equals zero and
#'   `blended_excess_loading` equals `portfolio_excess_loading`. The same output
#'   columns are kept for all allocation methods so downstream reporting code
#'   can use one consistent structure.
#'
#' - `"risk_factor"`: excess losses are allocated separately for each
#'   risk-factor level. The excess burden observed within a group is spread
#'   across all risks in that group and is not shared with other groups.
#'
#'   This produces the strongest link between excess loadings and observed
#'   group experience, but can lead to volatile results when excess losses are
#'   rare.
#'
#'   For risk-factor allocation, only risk-factor-level experience is used.
#'   Therefore, `sector_credibility` equals one and `blended_excess_loading`
#'   equals `sector_excess_loading` when `risk_factor = "sector"`.
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
#' The blended excess loading is calculated as:
#'
#' \deqn{
#' blended\_excess\_loading_g =
#' sector\_credibility_g \cdot sector\_excess\_loading_g +
#' (1 - sector\_credibility_g) \cdot portfolio\_excess\_loading
#' }
#'
#' In the output, `sector_excess_loading` is replaced by
#' `<risk_factor>_excess_loading` and `sector_credibility` by
#' `<risk_factor>_credibility` when another risk-factor column is supplied.
#' `expected_excess_loss` is then calculated as
#' `blended_excess_loading * allocation_weight`.
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
#' @param excess_amount Optional character string. Column containing the excess
#'   claim amount to allocate. If `NULL`, the function uses the
#'   `claim_amount_excess_column` metadata created by [calculate_excess_loss()].
#' @param allocation_weight Character string. Column used as allocation weight,
#'   typically exposure, premium, insured value or another earned unit.
#' @param risk_factor Optional character string. Risk-factor column used for
#'   `allocation = "risk_factor"` or `allocation = "partial"`.
#' @param receives_allocation Optional character string. Name of a logical column
#'   indicating which rows receive a share of the total excess loss. Rows with
#'   `FALSE` receive no allocation, but their excess losses still contribute to
#'   the total amount being allocated. If `NULL`, all rows receive an
#'   allocation.
#' @param claim_count Optional character string. Claim-count column. If
#'   supplied, claim counts in the allocation summary are calculated as the sum
#'   of this column. If `NULL`, claim counts are inferred from the original
#'   claim amount column when available: rows with a positive claim amount count
#'   as one claim and rows with zero claim amount count as zero claims.
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
#' @return The input `data` enriched with allocation columns and class
#'   `"excess_allocation"`. The object prints as an ordinary data frame and has
#'   a custom [summary.excess_allocation()] method for aggregated allocation
#'   statistics. Original rows, columns, row order and metadata from
#'   [calculate_excess_loss()] are preserved. The added columns are:
#'   \describe{
#'     \item{`receives_allocation`}{Logical indicator showing whether the row
#'     receives a share of the total excess loss. This does not indicate whether
#'     the row contributed to the observed excess-loss amount: all excess losses
#'     can contribute to the total excess loss, while `receives_allocation`
#'     controls the target rows over which that total is redistributed. When the
#'     `receives_allocation` argument is supplied, rows receive allocation when
#'     the referenced logical column is `TRUE` and the allocation weight is
#'     positive. Rows with `FALSE` receive no allocation, but their excess
#'     losses still contribute to the total amount being allocated.}
#'     \item{`<risk_factor>_excess_loading`}{Excess loading estimated from the
#'     experience of the risk-factor level. For example, `risk_factor =
#'     "sector"` creates `sector_excess_loading`. When no risk factor is
#'     supplied, the column is named `risk_factor_excess_loading`.}
#'     \item{`<risk_factor>_credibility`}{Credibility weight assigned to the
#'     risk-factor-level estimate. For example, `risk_factor = "sector"`
#'     creates `sector_credibility`.}
#'     \item{`portfolio_excess_loading`}{Portfolio-level excess loading per
#'     unit of allocation weight.}
#'     \item{`blended_excess_loading`}{Credibility-weighted blend of the
#'     risk-factor and portfolio excess loadings. For `risk_factor = "sector"`,
#'     this is calculated as `sector_excess_loading * sector_credibility +
#'     portfolio_excess_loading * (1 - sector_credibility)`.}
#'     \item{`expected_excess_loss`}{Row-level expected excess-loss amount,
#'     calculated as `blended_excess_loading * allocation_weight`.}
#'   }
#'
#' @seealso [summary.excess_allocation()]
#'
#' @author Martin Haringa
#'
#' @examples
#' portfolio <- data.frame(
#'   policy_id = 1:10,
#'   sector = rep(c("Industry", "Retail"), each = 5),
#'   claim_count = c(0, 1, 1, 1, 1, 0, 1, 1, 1, 1),
#'   claim_amount = c(
#'     0, 25000, 120000, 50000, 175000,
#'     0, 40000, 90000, 150000, 750000
#'   ),
#'   earned_exposure = rep(1, 10)
#' )
#'
#' decomposed <- calculate_excess_loss(
#'   portfolio,
#'   claim_amount = "claim_amount",
#'   threshold = 100000
#' )
#'
#' # Pool all excess losses across the portfolio
#' portfolio_allocation <- allocate_excess_loss(
#'   decomposed,
#'   allocation_weight = "earned_exposure",
#'   claim_count = "claim_count",
#'   allocation = "portfolio"
#' )
#' # No sector-level experience is used here: risk_factor_credibility is zero
#' # and blended_excess_loading equals portfolio_excess_loading.
#'
#' # Allocate excess losses separately by sector
#' sector_allocation <- allocate_excess_loss(
#'   decomposed,
#'   allocation_weight = "earned_exposure",
#'   claim_count = "claim_count",
#'   risk_factor = "sector",
#'   allocation = "risk_factor"
#' )
#' # Only sector-level experience is used here: sector_credibility is one
#' # and blended_excess_loading equals sector_excess_loading.
#'
#' # Blend sector and portfolio experience using credibility
#' partial_allocation <- allocate_excess_loss(
#'   decomposed,
#'   allocation_weight = "earned_exposure",
#'   claim_count = "claim_count",
#'   risk_factor = "sector",
#'   allocation = "partial",
#'   credibility_basis = "claims",
#'   credibility_threshold = 50
#' )
#'
#' summary(partial_allocation)
#'
#' # Allocate excess loss by insured amount, restricted to policies with an
#' # insured amount of at least 500,000.
#' portfolio <- portfolio |>
#'   dplyr::mutate(
#'     insured_amount = c(
#'       100000, 250000, 500000, 750000, 1000000,
#'       1500000, 2500000, 5000000, 7500000, 10000000
#'     ),
#'     receives_allocation = insured_amount >= 500000,
#'     allocation_weight = insured_amount * earned_exposure
#'   )
#'
#' decomposed <- calculate_excess_loss(
#'   portfolio,
#'   claim_amount = "claim_amount",
#'   threshold = 500000
#' )
#' insured_amount_allocation <- allocate_excess_loss(
#'   decomposed,
#'   allocation = "portfolio",
#'   allocation_weight = "allocation_weight",
#'   receives_allocation = "receives_allocation"
#' )
#' insured_amount_allocation
#'
#' # All losses above the threshold contribute to the total excess loss, but
#' # only rows with receives_allocation = TRUE receive a share. The share is
#' # proportional to insured_amount * earned_exposure. A larger insured amount
#' # therefore produces a larger absolute expected_excess_loss, while a policy
#' # with the same insured amount but lower earned exposure receives
#' # proportionally less. For receiving policies this gives one constant excess
#' # loading as a percentage of insured amount, adjusted for earned exposure.
#' # No risk-factor-level experience is used in this portfolio allocation.
#'
#' @export
allocate_excess_loss <- function(data,
                                 excess_amount = NULL,
                                 allocation_weight,
                                 risk_factor = NULL,
                                 receives_allocation = NULL,
                                 claim_count = NULL,
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
  excess_amount <- resolve_excess_amount_column(data, excess_amount)
  validate_allocate_excess_loss(
    data, excess_amount, allocation_weight, receives_allocation, claim_count,
    risk_factor,
    method, allocation, credibility, credibility_basis, credibility_threshold,
    credibility_scale, n_bootstrap, bootstrap_seed, severity_noise,
    severity_noise_sd, preserve_total_excess
  )

  allocation_data <- prepare_allocation_data(
    data = data,
    excess_amount = excess_amount,
    allocation_weight = allocation_weight,
    receives_allocation = receives_allocation,
    claim_count = claim_count,
    risk_factor = risk_factor
  )
  groups <- summarize_allocation_groups(allocation_data)
  portfolio_loading <- safe_ratio_excess(
    sum(allocation_data$excess_amount),
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
  out <- build_excess_allocation_output(
    data = data,
    allocation_data = allocation_data,
    risk_factor = risk_factor,
    receives_allocation_col = receives_allocation
  )
  attr(out, "summary") <- groups
  attr(out, "method") <- method
  attr(out, "allocation") <- allocation
  attr(out, "credibility_basis") <- credibility_basis
  attr(out, "credibility_threshold") <- credibility_threshold
  attr(out, "credibility_scale") <- credibility_scale
  attr(out, "preserve_total_excess") <- preserve_total_excess
  attr(out, "bootstrap") <- boot
  attr(out, "claim_count") <- claim_count
  attr(out, "receives_allocation") <- receives_allocation
  attr(out, "excess_amount") <- excess_amount
  attr(out, "allocation_weight") <- allocation_weight
  attr(out, "risk_factor") <- risk_factor
  class(out) <- c("excess_allocation", "data.frame")
  out
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
#' `expected_excess_loss` is the row-level monetary amount of excess loss
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
#' `expected_excess_loss` represents the monetary excess-loss burden allocated
#' to a row.
#'
#' `blended_excess_loading` represents the excess loading per unit of allocation
#' weight.
#'
#' In other words:
#'
#' \deqn{
#' expected\_excess\_loss =
#' blended\_excess\_loading \cdot weight
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
#' @param expected_excess_loss Optional character string. Column in
#'   `allocation` containing the expected excess-loss amount in monetary terms.
#'   If `NULL`, `expected_excess_loss` is used.
#' @param blended_excess_loading Optional character string. Column in
#'   `allocation` containing the blended excess loading per unit of
#'   allocation weight. If `NULL`, `blended_excess_loading` is used.
#' @param weight Optional character string. Weight column used to convert between
#'   premium amounts and rates when `output = "rate"`.
#' @param output Character string. Use `"premium"` to return premium amounts or
#'   `"rate"` to return rates per unit of weight.
#'
#' @return A data.frame. With `output = "premium"`, the result contains
#'   `base_premium`, `expected_excess_loss`, `blended_excess_loading`,
#'   `excess_loading` and `loaded_premium`. With `output = "rate"`, the result
#'   contains `base_rate`, `blended_excess_loading` and `loaded_rate`.
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
#'   excess_amount = "claim_amount_excess",
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
                                 expected_excess_loss = NULL,
                                 blended_excess_loading = NULL,
                                 weight = NULL,
                                 output = c("premium", "rate")) {
  output <- match.arg(output)
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!inherits(allocation, "excess_allocation")) {
    stop("`allocation` must be returned by `allocate_excess_loss()`.",
         call. = FALSE)
  }
  if (nrow(data) != nrow(allocation)) {
    stop("`data` must have the same number of rows as the allocation data.",
         call. = FALSE)
  }
  validate_character_column(data, base_premium, "base_premium")
  if (!is.numeric(data[[base_premium]]) || any(is.na(data[[base_premium]]))) {
    stop("`base_premium` must refer to a numeric column without missing values.",
         call. = FALSE)
  }
  expected_excess_loss <- expected_excess_loss %||% "expected_excess_loss"
  blended_excess_loading <- blended_excess_loading %||% "blended_excess_loading"
  validate_character_column(allocation, expected_excess_loss,
                            "expected_excess_loss")
  validate_character_column(allocation, blended_excess_loading,
                            "blended_excess_loading")
  out <- data
  if (identical(output, "premium")) {
    out$base_premium <- data[[base_premium]]
    out$expected_excess_loss <- allocation[[expected_excess_loss]]
    out$blended_excess_loading <- allocation[[blended_excess_loading]]
    out$excess_loading <- out$expected_excess_loss
    out$loaded_premium <- out$base_premium + out$expected_excess_loss
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
  out$blended_excess_loading <- allocation[[blended_excess_loading]]
  out$loaded_rate <- out$base_rate + out$blended_excess_loading
  out
}

#' Summarise an excess-loss allocation
#'
#' @description
#' Return the allocation audit table from an object produced by
#' [allocate_excess_loss()].
#'
#' @param object An object returned by [allocate_excess_loss()].
#' @param compare_to_empirical Logical. If `TRUE`, append
#'   `allocation_difference` and `allocation_difference_ratio` to compare the
#'   credibility-weighted allocation with historically observed excess loss.
#' @param ... Unused.
#'
#' @return A `data.frame` with aggregated allocation statistics. The returned
#'   table has one row per risk-factor level. The original risk-factor and
#'   allocation-weight names are preserved. "Observed" refers to historical
#'   excess loss in the input data; "allocated" refers to the
#'   credibility-weighted allocation. Loadings are amounts per unit of
#'   allocation weight and losses are total amounts for the level. The returned
#'   columns are:
#'   \describe{
#'     \item{`<risk_factor>`}{Risk-factor level. For example, `risk_factor =
#'     "sector"` returns a `sector` column. When no risk factor is supplied,
#'     this column is named `risk_factor` and contains `"portfolio"`.}
#'     \item{`<allocation_weight>`}{Total allocation weight for the level. For
#'     example, `allocation_weight = "earned_exposure"` returns an
#'     `earned_exposure` column.}
#'     \item{`<claim_count>`}{Number of claims in the level. When `claim_count`
#'     is supplied, its original column name is preserved. When
#'     `claim_count = NULL`, inferred counts are returned as `claim_count`.}
#'     \item{`excess_claim_count`}{Number of claim records with excess loss.}
#'     \item{`observed_excess_loss`}{Historically observed excess loss.}
#'     \item{`observed_excess_loading`}{Observed excess loss per unit of
#'     allocation weight.}
#'     \item{`<risk_factor>_excess_loading`}{Risk-factor-specific excess
#'     loading before pooling. When no risk factor is supplied, this column is
#'     named `risk_factor_excess_loading`.}
#'     \item{`<risk_factor>_credibility`}{Credibility weight assigned to the
#'     risk-factor-level estimate.}
#'     \item{`portfolio_excess_loading`}{Portfolio-level excess loading per unit
#'     of allocation weight.}
#'     \item{`blended_excess_loading`}{Credibility-weighted blend of the
#'     risk-factor and portfolio estimates. For `risk_factor = "sector"`, this
#'     is calculated as `sector_excess_loading * sector_credibility +
#'     portfolio_excess_loading * (1 - sector_credibility)`.}
#'     \item{`expected_excess_loss`}{Total expected excess loss allocated to the
#'     level, calculated as `blended_excess_loading * <allocation_weight>`.}
#'     \item{`allocation_difference`}{Expected minus observed excess loss,
#'     included when `compare_to_empirical = TRUE`. A positive value means the
#'     level receives more allocated excess loss than it generated historically.}
#'     \item{`allocation_difference_ratio`}{Allocation difference relative to
#'     observed excess loss, included when `compare_to_empirical = TRUE`. When
#'     observed excess loss is zero this value is `NA_real_`, because a relative
#'     comparison with zero observed excess loss is undefined.}
#'   }
#'
#' @seealso [allocate_excess_loss()]
#'
#' @author Martin Haringa
#' @keywords internal
#' @export
summary.excess_allocation <- function(object,
                                           compare_to_empirical = FALSE,
                                           ...) {
  .check_dots_empty(...)
  out <- attr(object, "summary", exact = TRUE)
  if (is.null(out)) {
    stop("Allocation summary is not available on `object`.", call. = FALSE)
  }
  risk_factor <- attr(object, "risk_factor", exact = TRUE)
  risk_factor_col <- if (is.null(risk_factor)) "risk_factor" else risk_factor
  allocation_weight <- attr(object, "allocation_weight", exact = TRUE)
  allocation_weight_col <- if (is.null(allocation_weight)) {
    "allocation_weight"
  } else {
    allocation_weight
  }
  claim_count_col <- attr(object, "claim_count", exact = TRUE)
  if (is.null(claim_count_col)) {
    claim_count_col <- "claim_count"
  }
  risk_factor_loading_col <- paste0(risk_factor_col, "_excess_loading")
  risk_factor_credibility_col <- paste0(risk_factor_col, "_credibility")
  if ("group_value" %in% names(out)) {
    out[[risk_factor_col]] <- out$group_value
  } else if ("group" %in% names(out)) {
    out[[risk_factor_col]] <- out$group
  }
  out[[allocation_weight_col]] <- out$group_weight
  out[[claim_count_col]] <- out$n_claims
  out$excess_claim_count <- out$n_excess_claims
  out$observed_excess_loss <- out$historical_excess_loss
  out$observed_excess_loading <- safe_ratio_excess_zero(
    out$observed_excess_loss,
    out[[allocation_weight_col]]
  )
  out[[risk_factor_loading_col]] <- out$group_loading
  out[[risk_factor_credibility_col]] <- out$credibility
  out$portfolio_excess_loading <- out$portfolio_loading
  out$blended_excess_loading <- out$allocated_loading
  out$expected_excess_loss <- out$allocated_excess_loss
  if (isTRUE(compare_to_empirical)) {
    out$allocation_difference <- out$expected_excess_loss -
      out$observed_excess_loss
    out$allocation_difference_ratio <- ifelse(
      out$observed_excess_loss == 0,
      NA_real_,
      out$allocation_difference / out$observed_excess_loss
    )
  }
  preferred <- c(
    risk_factor_col, allocation_weight_col,
    claim_count_col, "excess_claim_count",
    "observed_excess_loss", "observed_excess_loading",
    risk_factor_loading_col, risk_factor_credibility_col,
    "portfolio_excess_loading", "blended_excess_loading",
    "expected_excess_loss"
  )
  if (isTRUE(compare_to_empirical)) {
    preferred <- c(preferred, "allocation_difference",
                   "allocation_difference_ratio")
  }
  out <- out[, intersect(preferred, names(out)), drop = FALSE]
  attr(out, "credibility_basis") <- attr(object, "credibility_basis", exact = TRUE)
  attr(out, "credibility_threshold") <- attr(object, "credibility_threshold", exact = TRUE)
  attr(out, "risk_factor") <- risk_factor
  attr(out, "allocation_weight") <- allocation_weight
  attr(out, "allocation") <- attr(object, "allocation", exact = TRUE)
  attr(out, "method") <- attr(object, "method", exact = TRUE)
  row.names(out) <- NULL
  out
}

#' Convert an object to a gt table
#'
#' @description
#' Generic presentation helper. Methods return a `gt` table for objects where a
#' formatted reporting table is more useful than another plot.
#'
#' @param x An object.
#' @param ... Arguments passed to methods.
#'
#' @return A `gt_tbl` object for supported methods.
#'
#' @author Martin Haringa
#' @export
as_gt <- function(x, ...) {
  UseMethod("as_gt")
}

#' Present an excess threshold assessment as a gt table
#'
#' @description
#' Create a formatted `gt` table from an object returned by
#' [assess_excess_threshold()]. The original object remains a regular
#' `data.frame` subclass; `as_gt()` is only used when a presentation table is
#' needed for a report, tariff note or pricing review.
#'
#' @param x An object returned by [assess_excess_threshold()].
#' @param claims Logical. If `TRUE`, include claim-count columns.
#' @param loss Logical. If `TRUE`, include loss amount columns. The default is
#'   `FALSE` to keep the threshold comparison compact.
#' @param premium Logical. If `TRUE`, include pure-premium and premium-reduction
#'   columns.
#' @param locale Character. Locale used for number formatting, for example
#'   `"nl-NL"` or `"en-US"`.
#' @param loss_decimals,premium_decimals,ratio_decimals Non-negative whole
#'   numbers controlling displayed decimals for loss amounts, premium amounts
#'   and percentage ratios.
#' @param color_last_column Logical. If `TRUE`, color the final displayed
#'   column from white to yellow so the highest values stand out in the
#'   presentation table.
#' @param title Optional character. Table title. If `NULL`, no table title is
#'   added.
#' @param subtitle Optional character. Table subtitle. If `NULL`, no table
#'   subtitle is added.
#' @param ... Unused.
#'
#' @return A `gt_tbl` object.
#'
#' @author Martin Haringa
#' @rdname as_gt
#'
#' @examples
#' portfolio <- data.frame(
#'   policy_id = 1:10,
#'   sector = rep(c("Industry", "Retail"), each = 5),
#'   claim_count = c(
#'     0, 1, 1, 1, 1,
#'     0, 1, 1, 1, 1
#'   ),
#'   claim_amount = c(
#'     0, 25000, 120000, 50000, 175000,
#'     0, 40000, 90000, 150000, 300000
#'   ),
#'   policy_years = rep(1, 10)
#' )
#'
#' thresholds <- assess_excess_threshold(
#'   data = portfolio,
#'   claim_amount = "claim_amount",
#'   thresholds = c(25000, 50000, 100000, 150000),
#'   exposure = "policy_years",
#'   group = "sector",
#'   claim_count = "claim_count"
#' )
#'
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   as_gt(thresholds)
#' }
#'
#' @export
as_gt.threshold_assessment <- function(x,
                                       claims = TRUE,
                                       loss = FALSE,
                                       premium = TRUE,
                                       locale = "nl-NL",
                                       loss_decimals = 0,
                                       premium_decimals = 0,
                                       ratio_decimals = 1,
                                       color_last_column = TRUE,
                                       title = NULL,
                                       subtitle = NULL,
                                       ...) {
  rlang::check_installed("gt")
  .check_dots_empty(...)
  validate_as_gt_threshold_assessment(
    x = x,
    claims = claims,
    loss = loss,
    premium = premium,
    locale = locale,
    loss_decimals = loss_decimals,
    premium_decimals = premium_decimals,
    ratio_decimals = ratio_decimals,
    color_last_column = color_last_column,
    title = title,
    subtitle = subtitle
  )

  group_col <- attr(x, "group", exact = TRUE)
  exposure_attr <- attr(x, "exposure", exact = TRUE)
  exposure_col <- exposure_attr %||% "exposure"

  display_cols <- c(group_col, "threshold", exposure_col)
  claims_cols <- c("n_claims", "n_excess_records")
  loss_cols <- c("total_loss", "capped_loss", "excess_loss")
  premium_cols <- c(
    "pure_premium_before",
    "pure_premium_after",
    "premium_reduction",
    "premium_reduction_ratio"
  )
  if (claims) display_cols <- c(display_cols, claims_cols)
  if (loss) display_cols <- c(display_cols, loss_cols)
  if (premium) display_cols <- c(display_cols, premium_cols)

  table_data <- x[, display_cols, drop = FALSE]
  if (!is.null(group_col)) {
    out <- gt::gt(
      data = table_data,
      groupname_col = group_col,
      row_group_as_column = TRUE,
      locale = locale
    )
  } else {
    out <- gt::gt(data = table_data, locale = locale)
  }

  labels <- c(
    threshold = "Threshold",
    n_claims = "Count",
    n_excess_records = "Above threshold",
    total_loss = "Total",
    capped_loss = "Retained",
    excess_loss = "Excess",
    pure_premium_before = "Before",
    pure_premium_after = "After",
    premium_reduction = "Reduction",
    premium_reduction_ratio = "Reduction (%)"
  )
  labels[[exposure_col]] <- threshold_pretty_label(exposure_col)
  if (!is.null(group_col)) {
    labels[[group_col]] <- threshold_pretty_label(group_col)
  }
  label_args <- stats::setNames(as.list(labels[intersect(names(labels), display_cols)]),
                                intersect(names(labels), display_cols))

  out <- do.call(gt::cols_label, c(list(.data = out), label_args))
  if (claims) {
    out <- gt::tab_spanner(out, label = "Claims", columns = claims_cols)
  }
  if (loss) {
    out <- gt::tab_spanner(out, label = "Loss", columns = loss_cols)
  }
  if (premium) {
    out <- gt::tab_spanner(out, label = "Risk premium", columns = premium_cols)
  }

  out <- gt::fmt_number(
    out,
    columns = "threshold",
    decimals = 0,
    locale = locale
  )
  out <- gt::fmt_number(
    out,
    columns = exposure_col,
    decimals = 0,
    locale = locale
  )
  if (claims) {
    out <- gt::fmt_integer(out, columns = claims_cols, locale = locale)
  }
  if (loss) {
    out <- gt::fmt_number(
      out,
      columns = loss_cols,
      decimals = loss_decimals,
      locale = locale
    )
  }
  if (premium) {
    out <- gt::fmt_number(
      out,
      columns = premium_cols[1:3],
      decimals = premium_decimals,
      locale = locale
    )
    out <- gt::fmt_percent(
      out,
      columns = "premium_reduction_ratio",
      decimals = ratio_decimals,
      locale = locale
    )
  }
  if (color_last_column) {
    out <- gt::data_color(
      out,
      columns = utils::tail(display_cols, 1),
      palette = c("white", "#F7E94D")
    )
  }
  if (!is.null(title) || !is.null(subtitle)) {
    out <- gt::tab_header(out, title = title, subtitle = subtitle)
  }
  out
}

#' Plot an excess-loss allocation
#'
#' @description
#' Visualise the blended excess loading, expected excess loss or credibility by
#' allocation group.
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
autoplot.excess_allocation <- function(object,
                                            y = c("blended_excess_loading",
                                                  "expected_excess_loss",
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
  summary_data <- summary(object)
  group_col <- attr(object, "risk_factor", exact = TRUE)
  if (is.null(group_col)) {
    group_col <- "risk_factor"
  }
  if (identical(y, "credibility")) {
    y <- paste0(group_col, "_credibility")
  }
  plot_data <- summary_data[order(summary_data[[y]], decreasing = TRUE), ,
                            drop = FALSE]
  if (!is.null(top_n)) {
    plot_data <- utils::head(plot_data, top_n)
  }
  plot_data[[group_col]] <- stats::reorder(plot_data[[group_col]], plot_data[[y]])
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[group_col]], y = .data[[y]])) +
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
                                             exposure, group, claim_count) {
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
  if (!is.null(claim_count)) {
    validate_character_column(data, claim_count, "claim_count")
    if (!is.numeric(data[[claim_count]]) ||
        any(is.na(data[[claim_count]])) ||
        any(!is.finite(data[[claim_count]])) ||
        any(data[[claim_count]] < 0)) {
      stop(
        "`claim_count` must refer to a numeric column with finite non-negative values.",
        call. = FALSE
      )
    }
  }
}

validate_as_gt_threshold_assessment <- function(x, claims, loss, premium,
                                                locale, loss_decimals,
                                                premium_decimals,
                                                ratio_decimals,
                                                color_last_column,
                                                title, subtitle) {
  if (!inherits(x, "threshold_assessment")) {
    stop("`x` must be an object returned by `assess_excess_threshold()`.",
         call. = FALSE)
  }
  validate_single_logical(claims, "claims")
  validate_single_logical(loss, "loss")
  validate_single_logical(premium, "premium")
  validate_single_logical(color_last_column, "color_last_column")
  if (!isTRUE(claims) && !isTRUE(loss) && !isTRUE(premium)) {
    stop("At least one of `claims`, `loss` or `premium` must be TRUE.",
         call. = FALSE)
  }
  validate_single_character(locale, "locale")
  validate_decimal_count(loss_decimals, "loss_decimals")
  validate_decimal_count(premium_decimals, "premium_decimals")
  validate_decimal_count(ratio_decimals, "ratio_decimals")
  if (!is.null(title)) {
    validate_single_character(title, "title")
  }
  if (!is.null(subtitle)) {
    validate_single_character(subtitle, "subtitle")
  }

  group_col <- attr(x, "group", exact = TRUE)
  exposure_col <- attr(x, "exposure", exact = TRUE) %||% "exposure"
  if (!is.null(group_col) &&
      (!is.character(group_col) || length(group_col) != 1L ||
       is.na(group_col) || !group_col %in% names(x))) {
    stop("The grouping column stored in `attr(x, 'group')` is not available.",
         call. = FALSE)
  }
  if (!is.character(exposure_col) || length(exposure_col) != 1L ||
      is.na(exposure_col) || !exposure_col %in% names(x)) {
    stop("The exposure column stored in `attr(x, 'exposure')` is not available.",
         call. = FALSE)
  }

  required <- c(group_col, "threshold", exposure_col)
  if (claims) required <- c(required, "n_claims", "n_excess_records")
  if (loss) required <- c(required, "total_loss", "capped_loss", "excess_loss")
  if (premium) {
    required <- c(
      required,
      "pure_premium_before",
      "pure_premium_after",
      "premium_reduction",
      "premium_reduction_ratio"
    )
  }
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    stop(
      "Required column(s) missing from `x`: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

validate_single_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }
}

validate_single_character <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || x == "") {
    stop("`", arg, "` must be a single non-missing character value.",
         call. = FALSE)
  }
}

validate_decimal_count <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < 0 || x != floor(x)) {
    stop("`", arg, "` must be a single non-negative whole number.",
         call. = FALSE)
  }
}

threshold_pretty_label <- function(x) {
  x <- gsub("_+", " ", x)
  tools::toTitleCase(x)
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

resolve_excess_amount_column <- function(data, excess_amount) {
  if (!is.null(excess_amount)) {
    validate_character_column(data, excess_amount, "excess_amount")
    return(excess_amount)
  }
  resolved <- attr(data, "claim_amount_excess_column", exact = TRUE)
  if (is.character(resolved) && length(resolved) == 1L &&
      !is.na(resolved) && nzchar(resolved) && resolved %in% names(data)) {
    return(resolved)
  }
  stop(
    "`excess_amount` is NULL and no valid excess column could be resolved. ",
    "Supply `excess_amount` or use data returned by `calculate_excess_loss()`.",
    call. = FALSE
  )
}

validate_allocate_excess_loss <- function(data, excess_amount,
                                          allocation_weight,
                                          receives_allocation, claim_count,
                                          risk_factor,
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
  if (!is.null(receives_allocation)) {
    validate_character_column(data, receives_allocation, "receives_allocation")
    if (!is.logical(data[[receives_allocation]]) ||
        any(is.na(data[[receives_allocation]]))) {
      stop("`receives_allocation` must refer to a logical column without missing values.",
           call. = FALSE)
    }
  }
  if (!is.null(claim_count)) {
    validate_character_column(data, claim_count, "claim_count")
    if (!is.numeric(data[[claim_count]]) ||
        any(is.na(data[[claim_count]])) ||
        any(!is.finite(data[[claim_count]])) ||
        any(data[[claim_count]] < 0)) {
      stop(
        "`claim_count` must refer to a numeric column with finite non-negative values.",
        call. = FALSE
      )
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

build_excess_allocation_output <- function(data, allocation_data, risk_factor,
                                           receives_allocation_col) {
  allocation_data <- allocation_data[order(allocation_data$row_id), , drop = FALSE]
  if (is.null(risk_factor)) {
    risk_factor_loading_col <- "risk_factor_excess_loading"
    risk_factor_credibility_col <- "risk_factor_credibility"
  } else {
    risk_factor_loading_col <- paste0(risk_factor, "_excess_loading")
    risk_factor_credibility_col <- paste0(risk_factor, "_credibility")
  }
  added_cols <- c(
    "receives_allocation",
    risk_factor_loading_col,
    risk_factor_credibility_col,
    "portfolio_excess_loading",
    "blended_excess_loading",
    "expected_excess_loss"
  )
  existing_cols <- intersect(added_cols, names(data))
  if (identical(receives_allocation_col, "receives_allocation")) {
    existing_cols <- setdiff(existing_cols, "receives_allocation")
  }
  if (length(existing_cols) > 0) {
    stop(
      "Allocation output column names already exist in `data`: ",
      paste(existing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  out <- data
  out$receives_allocation <- allocation_data$included
  out[[risk_factor_loading_col]] <- allocation_data$group_loading
  out[[risk_factor_credibility_col]] <- allocation_data$credibility
  out$portfolio_excess_loading <- allocation_data$portfolio_loading
  out$blended_excess_loading <- allocation_data$allocated_loading
  out$expected_excess_loss <- allocation_data$allocated_excess_loss
  out
}

prepare_allocation_data <- function(data, excess_amount, allocation_weight,
                                    receives_allocation, claim_count,
                                    risk_factor) {
  included <- if (is.null(receives_allocation)) {
    rep(TRUE, nrow(data))
  } else {
    data[[receives_allocation]]
  }
  claim_amount_col <- attr(data, "claim_amount_column", exact = TRUE)
  claim_count_values <- if (!is.null(claim_count)) {
    data[[claim_count]]
  } else if (!is.null(claim_amount_col) && claim_amount_col %in% names(data)) {
    as.numeric(data[[claim_amount_col]] > 0)
  } else if ("claim_amount" %in% names(data)) {
    as.numeric(data[["claim_amount"]] > 0)
  } else {
    as.numeric(data[[excess_amount]] > 0)
  }
  loss_amount_values <- if (!is.null(claim_amount_col) &&
                            claim_amount_col %in% names(data)) {
    data[[claim_amount_col]]
  } else if ("claim_amount" %in% names(data)) {
    data[["claim_amount"]]
  } else {
    data[[excess_amount]]
  }
  out <- data.frame(
    row_id = seq_len(nrow(data)),
    excess_amount = data[[excess_amount]],
    claim_amount = loss_amount_values,
    claim_count = claim_count_values,
    weight = data[[allocation_weight]],
    included = included,
    group = if (is.null(risk_factor)) "portfolio" else as.character(data[[risk_factor]]),
    stringsAsFactors = FALSE
  )
  out$group_value <- if (is.null(risk_factor)) {
    "portfolio"
  } else {
    data[[risk_factor]]
  }
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
    historical_excess <- sum(z$excess_amount)
    group_weight <- sum(included$weight)
    empirical_loss <- sum(z$claim_amount)
    data.frame(
      group = g,
      group_value = z$group_value[1],
      group_weight = group_weight,
      n_claims = sum(z$claim_count),
      n_excess_claims = sum(z$excess_amount > 0),
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
  tail <- allocation_data[allocation_data$excess_amount > 0, , drop = FALSE]
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

safe_ratio_excess_zero <- function(num, den) {
  ifelse(is.na(den), NA_real_, ifelse(den <= 0, 0, num / den))
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
