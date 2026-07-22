#' Redistribute large losses for severity or risk-premium modelling
#'
#' @description
#' Large claims can have a disproportionate influence on observed severity and
#' on estimated risk-factor effects. `redistribute_excess_loss()` decomposes
#' each selected claim amount into a retained component up to a specified
#' threshold and an excess component above that threshold. The excess component
#' is subsequently allocated using portfolio-wide, risk-factor-level or
#' partially pooled experience. The allocation preserves the total excess loss,
#' subject to numerical tolerance.
#'
#' The allocated excess can be incorporated in the pricing analysis in two
#' ways:
#'
#' - `output = "redistributed_claim"` adds the allocated excess loss to the
#'   retained claim amount. The resulting variable contains both components and
#'   can be used as the response in a single severity model.
#' - `output = "excess_loading"` keeps retained claim severity and excess loss
#'   as separate quantities. The function returns an excess loading per unit of
#'   `redistribution_weight`. This loading can be added to the risk premium based
#'   on predicted frequency and retained severity.
#'
#' Both output forms use the same threshold, credibility and allocation
#' calculations. They therefore differ only in how the allocated excess loss is
#' represented in subsequent modelling; the total amount allocated is the same.
#'
#' The default is `output = "redistributed_claim"`.
#'
#' @details
#' For each row selected through `redistribute_excess`, the observed claim
#' amount is decomposed as:
#'
#' \deqn{
#' claim\_amount = capped\_claim\_amount + excess\_claim\_amount
#' }
#'
#' If a row is not selected through `redistribute_excess`, its full observed
#' amount is retained. Any amount above the threshold remains available as a
#' diagnostic quantity but is excluded from the amount to be allocated.
#'
#' The excess amount is allocated over eligible rows in proportion to
#' `redistribution_weight`. If no weight column is supplied, claim count is
#' used. For redistributed-claim output, only rows with a positive claim count
#' are eligible. For excess-loading output, eligibility is determined by a
#' positive redistribution weight, so policy rows without observed claims may
#' receive a loading when exposure is used.
#'
#' For `output = "redistributed_claim"`, the redistributed claim amount is:
#'
#' \deqn{
#' adjusted\_claim\_amount = capped\_claim\_amount + redistributed\_excess
#' }
#'
#' This redistributed claim amount is returned as `<claim_amount>_adjusted`.
#' The corresponding average per claim is suitable as the response in a
#' claim-count-weighted severity GLM. Because this response includes allocated
#' excess loss, the same excess component should not subsequently be added to
#' the estimated risk premium.
#'
#' For `output = "excess_loading"`, capped claim severity remains separate and:
#'
#' \deqn{
#' excess\_loading_i =
#' \frac{allocated\_excess\_loss_i}{redistribution\_weight_i}
#' }
#'
#' The loading can be added to the risk premium derived from predicted
#' frequency and retained severity. When earned exposure is used as
#' `redistribution_weight`, the loading is expressed per unit of earned
#' exposure. The total allocated excess loss is preserved, subject to numerical
#' tolerance.
#'
#' ## Interpretation of the output forms
#'
#' Redistributed-claim output combines retained and allocated loss in one model
#' response. It requires one severity model and assigns the complete historical
#' loss burden to that response. Its interpretation is most direct when the
#' modelled risk-factor levels contain sufficient claim experience and the
#' estimated effects are stable across observation periods.
#'
#' The allocated component is not an observed loss for the receiving row. For
#' example, an observed claim of 10,000 may receive an allocation of 20,000,
#' resulting in a redistributed amount of 30,000. If the row belongs to a
#' risk-factor level with few claims, the fitted model may attribute a material
#' part of this allocated portfolio experience to that individual level. This
#' can increase the sampling variability of its estimated effect.
#'
#' Excess-loading output estimates retained severity from observed loss up to
#' the threshold and represents allocated excess as a separate risk-premium
#' component:
#'
#' \deqn{
#' retained\ risk\ premium = predicted\ frequency \cdot
#' predicted\ retained\ severity
#' }
#'
#' \deqn{
#' total\ risk\ premium = retained\ risk\ premium + excess\ loading
#' }
#'
#' The two output forms therefore imply different model interpretations rather
#' than different total loss amounts. The selection should reflect claim volume,
#' the stability of risk-factor effects and the intended construction of the
#' technical risk premium.
#'
#' ## Sparse risk-factor levels
#'
#' Before fitting a redistributed-claim severity model, claim volume should be
#' assessed by risk-factor level. Levels with limited information may be
#' combined using an economically or actuarially meaningful hierarchy.
#' Coefficient stability across periods and agreement between observed and
#' predicted severity provide additional diagnostics. Excess-loading output is
#' an alternative when separate level estimates remain weakly supported.
#'
#' For example, a model-preparation rule may map sectors with fewer than 20
#' claims to `"Other"`. The value 20 is illustrative and should not be treated
#' as a general minimum. An appropriate threshold depends on portfolio size,
#' heterogeneity and validation results. Grouping is therefore outside the
#' scope of this function.
#'
#' ## Reproducing the redistribution
#'
#' The optional calculation columns allow the allocation to be reproduced. For
#' partial redistribution, the loading before total-preservation scaling is:
#'
#' \deqn{
#' blended\_loading = Z_g \cdot risk\_factor\_loading_g +
#' (1 - Z_g) \cdot portfolio\_loading
#' }
#'
#' where `Z_g` denotes the credibility assigned to risk-factor level `g`.
#' Blending may change the total amount implied by the unscaled loadings. A
#' common scaling factor is therefore applied:
#'
#' \deqn{
#' preservation\_factor =
#' \frac{total\ excess\ to\ redistribute}
#' {\sum_i blended\_loading_i \cdot redistribution\_weight_i}
#' }
#'
#' The final amount received by row `i` is:
#'
#' \deqn{
#' redistributed\_excess_i = blended\_loading_i \cdot
#' preservation\_factor \cdot redistribution\_weight_i
#' }
#'
#' For example, let the sector loading be 30 per unit of weight, the portfolio
#' loading 20 and sector credibility 0.40. The blended loading equals
#' `0.40 * 30 + 0.60 * 20 = 24`. With a scaling factor of 1.10, the final
#' loading equals 26.4. A receiving row with redistribution weight 2 is then
#' allocated `26.4 * 2 = 52.8`. In redistributed-claim output, 52.8 is added to
#' the retained claim amount; in excess-loading output, 26.4 is retained as the
#' loading per unit of weight.
#'
#' With portfolio redistribution, credibility is zero and the blend equals the
#' portfolio loading. With risk-factor redistribution, credibility is one and
#' the blend equals the risk-factor loading.
#'
#' ## Eligibility and redistribution weights
#'
#' `receives_redistribution` identifies the rows to which excess loss may be
#' allocated. Rows with value `FALSE` receive zero. Their observed excess loss
#' is nevertheless included in the total amount to be allocated unless excluded
#' through `redistribute_excess`. For redistributed-claim output, a receiving
#' row must also have a positive claim count. For excess-loading output, a
#' receiving row must have a positive redistribution weight.
#'
#' `redistribute_excess` controls which large losses contribute their excess
#' part to the redistribution. If it is `NULL`, every row with
#' `claim_amount > threshold` contributes. For a row marked `FALSE`, the claim
#' is not capped: its full observed amount is retained and its excess does not
#' enter the allocation pool. This permits specific loss types, such as events
#' treated outside the regular large-loss procedure, to remain unchanged.
#'
#' `redistribution_weight` controls both the relative shares among receiving
#' rows and the unit of the resulting loading. Claim count produces an amount
#' per claim, expected claim count an amount per expected claim, earned exposure
#' an amount per exposure unit, and insured amount an amount per unit insured.
#' If it is `NULL`, claim count is used. For an excess loading that is added to
#' an annual risk premium, earned exposure is usually the corresponding unit.
#'
#' Rows with zero redistribution weight remain in the output but receive zero.
#' For example, `claim_count * insured_amount` assigns a larger share to claim
#' observations with a higher insured amount. The excess threshold and an
#' insured-amount criterion for receiving allocations are separate model
#' specifications.
#'
#' ## Redistribution methods
#'
#' The `redistribution_method` argument determines the level at which excess
#' experience is estimated:
#'
#' - `"portfolio"` derives one loading from the complete allocation portfolio.
#'   Risk-factor-level excess experience is not used.
#' - `"risk_factor"` derives a separate loading for each risk-factor level.
#'   Each level is based only on its own excess experience and allocation
#'   weight.
#' - `"partial"` blends the risk-factor loading with the portfolio loading
#'   using credibility.
#'
#' For partial redistribution, credibility is either supplied directly through
#' `credibility` or calculated as:
#'
#' \deqn{
#' Z_g = \frac{n_g}{n_g + credibility\_threshold}
#' }
#'
#' With `credibility_basis = "claims"`, `n_g` is the number of claims in the
#' risk-factor level. With `credibility_basis = "excess_records"`, it is the
#' number of records containing a positive excess amount. The resulting value
#' is multiplied by `credibility_scale` and bounded between zero and one.
#'
#' Credibility and redistribution weight have distinct roles. Credibility
#' determines the contribution of risk-factor-level experience to a partial
#' loading. Redistribution weight determines the row-level allocation shares
#' and the unit of the final loading. Thus, with
#' `credibility_basis = "claims"` and earned exposure as
#' `redistribution_weight`, claim volume determines credibility while the
#' resulting loading is expressed per unit of exposure.
#'
#' ## Aggregated portfolio rows
#'
#' When a row contains multiple claims, `claim_amount` is treated as the total
#' loss for that row and `threshold` is applied to that row total. The function
#' cannot identify which individual claims exceeded the threshold from an
#' aggregated row. Claim-level input is required when the threshold is intended
#' to apply separately to each claim.
#'
#' @param data A data.frame containing portfolio-level or claim-level
#'   observations.
#' @param claim_amount Character string naming a finite, non-negative numeric
#'   column with observed claim amounts or aggregate claim loss per row.
#' @param threshold Positive numeric scalar defining the boundary between
#'   retained and excess loss. For selected rows, the amount above this value is
#'   allocated.
#' @param claim_count Optional character string naming a non-negative,
#'   whole-number claim-count column. Claim count identifies claim-bearing rows
#'   and is the denominator of the adjusted average claim amount. It is also the
#'   default redistribution weight. If `NULL`, each row with
#'   `claim_amount > 0` is treated as one claim.
#' @param redistribution_weight Optional character string naming a finite,
#'   non-negative numeric column. The column determines the relative allocation
#'   shares and the unit of the resulting loading. If `NULL`, claim count is
#'   used. Claim count or expected claim count expresses the allocation per
#'   claim; earned exposure expresses it per exposure unit. Rows with zero
#'   weight receive no allocation. At least one eligible row must have positive
#'   weight.
#' @param receives_redistribution Optional character string. Logical column
#'   indicating which rows may receive allocated excess loss. Rows with `FALSE`
#'   receive zero, while their observed excess remains in the total allocation
#'   unless excluded by `redistribute_excess`. If `NULL`, all otherwise eligible
#'   rows are included. Eligibility additionally requires positive claim count
#'   for redistributed claims and positive redistribution weight for excess
#'   loadings.
#' @param redistribute_excess Optional character string. Logical column
#'   indicating which rows contribute their excess component to the allocation.
#'   Rows with `FALSE` retain their full observed amount and contribute no excess
#'   to the allocation pool. If `NULL`, every row above `threshold` contributes.
#' @param risk_factor Optional character string naming the risk-factor column for
#'   `redistribution_method = "risk_factor"` or
#'   `redistribution_method = "partial"`.
#' @param redistribution_method Character string specifying the experience level
#'   used in the allocation: `"portfolio"`, `"risk_factor"` or `"partial"`.
#' @param credibility Optional numeric scalar in `[0, 1]`. For partial
#'   redistribution, the supplied value is applied to every risk-factor level.
#'   If `NULL`, credibility is calculated from `credibility_basis`.
#' @param credibility_basis Character string specifying the experience measure
#'   used in automatic credibility: `"claims"` or `"excess_records"`.
#' @param credibility_threshold Positive numeric scalar representing the amount
#'   of credibility-basis experience at which automatic credibility equals
#'   0.5, before applying `credibility_scale`.
#' @param credibility_scale Non-negative numeric scalar multiplying automatic
#'   credibility before truncation to `[0, 1]`.
#' @param calculation_details Logical. If `TRUE`, append the risk-factor
#'   loading, credibility, portfolio loading, blended loading, scaling factor
#'   and final loading used in the row-level calculation. If `FALSE`, these
#'   columns are omitted from `data` but remain available through
#'   [summary.excess_redistribution()].
#' @param output Character string specifying the representation of allocated
#'   excess. `"redistributed_claim"` adds it to retained claim amounts;
#'   `"excess_loading"` returns it separately per unit of
#'   `redistribution_weight`.
#'
#' @return The input data.frame with additional columns and class
#'   `"excess_redistribution"`. The object uses standard data.frame printing.
#'   [summary.excess_redistribution()] aggregates contributed, allocated and
#'   shifted loss. Both output forms add:
#'   \describe{
#'     \item{`<claim_amount>_capped`}{Observed claim amount capped at
#'     `threshold` when its excess is allocated. Rows excluded through
#'     `redistribute_excess` retain their full observed amount.}
#'     \item{`<claim_amount>_excess`}{Observed amount above `threshold`, whether
#'     or not that amount is selected for allocation.}
#'     \item{`<claim_amount>_is_excess`}{Logical indicator that the observed
#'     claim amount exceeds `threshold`.}
#'   }
#'   With `output = "redistributed_claim"`, the result additionally contains:
#'   \describe{
#'     \item{`<claim_amount>_redistributed_excess`}{Row-level allocated excess
#'     loss. Rows without claims receive zero.}
#'     \item{`<claim_amount>_adjusted`}{Retained claim amount plus row-level
#'     allocated excess loss.}
#'     \item{`<claim_amount>_adjusted_average`}{Redistributed claim amount divided
#'     by claim count. Rows without claims contain zero.}
#'   }
#'   With `output = "excess_loading"`, the result additionally contains:
#'   \describe{
#'     \item{`allocated_excess_loss`}{Absolute excess-loss amount allocated to
#'     the row.}
#'     \item{`excess_loading`}{Allocated excess loss per unit of
#'     `redistribution_weight`.}
#'   }
#'   With `calculation_details = TRUE`, the result also contains
#'   `<risk_factor>_excess_loading`, `<risk_factor>_credibility`,
#'   `portfolio_excess_loading`, `blended_excess_loading`,
#'   `redistribution_scaling_factor` and `final_redistribution_loading`.
#'   For receiving rows, `final_redistribution_loading` equals
#'   `blended_excess_loading` multiplied by
#'   `redistribution_scaling_factor`.
#'   The selected output and effective redistribution-weight label are stored in
#'   the `"output"` and `"redistribution_weight_label"` attributes.
#'
#' @seealso [summary.excess_redistribution()]
#'
#' @author Martin Haringa
#'
#' @examples
#' portfolio <- data.frame(
#'   policy_id = 1:10,
#'   sector = c(rep("Industry", 5), rep("Retail", 4), "Office"),
#'   claim_count = c(0, 1, 1, 1, 1, 0, 1, 1, 1, 1),
#'   claim_amount = c(
#'     0, 25000, 120000, 50000, 175000,
#'     0, 40000, 90000, 150000, 300000
#'   ),
#'   policy_years = rep(1, 10)
#' )
#'
#' # Output form 1: include allocated excess in the severity response.
#' adjusted <- redistribute_excess_loss(
#'   portfolio,
#'   claim_amount = "claim_amount",
#'   threshold = 100000,
#'   claim_count = "claim_count",
#'   risk_factor = "sector",
#'   redistribution_method = "partial",
#'   output = "redistributed_claim"
#' )
#' summary(adjusted)
#'
#' # Inspect the row-level calculation. The allocated amount in the final column
#' # equals final_redistribution_loading times redistribution weight.
#' adjusted[c(
#'   "sector_excess_loading", "sector_credibility",
#'   "portfolio_excess_loading", "blended_excess_loading",
#'   "redistribution_scaling_factor", "final_redistribution_loading",
#'   "claim_amount_redistributed_excess"
#' )]
#'
#' # Omit row-level calculation columns while retaining them in summary().
#' compact_adjusted <- redistribute_excess_loss(
#'   portfolio,
#'   claim_amount = "claim_amount",
#'   threshold = 100000,
#'   claim_count = "claim_count",
#'   risk_factor = "sector",
#'   redistribution_method = "partial",
#'   calculation_details = FALSE
#' )
#' summary(compact_adjusted)
#'
#' # Combine levels with limited claim experience before model estimation.
#' # Three claims is used for this small example; it is not a general minimum.
#' adjusted$sector_claim_count <- ave(
#'   adjusted$claim_count, adjusted$sector, FUN = sum
#' )
#' adjusted$sector_model <- ifelse(
#'   adjusted$sector_claim_count >= 3,
#'   adjusted$sector,
#'   "Other"
#' )
#'
#' # Fit a severity model to redistributed average claim amount. For aggregated
#' # rows, claim count represents the number of observations underlying each
#' # average. With one row per claim, this additional weight is unnecessary.
#' severity_data <- adjusted[adjusted$claim_count > 0, ]
#' stats::glm(
#'   claim_amount_adjusted_average ~ sector_model,
#'   weights = claim_count,
#'   family = stats::Gamma(link = "log"),
#'   data = severity_data
#' )
#'
#' # Output form 2: estimate retained severity and excess loading separately.
#' # Using policy years expresses excess_loading per policy year.
#' loading_result <- redistribute_excess_loss(
#'   portfolio,
#'   claim_amount = "claim_amount",
#'   threshold = 100000,
#'   claim_count = "claim_count",
#'   redistribution_weight = "policy_years",
#'   risk_factor = "sector",
#'   redistribution_method = "partial",
#'   output = "excess_loading"
#' )
#'
#' frequency_model <- stats::glm(
#'   claim_count ~ sector + offset(log(policy_years)),
#'   family = stats::poisson(link = "log"),
#'   data = loading_result
#' )
#' retained_severity_model <- stats::glm(
#'   claim_amount_capped ~ sector,
#'   weights = claim_count,
#'   family = stats::Gamma(link = "log"),
#'   data = loading_result[loading_result$claim_count > 0, ]
#' )
#'
#' loading_result$predicted_claim_frequency <- stats::predict(
#'   frequency_model,
#'   newdata = loading_result,
#'   type = "response"
#' ) / loading_result$policy_years
#' loading_result$predicted_retained_severity <- stats::predict(
#'   retained_severity_model,
#'   newdata = loading_result,
#'   type = "response"
#' )
#' loading_result$predicted_retained_risk_premium <-
#'   loading_result$predicted_claim_frequency *
#'   loading_result$predicted_retained_severity
#' loading_result$predicted_total_risk_premium <-
#'   loading_result$predicted_retained_risk_premium +
#'   loading_result$excess_loading
#'
#' # Portfolio redistribution estimates one loading across all sectors. Sector-
#' # specific excess experience does not enter the allocation loading.
#' portfolio_adjusted <- redistribute_excess_loss(
#'   portfolio,
#'   claim_amount = "claim_amount",
#'   threshold = 100000,
#'   claim_count = "claim_count",
#'   redistribution_method = "portfolio"
#' )
#' summary(portfolio_adjusted, by = "sector")
#'
#' # Risk-factor redistribution estimates a separate loading for each sector.
#' # The estimate for a sector uses only that sector's excess loss and weight.
#' sector_adjusted <- redistribute_excess_loss(
#'   portfolio,
#'   claim_amount = "claim_amount",
#'   threshold = 100000,
#'   claim_count = "claim_count",
#'   risk_factor = "sector",
#'   redistribution_method = "risk_factor"
#' )
#'
#' # Allocate in proportion to claim count times insured amount, restricted to
#' # policies with an insured amount of at least 100,000.
#' weighted_portfolio <- transform(
#'   portfolio,
#'   insured_amount = rep(c(50000, 250000), each = 5)
#' )
#' weighted_portfolio$receives_redistribution <-
#'   weighted_portfolio$insured_amount >= 100000
#' weighted_portfolio$redistribution_weight <-
#'   weighted_portfolio$claim_count * weighted_portfolio$insured_amount
#'
#' weighted_adjusted <- redistribute_excess_loss(
#'   weighted_portfolio,
#'   claim_amount = "claim_amount",
#'   threshold = 100000,
#'   claim_count = "claim_count",
#'   redistribution_weight = "redistribution_weight",
#'   receives_redistribution = "receives_redistribution"
#' )
#'
#' # Exclude catastrophe events and unsettled claims from the allocation pool.
#' # Their full observed claim amounts remain retained in the model data.
#' portfolio$is_catastrophe <- c(
#'   FALSE, FALSE, FALSE, FALSE, TRUE,
#'   FALSE, FALSE, FALSE, FALSE, FALSE
#' )
#' portfolio$claim_status <- c(
#'   "settled", "settled", "settled", "settled", "settled",
#'   "settled", "settled", "open", "settled", "settled"
#' )
#' portfolio$redistribute_excess <-
#'   !portfolio$is_catastrophe &
#'   portfolio$claim_status == "settled"
#'
#' selected_adjusted <- redistribute_excess_loss(
#'   portfolio,
#'   claim_amount = "claim_amount",
#'   threshold = 100000,
#'   claim_count = "claim_count",
#'   redistribute_excess = "redistribute_excess"
#' )
#'
#' @export
redistribute_excess_loss <- function(
    data,
    claim_amount,
    threshold,
    claim_count = NULL,
    redistribution_weight = NULL,
    receives_redistribution = NULL,
    redistribute_excess = NULL,
    risk_factor = NULL,
    redistribution_method = c("portfolio", "risk_factor", "partial"),
    credibility = NULL,
    credibility_basis = c("claims", "excess_records"),
    credibility_threshold = 50,
    credibility_scale = 1,
    calculation_details = TRUE,
    output = c("redistributed_claim", "excess_loading")) {
  output <- match.arg(output)
  redistribution_method <- match.arg(redistribution_method)
  credibility_basis <- match.arg(credibility_basis)
  validate_redistribute_excess_loss(
    data = data,
    claim_amount = claim_amount,
    threshold = threshold,
    claim_count = claim_count,
    redistribution_weight = redistribution_weight,
    receives_redistribution = receives_redistribution,
    redistribute_excess = redistribute_excess,
    risk_factor = risk_factor,
    redistribution_method = redistribution_method,
    credibility = credibility,
    credibility_threshold = credibility_threshold,
    credibility_scale = credibility_scale,
    calculation_details = calculation_details
  )

  counts <- if (is.null(claim_count)) {
    as.numeric(data[[claim_amount]] > 0)
  } else {
    data[[claim_count]]
  }
  if (any(data[[claim_amount]] > 0 & counts <= 0)) {
    stop("Rows with a positive `claim_amount` must have a positive claim count.",
         call. = FALSE)
  }
  if (!any(counts > 0)) {
    stop("At least one row must contain a claim.", call. = FALSE)
  }
  receives <- if (identical(output, "redistributed_claim")) {
    counts > 0
  } else {
    rep(TRUE, nrow(data))
  }
  if (!is.null(receives_redistribution)) {
    receives <- receives & data[[receives_redistribution]]
  }
  weights <- if (is.null(redistribution_weight)) {
    counts
  } else {
    data[[redistribution_weight]]
  }
  receives <- receives & weights > 0
  if (!any(receives)) {
    stop(
      "At least one eligible row must have a positive redistribution weight.",
      call. = FALSE
    )
  }
  redistributes <- if (is.null(redistribute_excess)) {
    rep(TRUE, nrow(data))
  } else {
    data[[redistribute_excess]]
  }

  capped_col <- paste0(claim_amount, "_capped")
  excess_col <- paste0(claim_amount, "_excess")
  indicator_col <- paste0(claim_amount, "_is_excess")
  redistributed_col <- paste0(claim_amount, "_redistributed_excess")
  adjusted_col <- paste0(claim_amount, "_adjusted")
  average_col <- paste0(claim_amount, "_adjusted_average")
  allocated_col <- "allocated_excess_loss"
  loading_col <- "excess_loading"
  risk_factor_label <- risk_factor %||% "risk_factor"
  risk_factor_loading_col <- paste0(risk_factor_label, "_excess_loading")
  credibility_col <- paste0(risk_factor_label, "_credibility")
  calculation_cols <- c(
    risk_factor_loading_col,
    credibility_col,
    "portfolio_excess_loading",
    "blended_excess_loading",
    "redistribution_scaling_factor",
    "final_redistribution_loading"
  )
  output_cols <- if (identical(output, "redistributed_claim")) {
    c(
      capped_col, excess_col, indicator_col, redistributed_col, adjusted_col,
      average_col
    )
  } else {
    c(capped_col, excess_col, indicator_col, allocated_col, loading_col)
  }
  if (isTRUE(calculation_details)) {
    output_cols <- c(output_cols, calculation_cols)
  }
  conflicts <- intersect(output_cols, names(data))
  if (length(conflicts) > 0L) {
    stop(
      "Output column names already exist in `data`: ",
      paste(conflicts, collapse = ", "),
      call. = FALSE
    )
  }

  work <- data
  observed_excess <- pmax(work[[claim_amount]] - threshold, 0)
  work[[capped_col]] <- ifelse(
    redistributes,
    pmin(work[[claim_amount]], threshold),
    work[[claim_amount]]
  )
  work[[excess_col]] <- observed_excess
  work[[indicator_col]] <- work[[claim_amount]] > threshold
  work$.redistributable_excess <- ifelse(redistributes, observed_excess, 0)
  work$.redistribution_claim_count <- counts
  work$.redistribution_weight <- weights
  work$.receives_redistribution <- receives

  allocated <- .allocate_excess_loss(
    data = work,
    excess_amount = ".redistributable_excess",
    allocation_weight = ".redistribution_weight",
    risk_factor = risk_factor,
    receives_allocation = ".receives_redistribution",
    claim_count = ".redistribution_claim_count",
    allocation = redistribution_method,
    credibility = credibility,
    credibility_basis = if (identical(credibility_basis, "excess_records")) {
      "excess_claims"
    } else {
      "claims"
    },
    credibility_threshold = credibility_threshold,
    credibility_scale = credibility_scale,
    preserve_total_excess = TRUE
  )

  risk_factor_loading <- allocated[[risk_factor_loading_col]]
  credibility_values <- allocated[[credibility_col]]
  portfolio_loading <- allocated$portfolio_excess_loading
  blended_loading <- credibility_values * risk_factor_loading +
    (1 - credibility_values) * portfolio_loading
  raw_total <- sum(blended_loading[receives] * weights[receives])
  redistributed_total <- sum(work$.redistributable_excess)
  if (redistributed_total > 0 && (!is.finite(raw_total) || raw_total <= 0)) {
    stop(
      "The blended redistribution loading does not provide a positive allocation base.",
      call. = FALSE
    )
  }
  scaling_factor <- if (redistributed_total == 0) {
    1
  } else {
    redistributed_total / raw_total
  }
  final_loading <- ifelse(receives, blended_loading * scaling_factor, 0)
  reconstructed_redistribution <- final_loading * weights
  reconstruction_tolerance <- sqrt(.Machine$double.eps) *
    max(1, redistributed_total)
  if (any(abs(
    reconstructed_redistribution - allocated$expected_excess_loss
  ) > reconstruction_tolerance)) {
    stop("Redistribution calculation details do not reconcile.", call. = FALSE)
  }

  out <- data
  out[[capped_col]] <- work[[capped_col]]
  out[[excess_col]] <- work[[excess_col]]
  out[[indicator_col]] <- work[[indicator_col]]
  if (identical(output, "redistributed_claim")) {
    out[[redistributed_col]] <- allocated$expected_excess_loss
    out[[adjusted_col]] <- out[[capped_col]] + out[[redistributed_col]]
    out[[average_col]] <- ifelse(counts > 0, out[[adjusted_col]] / counts, 0)
  } else {
    out[[allocated_col]] <- allocated$expected_excess_loss
    out[[loading_col]] <- final_loading
  }
  if (isTRUE(calculation_details)) {
    out[[risk_factor_loading_col]] <- risk_factor_loading
    out[[credibility_col]] <- credibility_values
    out$portfolio_excess_loading <- portfolio_loading
    out$blended_excess_loading <- blended_loading
    out$redistribution_scaling_factor <- rep(scaling_factor, nrow(out))
    out$final_redistribution_loading <- final_loading
  }

  observed_total <- sum(out[[claim_amount]])
  retained_total <- sum(out[[capped_col]])
  allocated_total <- sum(allocated$expected_excess_loss)
  tolerance <- sqrt(.Machine$double.eps) * max(1, observed_total)
  if (abs(redistributed_total - allocated_total) > tolerance) {
    stop("Allocated excess loss does not reconcile to redistributed excess.",
         call. = FALSE)
  }
  if (identical(output, "redistributed_claim") &&
      abs(observed_total - sum(out[[adjusted_col]])) > tolerance) {
    stop("Redistributed claim amounts do not reconcile to observed loss.",
         call. = FALSE)
  }
  if (identical(output, "excess_loading") &&
      abs(observed_total - retained_total - allocated_total) > tolerance) {
    stop("Retained and allocated excess loss do not reconcile to observed loss.",
         call. = FALSE)
  }

  attr(out, "claim_amount") <- claim_amount
  attr(out, "claim_count") <- claim_count
  attr(out, "redistribution_weight") <- redistribution_weight
  attr(out, "redistribution_weight_label") <- redistribution_weight %||%
    claim_count %||% "inferred_claim_count"
  attr(out, "receives_redistribution") <- receives_redistribution
  attr(out, "redistribute_excess") <- redistribute_excess
  attr(out, "threshold") <- threshold
  attr(out, "risk_factor") <- risk_factor
  attr(out, "redistribution_method") <- redistribution_method
  attr(out, "output") <- output
  attr(out, "calculation_details") <- calculation_details
  attr(out, "claim_count_vector") <- counts
  attr(out, "redistribution_weight_vector") <- weights
  attr(out, "receives_redistribution_vector") <- receives
  attr(out, "redistributable_excess_vector") <-
    work$.redistributable_excess
  attr(out, "risk_factor_loading_vector") <- risk_factor_loading
  attr(out, "credibility_vector") <- credibility_values
  attr(out, "portfolio_excess_loading_vector") <- portfolio_loading
  attr(out, "blended_excess_loading_vector") <- blended_loading
  attr(out, "redistribution_scaling_factor") <- scaling_factor
  attr(out, "final_redistribution_loading_vector") <- final_loading
  attr(out, "retained_claim_amount_vector") <- work[[capped_col]]
  attr(out, "allocated_excess_loss_vector") <-
    allocated$expected_excess_loss
  attr(out, "observed_excess_column") <- excess_col
  attr(out, "retained_claim_amount_column") <- capped_col
  attr(out, "allocated_excess_loss_column") <- if (
    identical(output, "redistributed_claim")
  ) redistributed_col else allocated_col
  attr(out, "excess_loading_column") <- if (
    identical(output, "excess_loading")
  ) loading_col else NULL
  attr(out, "redistributed_excess_column") <- if (
    identical(output, "redistributed_claim")
  ) redistributed_col else NULL
  attr(out, "adjusted_claim_amount_column") <- if (
    identical(output, "redistributed_claim")
  ) adjusted_col else NULL
  redistribution_summary <- .summary_excess_allocation(allocated)
  claim_summary_name <- claim_count %||% "claim_count"
  weight_summary_name <- redistribution_weight %||% "redistribution_weight"
  if (identical(weight_summary_name, claim_summary_name)) {
    weight_summary_name <- "redistribution_weight"
  }
  names(redistribution_summary)[
    names(redistribution_summary) == ".redistribution_claim_count"
  ] <- claim_summary_name
  names(redistribution_summary)[
    names(redistribution_summary) == ".redistribution_weight"
  ] <- weight_summary_name
  attr(out, "redistribution_summary") <- redistribution_summary
  class(out) <- c("excess_redistribution", "data.frame")
  out
}

#' Summarise redistributed large-loss experience
#'
#' @description
#' Audit how much excess loss was contributed and received across portfolio
#' segments after [redistribute_excess_loss()]. The summary shows whether a
#' segment receives more large-loss cost than it contributes, or transfers part
#' of its observed excess burden to other segments. The same audit is available
#' for redistributed claims and separate excess loadings.
#'
#' @details
#' `redistributed_excess_contributed` is the excess amount removed from selected
#' large losses in a segment. `redistributed_excess_received` is the amount
#' assigned back to claim-bearing rows in that segment. Their difference is:
#'
#' \deqn{
#' net\_loss\_shift = received - contributed
#' }
#'
#' A positive value means that the segment receives more redistributed loss
#' than it contributed. A negative value means that it transfers loss to other
#' segments. Across the full portfolio, `net_loss_shift` sums to zero, subject
#' to numerical tolerance.
#'
#' When `by = NULL`, the summary uses the `risk_factor` supplied to
#' [redistribute_excess_loss()]. If no risk factor was supplied, one portfolio
#' row is returned. Supply `by` to inspect a portfolio redistribution by another
#' portfolio characteristic, for example `summary(x, by = "sector")`.
#'
#' The summary also exposes the loading calculation. For partial
#' redistribution, `<risk_factor>_excess_loading` is blended with
#' `portfolio_excess_loading` using `<risk_factor>_credibility`. The resulting
#' `blended_excess_loading` is multiplied by
#' `redistribution_scaling_factor` to obtain
#' `final_redistribution_loading`. Multiplying the final loading by the total
#' receiving `redistribution_weight` gives
#' `redistributed_excess_received`.
#'
#' If `by` differs from the risk factor used in the redistribution, loading and
#' credibility columns are receiving-weighted averages within each audit group.
#' For separate-loading output, `adjusted_loss` is shown only as a reconciliation
#' of retained plus allocated loss; it does not mean that the allocated amount
#' was added to the row-level severity response.
#'
#' @param object An object returned by [redistribute_excess_loss()].
#' @param by Optional character string. Column used to group the audit. If
#'   `NULL`, use the original risk factor or return a portfolio-level summary.
#' @param ... Unused.
#'
#' @return A data.frame with one row per audit group. The grouping column keeps
#'   its original name and type when `by` is used. The remaining columns are:
#'   \describe{
#'     \item{`n_records`}{Number of portfolio records in the group.}
#'     \item{`claim_count`}{Number of claims in the group.}
#'     \item{`redistribution_weight`}{Total redistribution weight of rows that
#'     receive redistributed loss.}
#'     \item{`n_excess_records`}{Number of records with an observed amount above
#'     the threshold.}
#'     \item{`n_redistributed_excess_records`}{Number of records whose excess
#'     amount was actually contributed to the redistribution pool.}
#'     \item{`observed_loss`}{Observed claim cost before redistribution.}
#'     \item{`retained_loss`}{Claim cost retained at or below the threshold,
#'     including full claims excluded through `redistribute_excess`.}
#'     \item{`observed_excess_loss`}{Observed claim cost above the threshold,
#'     including excess from rows excluded through `redistribute_excess`.}
#'     \item{`redistributed_excess_contributed`}{Excess removed from selected
#'     large losses and contributed to the redistribution pool.}
#'     \item{`<risk_factor>_excess_loading`}{Excess loading estimated from the
#'     risk-factor-level experience, per unit of redistribution weight.}
#'     \item{`<risk_factor>_credibility`}{Weight assigned to the risk-factor
#'     loading. It is zero for portfolio redistribution, one for risk-factor
#'     redistribution and between zero and one for partial redistribution.}
#'     \item{`portfolio_excess_loading`}{Portfolio-wide excess loading per unit
#'     of redistribution weight.}
#'     \item{`blended_excess_loading`}{Risk-factor loading times credibility
#'     plus portfolio loading times one minus credibility.}
#'     \item{`redistribution_scaling_factor`}{Factor that preserves the total
#'     amount being redistributed after blending.}
#'     \item{`final_redistribution_loading`}{Blended loading multiplied by the
#'     redistribution scaling factor.}
#'     \item{`allocated_excess_loss`}{Absolute excess-loss amount allocated to
#'     the audit group.}
#'     \item{`average_excess_loading`}{Allocated excess loss per unit of total
#'     receiving redistribution weight.}
#'     \item{`redistributed_excess_received`}{Excess assigned to receiving
#'     claim-bearing rows.}
#'     \item{`net_loss_shift`}{Received minus contributed redistributed excess.}
#'     \item{`adjusted_loss`}{Claim cost after redistribution.}
#'     \item{`observed_average_claim`}{Observed loss divided by claim count.}
#'     \item{`adjusted_average_claim`}{Adjusted loss divided by claim count.}
#'   }
#'
#' @seealso [redistribute_excess_loss()]
#'
#' @author Martin Haringa
#' @keywords internal
#' @export
summary.excess_redistribution <- function(object, by = NULL, ...) {
  .check_dots_empty(...)
  if (!inherits(object, "excess_redistribution")) {
    stop("`object` must be returned by `redistribute_excess_loss()`.",
         call. = FALSE)
  }
  if (!is.null(by)) {
    validate_character_column(object, by, "by")
    if (anyNA(object[[by]])) {
      stop("`by` must not contain missing values.", call. = FALSE)
    }
  } else {
    by <- attr(object, "risk_factor", exact = TRUE)
  }

  claim_amount <- attr(object, "claim_amount", exact = TRUE)
  excess_col <- attr(object, "observed_excess_column", exact = TRUE)
  retained <- attr(object, "retained_claim_amount_vector", exact = TRUE)
  allocated_excess <- attr(
    object, "allocated_excess_loss_vector", exact = TRUE
  )
  counts <- attr(object, "claim_count_vector", exact = TRUE)
  weights <- attr(object, "redistribution_weight_vector", exact = TRUE)
  receives <- attr(object, "receives_redistribution_vector", exact = TRUE)
  contributed <- attr(object, "redistributable_excess_vector", exact = TRUE)
  risk_factor_loading <- attr(
    object, "risk_factor_loading_vector", exact = TRUE
  )
  credibility_values <- attr(object, "credibility_vector", exact = TRUE)
  portfolio_loading <- attr(
    object, "portfolio_excess_loading_vector", exact = TRUE
  )
  blended_loading <- attr(
    object, "blended_excess_loading_vector", exact = TRUE
  )
  scaling_factor <- attr(
    object, "redistribution_scaling_factor", exact = TRUE
  )
  final_loading <- attr(
    object, "final_redistribution_loading_vector", exact = TRUE
  )
  required <- list(claim_amount, excess_col)
  required_names <- unlist(required, use.names = FALSE)
  if (any(vapply(required, is.null, logical(1))) ||
      length(required_names) != 2L ||
      !all(required_names %in% names(object)) ||
      any(vapply(list(
        counts, weights, receives, contributed, retained, allocated_excess,
        risk_factor_loading, credibility_values, portfolio_loading,
        blended_loading, scaling_factor, final_loading
      ), is.null, logical(1)))) {
    stop("Redistribution audit metadata is missing from `object`.",
         call. = FALSE)
  }

  groups <- if (is.null(by)) {
    list(Portfolio = seq_len(nrow(object)))
  } else {
    split(seq_len(nrow(object)), as.character(object[[by]]), drop = TRUE)
  }
  risk_factor <- attr(object, "risk_factor", exact = TRUE)
  risk_factor_label <- risk_factor %||% "risk_factor"
  risk_factor_loading_col <- paste0(risk_factor_label, "_excess_loading")
  credibility_col <- paste0(risk_factor_label, "_credibility")
  receiving_weighted_mean <- function(values, idx) {
    group_weights <- ifelse(receives[idx], weights[idx], 0)
    total_weight <- sum(group_weights)
    if (total_weight <= 0) {
      return(0)
    }
    sum(values[idx] * group_weights) / total_weight
  }
  rows <- lapply(groups, function(idx) {
    claim_total <- sum(counts[idx])
    observed_loss <- sum(object[[claim_amount]][idx])
    retained_loss <- sum(retained[idx])
    adjusted_loss <- sum(retained[idx] + allocated_excess[idx])
    received <- sum(allocated_excess[idx])
    contributed_total <- sum(contributed[idx])
    receiving_weight <- sum(weights[idx][receives[idx]])
    row <- data.frame(
      n_records = length(idx),
      claim_count = claim_total,
      redistribution_weight = receiving_weight,
      n_excess_records = sum(object[[excess_col]][idx] > 0),
      n_redistributed_excess_records = sum(contributed[idx] > 0),
      observed_loss = observed_loss,
      retained_loss = retained_loss,
      observed_excess_loss = sum(object[[excess_col]][idx]),
      redistributed_excess_contributed = contributed_total,
      stringsAsFactors = FALSE
    )
    row[[risk_factor_loading_col]] <- receiving_weighted_mean(
      risk_factor_loading, idx
    )
    row[[credibility_col]] <- receiving_weighted_mean(
      credibility_values, idx
    )
    row$portfolio_excess_loading <- receiving_weighted_mean(
      portfolio_loading, idx
    )
    row$blended_excess_loading <- receiving_weighted_mean(
      blended_loading, idx
    )
    row$redistribution_scaling_factor <- scaling_factor
    row$final_redistribution_loading <- receiving_weighted_mean(
      final_loading, idx
    )
    row$allocated_excess_loss <- received
    row$average_excess_loading <- safe_ratio_excess_zero(
      received, receiving_weight
    )
    row$redistributed_excess_received <- received
    row$net_loss_shift <- received - contributed_total
    row$adjusted_loss <- adjusted_loss
    row$observed_average_claim <- safe_ratio_excess_zero(
      observed_loss, claim_total
    )
    row$adjusted_average_claim <- safe_ratio_excess_zero(
      adjusted_loss, claim_total
    )
    row
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  if (!is.null(by)) {
    values <- lapply(groups, function(idx) object[[by]][idx][1])
    group_values <- do.call(c, values)
    out[[by]] <- group_values
    out <- out[, c(by, setdiff(names(out), by)), drop = FALSE]
  }
  attr(out, "by") <- by
  attr(out, "threshold") <- attr(object, "threshold", exact = TRUE)
  attr(out, "redistribution_method") <-
    attr(object, "redistribution_method", exact = TRUE)
  attr(out, "output") <- attr(object, "output", exact = TRUE)
  attr(out, "risk_factor") <- risk_factor
  attr(out, "redistribution_weight") <-
    attr(object, "redistribution_weight", exact = TRUE)
  attr(out, "redistribution_weight_label") <-
    attr(object, "redistribution_weight_label", exact = TRUE)
  out
}

validate_redistribute_excess_loss <- function(
    data, claim_amount, threshold, claim_count, redistribution_weight,
    receives_redistribution, redistribute_excess, risk_factor,
    redistribution_method, credibility, credibility_threshold,
    credibility_scale, calculation_details) {
  validate_data_frame(data)
  validate_character_column(data, claim_amount, "claim_amount")
  amount <- data[[claim_amount]]
  if (!is.numeric(amount) || any(!is.finite(amount)) || any(amount < 0)) {
    stop("`claim_amount` must refer to finite non-negative numeric values.",
         call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      !is.finite(threshold) || threshold <= 0) {
    stop("`threshold` must be a single positive number.", call. = FALSE)
  }
  if (!is.null(claim_count)) {
    validate_character_column(data, claim_count, "claim_count")
    counts <- data[[claim_count]]
    if (!is.numeric(counts) || any(!is.finite(counts)) || any(counts < 0) ||
        any(counts != floor(counts))) {
      stop("`claim_count` must refer to finite non-negative whole numbers.",
           call. = FALSE)
    }
  }
  if (!is.null(redistribution_weight)) {
    validate_character_column(data, redistribution_weight,
                              "redistribution_weight")
    weights <- data[[redistribution_weight]]
    if (!is.numeric(weights) || any(!is.finite(weights)) || any(weights < 0)) {
      stop("`redistribution_weight` must refer to finite non-negative numeric values.",
           call. = FALSE)
    }
  }
  if (!is.null(receives_redistribution)) {
    validate_character_column(data, receives_redistribution,
                              "receives_redistribution")
    receives <- data[[receives_redistribution]]
    if (!is.logical(receives) || anyNA(receives)) {
      stop("`receives_redistribution` must refer to logical values without missing values.",
           call. = FALSE)
    }
  }
  if (!is.null(redistribute_excess)) {
    validate_character_column(data, redistribute_excess, "redistribute_excess")
    selected <- data[[redistribute_excess]]
    if (!is.logical(selected) || anyNA(selected)) {
      stop("`redistribute_excess` must refer to logical values without missing values.",
           call. = FALSE)
    }
  }
  if (!is.null(risk_factor)) {
    validate_character_column(data, risk_factor, "risk_factor")
    if (anyNA(data[[risk_factor]])) {
      stop("`risk_factor` must not contain missing values.", call. = FALSE)
    }
  }
  if (!identical(redistribution_method, "portfolio") && is.null(risk_factor)) {
    stop("`risk_factor` must be supplied for risk-factor or partial redistribution.",
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
  if (!is.logical(calculation_details) || length(calculation_details) != 1L ||
      is.na(calculation_details)) {
    stop("`calculation_details` must be TRUE or FALSE.", call. = FALSE)
  }
}
