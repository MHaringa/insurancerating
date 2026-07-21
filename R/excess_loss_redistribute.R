#' Redistribute large losses before fitting a severity model
#'
#' @description
#' Reduce the influence of individual large losses without removing their cost
#' from the portfolio. `redistribute_excess_loss()` caps observed claim amounts
#' at a selected threshold and redistributes the excess amount across the claims
#' used for severity modelling.
#'
#' In practical terms, the function shifts part of the observed loss cost from
#' claims above the threshold to other claims before the severity model is
#' fitted. Large claims therefore have less direct influence on the fitted
#' severity relativities, while the total portfolio loss remains unchanged.
#'
#' The redistributed large-loss cost is already included in the adjusted claim
#' amounts. It should not be added to the risk premium again as a separate
#' large-loss loading. This provides a practical alternative to modelling and
#' adding a separate excess-loss premium component after the severity model has
#' been fitted.
#'
#' The function combines decomposition and redistribution in one step. It is
#' intended for a workflow in which a severity GLM is fitted to adjusted claim
#' amounts. The resulting adjusted average claim amount can be used directly as
#' the response in a claim-count-weighted severity GLM.
#'
#' @details
#' For each row selected through `redistribute_excess`, the observed claim
#' amount is decomposed as:
#'
#' \deqn{
#' claim\_amount = capped\_claim\_amount + excess\_claim\_amount
#' }
#'
#' An unselected row retains its full observed amount as its capped component;
#' its observed excess is reported for diagnostics but is not redistributed.
#'
#' The total excess amount is then redistributed over eligible rows with a
#' positive claim count. Rows without claims remain zero and do not receive
#' redistributed loss. By default, redistribution is proportional to the
#' number of claims in each row. A separate `redistribution_weight` can be used
#' when large-loss exposure also depends on another measure, such as insured
#' amount.
#'
#' The adjusted loss is:
#'
#' \deqn{
#' adjusted\_claim\_amount = capped\_claim\_amount + redistributed\_excess
#' }
#'
#' The function always rescales the redistribution so that the total adjusted
#' claim amount equals the total observed claim amount, subject to numerical
#' tolerance.
#'
#' ## Reproducing the redistribution
#'
#' The calculation columns make the redistribution directly auditable. For
#' partial redistribution, the loading before preservation is:
#'
#' \deqn{
#' blended\_loading = Z_g \cdot risk\_factor\_loading_g +
#' (1 - Z_g) \cdot portfolio\_loading
#' }
#'
#' where `Z_g` is the credibility of risk-factor level `g`. Because blending
#' can change the total amount implied by these loadings, a single preservation
#' factor is applied:
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
#' For example, suppose a sector loading is 30 per unit of weight, the
#' portfolio loading is 20 and sector credibility is 40 percent. The blended
#' loading is `0.40 * 30 + 0.60 * 20 = 24`. If the preservation factor is 1.10,
#' the final loading is 26.4. A receiving row with redistribution weight 2 is
#' assigned `26.4 * 2 = 52.8`. Its adjusted claim amount equals its capped claim
#' amount plus 52.8.
#'
#' With portfolio redistribution, credibility is zero and the blend equals the
#' portfolio loading. With risk-factor redistribution, credibility is one and
#' the blend equals the risk-factor loading.
#'
#' ## Eligibility and redistribution weights
#'
#' `receives_redistribution` controls which claim-bearing rows receive part of
#' the excess loss. Rows for which this logical column is `FALSE` receive zero,
#' but their observed excess loss still contributes to the total amount being
#' redistributed. If the argument is `NULL`, all rows with a positive claim
#' count receive redistribution.
#'
#' `redistribute_excess` controls which large losses contribute their excess
#' part to the redistribution. If it is `NULL`, every row with
#' `claim_amount > threshold` contributes. For a row marked `FALSE`, the claim
#' is not capped: its full observed amount remains in the adjusted claim amount
#' and none of its excess enters the redistribution pool. This keeps excluded
#' large losses in the severity experience without spreading them over other
#' claims.
#'
#' `redistribution_weight` controls the relative shares among receiving rows.
#' If it is `NULL`, claim count is used. For example, a user can create
#' `claim_count * insured_amount` as a weight when claims on policies with a
#' higher insured amount should receive a larger part of the large-loss burden.
#' The function does not automatically link the claim threshold to an insured-
#' amount threshold; these are separate actuarial choices.
#'
#' ## Redistribution methods
#'
#' The `redistribution_method` argument controls how excess experience is
#' shared:
#'
#' - `"portfolio"` gives every claim the same excess amount per claim. No
#'   risk-factor-level experience is used.
#' - `"risk_factor"` redistributes excess loss within each risk-factor level.
#'   Only the experience of that level is used.
#' - `"partial"` blends the risk-factor loading with the portfolio loading
#'   using credibility. This balances group responsiveness with portfolio
#'   stability.
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
#' ## Aggregated portfolio rows
#'
#' When a row contains multiple claims, `claim_amount` is treated as the total
#' loss for that row and `threshold` is applied to that row total. The function
#' cannot determine which individual claim crossed the threshold from
#' aggregated data. Use claim-level data when the threshold must be applied to
#' each individual claim.
#'
#' @param data A data.frame with portfolio-level or claim-level observations.
#' @param claim_amount Character string. Numeric column containing observed
#'   claim amounts or aggregate claim loss per row.
#' @param threshold Positive numeric scalar. Claim amounts above this value are
#'   capped before the excess amount is redistributed.
#' @param claim_count Optional character string. Numeric claim-count column.
#'   Claim count determines which rows contain claims and is the denominator for
#'   adjusted average claim amount. It is also the default redistribution
#'   weight. If `NULL`, every row with `claim_amount > 0` is treated as one
#'   claim.
#' @param redistribution_weight Optional character string. Numeric non-negative
#'   column determining the relative redistribution shares. If `NULL`, claim
#'   count is used. Values must be positive for rows that receive
#'   redistribution.
#' @param receives_redistribution Optional character string. Logical column
#'   indicating which rows may receive redistributed excess loss. A row only
#'   receives redistribution when this column is `TRUE` and its claim count is
#'   positive. Rows with `FALSE` receive zero, while their observed excess loss
#'   remains part of the total amount being redistributed. If `NULL`, all rows
#'   with a positive claim count receive redistribution.
#' @param redistribute_excess Optional character string. Logical column
#'   indicating which large losses have their excess part redistributed. Rows
#'   with `FALSE` are not capped: their full observed claim amount remains in
#'   the adjusted claim amount and their excess does not enter the
#'   redistribution pool. If `NULL`, all rows above `threshold` contribute
#'   their excess.
#' @param risk_factor Optional character string. Risk-factor column used for
#'   `redistribution_method = "risk_factor"` or
#'   `redistribution_method = "partial"`.
#' @param redistribution_method Character string. Method used to redistribute
#'   excess loss: `"portfolio"`, `"risk_factor"` or `"partial"`.
#' @param credibility Optional numeric scalar between zero and one. For partial
#'   redistribution, use this credibility for every risk-factor level. If
#'   `NULL`, credibility is derived from `credibility_basis`.
#' @param credibility_basis Character string. Experience measure used for
#'   automatic credibility: `"claims"` or `"excess_records"`.
#' @param credibility_threshold Positive numeric scalar. Amount of experience
#'   required to reach 50 percent credibility.
#' @param credibility_scale Non-negative numeric scalar. Multiplier applied to
#'   credibility before it is bounded between zero and one.
#' @param calculation_details Logical. If `TRUE`, append the risk-factor
#'   loading, credibility, portfolio loading, blended loading, preservation
#'   factor and final redistribution loading used in the row-level calculation.
#'   Set to `FALSE` for a compact modelling data set. The same information
#'   remains available through [summary.excess_redistribution()].
#'
#' @return The original data.frame with class `"excess_redistribution"` and six
#'   dynamically named claim-amount columns. The object prints as an ordinary
#'   data.frame;
#'   [summary.excess_redistribution()] provides an audit of contributed,
#'   received and shifted loss. The added columns are:
#'   \describe{
#'     \item{`<claim_amount>_capped`}{Observed claim amount capped at
#'     `threshold` when its excess is redistributed. An unselected row retains
#'     its full observed amount.}
#'     \item{`<claim_amount>_excess`}{Observed claim amount above `threshold`.}
#'     \item{`<claim_amount>_is_excess`}{Whether the row's claim amount exceeds
#'     `threshold`.}
#'     \item{`<claim_amount>_redistributed_excess`}{Excess loss redistributed
#'     to the row. Rows without claims receive zero.}
#'     \item{`<claim_amount>_adjusted`}{Capped claim amount plus redistributed
#'     excess loss.}
#'     \item{`<claim_amount>_adjusted_average`}{Adjusted claim amount divided by
#'     the row's claim count. This is the response that can be used in a
#'     claim-count-weighted severity GLM. Rows without claims contain zero.}
#'   }
#'   With `calculation_details = TRUE`, the result also contains
#'   `<risk_factor>_excess_loading`, `<risk_factor>_credibility`,
#'   `portfolio_excess_loading`, `blended_excess_loading`,
#'   `redistribution_scaling_factor` and `final_redistribution_loading`.
#'   The scaling factor preserves the total loss being redistributed;
#'   `final_redistribution_loading` equals `blended_excess_loading` times this
#'   factor for receiving rows.
#'
#' @seealso [summary.excess_redistribution()]
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
#'     0, 40000, 90000, 150000, 300000
#'   )
#' )
#'
#' adjusted <- redistribute_excess_loss(
#'   portfolio,
#'   claim_amount = "claim_amount",
#'   threshold = 100000,
#'   claim_count = "claim_count",
#'   risk_factor = "sector",
#'   redistribution_method = "partial"
#' )
#' summary(adjusted)
#'
#' # Inspect the complete row-level calculation. The last amount equals the
#' # final loading multiplied by the row's redistribution weight.
#' adjusted[c(
#'   "sector_excess_loading", "sector_credibility",
#'   "portfolio_excess_loading", "blended_excess_loading",
#'   "redistribution_scaling_factor", "final_redistribution_loading",
#'   "claim_amount_redistributed_excess"
#' )]
#'
#' # Keep the modelling data compact when row-level calculation details are not
#' # needed. summary() still provides the complete allocation audit.
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
#' # Fit a severity model to the adjusted average claim amount.
#' severity_data <- adjusted[adjusted$claim_count > 0, ]
#' stats::glm(
#'   claim_amount_adjusted_average ~ sector,
#'   weights = claim_count,
#'   family = stats::Gamma(link = "log"),
#'   data = severity_data
#' )
#'
#' # Portfolio redistribution pools the excess loss across all sectors. Every
#' # receiving claim gets the same redistributed amount per unit of weight.
#' # A sector's own historical large-loss experience does not determine its
#' # share; sector differences are subsequently estimated by the severity GLM.
#' portfolio_adjusted <- redistribute_excess_loss(
#'   portfolio,
#'   claim_amount = "claim_amount",
#'   threshold = 100000,
#'   claim_count = "claim_count",
#'   redistribution_method = "portfolio"
#' )
#' summary(portfolio_adjusted, by = "sector")
#'
#' # Risk-factor redistribution keeps each sector's excess burden within that
#' # sector. Claims in Industry receive only redistributed excess originating
#' # from Industry, and the same applies to Retail. This preserves sector-level
#' # large-loss experience, but can be less stable for sectors with few claims.
#' sector_adjusted <- redistribute_excess_loss(
#'   portfolio,
#'   claim_amount = "claim_amount",
#'   threshold = 100000,
#'   claim_count = "claim_count",
#'   risk_factor = "sector",
#'   redistribution_method = "risk_factor"
#' )
#'
#' # Give claims on policies with a higher insured amount a larger share, while
#' # restricting redistribution to policies insured for at least 100,000.
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
#' # Exclude catastrophe events and unsettled claims from the excess amounts
#' # that are redistributed. Their full observed claim amounts remain in the
#' # adjusted severity data.
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
    calculation_details = TRUE) {
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
  receives <- counts > 0
  if (!is.null(receives_redistribution)) {
    receives <- receives & data[[receives_redistribution]]
  }
  if (!any(receives)) {
    stop("At least one claim-bearing row must receive redistribution.",
         call. = FALSE)
  }
  weights <- if (is.null(redistribution_weight)) {
    counts
  } else {
    data[[redistribution_weight]]
  }
  if (any(weights[receives] <= 0)) {
    stop("`redistribution_weight` must be positive for rows that receive redistribution.",
         call. = FALSE)
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
  output_cols <- c(
    capped_col, excess_col, indicator_col, redistributed_col, adjusted_col,
    average_col
  )
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
  out[[redistributed_col]] <- allocated$expected_excess_loss
  out[[adjusted_col]] <- out[[capped_col]] + out[[redistributed_col]]
  out[[average_col]] <- ifelse(counts > 0, out[[adjusted_col]] / counts, 0)
  if (isTRUE(calculation_details)) {
    out[[risk_factor_loading_col]] <- risk_factor_loading
    out[[credibility_col]] <- credibility_values
    out$portfolio_excess_loading <- portfolio_loading
    out$blended_excess_loading <- blended_loading
    out$redistribution_scaling_factor <- rep(scaling_factor, nrow(out))
    out$final_redistribution_loading <- final_loading
  }

  observed_total <- sum(out[[claim_amount]])
  adjusted_total <- sum(out[[adjusted_col]])
  tolerance <- sqrt(.Machine$double.eps) * max(1, observed_total)
  if (abs(observed_total - adjusted_total) > tolerance) {
    stop("Redistributed claim amounts do not reconcile to observed loss.",
         call. = FALSE)
  }

  attr(out, "claim_amount") <- claim_amount
  attr(out, "claim_count") <- claim_count
  attr(out, "redistribution_weight") <- redistribution_weight
  attr(out, "receives_redistribution") <- receives_redistribution
  attr(out, "redistribute_excess") <- redistribute_excess
  attr(out, "threshold") <- threshold
  attr(out, "risk_factor") <- risk_factor
  attr(out, "redistribution_method") <- redistribution_method
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
  attr(out, "observed_excess_column") <- excess_col
  attr(out, "redistributed_excess_column") <- redistributed_col
  attr(out, "adjusted_claim_amount_column") <- adjusted_col
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
#' of its observed excess burden to other segments.
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
  redistributed_col <- attr(object, "redistributed_excess_column", exact = TRUE)
  adjusted_col <- attr(object, "adjusted_claim_amount_column", exact = TRUE)
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
  required <- list(claim_amount, excess_col, redistributed_col, adjusted_col)
  required_names <- unlist(required, use.names = FALSE)
  if (any(vapply(required, is.null, logical(1))) ||
      length(required_names) != 4L ||
      !all(required_names %in% names(object)) ||
      any(vapply(list(
        counts, weights, receives, contributed, risk_factor_loading,
        credibility_values, portfolio_loading, blended_loading,
        scaling_factor, final_loading
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
    adjusted_loss <- sum(object[[adjusted_col]][idx])
    received <- sum(object[[redistributed_col]][idx])
    contributed_total <- sum(contributed[idx])
    row <- data.frame(
      n_records = length(idx),
      claim_count = claim_total,
      redistribution_weight = sum(weights[idx][receives[idx]]),
      n_excess_records = sum(object[[excess_col]][idx] > 0),
      n_redistributed_excess_records = sum(contributed[idx] > 0),
      observed_loss = observed_loss,
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
  attr(out, "risk_factor") <- risk_factor
  attr(out, "redistribution_weight") <-
    attr(object, "redistribution_weight", exact = TRUE)
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
