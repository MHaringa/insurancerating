#' Assess possible excess-loss thresholds
#'
#' @description
#' `assess_excess_thresholds()` is a diagnostic helper for capped severity and
#' pure premium modelling. It does not choose the best threshold automatically.
#' Instead, it shows how much claim cost and claim count sits above candidate
#' thresholds, so actuaries and governance stakeholders can make an informed
#' threshold choice before using [calculate_excess_loss()].
#'
#' This is useful when the regular severity GLM is fitted on capped claim
#' amounts, for example `pmin(claim_amount, 100000)`, and the expected part
#' above the cap is assessed separately as a technical risk premium component.
#'
#' @param data A `data.frame` with claim-level observations.
#' @param claim_amount Character string. Claim amount column.
#' @param exposure Character string. Exposure column.
#' @param thresholds Numeric vector of candidate thresholds.
#' @param by Optional character string. Grouping column.
#' @param premium Optional character string. Premium column.
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
#'   earned_exposure = rep(1, 10),
#'   earned_premium = rep(10000, 10)
#' )
#'
#' thresholds <- assess_excess_thresholds(
#'   data = claims,
#'   claim_amount = "claim_amount",
#'   exposure = "earned_exposure",
#'   thresholds = c(25000, 50000, 100000, 150000),
#'   by = "sector",
#'   premium = "earned_premium"
#' )
#'
#' autoplot(thresholds, y = "excess_per_exposure")
#'
#' @export
assess_excess_thresholds <- function(data,
                                     claim_amount,
                                     exposure,
                                     thresholds,
                                     by = NULL,
                                     premium = NULL) {
  validate_threshold_assessment_inputs(data, claim_amount, exposure, thresholds, by, premium)

  groups <- if (is.null(by)) {
    list(All = seq_len(nrow(data)))
  } else {
    split(seq_len(nrow(data)), as.character(data[[by]]))
  }

  out <- lapply(thresholds, function(threshold) {
    rows <- lapply(names(groups), function(g) {
      idx <- groups[[g]]
      amount_i <- data[[claim_amount]][idx]
      exposure_i <- data[[exposure]][idx]
      total_loss <- sum(amount_i, na.rm = TRUE)
      capped_loss <- sum(pmin(amount_i, threshold), na.rm = TRUE)
      excess_loss <- sum(pmax(amount_i - threshold, 0), na.rm = TRUE)
      n_claims <- length(amount_i)
      claims_above <- sum(amount_i > threshold, na.rm = TRUE)
      exposure_sum <- sum(exposure_i, na.rm = TRUE)
      ans <- data.frame(
        threshold = threshold,
        n_claims = n_claims,
        claims_above = claims_above,
        share_claims_above = claims_above / n_claims,
        total_loss = total_loss,
        capped_loss = capped_loss,
        excess_loss = excess_loss,
        excess_loss_share = safe_ratio_excess(excess_loss, total_loss),
        exposure = exposure_sum,
        excess_per_exposure = safe_ratio_excess(excess_loss, exposure_sum),
        stringsAsFactors = FALSE
      )
      if (!is.null(by)) {
        ans$group <- g
      }
      if (!is.null(premium)) {
        premium_sum <- sum(data[[premium]][idx], na.rm = TRUE)
        ans$premium <- premium_sum
        ans$excess_as_premium_pct <- 100 * safe_ratio_excess(excess_loss, premium_sum)
      }
      ans
    })
    do.call(rbind, rows)
  })
  out <- do.call(rbind, out)
  if (!is.null(by)) {
    out <- out[, c("threshold", "group", setdiff(names(out), c("threshold", "group"))),
               drop = FALSE]
  }
  row.names(out) <- NULL
  attr(out, "by") <- by
  attr(out, "claim_amount") <- claim_amount
  attr(out, "exposure") <- exposure
  attr(out, "premium") <- premium
  class(out) <- c("excess_threshold_assessment", "data.frame")
  out
}

validate_threshold_assessment_inputs <- function(data, claim_amount, exposure,
                                                 thresholds, by, premium) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  for (arg in c("claim_amount", "exposure")) {
    val <- get(arg)
    if (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val)) {
      stop("`", arg, "` must be a single non-empty character string.", call. = FALSE)
    }
  }
  for (arg in c("by", "premium")) {
    val <- get(arg)
    if (!is.null(val) &&
        (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val))) {
      stop("`", arg, "` must be NULL or a single non-empty character string.",
           call. = FALSE)
    }
  }
  missing_cols <- setdiff(c(claim_amount, exposure, by, premium), names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in `data`: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[claim_amount]]) || any(is.na(data[[claim_amount]])) ||
      any(data[[claim_amount]] < 0)) {
    stop("`claim_amount` must refer to a numeric column with non-negative values.",
         call. = FALSE)
  }
  if (!is.numeric(data[[exposure]]) || any(is.na(data[[exposure]])) ||
      any(data[[exposure]] < 0)) {
    stop("`exposure` must refer to a numeric column with non-negative values.",
         call. = FALSE)
  }
  if (!is.null(premium) &&
      (!is.numeric(data[[premium]]) || any(is.na(data[[premium]])) ||
       any(data[[premium]] < 0))) {
    stop("`premium` must refer to a numeric column with non-negative values.",
         call. = FALSE)
  }
  if (!is.numeric(thresholds) || length(thresholds) < 1L ||
      any(!is.finite(thresholds)) || any(thresholds <= 0)) {
    stop("`thresholds` must be a numeric vector with positive finite values.",
         call. = FALSE)
  }
}

safe_ratio_excess <- function(num, den) {
  ifelse(is.na(den) | den <= 0, NA_real_, num / den)
}

#' Print an excess threshold assessment
#'
#' @description
#' Compact print method for objects returned by [assess_excess_thresholds()].
#'
#' @param x An object of class `"excess_threshold_assessment"`.
#' @param ... Reserved for future extensions.
#'
#' @return Invisibly returns `x`.
#'
#' @author Martin Haringa
#'
#' @keywords internal
#' @export
print.excess_threshold_assessment <- function(x, ...) {
  by <- attr(x, "by")
  cat("Excess threshold assessment\n")
  cat("Thresholds: ", length(unique(x$threshold)), "\n", sep = "")
  cat("Group variable: ", if (is.null(by)) "none" else by, "\n", sep = "")
  cat("Threshold range: ",
      format(min(x$threshold), big.mark = ","), " - ",
      format(max(x$threshold), big.mark = ","), "\n", sep = "")
  cat("Total claims: ", sum(x$n_claims[x$threshold == min(x$threshold)]),
      "\n", sep = "")
  cat("Total exposure: ",
      format(sum(x$exposure[x$threshold == min(x$threshold)]), big.mark = ","),
      "\n", sep = "")
  invisible(x)
}

#' Summarise an excess threshold assessment
#'
#' @description
#' Summarises the threshold assessment across groups, returning one row per
#' threshold.
#'
#' @param object An object of class `"excess_threshold_assessment"`.
#' @param ... Reserved for future extensions.
#'
#' @return A `data.frame` with threshold-level diagnostics.
#'
#' @author Martin Haringa
#'
#' @keywords internal
#' @export
summary.excess_threshold_assessment <- function(object, ...) {
  fun <- function(z) {
    out <- data.frame(
      threshold = z$threshold[1],
      claims_above = sum(z$claims_above),
      excess_loss = sum(z$excess_loss),
      excess_loss_share = safe_ratio_excess(sum(z$excess_loss), sum(z$total_loss)),
      excess_per_exposure = safe_ratio_excess(sum(z$excess_loss), sum(z$exposure)),
      stringsAsFactors = FALSE
    )
    if ("premium" %in% names(z)) {
      out$excess_as_premium_pct <- 100 * safe_ratio_excess(
        sum(z$excess_loss),
        sum(z$premium)
      )
    }
    out
  }
  out <- do.call(rbind, lapply(split(object, object$threshold), fun))
  row.names(out) <- NULL
  out
}

#' Plot an excess threshold assessment
#'
#' @description
#' Visualise how excess-loss diagnostics move across candidate thresholds.
#'
#' @param object An object returned by [assess_excess_thresholds()].
#' @param y Character. Diagnostic to plot on the y-axis.
#' @param ... Reserved for future extensions.
#'
#' @return A ggplot object.
#'
#' @author Martin Haringa
#'
#' @export
autoplot.excess_threshold_assessment <- function(object,
                                                 y = c(
                                                   "excess_loss",
                                                   "excess_per_exposure",
                                                   "claims_above",
                                                   "excess_loss_share"
                                                 ),
                                                 ...) {
  y <- match.arg(y)
  .check_dots_empty(...)
  pal <- .plot_palette_ir()
  grid_theme <- .plot_grid_theme_ir()
  grouped <- "group" %in% names(object)
  p <- ggplot2::ggplot(object, ggplot2::aes(x = .data[["threshold"]], y = .data[[y]]))
  if (grouped) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(color = .data[["group"]]), linewidth = 0.6) +
      ggplot2::geom_point(ggplot2::aes(color = .data[["group"]]), size = 1.8)
  } else {
    p <- p +
      ggplot2::geom_line(color = pal$risk_premium, linewidth = 0.6) +
      ggplot2::geom_point(color = pal$risk_premium, size = 1.8)
  }
  p +
    ggplot2::labs(
      title = "Excess threshold assessment",
      x = "Threshold",
      y = y,
      color = NULL
    ) +
    ggplot2::theme_minimal() +
    grid_theme
}

#' Calculate an excess-loss vector for capped severity modelling
#'
#' @description
#' Estimate and allocate the expected cost of large claims above a chosen cap.
#'
#' In pricing work, a severity model is often fitted on capped claim amounts,
#' for example `pmin(claim_amount, 100000)`, because very large claims can be
#' too sparse or volatile to model directly in the regular severity GLM.
#' `calculate_excess_loss()` helps add this missing part back in by estimating
#' the claim cost above the cap and allocating that amount to the selected
#' portfolio rows. The resulting vector can then be added to the technical risk
#' premium or pure premium.
#'
#' The excess-loss amount is part of the technical risk premium. It is not meant
#' as a commercial loading or margin. [add_excess_loss()] can be used afterwards
#' to copy the calculated vectors to a data frame without recalculating them.
#'
#' `method` determines the total excess-loss amount:
#'
#' \describe{
#'   \item{`"empirical"`}{Uses the observed excess above `excess_threshold`.
#'   This is transparent and easy to reconcile, but sensitive to a few very
#'   large claims.}
#'   \item{`"bootstrap"`}{Bootstraps claims above `fit_threshold` and calculates
#'   the excess above `excess_threshold` in each sample. This keeps the method
#'   data-driven while giving a sense of sampling variability in sparse
#'   large-loss experience. With `bootstrap_smooth = TRUE`, the sampled large
#'   claims are perturbed around their observed values using
#'   `bootstrap_bandwidth`. This avoids relying only on exact historical claim
#'   amounts and can be useful when large-loss experience is sparse, discrete or
#'   strongly influenced by a few individual claims.}
#'   \item{`"manual"`}{Uses `manual_excess` as the total excess-loss amount.
#'   This is useful when the amount comes from expert judgement, governance,
#'   reinsurance information or an external benchmark.}
#' }
#'
#' `allocation_method` determines how the total excess-loss amount is allocated
#' back to rows:
#'
#' \describe{
#'   \item{`"exposure"`}{Allocates in proportion to `allocation_weights`, with
#'   allocation factor 1 for selected rows. In practice this means that every
#'   selected row receives the same excess-loss loading per unit of allocation
#'   weight, for example per unit of earned exposure. This is the most stable
#'   choice when large-loss experience is too sparse to support a credible split
#'   between groups.}
#'   \item{`"historical_excess"`}{Derives allocation factors from observed
#'   excess above `excess_threshold` per allocation group. Groups that produced
#'   more observed excess receive a larger allocation factor, after allowing for
#'   their allocation weight. This is more risk-sensitive than exposure
#'   allocation, but can be volatile when a few large claims dominate the
#'   experience.}
#'   \item{`"bootstrap_excess"`}{Derives allocation factors from bootstrapped
#'   excess shares per allocation group. For each bootstrap sample the excess
#'   share by group is calculated, and the average share is translated into an
#'   allocation factor. This is a smoother data-driven alternative to using the
#'   raw historical excess shares, especially when large losses are sparse but
#'   you still want the allocation to reflect observed group differences.}
#'   \item{`"factor"`}{Uses a user supplied `allocation_factor` column. This is
#'   appropriate when the allocation follows a pricing, underwriting or
#'   governance decision rather than the observed large-loss split alone. For
#'   example, a factor can restrict allocation to selected segments or give one
#'   segment a higher share based on expert judgement.}
#' }
#'
#' The allocation arguments work together as follows. `allocation_by` and
#' `allocation_levels` define the part of the portfolio that receives the
#' excess-loss component, for example selected industry groups or coverage
#' segments. Rows outside those levels receive allocation factor 0. Within the
#' selected part, `allocation_weights` defines the volume measure used to spread
#' the amount, usually exposure or another earned-volume measure.
#'
#' `allocation_factor` is the row-level multiplier used in the allocation base:
#' `allocation_base = allocation_weights * allocation_factor`. A factor of 0
#' means no allocation, 1 means standard allocation, and values above or below 1
#' allocate relatively more or less excess-loss to that row or group. For
#' `allocation_method = "exposure"`, the factor is 1 for selected rows. For
#' `allocation_method = "historical_excess"` and `"bootstrap_excess"`, the
#' factor is derived so that the final allocation follows the observed or
#' bootstrapped excess shares by group, after allowing for `allocation_weights`.
#' For `allocation_method = "factor"`, the factor is read directly from the
#' column named in `allocation_factor`.
#'
#' `output` determines which row-level vector is returned directly. All variants
#' are also stored as attributes, so [add_excess_loss()] can add several
#' diagnostic columns without recalculating.
#'
#' `fit_threshold` and `excess_threshold` have different roles. `fit_threshold`
#' determines which claims are used as large-loss information. `excess_threshold`
#' determines which part is added as excess-loss. With `fit_threshold = 20000`
#' and `excess_threshold = 100000`, claims between 20k and 100k are sampled in
#' bootstrap methods but contribute zero excess through
#' `pmax(sampled_amount - excess_threshold, 0)`.
#'
#' @param data A `data.frame`.
#' @param claim_amount Character string. Claim amount column.
#' @param exposure Character string. Exposure column.
#' @param fit_threshold Positive numeric scalar. Claims above this value are
#'   used as large-loss information in bootstrap calculations.
#' @param excess_threshold Numeric scalar larger than `fit_threshold`. Only the
#'   part of a claim above this value is added as excess-loss.
#' @param method Character. One of `"empirical"`, `"bootstrap"` or `"manual"`.
#' @param allocation_method Character. One of `"exposure"`,
#'   `"historical_excess"`, `"bootstrap_excess"` or `"factor"`.
#' @param allocation_by Optional character string. Allocation grouping column.
#' @param allocation_levels Optional character vector with selected levels.
#' @param allocation_factor Optional character string with user supplied
#'   allocation factors. Required when `allocation_method = "factor"`.
#' @param allocation_weights Character string. Allocation weight column.
#' @param bootstrap_samples Positive whole number. Number of bootstrap samples.
#' @param bootstrap_smooth Logical. If `TRUE`, sampled large losses are
#'   multiplied by `exp(rnorm(..., 0, bootstrap_bandwidth))`. This adds a small
#'   multiplicative perturbation, making the bootstrap less discrete. A value
#'   around `bootstrap_bandwidth = 0.10` is a practical starting point; higher
#'   values create more tail variability and should be justified.
#' @param bootstrap_bandwidth Non-negative numeric scalar used when
#'   `bootstrap_smooth = TRUE`.
#' @param bootstrap_seed Optional numeric seed.
#' @param manual_excess Optional non-negative numeric scalar for
#'   `method = "manual"`. This is the total excess-loss amount to allocate.
#' @param output Character. One of `"amount"`, `"share"`, `"factor"` or
#'   `"base"`.
#'
#' @return A numeric vector with class `c("excess_loss_vector", "numeric")`.
#'
#' @author Martin Haringa
#'
#' @examples
#' claims <- data.frame(
#'   sector = rep(c("Industry", "Retail", "Services"), each = 6),
#'   claim_amount = c(
#'     1000, 25000, 120000, 8000, 45000, 170000,
#'     2000, 30000, 90000, 150000, 6000, 35000,
#'     1500, 12000, 18000, 22000, 30000, 40000
#'   ),
#'   earned_exposure = c(rep(1, 12), rep(2, 6)),
#'   earned_premium = rep(10000, 18)
#' )
#'
#' x <- calculate_excess_loss(
#'   data = claims,
#'   claim_amount = "claim_amount",
#'   exposure = "earned_exposure",
#'   fit_threshold = 20000,
#'   excess_threshold = 100000,
#'   method = "empirical",
#'   allocation_method = "exposure",
#'   allocation_by = "sector",
#'   allocation_levels = c("Industry", "Retail"),
#'   allocation_weights = "earned_exposure"
#' )
#'
#' x <- calculate_excess_loss(
#'   data = claims,
#'   claim_amount = "claim_amount",
#'   exposure = "earned_exposure",
#'   fit_threshold = 20000,
#'   excess_threshold = 100000,
#'   method = "bootstrap",
#'   allocation_method = "bootstrap_excess",
#'   allocation_by = "sector",
#'   allocation_levels = c("Industry", "Retail"),
#'   allocation_weights = "earned_exposure",
#'   bootstrap_samples = 100,
#'   bootstrap_smooth = TRUE,
#'   bootstrap_seed = 123
#' )
#'
#' claims <- add_excess_loss(claims, x, name = "large_loss")
#' summary(x)
#' allocation_factor(x, type = "summary")
#' autoplot(x, by = "sector", y = "allocation_factor")
#'
#' @importFrom ggplot2 autoplot
#' @importFrom stats aggregate median quantile rnorm sd
#' @export
calculate_excess_loss <- function(data,
                                  claim_amount,
                                  exposure,
                                  fit_threshold,
                                  excess_threshold,
                                  method = c("empirical", "bootstrap", "manual"),
                                  allocation_method = c(
                                    "exposure",
                                    "historical_excess",
                                    "bootstrap_excess",
                                    "factor"
                                  ),
                                  allocation_by = NULL,
                                  allocation_levels = NULL,
                                  allocation_factor = NULL,
                                  allocation_weights = exposure,
                                  bootstrap_samples = 1000,
                                  bootstrap_smooth = FALSE,
                                  bootstrap_bandwidth = 0.10,
                                  bootstrap_seed = NULL,
                                  manual_excess = NULL,
                                  output = c("amount", "share", "factor", "base")) {
  method <- match.arg(method)
  allocation_method <- match.arg(allocation_method)
  output <- match.arg(output)

  validate_calculate_excess_loss_inputs(
    data = data,
    claim_amount = claim_amount,
    exposure = exposure,
    fit_threshold = fit_threshold,
    excess_threshold = excess_threshold,
    method = method,
    allocation_method = allocation_method,
    allocation_by = allocation_by,
    allocation_levels = allocation_levels,
    allocation_factor = allocation_factor,
    allocation_weights = allocation_weights,
    bootstrap_samples = bootstrap_samples,
    bootstrap_smooth = bootstrap_smooth,
    bootstrap_bandwidth = bootstrap_bandwidth,
    bootstrap_seed = bootstrap_seed,
    manual_excess = manual_excess
  )

  df <- prepare_excess_loss_data(
    data = data,
    claim_amount = claim_amount,
    exposure = exposure,
    excess_threshold = excess_threshold,
    allocation_by = allocation_by,
    allocation_levels = allocation_levels,
    allocation_factor = allocation_factor,
    allocation_weights = allocation_weights
  )

  estimate <- estimate_excess_loss(
    df = df,
    fit_threshold = fit_threshold,
    excess_threshold = excess_threshold,
    method = method,
    bootstrap_samples = bootstrap_samples,
    bootstrap_smooth = bootstrap_smooth,
    bootstrap_bandwidth = bootstrap_bandwidth,
    bootstrap_seed = bootstrap_seed,
    manual_excess = manual_excess
  )

  allocation <- derive_excess_loss_allocation(
    df = df,
    allocation_method = allocation_method,
    bootstrap_samples = bootstrap_samples,
    bootstrap_smooth = bootstrap_smooth,
    bootstrap_bandwidth = bootstrap_bandwidth,
    bootstrap_seed = bootstrap_seed,
    fit_threshold = fit_threshold,
    excess_threshold = excess_threshold
  )

  vectors <- build_excess_loss_vectors(
    df = df,
    allocation_factor_vector = allocation$factor_vector,
    total_excess = estimate$total_excess
  )

  allocation_data <- data.frame(
    row_id = seq_len(nrow(df)),
    allocation_group = df$.allocation_group,
    selected = df$.selected,
    exposure = df$.exposure,
    allocation_weight = df$.allocation_weight,
    allocation_factor = vectors$factor,
    allocation_base = vectors$base,
    allocated_share = vectors$share,
    allocated_excess = vectors$amount,
    stringsAsFactors = FALSE
  )
  if (!is.null(allocation_by)) {
    allocation_data[[allocation_by]] <- df$.allocation_group
  }
  allocation_summary <- summarize_excess_loss_allocation(
    allocation_data = allocation_data,
    allocation_by = allocation_by,
    bootstrap_summary = allocation$bootstrap_summary
  )

  out <- switch(
    output,
    amount = vectors$amount,
    share = vectors$share,
    factor = vectors$factor,
    base = vectors$base
  )

  structure(
    as.numeric(out),
    class = c("excess_loss_vector", "numeric"),
    amount_vector = vectors$amount,
    share_vector = vectors$share,
    factor_vector = vectors$factor,
    base_vector = vectors$base,
    method = method,
    allocation_method = allocation_method,
    fit_threshold = fit_threshold,
    excess_threshold = excess_threshold,
    total_excess = estimate$total_excess,
    total_exposure = sum(df$.exposure),
    claim_amount_column = claim_amount,
    output = output,
    bootstrap_samples_vector = estimate$bootstrap_samples_vector,
    bootstrap_summary = allocation$bootstrap_summary,
    allocation_data = allocation_data,
    allocation_summary = allocation_summary,
    allocation_by = allocation_by,
    allocation_levels = allocation_levels,
    allocation_factor_column = allocation_factor,
    allocation_weights_column = allocation_weights,
    allocation_factor_source = allocation$allocation_factor_source
  )
}

#' Add calculated excess-loss columns to data
#'
#' @description
#' `add_excess_loss()` adds output from [calculate_excess_loss()] to a
#' `data.frame`. It does not calculate anything itself and only copies stored
#' vectors from the attributes of `x`. This keeps the workflow auditable: the
#' excess-loss calculation is done once in [calculate_excess_loss()], while
#' `add_excess_loss()` is only a data-preparation step for modelling, reporting
#' or later tariff refinement.
#'
#' With the default `include = c("amount", "share", "factor")`, the function
#' adds the allocated excess-loss amount, the row-level share of the total
#' excess-loss amount and the allocation factor used to distribute the excess.
#' Use `include = "base"` when you also want to inspect the allocation base
#' (`allocation_weights * allocation_factor`).
#'
#' @param data A `data.frame`.
#' @param x An `"excess_loss_vector"` returned by [calculate_excess_loss()].
#' @param name Character string. Base output column name.
#' @param include Character vector with any of `"amount"`, `"share"`,
#'   `"factor"` and `"base"`. `"amount"` refers to the allocated excess-loss
#'   amount, not the original claim amount column.
#' @param overwrite Logical. If `FALSE`, existing output columns cause an error.
#'
#' @return A `data.frame` with added columns.
#'
#' @author Martin Haringa
#'
#' @examples
#' claims <- data.frame(
#'   segment = rep(c("A", "B"), each = 4),
#'   claim_amount = c(1000, 120000, 30000, 8000, 2000, 150000, 40000, 6000),
#'   exposure = rep(1, 8)
#' )
#' x <- calculate_excess_loss(
#'   claims,
#'   claim_amount = "claim_amount",
#'   exposure = "exposure",
#'   fit_threshold = 20000,
#'   excess_threshold = 100000,
#'   method = "empirical",
#'   allocation_method = "exposure",
#'   allocation_by = "segment",
#'   allocation_levels = c("A", "B")
#' )
#' add_excess_loss(claims, x, name = "large_loss")
#'
#' @export
add_excess_loss <- function(data,
                            x,
                            name = "excess_loss",
                            include = c("amount", "share", "factor"),
                            overwrite = FALSE) {
  validate_add_excess_loss_inputs(data, x, name, include, overwrite)

  out <- data
  col_names <- excess_loss_column_names(name, include)
  existing <- intersect(col_names, names(out))
  if (length(existing) > 0 && !isTRUE(overwrite)) {
    stop("Column(s) already exist in `data`: ",
         paste(existing, collapse = ", "),
         ". Use `overwrite = TRUE` to replace them.",
         call. = FALSE)
  }
  vectors <- list(
    amount = attr(x, "amount_vector"),
    share = attr(x, "share_vector"),
    factor = attr(x, "factor_vector"),
    base = attr(x, "base_vector")
  )
  for (nm in include) {
    out[[excess_loss_column_names(name, nm)]] <- vectors[[nm]]
  }
  out
}

.check_dots_empty <- function(...) {
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused argument(s): ", paste(names(dots), collapse = ", "),
         call. = FALSE)
  }
}

validate_calculate_excess_loss_inputs <- function(data,
                                                  claim_amount,
                                                  exposure,
                                                  fit_threshold,
                                                  excess_threshold,
                                                  method,
                                                  allocation_method,
                                                  allocation_by,
                                                  allocation_levels,
                                                  allocation_factor,
                                                  allocation_weights,
                                                  bootstrap_samples,
                                                  bootstrap_smooth,
                                                  bootstrap_bandwidth,
                                                  bootstrap_seed,
                                                  manual_excess) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  for (arg in c("claim_amount", "exposure", "allocation_weights")) {
    val <- get(arg)
    if (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val)) {
      stop("`", arg, "` must be a single non-empty character string.",
           call. = FALSE)
    }
  }
  for (arg in c("allocation_by", "allocation_factor")) {
    val <- get(arg)
    if (!is.null(val) &&
        (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val))) {
      stop("`", arg, "` must be NULL or a single non-empty character string.",
           call. = FALSE)
    }
  }
  missing_cols <- setdiff(
    c(claim_amount, exposure, allocation_weights, allocation_by, allocation_factor),
    names(data)
  )
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in `data`: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[claim_amount]]) || any(is.na(data[[claim_amount]])) ||
      any(data[[claim_amount]] < 0)) {
    stop("`claim_amount` must refer to a numeric column with non-negative values.",
         call. = FALSE)
  }
  if (!is.numeric(data[[exposure]]) || any(is.na(data[[exposure]])) ||
      any(data[[exposure]] < 0)) {
    stop("`exposure` must refer to a numeric column with non-negative values.",
         call. = FALSE)
  }
  if (!is.numeric(data[[allocation_weights]]) ||
      any(is.na(data[[allocation_weights]])) ||
      any(data[[allocation_weights]] < 0)) {
    stop("`allocation_weights` must refer to a numeric column with non-negative values.",
         call. = FALSE)
  }
  if (!is.numeric(fit_threshold) || length(fit_threshold) != 1L ||
      !is.finite(fit_threshold) || fit_threshold <= 0) {
    stop("`fit_threshold` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(excess_threshold) || length(excess_threshold) != 1L ||
      !is.finite(excess_threshold) || excess_threshold <= fit_threshold) {
    stop("`excess_threshold` must be larger than `fit_threshold`.",
         call. = FALSE)
  }
  if (is.null(allocation_by) != is.null(allocation_levels)) {
    stop("`allocation_by` and `allocation_levels` must be supplied together.",
         call. = FALSE)
  }
  if (!is.null(allocation_levels) &&
      (!is.character(allocation_levels) || length(allocation_levels) < 1L ||
       any(is.na(allocation_levels)))) {
    stop("`allocation_levels` must be a non-empty character vector.",
         call. = FALSE)
  }
  if (!identical(allocation_method, "factor") && is.null(allocation_by)) {
    stop("`allocation_by` and `allocation_levels` are required when `allocation_method != 'factor'`.",
         call. = FALSE)
  }
  if (identical(allocation_method, "factor") && is.null(allocation_factor)) {
    stop("`allocation_method = 'factor'` requires `allocation_factor`.",
         call. = FALSE)
  }
  if (!is.null(allocation_factor)) {
    af <- data[[allocation_factor]]
    if (!is.numeric(af) || any(is.na(af)) || any(af < 0)) {
      stop("`allocation_factor` must refer to a numeric column with non-missing non-negative values.",
           call. = FALSE)
    }
  }
  if ((identical(method, "bootstrap") ||
       identical(allocation_method, "bootstrap_excess")) &&
      (!is.numeric(bootstrap_samples) || length(bootstrap_samples) != 1L ||
       is.na(bootstrap_samples) || bootstrap_samples < 1 ||
       bootstrap_samples != floor(bootstrap_samples))) {
    stop("`bootstrap_samples` must be a positive whole number.", call. = FALSE)
  }
  if (!is.logical(bootstrap_smooth) || length(bootstrap_smooth) != 1L ||
      is.na(bootstrap_smooth)) {
    stop("`bootstrap_smooth` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(bootstrap_bandwidth) || length(bootstrap_bandwidth) != 1L ||
      !is.finite(bootstrap_bandwidth) || bootstrap_bandwidth < 0) {
    stop("`bootstrap_bandwidth` must be a single non-negative number.",
         call. = FALSE)
  }
  if (!is.null(bootstrap_seed) &&
      (!is.numeric(bootstrap_seed) || length(bootstrap_seed) != 1L ||
       is.na(bootstrap_seed))) {
    stop("`bootstrap_seed` must be NULL or a single number.", call. = FALSE)
  }
  if (identical(method, "manual") &&
      (!is.numeric(manual_excess) || length(manual_excess) != 1L ||
       !is.finite(manual_excess) || manual_excess < 0)) {
    stop("`manual_excess` must be a single non-negative number when `method = 'manual'`.",
         call. = FALSE)
  }
}

prepare_excess_loss_data <- function(data,
                                     claim_amount,
                                     exposure,
                                     excess_threshold,
                                     allocation_by,
                                     allocation_levels,
                                     allocation_factor,
                                     allocation_weights) {
  df <- data.frame(
    .amount = data[[claim_amount]],
    .exposure = data[[exposure]],
    .allocation_weight = data[[allocation_weights]],
    stringsAsFactors = FALSE
  )
  df$.historical_excess <- pmax(df$.amount - excess_threshold, 0)
  df$.allocation_group <- if (is.null(allocation_by)) {
    "allocated"
  } else {
    as.character(data[[allocation_by]])
  }
  df$.selected <- if (is.null(allocation_by)) {
    TRUE
  } else {
    df$.allocation_group %in% allocation_levels
  }
  if (!is.null(allocation_factor)) {
    df$.supplied_allocation_factor <- data[[allocation_factor]]
    if (is.null(allocation_by)) {
      df$.selected <- df$.supplied_allocation_factor > 0
      df$.allocation_group[!df$.selected] <- "not_allocated"
    } else {
      df$.selected <- df$.selected & df$.supplied_allocation_factor > 0
    }
  } else {
    df$.supplied_allocation_factor <- NA_real_
  }
  if (!any(df$.selected)) {
    stop("No allocation target could be determined.", call. = FALSE)
  }
  if (any(df$.selected & df$.allocation_weight <= 0)) {
    stop("`allocation_weights` must be positive for selected rows.",
         call. = FALSE)
  }
  df
}

estimate_excess_loss <- function(df,
                                 fit_threshold,
                                 excess_threshold,
                                 method,
                                 bootstrap_samples,
                                 bootstrap_smooth,
                                 bootstrap_bandwidth,
                                 bootstrap_seed,
                                 manual_excess) {
  if (identical(method, "manual")) {
    return(list(total_excess = manual_excess, bootstrap_samples_vector = numeric()))
  }
  if (identical(method, "empirical")) {
    return(list(
      total_excess = sum(df$.historical_excess, na.rm = TRUE),
      bootstrap_samples_vector = numeric()
    ))
  }
  samples <- bootstrap_excess_totals(
    amount = df$.amount,
    fit_threshold = fit_threshold,
    excess_threshold = excess_threshold,
    bootstrap_samples = bootstrap_samples,
    bootstrap_smooth = bootstrap_smooth,
    bootstrap_bandwidth = bootstrap_bandwidth,
    bootstrap_seed = bootstrap_seed
  )
  list(total_excess = mean(samples), bootstrap_samples_vector = samples)
}

bootstrap_excess_totals <- function(amount,
                                    fit_threshold,
                                    excess_threshold,
                                    bootstrap_samples,
                                    bootstrap_smooth,
                                    bootstrap_bandwidth,
                                    bootstrap_seed) {
  tail_amount <- amount[amount > fit_threshold]
  if (length(tail_amount) == 0) {
    stop("No claims exceed `fit_threshold`.", call. = FALSE)
  }
  if (!is.null(bootstrap_seed)) {
    set.seed(bootstrap_seed)
  }
  replicate(bootstrap_samples, {
    sampled <- sample(tail_amount, length(tail_amount), replace = TRUE)
    if (isTRUE(bootstrap_smooth)) {
      sampled <- sampled * exp(stats::rnorm(
        length(sampled),
        mean = 0,
        sd = bootstrap_bandwidth
      ))
    }
    sum(pmax(sampled - excess_threshold, 0))
  })
}

derive_excess_loss_allocation <- function(df,
                                          allocation_method,
                                          bootstrap_samples,
                                          bootstrap_smooth,
                                          bootstrap_bandwidth,
                                          bootstrap_seed,
                                          fit_threshold,
                                          excess_threshold) {
  selected <- df[df$.selected, , drop = FALSE]
  if (identical(allocation_method, "exposure")) {
    factor <- numeric(nrow(df))
    factor[df$.selected] <- 1
    return(list(
      factor_vector = factor,
      bootstrap_summary = NULL,
      allocation_factor_source = "derived_exposure"
    ))
  }
  if (identical(allocation_method, "factor")) {
    factor <- numeric(nrow(df))
    factor[df$.selected] <- selected$.supplied_allocation_factor
    return(list(
      factor_vector = factor,
      bootstrap_summary = NULL,
      allocation_factor_source = "user_supplied"
    ))
  }
  group_weight <- rowsum(selected$.allocation_weight,
                         selected$.allocation_group,
                         reorder = FALSE)
  group_weight <- data.frame(
    allocation_group = rownames(group_weight),
    allocation_weight = as.numeric(group_weight[, 1]),
    stringsAsFactors = FALSE
  )
  group_weight$weight_share <- group_weight$allocation_weight /
    sum(group_weight$allocation_weight)
  if (identical(allocation_method, "historical_excess")) {
    group_excess <- rowsum(selected$.historical_excess,
                           selected$.allocation_group,
                           reorder = FALSE)
    group_excess <- data.frame(
      allocation_group = rownames(group_excess),
      excess = as.numeric(group_excess[, 1]),
      stringsAsFactors = FALSE
    )
    group <- merge(group_weight, group_excess, by = "allocation_group",
                   all.x = TRUE)
    if (sum(group$excess) <= 0) {
      stop("Historical excess is zero for the selected allocation target.",
           call. = FALSE)
    }
    group$target_share <- group$excess / sum(group$excess)
    group$allocation_factor <- ifelse(
      group$weight_share > 0,
      group$target_share / group$weight_share,
      0
    )
    return(list(
      factor_vector = match_group_factor(df, group),
      bootstrap_summary = NULL,
      allocation_factor_source = "derived_historical_excess"
    ))
  }
  boot <- bootstrap_group_excess_shares(
    df = selected,
    fit_threshold = fit_threshold,
    excess_threshold = excess_threshold,
    bootstrap_samples = bootstrap_samples,
    bootstrap_smooth = bootstrap_smooth,
    bootstrap_bandwidth = bootstrap_bandwidth,
    bootstrap_seed = bootstrap_seed
  )
  group <- merge(group_weight, boot, by = "allocation_group", all.x = TRUE)
  group$bootstrap_mean[is.na(group$bootstrap_mean)] <- 0
  group$allocation_factor <- ifelse(
    group$weight_share > 0,
    group$bootstrap_mean / group$weight_share,
    0
  )
  list(
    factor_vector = match_group_factor(df, group),
    bootstrap_summary = boot,
    allocation_factor_source = "derived_bootstrap_excess"
  )
}

bootstrap_group_excess_shares <- function(df,
                                          fit_threshold,
                                          excess_threshold,
                                          bootstrap_samples,
                                          bootstrap_smooth,
                                          bootstrap_bandwidth,
                                          bootstrap_seed) {
  tail_df <- df[df$.amount > fit_threshold, , drop = FALSE]
  groups <- unique(df$.allocation_group)
  if (nrow(tail_df) == 0) {
    stop("No selected allocation records exceed `fit_threshold`.", call. = FALSE)
  }
  if (!is.null(bootstrap_seed)) {
    set.seed(bootstrap_seed)
  }
  share_mat <- replicate(bootstrap_samples, {
    idx <- sample(seq_len(nrow(tail_df)), nrow(tail_df), replace = TRUE)
    sampled <- tail_df[idx, , drop = FALSE]
    amount <- sampled$.amount
    if (isTRUE(bootstrap_smooth)) {
      amount <- amount * exp(stats::rnorm(
        length(amount),
        mean = 0,
        sd = bootstrap_bandwidth
      ))
    }
    excess <- pmax(amount - excess_threshold, 0)
    total <- sum(excess)
    shares <- stats::setNames(rep(0, length(groups)), groups)
    if (total > 0) {
      by_group <- rowsum(excess, sampled$.allocation_group, reorder = FALSE)
      shares[rownames(by_group)] <- as.numeric(by_group[, 1]) / total
    }
    shares
  })
  share_mat <- t(share_mat)
  data.frame(
    allocation_group = colnames(share_mat),
    bootstrap_mean = colMeans(share_mat),
    bootstrap_sd = apply(share_mat, 2, stats::sd),
    p05 = apply(share_mat, 2, stats::quantile, probs = 0.05, names = FALSE),
    p50 = apply(share_mat, 2, stats::quantile, probs = 0.50, names = FALSE),
    p95 = apply(share_mat, 2, stats::quantile, probs = 0.95, names = FALSE),
    stringsAsFactors = FALSE
  )
}

match_group_factor <- function(df, group) {
  idx <- match(df$.allocation_group, group$allocation_group)
  out <- numeric(nrow(df))
  selected <- df$.selected & !is.na(idx)
  out[selected] <- group$allocation_factor[idx[selected]]
  out
}

build_excess_loss_vectors <- function(df, allocation_factor_vector, total_excess) {
  base <- df$.allocation_weight * allocation_factor_vector
  if (sum(base) <= 0) {
    stop("Sum of allocation base must be positive.", call. = FALSE)
  }
  share <- base / sum(base)
  amount <- total_excess * share
  list(
    amount = as.numeric(amount),
    share = as.numeric(share),
    factor = as.numeric(allocation_factor_vector),
    base = as.numeric(base)
  )
}

summarize_excess_loss_allocation <- function(allocation_data,
                                             allocation_by,
                                             bootstrap_summary) {
  groups <- unique(allocation_data$allocation_group)
  out <- lapply(groups, function(g) {
    z <- allocation_data[allocation_data$allocation_group == g, , drop = FALSE]
    allocation_weight <- sum(z$allocation_weight)
    allocation_base <- sum(z$allocation_base)
    data.frame(
      allocation_group = g,
      n = nrow(z),
      exposure = sum(z$exposure),
      allocation_weight = allocation_weight,
      allocation_factor = safe_ratio_excess(allocation_base, allocation_weight),
      allocation_base = allocation_base,
      allocated_excess = sum(z$allocated_excess),
      allocated_share = sum(z$allocated_share),
      selected = any(z$selected),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  if (!is.null(allocation_by)) {
    out[[allocation_by]] <- out$allocation_group
  }
  if (!is.null(bootstrap_summary)) {
    out <- merge(out, bootstrap_summary, by = "allocation_group",
                 all.x = TRUE, sort = FALSE)
  }
  row.names(out) <- NULL
  out
}

validate_add_excess_loss_inputs <- function(data, x, name, include, overwrite) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!inherits(x, "excess_loss_vector")) {
    stop("`x` must be an object returned by `calculate_excess_loss()`.",
         call. = FALSE)
  }
  if (length(x) != nrow(data)) {
    stop("`length(x)` must be equal to `nrow(data)`.", call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("`name` must be a single non-empty character string.", call. = FALSE)
  }
  allowed <- c("amount", "share", "factor", "base")
  if (!is.character(include) || length(include) < 1L ||
      any(is.na(include)) || any(!include %in% allowed)) {
    stop("`include` must be a subset of: ",
         paste(allowed, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(include)) {
    stop("`include` must not contain duplicates.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }
  required <- paste0(c("amount", "share", "factor", "base"), "_vector")
  missing <- required[vapply(required, function(nm) is.null(attr(x, nm)), logical(1))]
  if (length(missing) > 0) {
    stop("`x` is missing required attribute(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
}

excess_loss_column_names <- function(name, include) {
  suffix <- c(amount = "", share = "_share", factor = "_factor", base = "_base")
  paste0(name, suffix[include])
}

#' Extract allocation factors from an excess-loss vector
#'
#' @description
#' Extract allocation diagnostics from an object returned by
#' [calculate_excess_loss()]. Use `type = "vector"` for the row-level factor,
#' `type = "data"` for row-level allocation data, and `type = "summary"` for
#' grouped allocation diagnostics.
#'
#' @param x An object returned by [calculate_excess_loss()].
#' @param type Character. Output type.
#' @param ... Reserved for future extensions.
#'
#' @return A vector or `data.frame`.
#'
#' @author Martin Haringa
#'
#' @export
allocation_factor <- function(x, type = c("data", "vector", "summary"), ...) {
  UseMethod("allocation_factor")
}

#' @export
allocation_factor.excess_loss_vector <- function(x,
                                                 type = c("data", "vector", "summary"),
                                                 ...) {
  type <- match.arg(type)
  .check_dots_empty(...)
  switch(
    type,
    vector = attr(x, "factor_vector"),
    data = attr(x, "allocation_data"),
    summary = attr(x, "allocation_summary")
  )
}

#' Print an excess-loss vector
#'
#' @description
#' Compact print method for objects returned by [calculate_excess_loss()].
#'
#' @param x An object of class `"excess_loss_vector"`.
#' @param ... Reserved for future extensions.
#'
#' @return Invisibly returns `x`.
#'
#' @author Martin Haringa
#'
#' @keywords internal
#' @export
print.excess_loss_vector <- function(x, ...) {
  cat("Excess loss vector\n")
  cat("Method: ", attr(x, "method"), "\n", sep = "")
  cat("Allocation method: ", attr(x, "allocation_method"), "\n", sep = "")
  cat("Fit threshold: ",
      format(round(attr(x, "fit_threshold")), big.mark = ","), "\n", sep = "")
  cat("Excess threshold: ",
      format(round(attr(x, "excess_threshold")), big.mark = ","), "\n", sep = "")
  cat("Total excess loss: ",
      format(round(attr(x, "total_excess")), big.mark = ","), "\n", sep = "")
  cat("Output: ", attr(x, "output"), "\n", sep = "")
  allocation_data <- attr(x, "allocation_data")
  cat("Allocated rows: ", sum(allocation_data$selected), " / ",
      nrow(allocation_data), "\n", sep = "")
  invisible(x)
}

#' Summarise an excess-loss vector
#'
#' @description
#' Return the grouped allocation summary stored on an object created by
#' [calculate_excess_loss()].
#'
#' @param object An object of class `"excess_loss_vector"`.
#' @param ... Reserved for future extensions.
#'
#' @return A `data.frame` with allocation summary columns.
#'
#' @author Martin Haringa
#'
#' @keywords internal
#' @export
summary.excess_loss_vector <- function(object, ...) {
  attr(object, "allocation_summary")
}

#' Plot an excess-loss vector
#'
#' @description
#' Visualise the allocation stored on an excess-loss vector. Without `by`, the
#' plot shows allocated versus not allocated. With `by`, it shows the selected
#' allocation metric by group. `type = "histogram"` shows the bootstrap
#' distribution of total excess-loss estimates when available.
#'
#' @param object An object returned by [calculate_excess_loss()].
#' @param by Optional character string. Grouping column available in the stored
#'   allocation data.
#' @param y Character. Allocation metric to show for `type = "bar"`.
#' @param type Character. `"bar"` or `"histogram"`.
#' @param ... Reserved for future extensions.
#'
#' @return A ggplot object.
#'
#' @author Martin Haringa
#'
#' @export
autoplot.excess_loss_vector <- function(object,
                                        by = NULL,
                                        y = c(
                                          "allocated_excess",
                                          "allocated_share",
                                          "allocation_factor",
                                          "allocation_base"
                                        ),
                                        type = c("bar", "histogram"),
                                        ...) {
  y <- match.arg(y)
  type <- match.arg(type)
  .check_dots_empty(...)
  if (!is.null(by) &&
      (!is.character(by) || length(by) != 1L || is.na(by) || !nzchar(by))) {
    stop("`by` must be NULL or a single character string.", call. = FALSE)
  }
  pal <- .plot_palette_ir()
  grid_theme <- .plot_grid_theme_ir()
  if (identical(type, "histogram")) {
    samples <- attr(object, "bootstrap_samples_vector")
    if (length(samples) == 0) {
      stop("No bootstrap samples are available for this excess-loss vector.",
           call. = FALSE)
    }
    df <- data.frame(excess_loss = samples)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data[["excess_loss"]])) +
        ggplot2::geom_histogram(bins = 30, fill = "#E6E6E6", color = NA) +
        ggplot2::geom_vline(
          xintercept = attr(object, "total_excess"),
          color = pal$risk_premium,
          linetype = "dashed",
          linewidth = 0.6
        ) +
        ggplot2::labs(
          title = "Excess-loss allocation",
          x = "Estimated excess loss",
          y = "Bootstrap samples"
        ) +
        ggplot2::theme_minimal() +
        grid_theme
    )
  }
  summary_data <- attr(object, "allocation_summary")
  if (is.null(by)) {
    df <- stats::aggregate(
      summary_data[[y]],
      by = list(allocated = ifelse(summary_data$selected, "Allocated", "Not allocated")),
      FUN = sum
    )
    names(df)[2] <- y
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data[["allocated"]], y = .data[[y]])) +
        ggplot2::geom_col(fill = pal$risk_premium, width = 0.65) +
        ggplot2::labs(title = "Excess-loss allocation", x = NULL, y = y) +
        ggplot2::theme_minimal() +
        grid_theme
    )
  }
  if (!by %in% names(summary_data)) {
    stop("`by` is not available in the stored allocation data.", call. = FALSE)
  }
  ggplot2::ggplot(summary_data, ggplot2::aes(x = .data[[by]], y = .data[[y]])) +
    ggplot2::geom_col(fill = pal$risk_premium, width = 0.65) +
    ggplot2::labs(title = "Excess-loss allocation", x = by, y = y) +
    ggplot2::theme_minimal() +
    grid_theme
}
