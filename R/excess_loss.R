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
#' @description
#' Calculate the historical excess amount above a selected threshold.
#'
#' `calculate_excess_loss()` is deliberately deterministic. It does not perform
#' bootstrap simulation, smoothing or allocation. The function simply decomposes
#' each observed claim into a capped part and an excess part:
#' `excess_claim_amount = pmax(claim_amount - threshold, 0)`.
#'
#' Use the output as input for [allocate_excess_loss()] when the historical
#' excess burden needs to be pooled, credibility-weighted or bootstrapped.
#'
#' @param data A `data.frame` with claim-level observations.
#' @param claim_amount Character string. Claim amount column.
#' @param threshold Positive numeric scalar. Claims above this value are treated
#'   as excess claims.
#'
#' @return A `data.frame` with the original data and the columns
#'   `claim_amount`, `capped_claim_amount`, `excess_claim_amount` and
#'   `is_excess_claim`.
#'
#' @author Martin Haringa
#'
#' @examples
#' claims <- data.frame(claim_amount = c(1000, 120000, 30000))
#' calculate_excess_loss(claims, claim_amount = "claim_amount", threshold = 100000)
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
#' Allocate a historical excess burden over a portfolio and optionally model
#' uncertainty around that burden.
#'
#' `allocate_excess_loss()` is the core allocation step in the excess-loss
#' workflow. It starts from a deterministic excess amount, usually
#' `excess_claim_amount` produced by [calculate_excess_loss()], and converts it
#' into an excess loading per row.
#'
#' `pooling` controls how much excess risk is shared between groups:
#' \describe{
#'   \item{`"portfolio"`}{All included rows share one portfolio-wide excess
#'   loading: total excess divided by total weight.}
#'   \item{`"group"`}{Each group carries only its own excess burden. This is
#'   responsive to group experience, but can be volatile for sparse large-loss
#'   data.}
#'   \item{`"partial"`}{Blend the group-specific loading with the portfolio
#'   loading using credibility. This is useful when some groups have enough
#'   large-loss experience to be partly credible while smaller groups should
#'   pool back toward the portfolio.}
#' }
#'
#' When `pooling = "partial"` and `credibility = NULL`, credibility is determined
#' automatically from the amount and quality of group-specific experience.
#' The automatic credibility factor reflects the size of the group, the number
#' of observed claims, the number of excess claims and the share of loss above
#' the threshold. Groups with more stable and repeated excess loss experience
#' receive more credibility. Groups with limited or incidental excess loss
#' experience are pooled more strongly towards the portfolio-wide loading.
#'
#' `severity_noise` is only available with `method = "bootstrap"`. With
#' `"lognormal"`, sampled excess claims are multiplied by lognormal noise, which
#' is usually more natural for large claims because claim amounts remain
#' positive and variation is multiplicative. `severity_noise_sd = 0.25` is a
#' practical starting point; `0.10` gives limited variation and `0.50` strong
#' variation.
#'
#' @details
#' `method = "observed"` allocates the historically observed excess loss.
#'
#' `method = "bootstrap"` resamples only the positive excess claim amounts:
#'
#' `excess_amount[excess_amount > 0]`
#'
#' The full claim distribution is not bootstrapped.
#'
#' Bootstrap allocation introduces uncertainty in both:
#'
#' \itemize{
#'   \item the total amount of excess loss;
#'   \item the distribution of excess loss across allocation groups.
#' }
#'
#' This means that the bootstrap not only changes the total excess burden that
#' needs to be redistributed, but also how that burden is distributed across
#' portfolio segments.
#'
#' When `tail_fit_threshold` is supplied, claims above the lower threshold are
#' used to better approximate the tail of the claim distribution. This can be
#' useful when only a limited number of claims exceed the main excess threshold.
#'
#' Severity noise can optionally be added to bootstrapped excess claims. This is
#' useful when the number of excess claims is small and a regular bootstrap would
#' otherwise repeatedly reproduce only the same observed large losses.
#'
#' When severity noise is used, additional variability is introduced in the size
#' of individual excess claims.
#'
#' Lognormal severity noise is usually more natural for excess claims because
#' claim amounts are positive and large losses are typically right-skewed.
#'
#' For lognormal severity noise, the approximate multiplicative p10/p50/p90
#' factors can be inspected with:
#'
#' `exp(qnorm(c(0.10, 0.50, 0.90)) * severity_noise_sd)`
#'
#' For example, with `severity_noise_sd = 0.25`, this gives approximately
#' `0.73x`, `0.97x` and `1.38x`. An excess claim of 100,000 would therefore
#' typically vary between roughly 73,000 and 138,000.
#'
#' The following validation rules apply:
#'
#' \itemize{
#'   \item `tail_fit_threshold` can only be used when `method = "bootstrap"`.
#'   \item `tail_fit_threshold` must be smaller than or equal to `threshold`.
#'   \item `severity_noise != "none"` can only be used when
#'   `method = "bootstrap"`.
#' }
#'
#' @param data A `data.frame`, typically the output of [calculate_excess_loss()].
#' @param excess_amount Character string. Excess amount column.
#' @param weight Character string. Allocation weight column, usually exposure.
#' @param include Optional character string. Logical column indicating which
#'   rows participate in the allocation. If `NULL`, all rows are included.
#' @param group Optional character string. Grouping column for group or partial
#'   pooling.
#' @param threshold Optional numeric scalar. Main excess threshold, stored for
#'   audit output.
#' @param tail_fit_threshold Optional numeric scalar. Lower threshold used as
#'   tail information. Only allowed when `method = "bootstrap"` and must be
#'   smaller than or equal to `threshold`.
#' @param method Character. `"observed"` or `"bootstrap"`.
#' @param pooling Character. `"portfolio"`, `"group"` or `"partial"`.
#' @param credibility Optional numeric scalar between 0 and 1. Used for
#'   `pooling = "partial"`. If `NULL`, credibility is estimated by group.
#' @param n_boot Positive whole number. Number of bootstrap samples.
#' @param severity_noise Character. `"none"`, `"lognormal"` or `"normal"`.
#' @param severity_noise_sd Non-negative numeric scalar controlling severity
#'   noise in bootstrap samples.
#'
#' @return An object of class `"excess_loss_allocation"`.
#'
#' @author Martin Haringa
#'
#' @examples
#' claims <- data.frame(
#'   sector = rep(c("Industry", "Retail"), each = 4),
#'   claim_amount = c(1000, 120000, 30000, 8000, 2000, 150000, 40000, 6000),
#'   earned_exposure = rep(1, 8)
#' )
#' decomposed <- calculate_excess_loss(claims, "claim_amount", threshold = 100000)
#' allocation <- allocate_excess_loss(
#'   decomposed,
#'   excess_amount = "excess_claim_amount",
#'   weight = "earned_exposure",
#'   group = "sector",
#'   pooling = "partial"
#' )
#' summary(allocation)
#'
#' @export
allocate_excess_loss <- function(data,
                                 excess_amount,
                                 weight,
                                 include = NULL,
                                 group = NULL,
                                 threshold = NULL,
                                 tail_fit_threshold = NULL,
                                 method = c("observed", "bootstrap"),
                                 pooling = c("portfolio", "group", "partial"),
                                 credibility = NULL,
                                 n_boot = 1000,
                                 severity_noise = c("none", "lognormal", "normal"),
                                 severity_noise_sd = 0.25) {
  method <- match.arg(method)
  pooling <- match.arg(pooling)
  severity_noise <- match.arg(severity_noise)
  validate_allocate_excess_loss(
    data, excess_amount, weight, include, group, threshold, tail_fit_threshold,
    method, pooling, credibility, n_boot, severity_noise, severity_noise_sd
  )

  allocation_data <- prepare_allocation_data(data, excess_amount, weight, include, group)
  groups <- summarize_allocation_groups(allocation_data)
  portfolio_loading <- safe_ratio_excess(
    sum(allocation_data$excess_amount[allocation_data$included]),
    sum(allocation_data$weight[allocation_data$included])
  )

  boot <- NULL
  if (identical(method, "bootstrap")) {
    boot <- bootstrap_excess_allocation(
      allocation_data = allocation_data,
      n_boot = n_boot,
      severity_noise = severity_noise,
      severity_noise_sd = severity_noise_sd
    )
    groups <- merge(groups, boot$group_summary, by = "group", all.x = TRUE,
                    sort = FALSE)
    groups$group_loading <- groups$bootstrap_loading_mean
    portfolio_loading <- boot$portfolio_loading
  }

  groups <- derive_final_loading(groups, pooling, portfolio_loading, credibility)
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

  groups <- summarize_allocated_groups(allocation_data, groups)
  structure(
    list(
      data = allocation_data,
      summary = groups,
      method = method,
      pooling = pooling,
      threshold = threshold,
      tail_fit_threshold = tail_fit_threshold,
      bootstrap = boot
    ),
    class = "excess_loss_allocation"
  )
}

#' Add excess loading to a pricing portfolio
#'
#' @description
#' Add an allocated excess loading to a portfolio data set.
#'
#' `add_excess_loading()` is the final workflow step. It does not estimate or
#' allocate excess loss. It takes an object produced by [allocate_excess_loss()]
#' and adds row-level loading columns to `data`.
#'
#' @param data A `data.frame`.
#' @param allocation An object returned by [allocate_excess_loss()].
#'
#' @return A `data.frame` with `base_premium`, `excess_loading` and
#'   `loaded_premium`. If `data` already contains `base_premium`, that column is
#'   used as the starting premium. Otherwise `base_premium` is set to zero.
#'
#' @author Martin Haringa
#'
#' @examples
#' claims <- data.frame(
#'   sector = rep(c("Industry", "Retail"), each = 4),
#'   claim_amount = c(1000, 120000, 30000, 8000, 2000, 150000, 40000, 6000),
#'   earned_exposure = rep(1, 8)
#' )
#' decomposed <- calculate_excess_loss(claims, "claim_amount", threshold = 100000)
#' allocation <- allocate_excess_loss(
#'   decomposed,
#'   excess_amount = "excess_claim_amount",
#'   weight = "earned_exposure"
#' )
#' add_excess_loading(decomposed, allocation)
#'
#' @export
add_excess_loading <- function(data, allocation) {
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
  out <- data
  if (!"base_premium" %in% names(out)) {
    out$base_premium <- 0
  } else if (!is.numeric(out$base_premium) || any(is.na(out$base_premium))) {
    stop("`base_premium` must be numeric without missing values when present.",
         call. = FALSE)
  }
  out$excess_loading <- allocation$data$allocated_loading
  out$loaded_premium <- out$base_premium + out$excess_loading
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
  if (!isTRUE(compare_to_empirical)) {
    drop <- intersect(c("empirical_loss", "empirical_excess_loss"), names(out))
    out <- out[, setdiff(names(out), drop), drop = FALSE]
  }
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
  cat("Pooling: ", x$pooling, "\n", sep = "")
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
                                            ...) {
  y <- match.arg(y)
  .check_dots_empty(...)
  pal <- .plot_palette_ir()
  grid_theme <- .plot_grid_theme_ir()
  ggplot2::ggplot(object$summary, ggplot2::aes(x = .data[["group"]], y = .data[[y]])) +
    ggplot2::geom_col(fill = pal$risk_premium, width = 0.65) +
    ggplot2::labs(x = NULL, y = y) +
    ggplot2::theme_minimal() +
    grid_theme
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

validate_allocate_excess_loss <- function(data, excess_amount, weight, include,
                                          group, threshold, tail_fit_threshold,
                                          method, pooling, credibility, n_boot,
                                          severity_noise, severity_noise_sd) {
  validate_data_frame(data)
  validate_character_column(data, excess_amount, "excess_amount")
  validate_character_column(data, weight, "weight")
  if (!is.numeric(data[[excess_amount]]) || any(is.na(data[[excess_amount]])) ||
      any(data[[excess_amount]] < 0)) {
    stop("`excess_amount` must refer to a numeric column with non-negative values.",
         call. = FALSE)
  }
  if (!is.numeric(data[[weight]]) || any(is.na(data[[weight]])) ||
      any(data[[weight]] < 0)) {
    stop("`weight` must refer to a numeric column with non-negative values.",
         call. = FALSE)
  }
  if (!is.null(include)) {
    validate_character_column(data, include, "include")
    if (!is.logical(data[[include]]) || any(is.na(data[[include]]))) {
      stop("`include` must refer to a logical column without missing values.",
           call. = FALSE)
    }
  }
  if (!is.null(group)) {
    validate_character_column(data, group, "group")
  }
  if (!is.null(threshold) &&
      (!is.numeric(threshold) || length(threshold) != 1L ||
       !is.finite(threshold) || threshold <= 0)) {
    stop("`threshold` must be NULL or a single positive number.", call. = FALSE)
  }
  if (!is.null(tail_fit_threshold)) {
    if (!identical(method, "bootstrap")) {
      stop("`tail_fit_threshold` is only allowed when `method = 'bootstrap'`.",
           call. = FALSE)
    }
    if (is.null(threshold) || !is.numeric(tail_fit_threshold) ||
        length(tail_fit_threshold) != 1L || !is.finite(tail_fit_threshold) ||
        tail_fit_threshold <= 0 || tail_fit_threshold > threshold) {
      stop("`tail_fit_threshold` must be positive and <= `threshold`.",
           call. = FALSE)
    }
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
  if (!is.numeric(n_boot) || length(n_boot) != 1L || is.na(n_boot) ||
      n_boot < 1 || n_boot != floor(n_boot)) {
    stop("`n_boot` must be a positive whole number.", call. = FALSE)
  }
  if (!is.numeric(severity_noise_sd) || length(severity_noise_sd) != 1L ||
      !is.finite(severity_noise_sd) || severity_noise_sd < 0) {
    stop("`severity_noise_sd` must be a single non-negative number.",
         call. = FALSE)
  }
}

prepare_allocation_data <- function(data, excess_amount, weight, include, group) {
  included <- if (is.null(include)) rep(TRUE, nrow(data)) else data[[include]]
  out <- data.frame(
    row_id = seq_len(nrow(data)),
    excess_amount = data[[excess_amount]],
    weight = data[[weight]],
    included = included,
    group = if (is.null(group)) "portfolio" else as.character(data[[group]]),
    stringsAsFactors = FALSE
  )
  if (any(out$included & out$weight <= 0)) {
    stop("`weight` must be positive for included rows.", call. = FALSE)
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
    empirical_loss <- sum(z$excess_amount)
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

derive_final_loading <- function(groups, pooling, portfolio_loading, credibility) {
  groups$portfolio_loading <- portfolio_loading
  if (identical(pooling, "portfolio")) {
    groups$credibility <- 0
    groups$allocated_loading <- portfolio_loading
    return(groups)
  }
  if (identical(pooling, "group")) {
    groups$credibility <- 1
    groups$allocated_loading <- groups$group_loading
    return(groups)
  }
  groups$credibility <- if (is.null(credibility)) {
    automatic_excess_credibility(groups)
  } else {
    credibility
  }
  groups$allocated_loading <- groups$credibility * groups$group_loading +
    (1 - groups$credibility) * portfolio_loading
  groups
}

automatic_excess_credibility <- function(groups) {
  weight_score <- safe_ratio_excess(groups$group_weight, max(groups$group_weight))
  claim_score <- safe_ratio_excess(groups$n_claims, max(groups$n_claims))
  excess_claim_score <- safe_ratio_excess(groups$n_excess_claims, max(groups$n_excess_claims))
  loss_score <- safe_ratio_excess(groups$historical_excess_loss,
                                  max(groups$historical_excess_loss))
  ratio_score <- pmin(groups$excess_loss_ratio / max(groups$excess_loss_ratio, na.rm = TRUE), 1)
  score <- rowMeans(
    cbind(weight_score, claim_score, excess_claim_score, loss_score, ratio_score),
    na.rm = TRUE
  )
  pmin(pmax(score, 0), 1)
}

bootstrap_excess_allocation <- function(allocation_data, n_boot,
                                        severity_noise, severity_noise_sd) {
  included <- allocation_data[allocation_data$included, , drop = FALSE]
  tail <- included[included$excess_amount > 0, , drop = FALSE]
  if (nrow(tail) == 0) {
    stop("No positive excess amounts are available for bootstrap allocation.",
         call. = FALSE)
  }
  group_weights <- rowsum(included$weight, included$group, reorder = FALSE)
  total_weight <- sum(included$weight)
  boot_summaries <- replicate(n_boot, {
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

.check_dots_empty <- function(...) {
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused argument(s): ", paste(names(dots), collapse = ", "),
         call. = FALSE)
  }
}
