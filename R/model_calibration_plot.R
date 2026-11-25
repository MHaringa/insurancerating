#' Plot observed-versus-expected calibration
#'
#' @description
#' Plot overall O/E, observed versus expected outcomes by predicted-risk
#' group, or O/E across periods or rating-factor levels.
#'
#' @param object Result from [calibration_analysis()].
#' @param ... Unused.
#' @param type Plot type. The default, `"auto"`, uses the calibration
#'   grouping. For a result grouped with `by`, it uses a temporal plot for
#'   `Date`, date-time or whole-number years from 1800 to 2200, and a
#'   factor plot otherwise. Set `"temporal"` or `"factor"` to override
#'   that choice for a single grouping column. With multiple grouping columns,
#'   the plot shows a point for each combination, rather than a time series.
#'
#' @details
#' Overall, temporal and factor plots use a reference line at O/E = 1.
#' Factor plots preserve the result's order and label missing groups.
#' Temporal plots omit missing periods; undefined O/E values are not drawn.
#'
#' Each point in the predicted-risk plot represents one group from
#' [calibration_analysis()]. Expected is on the horizontal axis and observed
#' on the vertical axis, using the same scale. A point above the diagonal
#' means the model predicted too little for that group; a point below
#' means it predicted too much.
#'
#' If exposure is available, both totals are divided by the group's total
#' exposure. For a claim-count model this shows claims per unit of exposure.
#' Groups with zero exposure are not plotted. If no group has positive
#' exposure, the plot uses totals instead.
#' With case weights, these are weighted totals divided by the reported
#' exposure, not necessarily unweighted portfolio rates.
#'
#' @return A [ggplot2::ggplot] object.
#' @examples
#' portfolio <- as.data.frame(MTPL)
#' portfolio$zip <- factor(portfolio$zip)
#' # Illustrative years; the source data have no calendar-year field.
#' portfolio$year <- rep(2023:2024, length.out = nrow(portfolio))
#' model <- glm(nclaims ~ age_policyholder + offset(log(exposure)),
#'              family = poisson(), data = portfolio)
#'
#' autoplot(calibration_analysis(model, portfolio))
#' # On these estimation data, each point compares observed and expected
#' # claims per policy-year in a group of similar predicted risks.
#' autoplot(calibration_analysis(model, portfolio, groups = 5))
#' autoplot(calibration_analysis(model, portfolio, by = "year"))
#' autoplot(calibration_analysis(model, portfolio, by = "zip"))
#' autoplot(calibration_analysis(model, portfolio, by = c("year", "zip")))
#'
#' @seealso [calibration_analysis()]
#' @keywords internal
#' @export
autoplot.calibration_result <- function(object, ...,
                                        type = c("auto", "overall", "risk", "temporal", "factor")) {
  .check_dots_empty(...)
  type <- match.arg(type)
  grouping <- attr(object, "calibration_grouping", exact = TRUE)
  by <- attr(object, "calibration_by", exact = TRUE)
  data <- as.data.frame(object)
  if (length(grouping) != 1L || !all(c("Observed", "Expected", "OE") %in% names(data))) {
    stop("Plot a complete result from `calibration_analysis()`.", call. = FALSE)
  }
  allowed <- switch(grouping, overall = "overall", risk = "risk",
                    by = if (length(by) > 1L) "factor" else c("temporal", "factor"))
  if (is.null(allowed) ||
      (identical(grouping, "by") && (!length(by) || !all(by %in% names(data))))) {
    stop("Plot a complete result from `calibration_analysis()`.", call. = FALSE)
  }
  if (identical(type, "auto")) {
    type <- if (!identical(grouping, "by")) {
      grouping
    } else if (length(by) == 1L && .calibration_is_time(data[[by]])) {
      "temporal"
    } else {
      "factor"
    }
  }
  if (!type %in% allowed) {
    stop("Plot type '", type, "' is not available for this calibration result.",
         call. = FALSE)
  }

  colour <- .plot_palette_ir()$frequency
  if (identical(type, "risk")) {
    p <- .calibration_risk_plot(data, colour)
  } else {
    p <- .calibration_oe_plot(data, by, type, colour)
  }
  p + ggplot2::theme_minimal() + .plot_grid_theme_ir()
}


.calibration_is_time <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) return(TRUE)
  x <- x[!is.na(x)]
  is.numeric(x) && length(x) > 0L &&
    all(x >= 1800 & x <= 2200 & x == floor(x))
}


.calibration_risk_plot <- function(data, colour) {
  caption <- NULL
  unit <- "total"
  if (!is.null(data$Exposure) && any(data$Exposure > 0)) {
    keep <- data$Exposure > 0
    if (!all(keep)) caption <- "Groups with zero exposure are not shown."
    data <- data[keep, , drop = FALSE]
    data$Observed <- data$Observed / data$Exposure
    data$Expected <- data$Expected / data$Exposure
    unit <- "per unit exposure"
  }
  intervals <- all(c("Lower", "Upper") %in% names(data))
  if (intervals) {
    data$.lower <- data$Lower * data$Expected
    data$.upper <- data$Upper * data$Expected
  }
  values <- c(data$Observed, data$Expected, data$.upper, 0)
  maximum <- max(values[is.finite(values)])
  limits <- c(0, if (maximum > 0) maximum else 1)
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$Expected, y = .data$Observed)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, colour = "grey55", linetype = 2) +
    ggplot2::geom_point(colour = colour, size = 2.5) +
    ggplot2::coord_fixed(xlim = limits, ylim = limits) +
    ggplot2::labs(title = "Calibration by predicted risk",
                  x = paste("Expected", unit), y = paste("Observed", unit),
                  caption = caption)
  if (intervals) {
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$.lower, ymax = .data$.upper), colour = colour, na.rm = TRUE
    )
  }
  p
}


.calibration_oe_plot <- function(data, by, type, colour) {
  caption <- NULL
  subtitle <- NULL
  multiple <- length(by) > 1L
  if (identical(type, "overall")) {
    data$.group <- rep("Overall", nrow(data))
    title <- "Overall calibration"
    subtitle <- if (is.finite(data$OE[1L])) {
      paste("Overall O/E:", format(round(data$OE[1L], 3L), trim = TRUE))
    } else {
      "Overall O/E is undefined."
    }
  } else {
    if (multiple) {
      values <- lapply(data[by], .calibration_group_labels)
      data$.group <- do.call(paste, c(unname(values), sep = " / "))
      by <- paste(by, collapse = " / ")
    } else {
      data$.group <- data[[by]]
    }
    title <- if (identical(type, "temporal")) {
      "Calibration over time"
    } else {
      paste("Calibration by", by)
    }
    if (identical(type, "temporal") && anyNA(data$.group)) {
      caption <- "Missing periods are not shown."
      data <- data[!is.na(data$.group), , drop = FALSE]
    }
  }
  discrete <- identical(type, "factor") ||
    (identical(type, "temporal") && !is.numeric(data$.group) &&
       !inherits(data$.group, c("Date", "POSIXt")))
  if (discrete) {
    labels <- .calibration_group_labels(data$.group)
    data$.group <- factor(seq_len(nrow(data)))
  }
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$.group, y = .data$OE, group = 1)) +
    ggplot2::geom_hline(yintercept = 1, colour = "grey55", linetype = 2)
  if (identical(type, "temporal") && sum(is.finite(data$OE)) > 1L) {
    p <- p + ggplot2::geom_line(colour = colour, linewidth = 0.6, na.rm = TRUE)
  }
  p <- p + ggplot2::geom_point(colour = colour, size = 2.5, na.rm = TRUE)
  if (all(c("Lower", "Upper") %in% names(data))) {
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$Lower, ymax = .data$Upper), colour = colour, na.rm = TRUE
    )
  }
  if (discrete) {
    guide <- if (multiple) ggplot2::guide_axis(angle = 45, check.overlap = TRUE) else {
      ggplot2::waiver()
    }
    p <- p + ggplot2::scale_x_discrete(labels = labels, drop = FALSE, guide = guide)
  }
  if (identical(type, "temporal") && is.numeric(data$.group) &&
      .calibration_is_time(data$.group)) {
    breaks <- if (nrow(data) <= 10L) data$.group else pretty(data$.group, n = 6)
    p <- p + ggplot2::scale_x_continuous(breaks = breaks)
  }
  p + ggplot2::labs(title = title, subtitle = subtitle, x = by,
                    y = "Observed / expected", caption = caption)
}


.calibration_group_labels <- function(x) {
  ifelse(is.na(x), "(Missing)", as.character(x))
}
