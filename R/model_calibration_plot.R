#' Plot observed-versus-expected calibration
#'
#' @description
#' Plot overall O/E, observed versus expected outcomes by predicted-risk
#' group, or O/E across periods or rating-factor levels.
#'
#' @param object Result from [check_calibration()].
#' @param ... Unused.
#' @param type Plot type. The default, `"auto"`, uses the calibration
#'   grouping. For a result grouped with `by`, it uses a temporal plot for
#'   `Date`, date-time or whole-number years from 1800 to 2200, and a
#'   factor plot otherwise. Set `"temporal"` or `"factor"` to override
#'   that choice (for example, when years are stored as ordered factors).
#'
#' @details
#' Overall, temporal and factor plots use a reference line at O/E = 1.
#' Factor plots preserve the result's order and label missing groups.
#' Temporal plots omit missing periods; undefined O/E values are not drawn.
#'
#' The predicted-risk plot shows observed against expected with a 45-degree
#' reference line and the same scale on both axes. If exposure is available,
#' reported totals are divided by it and groups with zero exposure are not
#' plotted. If no group has positive exposure, the plot uses totals instead.
#' With case weights, these are weighted totals divided by the reported
#' exposure, not necessarily unweighted portfolio rates.
#'
#' @return A [ggplot2::ggplot] object.
#' @examples
#' portfolio <- as.data.frame(MTPL2)
#' portfolio$area <- factor(portfolio$area)
#' # Illustrative years; the source data have no calendar-year field.
#' portfolio$year <- rep(2023:2024, length.out = nrow(portfolio))
#' model <- glm(nclaims ~ area + offset(log(exposure)),
#'              family = poisson(), data = portfolio)
#'
#' autoplot(check_calibration(model, portfolio))
#' autoplot(check_calibration(model, portfolio, groups = 10))
#' autoplot(check_calibration(model, portfolio, by = "year"))
#' autoplot(check_calibration(model, portfolio, by = "area"))
#'
#' @seealso [check_calibration()]
#' @export
autoplot.calibration_result <- function(object, ...,
                                        type = c("auto", "overall", "risk", "temporal", "factor")) {
  .check_dots_empty(...)
  type <- match.arg(type)
  grouping <- attr(object, "calibration_grouping", exact = TRUE)
  by <- attr(object, "calibration_by", exact = TRUE)
  data <- as.data.frame(object)
  if (length(grouping) != 1L || !all(c("Observed", "Expected", "OE") %in% names(data))) {
    stop("Plot a complete result from `check_calibration()`.", call. = FALSE)
  }
  allowed <- switch(grouping, overall = "overall", risk = "risk",
                    by = c("temporal", "factor"))
  if (is.null(allowed) ||
      (identical(grouping, "by") && (length(by) != 1L || !by %in% names(data)))) {
    stop("Plot a complete result from `check_calibration()`.", call. = FALSE)
  }
  if (identical(type, "auto")) {
    type <- if (!identical(grouping, "by")) {
      grouping
    } else if (.calibration_is_time(data[[by]])) {
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
  maximum <- max(c(data$Observed, data$Expected, 0))
  limits <- c(0, if (maximum > 0) maximum else 1)
  ggplot2::ggplot(data, ggplot2::aes(x = .data$Expected, y = .data$Observed)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, colour = "grey55", linetype = 2) +
    ggplot2::geom_point(colour = colour, size = 2.5) +
    ggplot2::coord_fixed(xlim = limits, ylim = limits) +
    ggplot2::labs(title = "Calibration by predicted risk",
                  x = paste("Expected", unit), y = paste("Observed", unit),
                  caption = caption)
}


.calibration_oe_plot <- function(data, by, type, colour) {
  caption <- NULL
  subtitle <- NULL
  if (identical(type, "overall")) {
    data$.group <- rep("Overall", nrow(data))
    title <- "Overall calibration"
    subtitle <- if (is.finite(data$OE[1L])) {
      paste("Overall O/E:", format(round(data$OE[1L], 3L), trim = TRUE))
    } else {
      "Overall O/E is undefined."
    }
  } else {
    data$.group <- data[[by]]
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
    labels <- ifelse(is.na(data$.group), "(Missing)", as.character(data$.group))
    data$.group <- factor(seq_len(nrow(data)))
  }
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$.group, y = .data$OE, group = 1)) +
    ggplot2::geom_hline(yintercept = 1, colour = "grey55", linetype = 2)
  if (identical(type, "temporal") && sum(is.finite(data$OE)) > 1L) {
    p <- p + ggplot2::geom_line(colour = colour, linewidth = 0.6, na.rm = TRUE)
  }
  p <- p + ggplot2::geom_point(colour = colour, size = 2.5, na.rm = TRUE)
  if (discrete) p <- p + ggplot2::scale_x_discrete(labels = labels, drop = FALSE)
  if (identical(type, "temporal") && is.numeric(data$.group) &&
      .calibration_is_time(data$.group)) {
    breaks <- if (nrow(data) <= 10L) data$.group else pretty(data$.group, n = 6)
    p <- p + ggplot2::scale_x_continuous(breaks = breaks)
  }
  p + ggplot2::labs(title = title, subtitle = subtitle, x = by,
                    y = "Observed / expected", caption = caption)
}
