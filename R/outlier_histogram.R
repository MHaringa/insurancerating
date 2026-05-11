#' Portfolio histogram with tail bins
#'
#' @description
#' Visualize the distribution of a numeric portfolio variable while keeping
#' extreme tails readable.
#'
#' Insurance portfolios often contain skewed variables such as claim amounts,
#' premium, exposure, insured sums, deductibles, or fitted premiums. A few very
#' large policies or claim events can stretch a regular histogram so much that
#' the body of the portfolio becomes hard to inspect. `outlier_histogram()`
#' keeps the main range visible and groups values below `lower` or above `upper`
#' into dedicated tail bins.
#'
#' The plot is useful for actuarial portfolio checks, data quality review, and
#' model preparation: it helps show where most risks are concentrated while
#' still making the presence of extreme observations explicit.
#'
#' @param data A data.frame containing the portfolio variable to inspect.
#' @param x Character; numeric column in `data` to plot.
#' @param lower Optional numeric lower threshold. Values below this threshold are
#'   grouped into one left-tail bin.
#' @param upper Optional numeric upper threshold. Values above this threshold are
#'   grouped into one right-tail bin.
#' @param density Logical. If `TRUE`, add a density line. Default = `FALSE`.
#' @param bins Integer. Number of bins used for the displayed range. Default = 30.
#' @param bar_fill Fill color for regular histogram bars.
#' @param bar_color Border color for regular histogram bars.
#' @param tail_fill Fill color for tail bins.
#' @param tail_color Border color for tail bins.
#' @param density_color Color for the optional density line.
#' @param left,right Deprecated aliases for `lower` and `upper`.
#' @param line Deprecated alias for `density`.
#' @param fill,color,fill_outliers Deprecated aliases for `bar_fill`,
#'   `bar_color`, and `tail_fill`.
#'
#' @details
#' This function is intended as an exploratory portfolio diagnostic. It does not
#' remove or winsorize observations in `data`; it only groups tail values in the
#' visual display. The labels on the tail bins show the original range captured
#' by each tail bin.
#'
#' The method for handling outlier bins is based on
#' <https://edwinth.github.io/blog/outlier-bin/>.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @author Martin Haringa
#'
#' @examples
#' # Inspect the full premium distribution
#' outlier_histogram(MTPL2, "premium")
#'
#' # Keep the portfolio body readable while showing both tails
#' outlier_histogram(MTPL2, "premium", lower = 30, upper = 120, bins = 30)
#'
#' @export
outlier_histogram <- function(data, x, lower = NULL, upper = NULL,
                              density = FALSE,
                              bins = 30, bar_fill = "#E6E6E6",
                              bar_color = "white", tail_fill = "#F28E2B",
                              tail_color = "white",
                              density_color = "#2C7FB8",
                              left = NULL, right = NULL, line = NULL,
                              fill = NULL, color = NULL,
                              fill_outliers = NULL) {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }
  x <- tryCatch(eval.parent(substitute(x)), error = function(e) NULL)

  args <- resolve_outlier_histogram_args(
    lower = lower,
    upper = upper,
    density = density,
    bar_fill = bar_fill,
    bar_color = bar_color,
    tail_fill = tail_fill,
    tail_color = tail_color,
    density_color = density_color,
    lower_supplied = !missing(lower),
    upper_supplied = !missing(upper),
    density_supplied = !missing(density),
    bar_fill_supplied = !missing(bar_fill),
    bar_color_supplied = !missing(bar_color),
    tail_fill_supplied = !missing(tail_fill),
    left = left,
    right = right,
    line = line,
    fill = fill,
    color = color,
    fill_outliers = fill_outliers
  )
  lower <- args$lower
  upper <- args$upper
  density <- args$density
  bar_fill <- args$bar_fill
  bar_color <- args$bar_color
  tail_fill <- args$tail_fill
  tail_color <- args$tail_color
  density_color <- args$density_color

  validate_outlier_histogram_args(
    data = data,
    x = x,
    lower = lower,
    upper = upper,
    density = density,
    bins = bins,
    bar_fill = bar_fill,
    bar_color = bar_color,
    tail_fill = tail_fill,
    tail_color = tail_color,
    density_color = density_color
  )

  splitsing <- split_x_fn(data, x, left = lower, right = upper)
  plot_values <- do.call(rbind, splitsing)$x
  nbinwidth <- diff(range(plot_values, na.rm = TRUE)) / bins

  obj <- ggplot(data = splitsing[[2]], aes(x = x))

  if (isTRUE(density)) {
    obj <- obj +
      ggplot2::stat_density(
        geom = "line",
        aes(y = nbinwidth * ggplot2::after_stat(count)),
        color = density_color
      ) +
      ggplot2::ylab("count")
  }

  obj <- obj +
    ggplot2::geom_histogram(fill = bar_fill,
                            color = bar_color,
                            alpha = 1,
                            binwidth = nbinwidth) +
    ggplot2::scale_y_continuous(expand = expansion(mult = c(0, .1)))

  if (!is.null(splitsing[[1]])) {

    ticks_for_left <- update_tickmarks_left(
      obj, lower, round(min(data[[x]], na.rm = TRUE), 1)
      )

    obj <- obj +
      ggplot2::geom_histogram(data = splitsing[[1]],
                              fill = tail_fill,
                              color = tail_color,
                              binwidth = nbinwidth) +
      ggplot2::scale_x_continuous(breaks = ticks_for_left$tick_positions,
                                  labels = ticks_for_left$tick_labels)
  }

  if (!is.null(splitsing[[3]])) {

    ticks_for_right <- update_tickmarks_right(obj, upper,
                                              round(max(data[[x]],
                                                        na.rm = TRUE), 1))

    suppressMessages(
      obj <- obj +
        ggplot2::geom_histogram(data = splitsing[[3]],
                                fill = tail_fill,
                                color = tail_color,
                                binwidth = nbinwidth) +
        ggplot2::scale_x_continuous(breaks = ticks_for_right$tick_positions,
                                    labels = ticks_for_right$tick_labels)
    )
  }

  obj +
    ggplot2::theme_minimal() +
    .plot_grid_theme_ir() +
    ggplot2::xlab(x) +
    ggplot2::geom_hline(yintercept = 0, color = "white")
}


resolve_outlier_histogram_args <- function(lower = NULL,
                                           upper = NULL,
                                           density = FALSE,
                                           bar_fill = NULL,
                                           bar_color = NULL,
                                           tail_fill = NULL,
                                           tail_color = NULL,
                                           density_color = NULL,
                                           lower_supplied = FALSE,
                                           upper_supplied = FALSE,
                                           density_supplied = FALSE,
                                           bar_fill_supplied = FALSE,
                                           bar_color_supplied = FALSE,
                                           tail_fill_supplied = FALSE,
                                           left = NULL,
                                           right = NULL,
                                           line = NULL,
                                           fill = NULL,
                                           color = NULL,
                                           fill_outliers = NULL) {
  if (!is.null(left)) {
    if (lower_supplied) {
      stop("Use only one of `lower` and deprecated `left`.", call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "outlier_histogram(left)",
      "outlier_histogram(lower)"
    )
    lower <- left
  }
  if (!is.null(right)) {
    if (upper_supplied) {
      stop("Use only one of `upper` and deprecated `right`.", call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "outlier_histogram(right)",
      "outlier_histogram(upper)"
    )
    upper <- right
  }
  if (!is.null(line)) {
    if (density_supplied) {
      stop("Use only one of `density` and deprecated `line`.", call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "outlier_histogram(line)",
      "outlier_histogram(density)"
    )
    density <- line
  }
  if (!is.null(fill)) {
    if (bar_fill_supplied) {
      stop("Use only one of `bar_fill` and deprecated `fill`.", call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "outlier_histogram(fill)",
      "outlier_histogram(bar_fill)"
    )
    bar_fill <- fill
  }
  if (!is.null(color)) {
    if (bar_color_supplied) {
      stop("Use only one of `bar_color` and deprecated `color`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "outlier_histogram(color)",
      "outlier_histogram(bar_color)"
    )
    bar_color <- color
  }
  if (!is.null(fill_outliers)) {
    if (tail_fill_supplied) {
      stop("Use only one of `tail_fill` and deprecated `fill_outliers`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "outlier_histogram(fill_outliers)",
      "outlier_histogram(tail_fill)"
    )
    tail_fill <- fill_outliers
  }

  if (is.null(bar_fill)) bar_fill <- "#E6E6E6"
  if (is.null(bar_color)) bar_color <- "white"
  if (is.null(tail_fill)) tail_fill <- "#F28E2B"
  if (is.null(tail_color)) tail_color <- "white"
  if (is.null(density_color)) density_color <- "#2C7FB8"

  list(
    lower = lower,
    upper = upper,
    density = density,
    bar_fill = bar_fill,
    bar_color = bar_color,
    tail_fill = tail_fill,
    tail_color = tail_color,
    density_color = density_color
  )
}


validate_outlier_histogram_args <- function(data,
                                            x,
                                            lower = NULL,
                                            upper = NULL,
                                            density = FALSE,
                                            bins = 30,
                                            bar_fill = "#E6E6E6",
                                            bar_color = "white",
                                            tail_fill = "#F28E2B",
                                            tail_color = "white",
                                            density_color = "#2C7FB8") {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("`x` must be a single column name.", call. = FALSE)
  }
  if (!x %in% names(data)) {
    stop("Column `", x, "` not found in `data`.", call. = FALSE)
  }
  if (!is.numeric(data[[x]])) {
    stop("Column `", x, "` must be numeric.", call. = FALSE)
  }
  if (!is.logical(density) || length(density) != 1L || is.na(density)) {
    stop("`density` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(bins) || length(bins) != 1L || is.na(bins) ||
      bins < 1 || bins != floor(bins)) {
    stop("`bins` must be a positive whole number.", call. = FALSE)
  }

  validate_optional_cutoff <- function(value, name) {
    if (!is.null(value) &&
        (!is.numeric(value) || length(value) != 1L || !is.finite(value))) {
      stop("`", name, "` must be NULL or a single finite number.", call. = FALSE)
    }
  }
  validate_optional_cutoff(lower, "lower")
  validate_optional_cutoff(upper, "upper")

  if (!is.null(lower) && !is.null(upper) && lower >= upper) {
    stop("`upper` must be greater than `lower`.", call. = FALSE)
  }

  finite_values <- data[[x]][is.finite(data[[x]])]
  if (length(finite_values) == 0L) {
    stop("Column `", x, "` must contain at least one finite value.", call. = FALSE)
  }
  if (length(unique(finite_values)) < 2L) {
    stop("Column `", x, "` must contain at least two distinct finite values.",
         call. = FALSE)
  }

  min_x <- min(finite_values)
  max_x <- max(finite_values)
  if (!is.null(lower) && (lower <= min_x || lower >= max_x)) {
    stop("`lower` must be greater than the minimum and less than the maximum of `x`.",
         call. = FALSE)
  }
  if (!is.null(upper) && (upper <= min_x || upper >= max_x)) {
    stop("`upper` must be greater than the minimum and less than the maximum of `x`.",
         call. = FALSE)
  }

  validate_optional_color <- function(value, name) {
    if (!is.null(value) && (!is.character(value) || length(value) != 1L ||
                           is.na(value))) {
      stop("`", name, "` must be NULL or a single character string.",
           call. = FALSE)
    }
  }
  validate_optional_color(bar_fill, "bar_fill")
  validate_optional_color(bar_color, "bar_color")
  validate_optional_color(tail_fill, "tail_fill")
  validate_optional_color(tail_color, "tail_color")
  validate_optional_color(density_color, "density_color")

  invisible(TRUE)
}


#' Deprecated alias for `outlier_histogram()`
#'
#' @description
#' [histbin()] is deprecated as of version 0.8.0.
#' Please use [outlier_histogram()] instead.
#'
#' In addition, note that `x` must now be passed as **string**
#' (standard evaluation).
#'
#' @inheritParams outlier_histogram
#' @return See [outlier_histogram()].
#'
#' @export
#' @keywords internal
histbin <- function(data, x, left = NULL, right = NULL, line = FALSE, bins = 30,
                    fill = "#E6E6E6", color = "white",
                    fill_outliers = "#F28E2B") {
  lifecycle::deprecate_warn("0.8.0", "histbin()", "outlier_histogram()")
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }
  x_expr <- substitute(x)
  x <- if (is.character(x_expr)) {
    x_expr
  } else {
    x_value <- tryCatch(eval.parent(x_expr), error = function(e) NULL)
    if (is.character(x_value)) x_value else deparse(x_expr)
  }
  outlier_histogram(
    data = data,
    x = x,
    lower = left,
    upper = right,
    density = line,
    bins = bins,
    bar_fill = fill,
    bar_color = color,
    tail_fill = fill_outliers
  )
}
