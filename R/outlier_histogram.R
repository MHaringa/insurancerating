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
#' keeps the main range visible and groups values below `left` or above `right`
#' into dedicated tail bins.
#'
#' The plot is useful for actuarial portfolio checks, data quality review, and
#' model preparation: it helps show where most risks are concentrated while
#' still making the presence of extreme observations explicit.
#'
#' @param data A data.frame containing the portfolio variable to inspect.
#' @param x Character; numeric column in `data` to plot.
#' @param left Optional numeric lower threshold. Values below this threshold are
#'   grouped into one left-tail bin.
#' @param right Optional numeric upper threshold. Values above this threshold are
#'   grouped into one right-tail bin.
#' @param line Logical. If `TRUE`, add a density line. Default = `FALSE`.
#' @param bins Integer. Number of bins used for the displayed range. Default = 30.
#' @param fill Fill color for regular histogram bars.
#' @param color Border color for histogram bars.
#' @param fill_outliers Fill color for tail bins. Default = `"#a7d1a7"`.
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
#' outlier_histogram(MTPL2, "premium", left = 30, right = 120, bins = 30)
#'
#' @export
outlier_histogram <- function(data, x, left = NULL, right = NULL,
                              line = FALSE,
                              bins = 30, fill = "steelblue", color = "white",
                              fill_outliers = "#a7d1a7") {
  if (missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }
  x <- tryCatch(eval.parent(substitute(x)), error = function(e) NULL)

  validate_outlier_histogram_args(
    data = data,
    x = x,
    left = left,
    right = right,
    line = line,
    bins = bins,
    fill = fill,
    color = color,
    fill_outliers = fill_outliers
  )

  splitsing <- split_x_fn(data, x, left = left, right = right)
  plot_values <- do.call(rbind, splitsing)$x
  nbinwidth <- diff(range(plot_values, na.rm = TRUE)) / bins

  obj <- ggplot(data = splitsing[[2]], aes(x = x))

  if (isTRUE(line)) {
    obj <- obj +
      ggplot2::stat_density(geom = "line", aes(y = nbinwidth * ..count..),
                            color = "dodgerblue") +
      ggplot2::ylab("count")
  }

  obj <- obj +
    ggplot2::geom_histogram(fill = fill,
                            color = color,
                            alpha = 1,
                            binwidth = nbinwidth) +
    ggplot2::scale_y_continuous(expand = expansion(mult = c(0, .1)))

  if (!is.null(splitsing[[1]])) {

    ticks_for_left <- update_tickmarks_left(
      obj, left, round(min(data[[x]], na.rm = TRUE), 1)
      )

    obj <- obj +
      ggplot2::geom_histogram(data = splitsing[[1]],
                              fill = fill_outliers,
                              color = ifelse(fill_outliers == "#a7d1a7",
                                             "forestgreen", fill_outliers),
                              binwidth = nbinwidth) +
      ggplot2::scale_x_continuous(breaks = ticks_for_left$tick_positions,
                                  labels = ticks_for_left$tick_labels)
  }

  if (!is.null(splitsing[[3]])) {

    ticks_for_right <- update_tickmarks_right(obj, right,
                                              round(max(data[[x]],
                                                        na.rm = TRUE), 1))

    suppressMessages(
      obj <- obj +
        ggplot2::geom_histogram(data = splitsing[[3]],
                                fill = fill_outliers,
                                color = ifelse(fill_outliers == "#a7d1a7",
                                               "forestgreen", fill_outliers),
                                binwidth = nbinwidth) +
        ggplot2::scale_x_continuous(breaks = ticks_for_right$tick_positions,
                                    labels = ticks_for_right$tick_labels)
    )
  }

  obj +
    ggplot2::theme_minimal() +
    ggplot2::xlab(x) +
    ggplot2::geom_hline(yintercept = 0, color = "white")
}


validate_outlier_histogram_args <- function(data,
                                            x,
                                            left = NULL,
                                            right = NULL,
                                            line = FALSE,
                                            bins = 30,
                                            fill = "steelblue",
                                            color = "white",
                                            fill_outliers = "#a7d1a7") {
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
  if (!is.logical(line) || length(line) != 1L || is.na(line)) {
    stop("`line` must be TRUE or FALSE.", call. = FALSE)
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
  validate_optional_cutoff(left, "left")
  validate_optional_cutoff(right, "right")

  if (!is.null(left) && !is.null(right) && left >= right) {
    stop("`right` must be greater than `left`.", call. = FALSE)
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
  if (!is.null(left) && (left <= min_x || left >= max_x)) {
    stop("`left` must be greater than the minimum and less than the maximum of `x`.",
         call. = FALSE)
  }
  if (!is.null(right) && (right <= min_x || right >= max_x)) {
    stop("`right` must be greater than the minimum and less than the maximum of `x`.",
         call. = FALSE)
  }

  validate_optional_color <- function(value, name) {
    if (!is.null(value) && (!is.character(value) || length(value) != 1L ||
                           is.na(value))) {
      stop("`", name, "` must be NULL or a single character string.",
           call. = FALSE)
    }
  }
  validate_optional_color(fill, "fill")
  validate_optional_color(color, "color")
  validate_optional_color(fill_outliers, "fill_outliers")

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
                    fill = "steelblue", color = "white",
                    fill_outliers = "#a7d1a7") {
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
  outlier_histogram(data = data, x = x, left = left, right = right,
                    line = line, bins = bins, fill = fill,
                    color = color, fill_outliers = fill_outliers)
}
