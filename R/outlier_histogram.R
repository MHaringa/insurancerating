#' Histogram with outlier bins
#'
#' @description
#' Visualize the distribution of a continuous variable using bins.
#' Values below `left` or above `right` can be grouped into outlier bins
#' for compact display when the range of values is wide.
#'
#' @param data A data.frame containing the variable to plot.
#' @param x Variable name in `data` to map on the x-axis.
#' @param left Optional numeric, floor of the range. Values below are binned
#' together.
#' @param right Optional numeric, ceiling of the range. Values above are binned
#' together.
#' @param line Logical. If TRUE, add a density line. Default = FALSE.
#' @param bins Integer. Number of bins to use. Default = 30.
#' @param fill Fill color for bars. If NULL, a default is chosen.
#' @param color Line color for bars. If NULL, a default is chosen.
#' @param fill_outliers Fill color for outlier bins. Default = "#a7d1a7".
#'
#' @importFrom rlang as_name
#' @importFrom rlang enquo
#'
#' @details
#' This is a wrapper around [ggplot2::geom_histogram()].
#' The method for handling outliers is based on
#' <https://edwinth.github.io/blog/outlier-bin/>.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @author Martin Haringa
#'
#' @examples
#' outlier_histogram(MTPL2, "premium")
#' outlier_histogram(MTPL2, "premium", left = 30, right = 120, bins = 30)
#'
#' @export
outlier_histogram <- function(data, x, left = NULL, right = NULL,
                              line = FALSE,
                              bins = 30, fill = "steelblue", color = "white",
                              fill_outliers = "#a7d1a7") {

  splitsing <- split_x_fn(data, x, left = left, right = right)
  nbinwidth <- diff(range(do.call(rbind, splitsing)$x)) / bins

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


#' @rdname outlier_histogram
#'
#' @description
#' [histbin()] is deprecated as of version 0.8.0.
#' Please use [outlier_histogram()] instead.
#'
#' In addition, note that `x` must now be passed as **string**
#' (standard evaluation).
#'
#' @export
histbin <- function(data, x, left = NULL, right = NULL, line = FALSE, bins = 30,
                    fill = "steelblue", color = "white",
                    fill_outliers = "#a7d1a7") {
  lifecycle::deprecate_warn("0.8.0", "histbin()", "outlier_histogram()")
  x <- deparse(substitute(x))
  outlier_histogram(data = data, x = x, left = left, right = right,
                    line = line, bins = bins, fill = fill,
                    color = color, fill_outliers = fill_outliers)
}
