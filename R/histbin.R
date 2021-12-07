#' Create a histogram with outlier bins
#'
#' Visualize the distribution of a single continuous variable by dividing the x
#' axis into bins and counting the number of observations in each bin.
#' Data points that are considered outliers can be binned together. This might
#' be helpful to display numerical data over a very wide range of values in a
#' compact way.
#'
#' @param data data.frame
#' @param x variable name in data.frame `data` that should be mapped
#' @param left numeric indicating the floor of the range
#' @param right numeric indicating the ceiling of the range
#' @param line show density line (default is FALSE)
#' @param bins numeric to indicate number of bins
#' @param fill color used to fill bars
#' @param color color for bar lines
#' @param fill_outliers color used to fill outlier bars
#'
#' @details Wrapper function around `ggplot2::geom_histogram()`. The method is
#' based on suggestions from <https://edwinth.github.io/blog/outlier-bin/>.
#'
#' @return a ggplot2 object
#'
#' @import ggplot2
#'
#' @examples
#' histbin(MTPL2, premium)
#' histbin(MTPL2, premium, left = 30, right = 120, bins = 30)
#'
#' @export
histbin <- function(data, x, left = NULL, right = NULL, line = FALSE, bins = 30,
                    fill = NULL, color = NULL, fill_outliers = "#a7d1a7"){


  xvar00 <- deparse(substitute(x))

  if ( is.null(fill) & is.null(color) ){
    fill <- "steelblue"
    color <- darken_color(fill)[2]
  }

  if ( is.null(fill) & !is.null(color) ){
    color <- color
    fill <- lighten_color(color)[2]
  }

  if ( !is.null(fill) & is.null(color) ){
    fill <- fill
    color <- darken_color(fill)[2]
  }

  splitsing <- split_x_fn(data, xvar00, left = left, right = right)
  nbinwidth <- diff(range(do.call(rbind, splitsing)$x)) / bins

  obj <- ggplot(data = splitsing[[2]], aes(x = x))

  if ( isTRUE( line )){
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

  if ( !is.null(splitsing[[1]]) ){

    ticks_for_left <- update_tickmarks_left(
      obj, left, round(min(data[[xvar00]], na.rm = TRUE), 1)
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

  if ( !is.null(splitsing[[3]]) ){

    ticks_for_right <- update_tickmarks_right(obj, right,
                                              round(max(data[[xvar00]],
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

  obj <- obj +
    ggplot2::theme_minimal() +
    ggplot2::xlab(xvar00) +
    ggplot2::geom_hline(yintercept = 0, color = "white")

  return(obj)
}





