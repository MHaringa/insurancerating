#' Automatically create a ggplot for objects obtained from univariate_exposure() (deprecated function; use 'autoplot.univariate()' instead)
#' @noRd
#'
#' @description Takes an object produced by \code{univariate_exposure()}, and plots the exposure.
#'
#' @param x univ_exposure object produced by \code{univariate_exposure()}
#' @param labels show labels with the exposure (default is TRUE)
#' @param sort sort (or order) risk factor into descending order by exposure (default is FALSE)
#' @param sort_manual sort (or order) risk factor into own ordering; should be a character vector (default is NULL)
#' @param coord_flip flip cartesian coordinates so that horizontal becomes vertical, and vertical, horizontal (default is TRUE)
#' @param dec.mark control the format of the decimal point, as well as the mark between intervals before the decimal point, choose either "," (default) or "."
#' @param color_bg change the color of the histogram ("#E7B800" is default)
#' @param label_width width of labels on the x-axis (30 is default)
#'
#'
#' @import ggplot2
#' @importFrom stringr str_wrap
#'
#' @return a ggplot2 object
#' @examples
#' library(ggplot2)
#' x <- univariate_exposure(MTPL2, area, exposure)
#' autoplot(x, sort = TRUE)
#' autoplot(x, coord_flip = FALSE)
#'
autoplot.univ_exposure<- function(x, labels = TRUE, sort = FALSE, sort_manual = NULL, coord_flip = TRUE, dec.mark = ",", color_bg = "#E7B800", label_width = 30){

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }

  if (!inherits(x, "univ_exposure")) {
    stop("autoplot.univ_exposure requires a univ_exposure object, use x = object")
  }

  df <- x$df
  xvar <- x$xvar
  exposure <- x$exposure

  if ( !is.factor(df[[xvar]]) ){
    df[[xvar]] <- as.factor(df[[xvar]])
  }

  if ( isTRUE( sort ) ){
    df[[xvar]] <- factor(df[[xvar]], levels = df[[xvar]][order(df[[exposure]], decreasing = TRUE)])
  }

  if ( isTRUE( sort ) & isTRUE( coord_flip ) ){
    df[[xvar]] <- factor(df[[xvar]], levels = df[[xvar]][order(df[[exposure]], decreasing = FALSE)])
  }

  if ( dec.mark == ","){
    sep_fn <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  } else{
    sep_fn <- function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)
  }

  df[[exposure]] <- round(df[[exposure]], 0)

  if ( isTRUE( labels ) & !isTRUE( coord_flip )) {
    labels_bg <- list(
      ggplot2::geom_text(data = df, aes(x = .data[[xvar]], y = .data[[exposure]], label = sep_fn(.data[[exposure]])),
                vjust = "inward", size = 3)
    )
  }

  if ( isTRUE( labels ) & isTRUE( coord_flip )) {
    labels_bg <- list(
      ggplot2::geom_text(data = df, aes(x = .data[[xvar]], y = .data[[exposure]], label = sep_fn(.data[[exposure]])),
                hjust = "inward", size = 3)
    )
  }

  if ( !is.null( sort_manual )){
    hist_sort <- list(
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = label_width), limits = sort_manual )
    )
  } else {
    hist_sort <- list(
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = label_width) )
    )}

  p1 <- ggplot2::ggplot(data = df) +
    ggplot2::geom_bar(data = df, aes(x = .data[[xvar]], y = .data[[exposure]]),
             stat = "identity", color = color_bg, fill = color_bg, alpha = 0.4) +
    {if( isTRUE( labels )) labels_bg} +
    {if( isTRUE( coord_flip )) ggplot2::coord_flip()} +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = exposure, x = xvar) +
    ggplot2::scale_y_continuous(labels = sep_fn) +
    { if( !is.null( sort_manual )) hist_sort }

  return(p1)
}


