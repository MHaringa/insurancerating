#' Automatically create a ggplot for objects obtained from univariate_exposure()
#'
#' @description Takes an object produced by \code{univariate_exposure()}, and plots the exposure.
#'
#' @param x univ_exposure object produced by \code{univariate_exposure()}
#' @param labels show labels with the exposure (default is TRUE)
#' @param sort sort (or order) risk factor into descending order by exposure (default is FALSE)
#' @param coord_flip flip cartesian coordinates so that horizontal becomes vertical, and vertical, horizontal (default is TRUE)
#' @param dec.mark control the format of the decimal point, as well as the mark between intervals before the decimal point, choose either "," (default) or "."
#' @param color_bg change the color of the histogram ("#E7B800" is default)
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
#' @export
autoplot.univ_exposure<- function(x, labels = TRUE, sort = FALSE, coord_flip = TRUE, dec.mark = ",", color_bg = "#E7B800"){

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
      geom_text(data = df, aes(x = .data[[xvar]], y = .data[[exposure]], label = sep_fn(.data[[exposure]])),
                vjust = "inward", size = 3)
    )
  }

  if ( isTRUE( labels ) & isTRUE( coord_flip )) {
    labels_bg <- list(
      geom_text(data = df, aes(x = .data[[xvar]], y = .data[[exposure]], label = sep_fn(.data[[exposure]])),
                hjust = "inward", size = 3)
    )
  }

  p1 <- ggplot(data = df) +
    geom_bar(data = df, aes(x = .data[[xvar]], y = .data[[exposure]]),
             stat = "identity", color = color_bg, fill = color_bg, alpha = 0.4) +
    {if( isTRUE( labels )) labels_bg} +
    {if( isTRUE( coord_flip )) coord_flip()} +
    theme_minimal() +
    labs(y = exposure, x = xvar) +
    scale_y_continuous(labels = sep_fn) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))

  return(p1)
}


