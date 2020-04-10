#' Automatically create a ggplot for objects obtained from univariate_loss_ratio()
#' @noRd
#'
#' @description Takes an object produced by \code{univariate_loss_ratio()}, and plots the loss ratio.
#'
#' @param x univ_lossratio object produced by \code{univariate_loss_ratio()}
#' @param background show exposure as a histogram (default is TRUE)
#' @param labels show labels with the exposure (default is TRUE)
#' @param sort sort (or order) risk factor into descending order by exposure (default is FALSE)
#' @param sort_manual sort (or order) risk factor into own ordering; should be a character vector (default is NULL)
#' @param dec.mark control the format of the decimal point, as well as the mark between intervals before the decimal point, choose either "," (default) or "."
#' @param color change the color of the points and line ("dodgerblue" is default)
#' @param color_bg change the color of the histogram ("#E7B800" is default)
#' @param label_width width of labels on the x-axis (10 is default)
#'
#' @import ggplot2
#' @importFrom stringr str_wrap
#'
#' @return a ggplot2 object
#' @examples
#' library(ggplot2)
#' x <- univariate_loss_ratio(MTPL2, x = area, severity = amount, premium = premium)
#' autoplot(x)
#'
autoplot.univ_lossratio <- function(x, background = TRUE, labels = TRUE, sort = FALSE, sort_manual = NULL, dec.mark = ",", color = "dodgerblue", color_bg = "#E7B800", label_width = 10){

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }

  if (!inherits(x, "univ_lossratio")) {
    stop("autoplot.univ_lossratio requires a univ_lossratio object, use x = object")
  }

  df <- x$df
  xvar <- x$xvar
  severity <- x$severity
  nclaims <- x$nclaims
  exposure <- x$exposure
  premium <- x$premium

  if ( !is.factor(df[[xvar]]) ){
    df[[xvar]] <- as.factor(df[[xvar]])
  }

  if ( isTRUE( sort ) & exposure != "NULL"){
    df[[xvar]] <- factor(df[[xvar]], levels = df[[xvar]][order(df[[exposure]], decreasing=TRUE)])
  }

  if ( dec.mark == ","){
    sep_fn <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  } else{
    sep_fn <- function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)
  }

  if ( isTRUE ( background )){
    df$premium_scale <- df[[premium]] / max(df[[premium]], na.rm = TRUE) * max(df$loss_ratio, na.rm = TRUE)
    df[[premium]] <- round(df[[premium]], 0)

    hist_bg <- list(
      ggplot2::geom_bar(data = df, aes(x = .data[[xvar]], y = .data[["premium_scale"]]),
                     stat = "identity", color = color_bg, fill = color_bg, alpha = 0.4),
      ggplot2::scale_y_continuous(sec.axis = sec_axis(~ . * max(df[[premium]]) / max(df$loss_ratio),
                                             name = premium,
                                             labels = sep_fn),
                         labels = sep_fn,
                         limits = c(0, NA),
                         expand = expansion(mult = c(0, 0.01))
      )
    )
  }

  if (isTRUE( background ) & isTRUE( labels )) {
    labels_bg <- list(
      ggplot2::geom_text(data = df, aes(x = .data[[xvar]], y = .data[["premium_scale"]], label = sep_fn(.data[[premium]])),
                vjust = "inward", size = 3)
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
    { if( isTRUE( background )) hist_bg } +
    ggplot2::geom_point(aes(x = .data[[xvar]], y = .data[["loss_ratio"]]), color = color) +
    ggplot2::geom_line(aes(x = .data[[xvar]], y = .data[["loss_ratio"]], group = 1), color = color) +
    { if( isTRUE( background ) & isTRUE( labels )) labels_bg } +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = "Loss ratio", x = xvar) +
    { if( !isTRUE ( background )) ggplot2::scale_y_continuous(labels = sep_fn) }  +
    { if( !is.null( sort_manual )) hist_sort }

  return(p1)
}





