#' Automatically create a ggplot for objects obtained from univariate_frequency()
#'
#' @description Takes an object produced by \code{univariate_frequency()}, and plots the claim frequency.
#'
#' @param x univ_freq object produced by \code{univariate_frequency()}
#' @param background show exposure as a histogram (default is TRUE)
#' @param labels show labels with the exposure (default is TRUE)
#' @param sort sort (or order) risk factor into descending order by exposure (default is FALSE)
#' @param dec.mark control the format of the decimal point, as well as the mark between intervals before the decimal point, choose either "," (default) or "."
#' @param color change the color of the points and line ("dodgerblue" is default)
#' @param color_bg change the color of the histogram ("#E7B800" is default)
#'
#' @import ggplot2
#' @importFrom stringr str_wrap
#'
#' @return a ggplot2 object
#' @examples
#' library(ggplot2)
#' x <- univariate_frequency(MTPL2, x = area, nclaims = nclaims, exposure = exposure)
#' autoplot(x)
#' autoplot(x, sort = TRUE, dec.mark = ".", color_bg = "mediumseagreen")
#'
#' @export
autoplot.univ_freq <- function(x, background = TRUE, labels = TRUE, sort = FALSE, dec.mark = ",", color = "dodgerblue", color_bg = "#E7B800"){

  df <- x$df
  xvar <- x$xvar
  nclaims <- x$nclaims
  exposure <- x$exposure
  loss <- x$loss
  premium <- x$premium

  if ( !is.factor(df[[xvar]]) ){
    df[[xvar]] <- as.factor(df[[xvar]])
  }

  if ( isTRUE( sort )){
    df[[xvar]] <- factor(df[[xvar]], levels = df[[xvar]][order(df[[exposure]], decreasing=TRUE)])
  }

  if ( dec.mark == ","){
    sep_fn <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  } else{
    sep_fn <- function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)
  }

  if ( isTRUE ( background )){
    df$exposure_scale <- df[[exposure]] / max(df[[exposure]], na.rm = TRUE) * max(df$frequency, na.rm = TRUE)
    df[[exposure]] <- round(df[[exposure]], 0)

    hist_bg <- list(
      geom_bar(data = df, aes(x = .data[[xvar]], y = .data[["exposure_scale"]]),
                     stat = "identity", color = color_bg, fill = color_bg, alpha = 0.4),
      scale_y_continuous(sec.axis = sec_axis(~ . * max(df[[exposure]]) / max(df$frequency),
                                             name = exposure,
                                             labels = sep_fn),
                         labels = sep_fn,
                         limits = c(0, NA),
                         expand = expansion(mult = c(0, 0.01))
      )
    )
  }

  if (isTRUE( background ) & isTRUE( labels )) {
    labels_bg <- list(
      geom_text(data = df, aes(x = .data[[xvar]], y = .data[["exposure_scale"]], label = sep_fn(.data[[exposure]])),
                vjust = "inward", size = 3)
    )
  }

  p1 <- ggplot(data = df) +
    {if( isTRUE( background )) hist_bg} +
    geom_point(aes(x = .data[[xvar]], y = .data[["frequency"]]), color = color) +
    geom_line(aes(x = .data[[xvar]], y = .data[["frequency"]], group = 1), color = color) +
    {if( isTRUE( background ) & isTRUE( labels )) labels_bg} +
    theme_minimal() +
    labs(y = "Frequency", x = xvar) +
    {if( !isTRUE ( background )) scale_y_continuous(labels = sep_fn)} +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))

  return(p1)
}


