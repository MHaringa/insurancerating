#' Automatically create a ggplot for objects obtained from univariate_average_severity()
#'
#' @description Takes an object produced by \code{univariate_average_severity()}, and plots the claim frequency.
#'
#' @param x univ_avgsev object produced by \code{univariate_average_severity()}
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
#' x <- univariate_average_severity(MTPL2, x = area, severity = amount,
#'                                    nclaims = nclaims, premium = premium)
#' autoplot(x)
#'
#' @export
autoplot.univ_avgsev <- function(x, background = TRUE, labels = TRUE, sort = FALSE, dec.mark = ",", color = "dodgerblue", color_bg = "#E7B800"){

  df <- x$df
  xvar <- x$xvar
  severity <- x$severity
  nclaims <- x$nclaims
  exposure <- x$exposure
  premium <- x$premium

  if ( !is.factor(df[[xvar]]) ){
    df[[xvar]] <- as.factor(df[[xvar]])
  }

  if ( isTRUE( sort ) & exposure != "NULL" ){
    df[[xvar]] <- factor(df[[xvar]], levels = df[[xvar]][order(df[[exposure]], decreasing=TRUE)])
  }

  if ( dec.mark == ","){
    sep_fn <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  } else{
    sep_fn <- function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)
  }

  if ( isTRUE ( background )){
    df$nclaims_scale <- df[[nclaims]] / max(df[[nclaims]], na.rm = TRUE) * max(df$average_severity, na.rm = TRUE)
    df[[nclaims]] <- round(df[[nclaims]], 0)

    hist_bg <- list(
      geom_bar(data = df, aes(x = .data[[xvar]], y = .data[["nclaims_scale"]]),
                     stat = "identity", color = color_bg, fill = color_bg, alpha = 0.4),
      scale_y_continuous(sec.axis = sec_axis(~ . * max(df[[nclaims]]) / max(df$average_severity),
                                             name = nclaims,
                                             labels = sep_fn),
                         labels = sep_fn,
                         limits = c(0, NA),
                         expand = expansion(mult = c(0, 0.01))
      )
    )
  }

  if (isTRUE( background ) & isTRUE( labels )) {
    labels_bg <- list(
      geom_text(data = df, aes(x = .data[[xvar]], y = .data[["nclaims_scale"]], label = sep_fn(.data[[nclaims]])),
                vjust = "inward", size = 3)
    )
  }

  p1 <- ggplot(data = df) +
    {if( isTRUE( background )) hist_bg} +
    geom_point(aes(x = .data[[xvar]], y = .data[["average_severity"]]), color = color) +
    geom_line(aes(x = .data[[xvar]], y = .data[["average_severity"]], group = 1), color = color) +
    {if( isTRUE( background ) & isTRUE( labels )) labels_bg} +
    theme_minimal() +
    labs(y = "Average\nseverity", x = xvar) +
    {if( !isTRUE ( background )) scale_y_continuous(labels = sep_fn)} +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))

  return(p1)
}



