#' Automatically create a ggplot for objects obtained from univariate_all()
#'
#' @description Takes an object produced by \code{univariate_all()}, and plots the available input.
#'
#' @param x univ_all object produced by \code{univariate_all()}
#' @param show_plots numeric vector of plots to be shown (default is c(1,2,3,4)), there are six available plots:
#'  \itemize{
#'   \item{1. frequency (i.e. number of claims / expsore)}
#'   \item{2. average severity (i.e. severity / number of claims)}
#'   \item{3. risk premium (i.e. severity / exposure)}
#'   \item{4. loss ratio (i.e. severity / premium)}
#'   \item{5. average premium (i.e. premium / exposure)}
#'   \item{6. exposure}
#' }
#' @param ncol number of columns in output (default is 1)
#' @param background show exposure as a background histogram (default is TRUE)
#' @param labels show labels with the exposure (default is TRUE)
#' @param sort sort (or order) risk factor into descending order by exposure (default is FALSE)
#' @param dec.mark control the format of the decimal point, as well as the mark between intervals before the decimal point, choose either "," (default) or "."
#' @param color change the color of the points and line ("dodgerblue" is default)
#' @param color_bg change the color of the histogram ("#E7B800" is default)
#'
#' @import patchwork
#' @import ggplot2
#'
#' @return a ggplot2 object
#'
#' @examples
#' library(ggplot2)
#' x <- univariate_all(MTPL2, x = area, severity = amount, nclaims = nclaims, exposure = exposure)
#' autoplot(x)
#' autoplot(x, show_plots = c(6,1), background = FALSE, sort = TRUE)
#'
#' @export
autoplot.univ_all <- function(x, show_plots = c(1, 2, 3, 4), ncol = 1, background = TRUE, labels = TRUE,
                              sort = FALSE, dec.mark = ",", color = "dodgerblue", color_bg = "#E7B800"){

  df <- x$df
  xvar <- x$xvar
  nclaims <- x$nclaims
  exposure <- x$exposure
  loss <- x$loss
  premium <- x$premium

  if ( !is.numeric(show_plots) ){
    stop("show_plots should be numeric")
  }

  if ( !any(show_plots > 0 & show_plots < 7) ){
    stop("elements in show_plots are unknown")
  }

  if ( !is.factor(df[[xvar]]) ){
    df[[xvar]] <- as.factor(df[[xvar]])
  }

  if ( isTRUE(sort) & exposure != "NULL" ){
    df[[xvar]] <- factor(df[[xvar]], levels = df[[xvar]][order(df[[exposure]], decreasing=TRUE)])
  }

  if ( dec.mark == ","){
    sep_fn <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  } else{
    sep_fn <- function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)
  }

  plots_allowed <- show_plots[show_plots < 7 & show_plots > 0]

  not_allowed <- NULL

  if ( "frequency" %in% names(df) & 1 %in% show_plots ){
    x1 <- x
    class(x1) <- "univ_freq"
    p1 <- autoplot.univ_freq(x1, background, labels, sort, dec.mark, color, color_bg)
  } else( not_allowed <- c(not_allowed, 1))

  if ( "average_severity" %in% names(df) & 2 %in% show_plots ){
    x2 <- x
    class(x2) <- "univ_avgsev"
    p2 <- autoplot.univ_avgsev(x2, background, labels, sort, dec.mark, color, color_bg)
  } else( not_allowed <- c(not_allowed, 2))

  if ( "risk_premium" %in% names(df) & 3 %in% show_plots ){
    x3 <- x
    class(x3) <- "univ_premium"
    p3 <- autoplot.univ_premium(x3, background, labels, sort, dec.mark, color, color_bg)
  } else( not_allowed <- c(not_allowed, 3))

  if ( "loss_ratio" %in% names(df) & 4 %in% show_plots ){
    x4 <- x
    class(x4) <- "univ_lossratio"
    p4 <- autoplot.univ_lossratio(x4, background, labels = FALSE, sort, dec.mark, color, color_bg)
    plot_n <- c(plot_n, 4)
  } else( not_allowed <- c(not_allowed, 4))

  if ( "average_premium" %in% names(df) & 5 %in% show_plots ){
    x5 <- x
    class(x5) <- "univ_avgpremium"
    p5 <- autoplot.univ_avgpremium(x5, background, labels, sort, dec.mark, color, color_bg)
  } else( not_allowed <- c(not_allowed, 5))

  if ( exposure %in% names(df) & 6 %in% show_plots ){
    x6 <- x
    class(x6) <- "univ_exposure"
    p6 <- autoplot.univ_exposure(x6, labels = labels, sort = sort, coord_flip = FALSE, dec.mark = dec.mark, color_bg = color_bg)
  } else( not_allowed <- c(not_allowed, 6))

  plots_possible <- setdiff(plots_allowed, not_allowed)

   if ( length (plots_possible) == 0 ){
     stop("Ignoring plots: input is unknown")
   }

  diff_plots <- setdiff(show_plots, plots_possible)
  if ( length (diff_plots) > 0 ){
    message(paste0("Ignoring plots ", paste0(diff_plots, collapse = ", "), ": input is unknown"))
  }

  if ( isTRUE(sort) & exposure == "NULL" ){
    message("Ignoring sort: exposure is unknown")
  }

   remove_axis <- list( theme(axis.title.x = element_blank(),
                              axis.text.x = element_blank(),
                              axis.ticks.x = element_blank()) )

   plot_last <- paste0("p", plots_possible[length(plots_possible)], collapse = " + ")

   if ( length(plots_possible) == 1 ){
     plot_out <- eval(parse( text = plot_last ))
   }

   if ( length(plots_possible) > 1 ){
     plot_nrs <- paste0("p", plots_possible[-length(plots_possible)], " + remove_axis", collapse = " + ")
     plot_all <- paste0(plot_nrs, " + ", plot_last)
     plot_out <- eval(parse( text = plot_all )) + plot_layout(ncol = ncol)
   }

   return(plot_out)
}





