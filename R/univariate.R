#' Univariate analysis for discrete risk factors
#'
#' @description Univariate analysis for discrete risk factors in an insurance portfolio. The following summary statistics are calculated:
#' \itemize{
#'  \item{frequency (i.e. number of claims / exposure)}
#'  \item{average severity (i.e. severity / number of claims)}
#'  \item{risk premium (i.e. severity / exposure)}
#'  \item{loss ratio (i.e. severity / premium)}
#'  \item{average premium (i.e. premium / exposure)}
#' }
#' If input arguments are not specified, the summary statistics related to these arguments are ignored.
#'
#' @param df data.frame with insurance portfolio
#' @param x column in \code{df} with risk factor
#' @param severity column in \code{df} with severity (default is NULL)
#' @param premium column in \code{df} with premium (default is NULL)
#' @param exposure column in \code{df} with exposure (default is NULL)
#' @param nclaims column in \code{df} with number of claims (default is NULL)
#' @param by column(s) in \code{df} to group by
#'
#' @return A list of class \code{univ_all} with components
#' \item{df}{data frame}
#' \item{xvar}{name of column in df with risk factor}
#' \item{severity}{name of column in df with severity}
#' \item{nclaims}{name of column in df with number of claims}
#' \item{exposure}{name of column in df with exposure}
#' \item{premium}{name of column in df with premium}
#'
#' @importFrom data.table data.table
#'
#' @examples
#' univariate(MTPL2, x = area, severity = amount, nclaims = nclaims,
#'            exposure = exposure, premium = premium)
#'
#' # The summary statistics related to premium are not calculated
#' univariate(MTPL2, x = area, severity = amount, nclaims = nclaims, exposure = exposure)
#'
#' @export
univariate <- function(df, x, severity = NULL, nclaims = NULL, exposure = NULL, premium = NULL, by = NULL){

  x00 <- deparse(substitute(x))
  severity00 <- deparse(substitute(severity))
  nclaims00 <- deparse(substitute(nclaims))
  exposure00 <- deparse(substitute(exposure))
  premium00 <- deparse(substitute(premium))
  by00 <- deparse(substitute(by))

  cols <- c(severity00, nclaims00, exposure00, premium00)
  cols <- cols[cols != "NULL"]

  xby00 <- c(x00, by00)
  xby00 <- xby00[xby00 != "NULL"]

  if ( length( cols ) == 0 ) {
    stop("Define column names.")
  }

  dt <- data.table::data.table(df)[, lapply(.SD, sum, na.rm = TRUE), by = xby00, .SDcols = cols]

  if ( by00 != "NULL"){
    dt1 <- data.table::data.table(df)[, lapply(.SD, sum, na.rm = TRUE), by = x00, .SDcols = cols]
  } else {
    dt1 <- NULL
  }

  frequency = average_severity = risk_premium = loss_ratio = average_premium = NULL # due to NSE notes in R CMD check

  # Frequency
  if ( all(c(nclaims00, exposure00) %in% cols)  ){
    dt <- dt[, frequency := get(nclaims00) / get(exposure00)]
    if ( by00 != "NULL" ){
      dt1 <- dt1[, frequency := get(nclaims00) / get(exposure00)]
    }
  }

  # Average severity
  if ( all(c(severity00, nclaims00) %in% cols) ){
    dt <- dt[, average_severity := get(severity00) / get(nclaims00)]
    if ( by00 != "NULL" ){
      dt1 <- dt1[, average_severity := get(severity00) / get(nclaims00)]
    }
  }

  # Risk premium
  if ( all(c(severity00, exposure00) %in% cols) ){
    dt <- dt[, risk_premium := get(severity00) / get(exposure00)]
    if ( by00 != "NULL" ){
      dt1 <- dt1[, risk_premium := get(severity00) / get(exposure00)]
    }
  }

  # Loss ratio
  if ( all(c(severity00, premium00) %in% cols) ){
    dt <- dt[, loss_ratio := get(severity00) / get(premium00)]
    if ( by00 != "NULL" ){
      dt1 <- dt1[, loss_ratio := get(severity00) / get(premium00)]
    }
  }

  # Average premium
  if ( all(c(premium00, exposure00) %in% cols) ){
    dt <- dt[, average_premium := get(premium00) / get(exposure00)]
    if ( by00 != "NULL" ){
      dt1 <- dt1[, average_premium := get(premium00) / get(exposure00)]
    }
  }

  return(structure(list(df = as.data.frame(dt),
                        xvar = x00,
                        severity = severity00,
                        nclaims = nclaims00,
                        exposure = exposure00,
                        premium = premium00,
                        by = by00,
                        dfby = as.data.frame(dt1)),
                   class = "univariate"))
}


#' @export
print.univariate <- function(x, ...) {
  print(x$df)
}

#' @export
as.data.frame.univariate <- function(x, ...) {
  df <- x$df
  return(as.data.frame(df))
}

#' Automatically create a ggplot for objects obtained from univariate()
#'
#' @description Takes an object produced by \code{univariate()}, and plots the available input.
#'
#' @param object univariate object produced by \code{univariate()}
#' @param show_plots numeric vector of plots to be shown (default is c(1,2,3,4,5,6,7,8,9)), there are nine available plots:
#'  \itemize{
#'   \item{1. frequency (i.e. number of claims / exposure)}
#'   \item{2. average severity (i.e. severity / number of claims)}
#'   \item{3. risk premium (i.e. severity / exposure)}
#'   \item{4. loss ratio (i.e. severity / premium)}
#'   \item{5. average premium (i.e. premium / exposure)}
#'   \item{6. exposure}
#'   \item{7. severity}
#'   \item{8. nclaims}
#'   \item{9. premium}
#' }
#' @param ncol number of columns in output (default is 1)
#' @param background show exposure as a background histogram (default is TRUE)
#' @param labels show labels with the exposure (default is TRUE)
#' @param sort sort (or order) risk factor into descending order by exposure (default is FALSE)
#' @param sort_manual sort (or order) risk factor into own ordering; should be a character vector (default is NULL)
#' @param dec.mark control the format of the decimal point, as well as the mark between intervals before the decimal point, choose either "," (default) or "."
#' @param color change the color of the points and line ("dodgerblue" is default)
#' @param color_bg change the color of the histogram ("#E7B800" is default)
#' @param label_width width of labels on the x-axis (10 is default)
#' @param coord_flip flip cartesian coordinates so that horizontal becomes vertical, and vertical, horizontal (default is FALSE)
#' @param ... other plotting parameters to affect the plot
#'
#' @import patchwork
#' @import ggplot2
#'
#' @return a ggplot2 object
#'
#' @examples
#' library(ggplot2)
#' x <- univariate(MTPL2, x = area, severity = amount, nclaims = nclaims, exposure = exposure)
#' autoplot(x)
#' autoplot(x, show_plots = c(6,1), background = FALSE, sort = TRUE)
#'
#' MTPL2a <- MTPL2
#' MTPL2a$jaar <- sample(2015:2019, nrow(MTPL2a), replace = TRUE)
#' x1 <- univariate(MTPL2a, x = area, severity = amount, nclaims = nclaims,
#' exposure = exposure, by = jaar)
#' autoplot(x1, show_plots = 1:2)
#'
#' @export
autoplot.univariate <- function(object, show_plots = 1:9, ncol = 1, background = TRUE, labels = TRUE,
                                sort = FALSE, sort_manual = NULL, dec.mark = ",", color = "dodgerblue",
                                color_bg = "#E7B800", label_width = 10, coord_flip = FALSE, ...){

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("patchwork is needed for this function to work. Install it via install.packages(\"patchwork\")", call. = FALSE)
  }

  if (!inherits(object, "univariate")) {
    stop("autoplot.univariate requires a univariate object, use object = object")
  }

  xvar <- object$xvar
  nclaims <- object$nclaims
  exposure <- object$exposure
  severity <- object$severity
  premium <- object$premium
  by <- object$by

  if ( by == "NULL" ){
    df <- object$df
    dfby <- object$dfby
  } else {
    df <- object$dfby
    dfby <- object$df
  }

  if ( length(by) > 1){
    stop("length of by should not be greater than one")
  }

  if ( !is.numeric(show_plots) ){
    stop("show_plots should be numeric")
  }

  if ( !any(show_plots > 0 & show_plots < 10) ){
    stop("elements in show_plots are unknown")
  }

  if ( length(show_plots) > 1 & isTRUE(coord_flip) ){
    stop("length of show_plots should be equal to one in case coord_flip = TRUE")
  }

  if ( !is.factor(df[[xvar]]) ){
    df[[xvar]] <- as.factor(df[[xvar]])
    dfby[[xvar]] <- as.factor(dfby[[xvar]])
    dfby[[by]] <- as.factor(dfby[[by]])
  }

  if ( by != "NULL" ){
    if ( !is.factor(dfby[[xvar]]) ){
      dfby[[xvar]] <- as.factor(dfby[[xvar]])
    }

    if ( !is.factor(dfby[[by]]) ){
      dfby[[by]] <- as.factor(dfby[[by]])
    }
  }

  if ( isTRUE(sort) & exposure != "NULL" ){
    df[[xvar]] <- order_factors_exposure(df[[xvar]], df[[exposure]], decreasing = coord_flip)
  }

  sep_mark <- separation_mark(dec.mark)

  plots_allowed <- show_plots[show_plots < 10 & show_plots > 0]
  not_allowed <- NULL

  if ( "frequency" %in% names(df) & 1 %in% show_plots ){
    p1 <- ggbarline(background, df, dfby, xvar, "frequency", "Frequency", exposure,
                    color_bg, color, sep_mark, by, labels, sort_manual, label_width)
  } else( not_allowed <- c(not_allowed, 1) )

  if ( "average_severity" %in% names(df) & 2 %in% show_plots ){
    p2 <- ggbarline(background, df, dfby, xvar, "average_severity", "Average\nseverity", nclaims,
                    color_bg, color, sep_mark, by, labels, sort_manual, label_width)
    if ( !1 %in% not_allowed ) { p2 <- p2 + theme(legend.position = "none") }
  } else( not_allowed <- c(not_allowed, 2) )

  if ( "risk_premium" %in% names(df) & 3 %in% show_plots ){
    p3 <- ggbarline(background, df, dfby, xvar, "risk_premium", "Risk premium", exposure,
                    color_bg, color, sep_mark, by, labels, sort_manual, label_width)
    if ( !all(1:2 %in% not_allowed) ) { p3 <- p3 + theme(legend.position = "none") }
  } else( not_allowed <- c(not_allowed, 3) )

  if ( "loss_ratio" %in% names(df) & 4 %in% show_plots ){
    p4 <- ggbarline(background, df, dfby, xvar, "loss_ratio", "Loss ratio", premium,
                    color_bg, color, sep_mark, by, labels, sort_manual, label_width)
    if ( !all(1:3 %in% not_allowed) ) { p4 <- p4 + theme(legend.position = "none") }
  } else( not_allowed <- c(not_allowed, 4) )

  if ( "average_premium" %in% names(df) & 5 %in% show_plots ){
    p5 <- ggbarline(background, df, dfby, xvar, "average_premium", "Average\npremium", exposure,
                    color_bg, color, sep_mark, by, labels, sort_manual, label_width)
    if ( !all(1:4 %in% not_allowed) ) { p5 <- p5 + theme(legend.position = "none") }
  } else( not_allowed <- c(not_allowed, 5) )

  if ( exposure %in% names(df) & 6 %in% show_plots ){
    p6 <- ggbar(df, xvar, exposure, color_bg, sep_mark, coord_flip)
  } else( not_allowed <- c(not_allowed, 6))

  if ( severity %in% names(df) & 7 %in% show_plots ){
    p7 <- ggbar(df, xvar, severity, color_bg, sep_mark, coord_flip)
  } else( not_allowed <- c(not_allowed, 7))

  if ( nclaims %in% names(df) & 8 %in% show_plots ){
    p8 <- ggbar(df, xvar, nclaims, color_bg, sep_mark, coord_flip)
  } else( not_allowed <- c(not_allowed, 8))

  if ( premium %in% names(df) & 9 %in% show_plots ){
    p9 <- ggbar(df, xvar, premium, color_bg, sep_mark, coord_flip)
  } else( not_allowed <- c(not_allowed, 9))

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

  if ( isTRUE(coord_flip) & length(plots_possible) > 1 ) {
    message("`coord_flip` only works for one bar graph")
  }

  if ( ncol == 1 ){
    remove_axis <- list( theme(axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               axis.ticks.x = element_blank()) )

    plot_last <- paste0("p", plots_possible[length( plots_possible )], collapse = " + ")

    if ( length(plots_possible) == 1 ){
      plot_out <- eval(parse( text = plot_last ))
    }

    if ( length(plots_possible) > 1 ){
      plot_nrs <- paste0("p", plots_possible[-length(plots_possible)], " + remove_axis", collapse = " + ")
      plot_all <- paste0("(", plot_nrs, " + ", plot_last, ")")
      plot_out <- eval(parse( text = plot_all )) + patchwork::plot_layout(ncol = 1, guides = 'collect')
    }
  } else {
    plot_all <- paste0("p", plots_possible, collapse = " + ")
    plot_out <- eval(parse( text = plot_all )) + patchwork::plot_layout(ncol = ncol, guides = 'collect')
  }

  return(plot_out)
}


