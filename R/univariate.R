#' Univariate analysis for discrete risk factors
#'
#' @description Univariate analysis for discrete risk factors in an insurance
#' portfolio. The following summary statistics are calculated:
#' \itemize{
#'  \item{frequency (i.e. number of claims / exposure)}
#'  \item{average severity (i.e. severity / number of claims)}
#'  \item{risk premium (i.e. severity / exposure)}
#'  \item{loss ratio (i.e. severity / premium)}
#'  \item{average premium (i.e. premium / exposure)}
#' }
#' If input arguments are not specified, the summary statistics related to
#' these arguments are ignored.
#'
#' @param df data.frame with insurance portfolio
#' @param x column in `df` with risk factor, or use `vec_ext()` for use with
#' an external vector (see examples)
#' @param severity column in `df` with severity (default is NULL)
#' @param premium column in `df` with premium (default is NULL)
#' @param exposure column in `df` with exposure (default is NULL)
#' @param nclaims column in `df` with number of claims (default is NULL)
#' @param by list of column(s) in `df` to group by
#'
#' @author Martin Haringa
#'
#' @return A data.frame
#'
#' @importFrom data.table data.table
#'
#' @examples
#' # Summarize by `area`
#' univariate(MTPL2, x = area, severity = amount, nclaims = nclaims,
#'            exposure = exposure, premium = premium)
#'
#' # Summarize by `area`, with column name in external vector
#' xt <- "area"
#' univariate(MTPL2, x = vec_ext(xt), severity = amount, nclaims = nclaims,
#'            exposure = exposure, premium = premium)
#'
#' # Summarize by `zip` and `bm`
#' univariate(MTPL, x = zip, severity = amount, nclaims = nclaims,
#'            exposure = exposure, by = bm)
#'
#' # Summarize by `zip`, `bm` and `power`
#' univariate(MTPL, x = zip, severity = amount, nclaims = nclaims,
#'            exposure = exposure, by = list(bm, power))
#'
#' @export
univariate <- function(df, x, severity = NULL, nclaims = NULL, exposure = NULL,
                       premium = NULL, by = NULL) {

  nse_se_input <- function(x){
    expr <- substitute(x)
    if ( is.call(expr) && expr[[1]] == quote(vec_ext)){
      as.character(eval.parent(expr[[2]]))
    } else if ( is.character(expr) ){
      x
    } else{
      deparse(expr)
    }
  }

  .xvar <- eval.parent(substitute(nse_se_input(x)))
  .severity <- eval.parent(substitute(nse_se_input(severity)))
  .nclaims <- eval.parent(substitute(nse_se_input(nclaims)))
  .exposure <- eval.parent(substitute(nse_se_input(exposure)))
  .premium <- eval.parent(substitute(nse_se_input(premium)))

  mc <- as.list(match.call())
  byl <- as.list(mc$by)

  if( !.xvar %in% names(df) ) stop("Column ", .xvar, " can't be found",
                                   call. = FALSE)

  if ( length(byl) > 0L && byl[[1]] == "c" ) stop("`by` must be a list",
                                                  call. = FALSE)

  if (length(byl) > 0L && (byl[[1L]] == as.symbol("list") ||
                           byl[[1L]] == as.symbol(".")))
    byl <- byl[-1L]

  COLS <- c()
  if (!missing(severity))
    COLS <- deparse(mc$severity)
  if (!missing(nclaims))
    COLS <- c(COLS, deparse(mc$nclaims))
  if (!missing(exposure))
    COLS <- c(COLS, deparse(mc$exposure))
  if (!missing(premium))
    COLS <- c(COLS, deparse(mc$premium))

  if (length(COLS) == 0) {
    stop("Define column names.", call. = FALSE)
  }

  BY <- as.call(c(as.symbol("list"), as.symbol(.xvar), byl))
  dt <- eval(bquote(data.table(df)[, lapply(.SD, sum, na.rm = TRUE),
                                   by = .(BY), .SDcols = .(COLS)]))

  dt1 <- NULL
  if (!missing(by)){
    BYx <- as.call(c(as.symbol("list"), as.symbol(.xvar)))
    dt1 <- eval(bquote(
      data.table::data.table(df)[, lapply(.SD, sum, na.rm = TRUE),
                                 by = .(BYx), .SDcols=.(COLS)]
    ))
  }

  frequency = average_severity = risk_premium = loss_ratio =
    average_premium = NULL # due to NSE notes in R CMD check

  # Frequency
  if ( all(c(.nclaims, .exposure) %in% COLS)  ){
    dt <- dt[, frequency := get(.nclaims) / get(.exposure)]
    if (!missing(by)){
      dt1 <- dt1[, frequency := get(.nclaims) / get(.exposure)]
    }
  }

  # Average severity
  if ( all(c(.severity, .nclaims) %in% COLS) ){
    dt <- dt[, average_severity := get(.severity) / get(.nclaims)]
    if (!missing(by)){
      dt1 <- dt1[, average_severity := get(.severity) / get(.nclaims)]
    }
  }

  # Risk premium
  if ( all(c(.severity, .exposure) %in% COLS) ){
    dt <- dt[, risk_premium := get(.severity) / get(.exposure)]
    if (!missing(by)){
      dt1 <- dt1[, risk_premium := get(.severity) / get(.exposure)]
    }
  }

  # Loss ratio
  if ( all(c(.severity, .premium) %in% COLS) ){
    dt <- dt[, loss_ratio := get(.severity) / get(.premium)]
    if (!missing(by)){
      dt1 <- dt1[, loss_ratio := get(.severity) / get(.premium)]
    }
  }

  # Average premium
  if ( all(c(.premium, .exposure) %in% COLS) ){
    dt <- dt[, average_premium := get(.premium) / get(.exposure)]
    if (!missing(by)){
      dt1 <- dt1[, average_premium := get(.premium) / get(.exposure)]
    }
  }

  attr(dt, "xvar") <- .xvar
  attr(dt, "severity") <- .severity
  attr(dt, "nclaims") <- .nclaims
  attr(dt, "exposure") <- .exposure
  attr(dt, "premium") <- .premium
  attr(dt, "by") <- byl
  attr(dt, "dfby") <- as.data.frame(dt1)

  class(dt) <- append(class(dt), "univariate")
  return(dt)
}



#' Automatically create a ggplot for objects obtained from univariate()
#'
#' @description Takes an object produced by `univariate()`, and plots the
#' available input.
#'
#' @param object univariate object produced by `univariate()`
#' @param show_plots numeric vector of plots to be shown (default is
#' c(1,2,3,4,5,6,7,8,9)), there are nine available plots:
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
#' @param sort sort (or order) risk factor into descending order by exposure
#' (default is FALSE)
#' @param sort_manual sort (or order) risk factor into own ordering; should be
#' a character vector (default is NULL)
#' @param dec.mark decimal mark; defaults to ","
#' @param color change the color of the points and line ("dodgerblue" is
#' default)
#' @param color_bg change the color of the histogram ("#f8e6b1" is default)
#' @param label_width width of labels on the x-axis (10 is default)
#' @param coord_flip flip cartesian coordinates so that horizontal becomes
#' vertical, and vertical, horizontal (default is FALSE)
#' @param show_total show line for total if by is used in univariate (default
#' is FALSE)
#' @param total_color change the color for the total line ("black" is default)
#' @param total_name add legend name for the total line (e.g. "total")
#' @param rotate_angle numeric value for angle of labels on the x-axis (degrees)
#' @param ... other plotting parameters to affect the plot
#'
#' @import patchwork
#' @import ggplot2
#'
#' @author Marc Haine, Martin Haringa
#'
#' @return a ggplot2 object
#'
#' @examples
#' library(ggplot2)
#' x <- univariate(MTPL2, x = area, severity = amount, nclaims = nclaims,
#' exposure = exposure)
#' autoplot(x)
#' autoplot(x, show_plots = c(6,1), background = FALSE, sort = TRUE)
#'
#' # Group by `zip`
#' xzip <- univariate(MTPL, x = bm, severity = amount, nclaims = nclaims,
#' exposure = exposure, by = zip)
#' autoplot(xzip, show_plots = 1:2)
#'
#' @export
autoplot.univariate <- function(object, show_plots = 1:9, ncol = 1,
                                background = TRUE, labels = TRUE,
                                sort = FALSE, sort_manual = NULL,
                                dec.mark = ",", color = "dodgerblue",
                                color_bg = "lightskyblue", label_width = 10,
                                coord_flip = FALSE, show_total = FALSE,
                                total_color = NULL, total_name = NULL,
                                rotate_angle = NULL, ...){

  xvar <- attr(object, "xvar")
  nclaims <- attr(object, "nclaims")
  exposure <- attr(object, "exposure")
  severity <- attr(object, "severity")
  premium <- attr(object, "premium")
  by <- as.character(attr(object, "by"))

  if ( length(by) == 0 ) { by <- "NULL" }

  dfby0 <- attr(object, "dfby")

  if ( length(by) > 1){
    stop("length of `by` in `univariate()` must be equal to one", call. = FALSE)
  }

  if ( !is.numeric(show_plots) ){
    stop("`show_plots` should be numeric", call. = FALSE)
  }

  if ( !any(show_plots > 0 & show_plots < 10) ){
    stop("elements in `show_plots` are unknown", call. = FALSE)
  }

  if ( length(show_plots) > 1 & isTRUE(coord_flip) ){
    stop("length of `show_plots` must be equal to one in
         case `coord_flip = TRUE`", call. = FALSE)
  }

  if ( by == "NULL" ){
    df <- object
    dfby <- dfby0
  } else {
    df <- dfby0
    dfby <- object
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

  if ( is.null(total_color) ){
    total_color <- "black"
  }

  if ( isTRUE(sort) & exposure != "NULL" ){
    df[[xvar]] <- order_factors_exposure(df[[xvar]], df[[exposure]],
                                         decreasing = coord_flip)
  }

  sep_mark <- separation_mark(dec.mark)

  plots_allowed <- show_plots[show_plots < 10 & show_plots > 0]
  not_allowed <- NULL

  if ( "frequency" %in% names(df) & 1 %in% show_plots ){
    p1 <- ggbarline(background, df, dfby, xvar,
                    "frequency", "Frequency", exposure,
                    color_bg, color, sep_mark, by, labels,
                    sort_manual, label_width,
                    show_total, total_color, total_name)
  } else( not_allowed <- c(not_allowed, 1) )

  if ( "average_severity" %in% names(df) & 2 %in% show_plots ){
    p2 <- ggbarline(background, df, dfby, xvar,
                    "average_severity", "Average\nseverity", nclaims,
                    color_bg, color, sep_mark, by, labels,
                    sort_manual, label_width,
                    show_total, total_color, total_name)
    if ( !1 %in% not_allowed ) { p2 <- p2 + theme(legend.position = "none") }
  } else( not_allowed <- c(not_allowed, 2) )

  if ( "risk_premium" %in% names(df) & 3 %in% show_plots ){
    p3 <- ggbarline(background, df, dfby, xvar,
                    "risk_premium", "Risk premium", exposure,
                    color_bg, color, sep_mark, by, labels,
                    sort_manual, label_width,
                    show_total, total_color, total_name)
    if ( !all(1:2 %in% not_allowed) ) { p3 <- p3 +
      theme(legend.position = "none") }
  } else( not_allowed <- c(not_allowed, 3) )

  if ( "loss_ratio" %in% names(df) & 4 %in% show_plots ){
    p4 <- ggbarline(background, df, dfby, xvar,
                    "loss_ratio", "Loss ratio", premium,
                    color_bg, color, sep_mark, by, labels,
                    sort_manual, label_width,
                    show_total, total_color, total_name)
    if ( !all(1:3 %in% not_allowed) ) { p4 <- p4 +
      theme(legend.position = "none") }
  } else( not_allowed <- c(not_allowed, 4) )

  if ( "average_premium" %in% names(df) & 5 %in% show_plots ){
    p5 <- ggbarline(background, df, dfby, xvar,
                    "average_premium", "Average\npremium", exposure,
                    color_bg, color, sep_mark, by, labels,
                    sort_manual, label_width,
                    show_total, total_color, total_name)
    if ( !all(1:4 %in% not_allowed) ) { p5 <- p5 +
      theme(legend.position = "none") }
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
    stop("Ignoring plots: input is unknown", call. = FALSE)
  }

  diff_plots <- setdiff(show_plots, plots_possible)
  if ( length (diff_plots) > 0 ){
    message(paste0("Ignoring plots ",
                   paste0(diff_plots, collapse = ", "), ": input is unknown"))
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

    plot_last <- paste0("p",
                        plots_possible[length( plots_possible )],
                        collapse = " + ")

    if ( length(plots_possible) == 1 ){
      plot_out <- eval(parse( text = plot_last ))
    }

    if ( length(plots_possible) > 1 ){
      plot_nrs <- paste0("p", plots_possible[-length(plots_possible)],
                         " + remove_axis", collapse = " + ")
      plot_all <- paste0("(", plot_nrs, " + ", plot_last, ")")
      plot_out <- eval(parse( text = plot_all )) +
        patchwork::plot_layout(ncol = 1, guides = 'collect')
    }
  } else {
    plot_all <- paste0("p", plots_possible, collapse = " + ")
    plot_out <- eval(parse( text = plot_all )) +
      patchwork::plot_layout(ncol = ncol, guides = 'collect')
  }

  if ( !is.null(rotate_angle) ){
    plot_out <- plot_out +
      ggplot2::theme(axis.text.x = element_text(angle = rotate_angle,
                                                vjust = 0.5,
                                                hjust = 1))
  }

  return(plot_out)
}


