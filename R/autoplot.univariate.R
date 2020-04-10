#' Automatically create a ggplot for objects obtained from univariate()
#'
#' @description Takes an object produced by \code{univariate()}, and plots the available input.
#'
#' @param object univariate object produced by \code{univariate()}
#' @param show_plots numeric vector of plots to be shown (default is c(1,2,3,4,5,6,7,8,9)), there are nine available plots:
#'  \itemize{
#'   \item{1. frequency (i.e. number of claims / expsore)}
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

  df <- object$df
  xvar <- object$xvar
  nclaims <- object$nclaims
  exposure <- object$exposure
  severity <- object$severity
  premium <- object$premium

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
    df[[xvar]] <- factor(df[[xvar]], levels = df[[xvar]][order(df[[exposure]], decreasing = TRUE)])
  }

  if ( dec.mark == "," ){
    sep_fn <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  } else{
    sep_fn <- function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)
  }

  hist_bg_fn <- function(f_axis, s_axis){
    df$s_axis_scale <- df[[s_axis]] / max(df[[s_axis]], na.rm = TRUE) * max(df[[f_axis]], na.rm = TRUE)
    df$s_axis_print <- round(df[[s_axis]], 0)
    return(df)
  }

  hist_plot_fn <- function(f_axis, s_axis){
    list(
      ggplot2::geom_bar(data = df, aes(x = .data[[xvar]], y = .data[["s_axis_scale"]]),
                        stat = "identity", color = color_bg, fill = color_bg, alpha = 0.4),
      ggplot2::scale_y_continuous(sec.axis = sec_axis(~ . * max(df[[s_axis]]) / max(df[[f_axis]]),
                                                      name = s_axis,
                                                      labels = sep_fn),
                                  labels = sep_fn,
                                  limits = c(0, NA),
                                  expand = expansion(mult = c(0, 0.01))
      )
    )
  }

  point_line_fn <- function(y){
    list(
      ggplot2::geom_point(aes(x = .data[[xvar]], y = .data[[y]]), color = color),
      ggplot2::geom_line(aes(x = .data[[xvar]], y = .data[[y]], group = 1), color = color),
      ggplot2::theme_minimal()
    )
  }

  labels_bg_fn <- function(){
    list(
      ggplot2::geom_text(data = df, aes(x = .data[[xvar]], y = .data[["s_axis_scale"]], label = sep_fn(.data[["s_axis_print"]])),
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



  bar_df_fn <- function(y){
    df$y_print <- round(df[[y]], 0)

    if ( isTRUE( sort ) & isTRUE( coord_flip ) ){
      df[[xvar]] <- factor(df[[xvar]], levels = df[[xvar]][order(df[[y]], decreasing = FALSE)])
    }

    if ( isTRUE( sort ) & !isTRUE( coord_flip ) ){
      df[[xvar]] <- factor(df[[xvar]], levels = df[[xvar]][order(df[[y]], decreasing = TRUE)])
    }
    return(df)
  }

  bar_fn <- function(y){
    ggplot2::ggplot(data = df) +
      ggplot2::geom_bar(data = df, aes(x = .data[[xvar]], y = .data[[y]]),
                        stat = "identity", color = color_bg, fill = color_bg, alpha = 0.4)
  }

  theme_bar <- list(
    ggplot2::theme_minimal(),
    ggplot2::labs(y = exposure, x = xvar),
    ggplot2::scale_y_continuous(labels = sep_fn)
  )

  labels_bar_fn1 <- function(y){
    list(
      ggplot2::geom_text(data = df, aes(x = .data[[xvar]], y = .data[[y]], label = sep_fn(.data[["y_print"]])),
                         vjust = "inward", size = 3)
    )
  }

  labels_bar_fn2 <- function(y){
    list(
      ggplot2::geom_text(data = df, aes(x = .data[[xvar]], y = .data[[y]], label = sep_fn(.data[["y_print"]])),
                         hjust = "inward", size = 3)
      )
  }

  bar_ggplot <- function(y){

    p0 <- bar_fn(y) +
      theme_bar +
      { if( isTRUE( coord_flip )) ggplot2::coord_flip() } +
      ggplot2::labs(y = y, x = xvar) +
      { if( !is.null( sort_manual )) hist_sort } +
      { if ( isTRUE(labels) & !isTRUE(coord_flip) ) labels_bar_fn1(y) } +
      { if ( isTRUE(labels) & isTRUE(coord_flip) ) labels_bar_fn2(y) }
    return(p0)
  }

  plots_allowed <- show_plots[show_plots < 10 & show_plots > 0]
  not_allowed <- NULL

  if ( "frequency" %in% names(df) & 1 %in% show_plots ){
    if ( isTRUE(background) ) { df <- hist_bg_fn("frequency", exposure) }
    p1 <- ggplot2::ggplot(data = df) +
     {if ( isTRUE(background) ) { hist_plot_fn("frequency", exposure) }} +
      point_line_fn("frequency") +
      ggplot2::labs(y = "Frequency", x = xvar) +
      { if ( isTRUE( background ) & isTRUE( labels )) labels_bg_fn() } +
      { if( !isTRUE ( background )) ggplot2::scale_y_continuous(labels = sep_fn) } +
      { if( !is.null( sort_manual )) hist_sort }
  } else( not_allowed <- c(not_allowed, 1))

   if ( "average_severity" %in% names(df) & 2 %in% show_plots ){
     if ( isTRUE(background) ) { df <- hist_bg_fn("average_severity", nclaims) }
     p2 <- ggplot2::ggplot(data = df) +
       {if ( isTRUE(background) ) { hist_plot_fn("average_severity", nclaims) }} +
       point_line_fn("average_severity") +
       ggplot2::labs(y = "Average\nseverity", x = xvar) +
       { if ( isTRUE( background ) & isTRUE( labels )) labels_bg_fn() } +
       { if( !isTRUE ( background )) ggplot2::scale_y_continuous(labels = sep_fn) } +
       { if( !is.null( sort_manual )) hist_sort }
   } else( not_allowed <- c(not_allowed, 2))

  if ( "risk_premium" %in% names(df) & 3 %in% show_plots ){
    if ( isTRUE(background) ) { df <- hist_bg_fn("risk_premium", exposure) }
    p3 <- ggplot2::ggplot(data = df) +
      {if ( isTRUE(background) ) { hist_plot_fn("risk_premium", exposure) }} +
      point_line_fn("risk_premium") +
      ggplot2::labs(y = "Risk premium", x = xvar) +
      { if ( isTRUE( background ) & isTRUE( labels )) labels_bg_fn() } +
      { if( !isTRUE ( background )) ggplot2::scale_y_continuous(labels = sep_fn) } +
      { if( !is.null( sort_manual )) hist_sort }
  } else( not_allowed <- c(not_allowed, 3))

  if ( "loss_ratio" %in% names(df) & 4 %in% show_plots ){
    if ( isTRUE(background) ) { df <- hist_bg_fn("loss_ratio", premium) }
    p4 <- ggplot2::ggplot(data = df) +
      {if ( isTRUE(background) ) { hist_plot_fn("loss_ratio", premium) }} +
      point_line_fn("loss_ratio") +
      ggplot2::labs(y = "Loss ratio", x = xvar) +
      { if ( isTRUE( background ) & isTRUE( labels )) labels_bg_fn() } +
      { if( !isTRUE ( background )) ggplot2::scale_y_continuous(labels = sep_fn) } +
      { if( !is.null( sort_manual )) hist_sort }
  } else( not_allowed <- c(not_allowed, 4))

  if ( "average_premium" %in% names(df) & 5 %in% show_plots ){
    if ( isTRUE(background) ) { df <- hist_bg_fn("average_premium", exposure) }
    p5 <- ggplot2::ggplot(data = df) +
      {if ( isTRUE(background) ) { hist_plot_fn("average_premium", exposure) }} +
      point_line_fn("average_premium") +
      ggplot2::labs(y = "Average\npremium", x = xvar) +
      { if ( isTRUE( background ) & isTRUE( labels )) labels_bg_fn() } +
      { if( !isTRUE ( background )) ggplot2::scale_y_continuous(labels = sep_fn) } +
      { if( !is.null( sort_manual )) hist_sort }
  } else( not_allowed <- c(not_allowed, 5))

  if ( exposure %in% names(df) & 6 %in% show_plots ){
    if ( isTRUE( labels )) { df <- bar_df_fn(exposure) }
    p6 <- bar_ggplot(exposure)
  } else( not_allowed <- c(not_allowed, 6))

  if ( severity %in% names(df) & 7 %in% show_plots ){
    if ( isTRUE( labels )) { df <- bar_df_fn(severity) }
    p7 <- bar_ggplot(severity)
  } else( not_allowed <- c(not_allowed, 7))

  if ( nclaims %in% names(df) & 8 %in% show_plots ){
    if ( isTRUE( labels )) { df <- bar_df_fn(nclaims) }
    p8 <- bar_ggplot(nclaims)
  } else( not_allowed <- c(not_allowed, 8))

  if ( premium %in% names(df) & 9 %in% show_plots ){
    if ( isTRUE( labels )) { df <- bar_df_fn(premium) }
    p9 <- bar_ggplot(premium)
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
      plot_all <- paste0(plot_nrs, " + ", plot_last)
      plot_out <- eval(parse( text = plot_all )) + patchwork::plot_layout(ncol = 1)
    }
  } else {
    plot_all <- paste0("p", plots_possible, collapse = " + ")
    plot_out <- eval(parse( text = plot_all )) + patchwork::plot_layout(ncol = ncol)
  }

  return(plot_out)
}




