#' @keywords internal
make_stars <- function(pval) {
  if ( is.na(pval) ) {pval <- is.numeric(pval)}
  if(pval <= 0.001)
    stars = "***"
  else if(pval > 0.001 & pval <= 0.01)
    stars = "**"
  else if(pval > 0.01 & pval <= 0.05)
    stars = "*"
  else if(pval > 0.05 & pval <= 0.1)
    stars = "."
  else {stars = ""}
  stars
}


#' @keywords internal
elapsed_days <- function(end_date){
  as.POSIXlt(end_date)$mday - 1
}

#' @keywords internal
matchColClasses <- function(df1, df2) {

  sharedColNames <- names(df1)[names(df1) %in% names(df2)]
  sharedColTypes <- sapply(df1[,sharedColNames, drop = FALSE], class)

  for (n in sharedColNames) {
    attributes(df2[,n]) <- attributes(df1[,n])
    class(df2[, n]) <- sharedColTypes[n]
  }

  return(df2)
}


#' Get splits from partykit object
#' @noRd
#'
#' @param x A party object.
#'
get_splits <- function(x) {

  lrp <- utils::getFromNamespace(".list.rules.party", "partykit")
  splits_list <- lrp(x)
  last_line <- unname(splits_list[length(splits_list)])

  # Remove punctuation marks
  splits_vector <- regmatches(last_line, gregexpr("[[:digit:]]+", last_line))

  splits <- as.numeric(unlist(splits_vector))
  return(splits)
}


#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot


#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @keywords internal
order_factors_exposure <- function(x, weight, decreasing) {
  counts <- sort(tapply(weight, x, FUN = sum), decreasing = !decreasing)
  factor(x, levels = names(counts))
}


#' @keywords internal
scale_second_axis <- function(background, df, dfby, f_axis, s_axis, by){
  if ( isTRUE(background) ){

    if ( by == "NULL"){
      df$s_axis_scale <- df[[s_axis]] / max(df[[s_axis]], na.rm = TRUE) *
        max(df[[f_axis]], na.rm = TRUE)
      df$s_axis_print <- round(df[[s_axis]], 0)
    }

    if ( by != "NULL"){
      df$s_axis_scale <- df[[s_axis]] / max(df[[s_axis]], na.rm = TRUE) *
        max(dfby[[f_axis]], na.rm = TRUE)
      df$s_axis_print <- round(df[[s_axis]], 0)
    }

  }
  return(df)
}


#' @keywords internal
separation_mark <- function(dec.mark){
  if ( dec.mark == "," ){
    function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  } else{
    function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)
  }
}


#' @keywords internal
sort_x_axis <- function(sort_manual, label_width){ # hist_sort
  if ( !is.null(sort_manual) ){
    list(
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = label_width), limits = sort_manual )
    )
  } else{
    list(
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = label_width) )
    )
  }
}

#' @importFrom colorspace lighten
#' @keywords internal
lighten_color <- function(color, amount = 0.25, n = 3){
  x <- vector(mode = "character", length = n)
  x[1] <- color
  if (n > 1){
    for (i in 2:n){
      x[i] <- colorspace::lighten(color, i * amount)
    }
    x
  }
}

#' @importFrom colorspace darken
#' @keywords internal
darken_color <- function(color, amount = 0.25, n = 3){
  x <- vector(mode = "character", length = n)
  x[1] <- color
  if (n > 1){
    for (i in 2:n){
      x[i] <- colorspace::darken(color, i * amount)
    }
    x
  }
}



#' @keywords internal
ggbarplot <- function(background, df, dfby, xvar, f_axis, s_axis, color_bg, sep_mark, by){
  fill_bg <- lighten_color(color_bg)[2]
  if ( isTRUE(background) & by == "NULL" ){

      list(
        ggplot2::geom_bar(data = df, aes(x = .data[[xvar]], y = .data[["s_axis_scale"]]),
                          stat = "identity", color = color_bg, fill = fill_bg, alpha = 1),
        ggplot2::scale_y_continuous(sec.axis = sec_axis(~ . * max(df[[s_axis]]) / max(df[[f_axis]]),
                                                        name = s_axis,
                                                        labels = sep_mark),
                                    labels = sep_mark,
                                    limits = c(0, NA),
                                    expand = expansion(mult = c(0, 0.01))
        )
      )
  }

  else if ( isTRUE(background) & by != "NULL" ){

    list(
      ggplot2::geom_bar(data = df, aes(x = .data[[xvar]], y = .data[["s_axis_scale"]]),
                        stat = "identity", color = color_bg, fill = fill_bg, alpha = 1),
      ggplot2::scale_y_continuous(sec.axis = sec_axis(~ . * max(df[[s_axis]], na.rm = TRUE) / max(dfby[[f_axis]], na.rm = TRUE),
                                                      name = s_axis,
                                                      labels = sep_mark),
                                  labels = sep_mark,
                                  limits = c(0, NA),
                                  expand = expansion(mult = c(0, 0.01))
      )
    )
  }

  else{ NULL }
}


#' @keywords internal
ggpointline <- function(df, dfby, xvar, y, color, by,
                        show_total, total_color, total_name){
  if ( by == "NULL"){
    list(
      ggplot2::geom_point(data = df,
                          aes(x = .data[[xvar]],
                              y = .data[[y]]),
                          color = color),
      ggplot2::geom_line(data = df,
                         aes(x = .data[[xvar]],
                             y = .data[[y]],
                             group = 1),
                         color = color),
      ggplot2::theme_minimal()
    )
  } else {
    if ( isTRUE(show_total) ){
    list(
      ggplot2::geom_point(data = dfby,
                          aes(x = .data[[xvar]],
                              y = .data[[y]],
                              color = .data[[by]])),
      ggplot2::geom_line(data = dfby,
                         aes(x = .data[[xvar]],
                             y = .data[[y]],
                             group = .data[[by]],
                             color = as.factor(.data[[by]]))),
      ggplot2::theme_minimal(),
      ggplot2::labs(color = by, linetype = NULL),
      ggplot2::geom_point(data = df,
                          aes(x = .data[[xvar]],
                              y = .data[[y]]),
                          color = total_color),
      ggplot2::geom_line(data = df,
                         aes(x = .data[[xvar]],
                             y = .data[[y]],
                             linetype = total_name,
                             group = "black"),
                         color = total_color)
    )}
    else {
      list(
        ggplot2::geom_point(data = dfby,
                            aes(x = .data[[xvar]],
                                y = .data[[y]],
                                color = .data[[by]])),
        ggplot2::geom_line(data = dfby,
                           aes(x = .data[[xvar]],
                               y = .data[[y]],
                               group = .data[[by]],
                               color = as.factor(.data[[by]]))),
        ggplot2::theme_minimal(),
        ggplot2::labs(color = by)
      )
    }
  }
}


#' @keywords internal
gglabels <- function(background, labels, df, xvar, sep_mark){
  if ( isTRUE(background) & isTRUE(labels) ){
    list(
      ggplot2::geom_text(data = df, aes(x = .data[[xvar]],
                                        y = .data[["s_axis_scale"]],
                                        label = sep_mark(.data[["s_axis_print"]])),
                         vjust = "inward",
                         size = 3)
    )
  } else { NULL }
}


#' @keywords internal
ggbarlabels <- function(df, xvar, y, coord_flip, sep_mark){

  df$y_print <- round(df[[y]], 0)

  if ( isTRUE(coord_flip) ){
    list(
      ggplot2::geom_text(data = df,
                         aes(x = .data[[xvar]],
                             y = .data[[y]],
                             label = sep_mark(.data[["y_print"]])),
                         hjust = "inward",
                         size = 3)
    )
  }

  else if ( !isTRUE(coord_flip) ) {
    list(
      ggplot2::geom_text(data = df,
                         aes(x = .data[[xvar]],
                             y = .data[[y]],
                             label = sep_mark(.data[["y_print"]])),
                         vjust = "inward",
                         size = 3)
    )
  }
}


#' @keywords internal
ggyscale <- function(background, sep_mark){
  if ( !isTRUE( background )){
    list(
      ggplot2::scale_y_continuous(labels = sep_mark)
    )
  }
}


#' @keywords internal
ggbarline <- function(background, df, dfby, xvar, f_axis,
                      f_axis_name, exposure, color_bg, color,
                      sep_mark, by, labels, sort_manual, label_width,
                      show_total, total_color, total_name){
  df <- scale_second_axis(background, df, dfby, f_axis, exposure, by)
  ggplot2::ggplot() +
    ggbarplot(background, df, dfby, xvar, f_axis, exposure, color_bg, sep_mark, by) +
    ggpointline(df, dfby, xvar, f_axis, color, by,
                show_total, total_color, total_name) +
    ggplot2::labs(y = f_axis_name, x = xvar) +
    gglabels(background, labels, df, xvar, sep_mark) +
    ggyscale(background, sep_mark) +
    sort_x_axis(sort_manual, label_width)
}


#' @keywords internal
ggbar <- function(df, xvar, f_axis, color_bg, sep_mark, coord_flip){
  fill_bg <- lighten_color(color_bg)[2]
  ggplot2::ggplot(data = df) +
    ggplot2::geom_bar(data = df, aes(x = .data[[xvar]], y = .data[[f_axis]]),
                      stat = "identity", color = color_bg, fill = fill_bg, alpha = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = f_axis, x = xvar) +
    ggplot2::scale_y_continuous(labels = sep_mark) +
    ggbarlabels(df, xvar, f_axis, coord_flip, sep_mark) +
    ggcoordflip(coord_flip)
}


#' @keywords internal
ggcoordflip <- function(coord_flip){
  if ( isTRUE(coord_flip) ) {
    list(
      ggplot2::coord_flip()
    )
  } else { NULL }
}


#' @keywords internal
update_tickmarks_right <- function(plot_obj,
                                   cut_off,
                                   max_print) {
  ranges <- suppressMessages(
    ggplot2::ggplot_build(plot_obj)$layout$panel_params[[1]]$x
  )
  label_to_add <- sprintf("[%s, %s]", round(cut_off, 1), max_print)
  tick_positions <- ranges$get_breaks()
  tick_labels    <- ranges$get_labels()

  if (overlap_right(tick_positions, cut_off)) {
    tick_positions <- tick_positions[-length(tick_positions)]
    tick_labels    <- tick_labels[-length(tick_labels)]
  }

  return(list(tick_positions = c(tick_positions, cut_off),
              tick_labels    = c(tick_labels, label_to_add)))
}


#' @keywords internal
overlap_right <- function(positions, cut_off) {

  positions <- positions[!is.na(positions)]
  n <- length(positions)
  ticks_dif <- positions[n] - positions[n-1]
  (cut_off - positions[n]) / ticks_dif < 0.25
}


#' @importFrom ggplot2 ggplot_build
#' @keywords internal
update_tickmarks_left <- function(plot_obj,
                                  cut_off,
                                  min_print) {
  ranges <- suppressMessages(
    ggplot2::ggplot_build(plot_obj)$layout$panel_params[[1]]$x)
  label_to_add <- sprintf("[%s, %s]", min_print, round(cut_off, 1))
  tick_positions <- ranges$get_breaks()
  tick_labels    <- ranges$get_labels()

  if (overlap_left(tick_positions, cut_off)) {
    tick_positions <- tick_positions[-1]
    tick_labels    <- tick_labels[-1]
  }
  return(list(tick_positions = c(cut_off, tick_positions),
              tick_labels    = c(label_to_add, tick_labels)))
}


#' @keywords internal
overlap_left <- function(positions, cut_off) {
  positions <- positions[!is.na(positions)]
  ticks_dif <- positions[2] - positions[1]
  (positions[1] - cut_off) / ticks_dif < 0.25
}


#' @importFrom ggplot2 autoplot
#' @import data.table
#' @keywords internal
split_x_fn <- function(data, x, left = NULL, right = NULL){

  vec <- data[[x]]
  vec_new <- data.table::data.table(data)[get(x) > right, c(x) := right][get(x) < left, c(x) := left][,get(x)]

  if ( !is.null(left) ){
    if ( left <= min(vec, na.rm = TRUE)){ stop( "Left should be greater than minimum value", call. = FALSE ) }
    if ( left >= max(vec, na.rm = TRUE)){ stop( "Left should be less than maximum value", call. = FALSE )}
  }

  if ( !is.null(right) ){
    if ( right >= max(vec, na.rm = TRUE)){ stop( "Right should be less than maximum value", call. = FALSE ) }
    if ( right <= min(vec, na.rm = TRUE)){ stop( "Right should be greater than minimum value", call. = FALSE)}
  }

  if ( !is.null(left) & !is.null(right)){
    if ( left >= right ){ stop( "Right should be larger than left", call. = FALSE) }
  }

  l1 <- split(vec_new, cut(vec_new,
                           breaks = c(min(vec, na.rm = TRUE), left, right - 1e-10, max(vec, na.rm = TRUE)),
                           include.lowest = TRUE))

  l1 <- lapply(l1, function(x) data.frame(x = x))
  if ( is.null(left) ){ l1 <- append(list(NULL), l1) }
  if ( is.null(right) ){ l1 <- append(l1, list(NULL)) }
  return(l1)
}


#' @keywords internal
moments <- function(x, dist = c("gamma", "lognormal")){

  dist <- match.arg(dist)
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  v <- s^2

  if ( dist == "gamma" ){
    scale <- m ^ 2 / s
    shape <- s / m
    return(list(scale = scale, shape = shape))
  }

  if ( dist == "lognormal" ){
    meanlog <- log(m ^ 2 / sqrt(v + m ^ 2) )
    sdlog <- log( v / (m ^ 2) + 1)
    return(list(meanlog = meanlog, sdlog = sdlog))
  }
}


##' @keywords internal
#dtruncated_gamma <- function(x, scale, shape) {
#  if (!requireNamespace("truncdist", quietly = TRUE)) {
#    stop("Package \"truncdist\" needed for this function to work. Please install it.",
#         call. = FALSE)
#  }
#  truncdist::dtrunc(x, "gamma", a = left, b = right, scale = scale, shape = shape)
#}








