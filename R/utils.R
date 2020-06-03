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
scale_second_axis <- function(background, df, f_axis, s_axis){
  if ( isTRUE(background) ){
    df$s_axis_scale <- df[[s_axis]] / max(df[[s_axis]], na.rm = TRUE) * max(df[[f_axis]], na.rm = TRUE)
    df$s_axis_print <- round(df[[s_axis]], 0)
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

#' @keywords internal
ggbarplot <- function(background, df, xvar, f_axis, s_axis, color_bg, sep_mark){
  if ( isTRUE(background) ){
    list(
      ggplot2::geom_bar(data = df, aes(x = .data[[xvar]], y = .data[["s_axis_scale"]]),
                        stat = "identity", color = color_bg, fill = color_bg, alpha = 0.4),
      ggplot2::scale_y_continuous(sec.axis = sec_axis(~ . * max(df[[s_axis]]) / max(df[[f_axis]]),
                                                      name = s_axis,
                                                      labels = sep_mark),
                                  labels = sep_mark,
                                  limits = c(0, NA),
                                  expand = expansion(mult = c(0, 0.01))
      )
    )
  } else { NULL }
}

#' @keywords internal
ggpointline <- function(xvar, y, color){
  list(
    ggplot2::geom_point(aes(x = .data[[xvar]], y = .data[[y]]), color = color),
    ggplot2::geom_line(aes(x = .data[[xvar]], y = .data[[y]], group = 1), color = color),
    ggplot2::theme_minimal()
  )
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
ggyscale <- function(background, sep_mark){
  if ( !isTRUE( background )){
    list(
      ggplot2::scale_y_continuous(labels = sep_mark)
    )
  }
}










