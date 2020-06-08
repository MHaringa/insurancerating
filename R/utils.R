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
scale_second_axis <- function(background, df, dfby, f_axis, s_axis, by){
  if ( isTRUE(background) ){

    if ( by == "NULL"){
      df$s_axis_scale <- df[[s_axis]] / max(df[[s_axis]], na.rm = TRUE) * max(df[[f_axis]], na.rm = TRUE)
      df$s_axis_print <- round(df[[s_axis]], 0)
    }

    if ( by != "NULL"){
      df$s_axis_scale <- df[[s_axis]] / max(df[[s_axis]], na.rm = TRUE) * max(dfby[[f_axis]], na.rm = TRUE)
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



#' @keywords internal
ggbarplot <- function(background, df, dfby, xvar, f_axis, s_axis, color_bg, sep_mark, by){
  if ( isTRUE(background) & by == "NULL" ){

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
  }

  else if ( isTRUE(background) & by != "NULL" ){

    list(
      ggplot2::geom_bar(data = df, aes(x = .data[[xvar]], y = .data[["s_axis_scale"]]),
                        stat = "identity", color = color_bg, fill = color_bg, alpha = 0.4),
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
ggpointline <- function(df, dfby, xvar, y, color, by){
  if ( by == "NULL"){
    list(
      ggplot2::geom_point(data = df, aes(x = .data[[xvar]], y = .data[[y]]), color = color),
      ggplot2::geom_line(data = df, aes(x = .data[[xvar]], y = .data[[y]], group = 1), color = color),
      ggplot2::theme_minimal()
    )
  } else {
    list(
      ggplot2::geom_point(data = dfby, aes(x = .data[[xvar]], y = .data[[y]], color = .data[[by]])),
      ggplot2::geom_line(data = dfby, aes(x = .data[[xvar]], y = .data[[y]], group = .data[[by]], color = as.factor(.data[[by]]))),
      ggplot2::theme_minimal(),
      ggplot2::labs(color = by)
    )
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
      ggplot2::geom_text(data = df, aes(x = .data[[xvar]], y = .data[[y]], label = sep_mark(.data[["y_print"]])),
                         hjust = "inward", size = 3)
    )
  }

  else if ( !isTRUE(coord_flip) ) {
    list(
      ggplot2::geom_text(data = df, aes(x = .data[[xvar]], y = .data[[y]], label = sep_mark(.data[["y_print"]])),
                         vjust = "inward", size = 3)
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
ggbarline <- function(background, df, dfby, xvar, f_axis, f_axis_name, exposure, color_bg, color, sep_mark, by, labels, sort_manual, label_width){
  df <- scale_second_axis(background, df, dfby, f_axis, exposure, by)
  ggplot2::ggplot() +
    ggbarplot(background, df, dfby, xvar, f_axis, exposure, color_bg, sep_mark, by) +
    ggpointline(df, dfby, xvar, f_axis, color, by) +
    ggplot2::labs(y = f_axis_name, x = xvar) +
    gglabels(background, labels, df, xvar, sep_mark) +
    ggyscale(background, sep_mark) +
    sort_x_axis(sort_manual, label_width)
}

#' @keywords internal
ggbar <- function(df, xvar, f_axis, color_bg, sep_mark, coord_flip){
  ggplot2::ggplot(data = df) +
    ggplot2::geom_bar(data = df, aes(x = .data[[xvar]], y = .data[[f_axis]]),
                      stat = "identity", color = color_bg, fill = color_bg, alpha = 0.4) +
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










