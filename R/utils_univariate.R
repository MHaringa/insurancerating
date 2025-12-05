#' @keywords internal
add_metrics <- function(dt, cols, .nclaims, .exposure, .severity, .premium) {
  if (!is.null(.nclaims) && !is.null(.exposure) &&
      all(c(.nclaims, .exposure) %in% cols)) {
    dt[, frequency := get(.nclaims) / get(.exposure)]
  }

  if (!is.null(.severity) && !is.null(.nclaims) &&
      all(c(.severity, .nclaims) %in% cols)) {
    dt[, average_severity := get(.severity) / get(.nclaims)]
  }

  if (!is.null(.severity) && !is.null(.exposure) &&
      all(c(.severity, .exposure) %in% cols)) {
    dt[, risk_premium := get(.severity) / get(.exposure)]
  }

  if (!is.null(.severity) && !is.null(.premium) &&
      all(c(.severity, .premium) %in% cols)) {
    dt[, loss_ratio := get(.severity) / get(.premium)]
  }

  if (!is.null(.premium) && !is.null(.exposure) &&
      all(c(.premium, .exposure) %in% cols)) {
    dt[, average_premium := get(.premium) / get(.exposure)]
  }

  dt
}

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot



#' @keywords internal
order_factors_exposure <- function(x, weight, decreasing) {
  counts <- sort(tapply(weight, x, FUN = sum), decreasing = !decreasing)
  factor(x, levels = names(counts))
}


#' Scale secondary axis for background plotting
#'
#' @description
#' Internal helper to rescale a secondary variable (`s_axis`) so it aligns
#' with the scale of a first variable (`f_axis`). Adds two new columns
#' to the data frame: `s_axis_scale` and `s_axis_print`.
#'
#' @return The input data frame with two additional columns:
#'   \itemize{
#'     \item \code{s_axis_scale} – scaled version of `s_axis`
#'     \item \code{s_axis_print} – rounded version of `s_axis`
#'   }
#'
#'
#' @keywords internal
scale_second_axis <- function(background, df, dfby, f_axis, s_axis, by) {
  if (isTRUE(background)) {

    denom <- max(df[[s_axis]], na.rm = TRUE)
    if (denom <= 0 || is.infinite(denom)) {
      stop("Secondary axis variable has no valid positive values to scale.")
    }

    max_ref <- if (by == "NULL") {
      max(df[[f_axis]], na.rm = TRUE)
    } else {
      max(dfby[[f_axis]], na.rm = TRUE)
    }

    df$s_axis_scale <- df[[s_axis]] / denom * max_ref
    df$s_axis_print <- round(df[[s_axis]], 0)
  }

  df
}


#' @keywords internal
separation_mark <- function(dec.mark) {

  if (!dec.mark %in% c(",", ".")) {
    stop("`dec.mark` must be either ',' or '.'")
  }

  if (dec.mark == ",") {
    function(x) {
      format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    }
  } else {
    function(x) {
      format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)
    }
  }
}

#' @keywords internal
sort_x_axis <- function(sort_manual, label_width) {
  if (!is.null(sort_manual)) {
    list(
      ggplot2::scale_x_discrete(
        labels = function(x) stringr::str_wrap(x, width = label_width),
        limits = sort_manual
      )
    )
  } else {
    list(
      ggplot2::scale_x_discrete(
        labels = function(x) stringr::str_wrap(x, width = label_width)
      )
    )
  }
}


#' @keywords internal
ggbarplot <- function(background, df, dfby, xvar, f_axis, s_axis, color_bg,
                      sep_mark, by, remove_underscores, label_width) {
  #fill_bg <- lighten_color(color_bg)[2]
  if (isTRUE(background) && by == "NULL") {

    list(
      ggplot2::geom_bar(data = df, aes(x = .data[[xvar]],
                                       y = .data[["s_axis_scale"]]),
                        stat = "identity",
                        color = "white", #color_bg,
                        fill = color_bg, #fill_bg,
                        alpha = .7), #1),
      ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(
        ~ . * max(df[[s_axis]], na.rm = TRUE) / max(df[[f_axis]], na.rm = TRUE),
        name = stringr::str_wrap(
          ifelse(isTRUE(remove_underscores), gsub("_", " ", s_axis), s_axis),
          width = label_width
        ),
        #name = ifelse(isTRUE(remove_underscores), gsub("_", " ", s_axis), s_axis),
        #name = s_axis,
        labels = sep_mark
      ),
      labels = sep_mark,
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.01))
      )
    )
  } else if (isTRUE(background) && by != "NULL") {

    list(
      ggplot2::geom_bar(data = df, aes(x = .data[[xvar]],
                                       y = .data[["s_axis_scale"]]),
                        stat = "identity",
                        color = "white", #color_bg,
                        fill = color_bg, #fill_bg,
                        alpha = .7), #1),
      ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(
        ~ . * max(df[[s_axis]], na.rm = TRUE) / max(dfby[[f_axis]], na.rm = T),
        name = s_axis,
        labels = sep_mark
      ),
      labels = sep_mark,
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.01))
      )
    )
  } else {
    NULL
  }
}


#' @keywords internal
ggpointline <- function(df, dfby, xvar, y, color, by,
                        show_total, total_color, total_name) {
  if (by == "NULL") {
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
    if (isTRUE(show_total)) {
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
      )
    } else {
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
gglabels <- function(background, labels, df, xvar, sep_mark) {
  if (isTRUE(background) && isTRUE(labels)) {
    list(
      ggplot2::geom_text(data = df,
                         aes(x = .data[[xvar]],
                             y = .data[["s_axis_scale"]],
                             label = sep_mark(.data[["s_axis_print"]])),
                         vjust = "inward",
                         size = 3)
    )
  } else {
    NULL
  }
}


#' @keywords internal
ggbarlabels <- function(df, xvar, y, coord_flip, sep_mark) {

  if (is.null(y) || !y %in% names(df)) {
    stop("Column `y` not found in data.", call. = FALSE)
  }

  df$y_print <- round(df[[y]], 0)

  if (isTRUE(coord_flip)) {
    list(
      ggplot2::geom_text(data = df,
                         aes(x = .data[[xvar]],
                             y = .data[[y]],
                             label = sep_mark(.data[["y_print"]])),
                         hjust = "inward",
                         size = 3)
    )
  } else if (!isTRUE(coord_flip)) {
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
ggyscale <- function(background, sep_mark) {
  if (!isTRUE(background)) {
    list(
      ggplot2::scale_y_continuous(labels = sep_mark)
    )
  }
}


#' @keywords internal
ggbarline <- function(background, df, dfby, xvar, f_axis,
                      f_axis_name, exposure, color_bg, color,
                      sep_mark, by, labels, sort_manual, label_width,
                      show_total, total_color, total_name, remove_underscores) {
  df <- scale_second_axis(background, df, dfby, f_axis, exposure, by)
  ggplot2::ggplot() +
    ggbarplot(background, df, dfby, xvar, f_axis, exposure,
              color_bg, sep_mark, by, remove_underscores, label_width) +
    ggpointline(df, dfby, xvar, f_axis, color, by,
                show_total, total_color, total_name) +
    ggplot2::labs(y = f_axis_name, x = xvar) +
    gglabels(background, labels, df, xvar, sep_mark) +
    ggyscale(background, sep_mark) +
    sort_x_axis(sort_manual, label_width)
}


#' @keywords internal
ggbar <- function(df, xvar, f_axis, color_bg, sep_mark, coord_flip) {
  #fill_bg <- lighten_color(color_bg)[2]
  ggplot2::ggplot(data = df) +
    ggplot2::geom_bar(data = df, aes(x = .data[[xvar]], y = .data[[f_axis]]),
                      stat = "identity",
                      color = "white", #color_bg,
                      fill = color_bg, #fill_bg,
                      alpha = .7) + #1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = f_axis, x = xvar) +
    ggplot2::scale_y_continuous(labels = sep_mark) +
    ggbarlabels(df, xvar, f_axis, coord_flip, sep_mark) +
    ggcoordflip(coord_flip)
}


#' @keywords internal
ggcoordflip <- function(coord_flip) {
  if (isTRUE(coord_flip)) {
    list(
      ggplot2::coord_flip()
    )
  } else {
    NULL
  }
}
