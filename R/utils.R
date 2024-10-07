#' Set reference group to the group with largest exposure
#'
#' @description This function specifies the first level of a factor to the level
#' with the largest exposure. Levels of factors are sorted using an alphabetic
#' ordering. If the factor is used in a regression context, then the first level
#' will be the reference. For insurance applications it is common to specify
#' the reference level to the level with the largest exposure.
#'
#' @param x an unordered factor
#' @param weight a vector containing weights (e.g. exposure). Should be numeric.
#'
#' @author Martin Haringa
#'
#' @references Kaas, Rob & Goovaerts, Marc & Dhaene, Jan & Denuit, Michel.
#' (2008). Modern Actuarial Risk Theory: Using R. doi:10.1007/978-3-540-70998-5.
#'
#' @importFrom stats relevel
#'
#' @return a factor of the same length as x
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' df <- chickwts |>
#' mutate(across(where(is.character), as.factor)) |>
#' mutate(across(where(is.factor), ~biggest_reference(., weight)))
#' }
#'
#' @export
biggest_reference <- function(x, weight) {
  if (!is.numeric(weight)) weight <- is.numeric(weight)
  counts <- sort(tapply(weight, x, FUN = sum), decreasing = TRUE)
  xrelevel <- stats::relevel(x, ref = names(counts)[1])
  attr(xrelevel, "xoriginal") <- levels(x)
  xrelevel
}


#' Fisher's natural breaks classification
#'
#' @description The function provides an interface to finding class intervals
#' for continuous numerical variables, for example for choosing colours for
#' plotting maps.
#'
#' @param vec a continuous numerical variable
#' @param n number of classes required (n = 7 is default)
#' @param diglab number of digits (n = 2 is default)
#'
#' @return Vector with clustering
#'
#' @importFrom classInt classIntervals
#'
#' @author Martin Haringa
#'
#' @details The "fisher" style uses the algorithm proposed by W. D. Fisher
#' (1958) and discussed by Slocum et al. (2005) as the Fisher-Jenks algorithm.
#' This function is adopted from the classInt package.
#'
#' @references Bivand, R. (2018). classInt: Choose Univariate Class Intervals.
#' R package version 0.2-3. <https://CRAN.R-project.org/package=classInt>
#' @references Fisher, W. D. 1958 "On grouping for maximum homogeneity", Journal
#' of the American Statistical Association, 53, pp. 789–798.
#' doi: 10.1080/01621459.1958.10501479.
#'
#' @export
fisher <- function(vec, n = 7, diglab = 2) {
  cluster <- classInt::classIntervals(vec, n = n, style = "fisher",
                                      intervalClosure = "right")[[2]]
  cut(vec, breaks = cluster, include.lowest = TRUE, dig.lab = diglab)
}


#' @keywords internal
make_stars <- function(pval) {
  # returns character string
  if (is.na(pval)) {
    pval <- is.numeric(pval)
  }
  if (pval > 0 && pval <= 0.001) {
    stars <- "***"
  } else if (pval > 0.001 && pval <= 0.01) {
    stars <- "**"
  } else if (pval > 0.01 && pval <= 0.05) {
    stars <- "*"
  } else if (pval > 0.05 && pval <= 0.1) {
    stars <- "."
  } else {
    stars <- ""
  }
  stars
}


#' @keywords internal
elapsed_days <- function(end_date) {
  as.POSIXlt(end_date)$mday - 1
}

#' @keywords internal
matchColClasses <- function(df1, df2) {

  shared_colnames <- names(df1)[names(df1) %in% names(df2)]
  shared_coltypes <- sapply(df1[, shared_colnames, drop = FALSE], class)

  for (n in shared_colnames) {
    attributes(df2[, n]) <- attributes(df1[, n])
    class(df2[, n]) <- shared_coltypes[n]
  }

  df2
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



#' @keywords internal
order_factors_exposure <- function(x, weight, decreasing) {
  counts <- sort(tapply(weight, x, FUN = sum), decreasing = !decreasing)
  factor(x, levels = names(counts))
}


#' @keywords internal
scale_second_axis <- function(background, df, dfby, f_axis, s_axis, by) {
  if (isTRUE(background)) {

    if (by == "NULL") {
      df$s_axis_scale <- df[[s_axis]] / max(df[[s_axis]], na.rm = TRUE) *
        max(df[[f_axis]], na.rm = TRUE)
      df$s_axis_print <- round(df[[s_axis]], 0)
    }

    if (by != "NULL") {
      df$s_axis_scale <- df[[s_axis]] / max(df[[s_axis]], na.rm = TRUE) *
        max(dfby[[f_axis]], na.rm = TRUE)
      df$s_axis_print <- round(df[[s_axis]], 0)
    }

  }
  return(df)
}


#' @keywords internal
separation_mark <- function(dec.mark) {
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


#' @importFrom colorspace lighten
#' @keywords internal
lighten_color <- function(color, amount = 0.25, n = 3) {
  x <- vector(mode = "character", length = n)
  x[1] <- color
  if (n > 1) {
    for (i in 2:n){
      x[i] <- colorspace::lighten(color, i * amount)
    }
    x
  }
}

#' @importFrom colorspace darken
#' @keywords internal
darken_color <- function(color, amount = 0.25, n = 3) {
  x <- vector(mode = "character", length = n)
  x[1] <- color
  if (n > 1) {
    for (i in 2:n){
      x[i] <- colorspace::darken(color, i * amount)
    }
    x
  }
}



#' @keywords internal
ggbarplot <- function(background, df, dfby, xvar, f_axis, s_axis, color_bg,
                      sep_mark, by, remove_underscores, label_width) {
  fill_bg <- lighten_color(color_bg)[2]
  if (isTRUE(background) && by == "NULL") {

    list(
      ggplot2::geom_bar(data = df, aes(x = .data[[xvar]],
                                       y = .data[["s_axis_scale"]]),
                        stat = "identity", color = color_bg,
                        fill = fill_bg, alpha = 1),
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
                        stat = "identity", color = color_bg, fill = fill_bg,
                        alpha = 1),
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
  fill_bg <- lighten_color(color_bg)[2]
  ggplot2::ggplot(data = df) +
    ggplot2::geom_bar(data = df, aes(x = .data[[xvar]], y = .data[[f_axis]]),
                      stat = "identity", color = color_bg,
                      fill = fill_bg, alpha = 1) +
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
  ticks_dif <- positions[n] - positions[n - 1]
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
split_x_fn <- function(data, x, left = NULL, right = NULL) {

  vec <- data[[x]]
  vec_new <- data.table::data.table(data)[get(x) > right, c(x) := right][
    get(x) < left, c(x) := left][, get(x)]

  if (!is.null(left)) {
    if (left <= min(vec, na.rm = TRUE)) {
      stop("Left should be greater than minimum value", call. = FALSE)
    }
    if (left >= max(vec, na.rm = TRUE)) {
      stop("Left should be less than maximum value", call. = FALSE)
    }
  }

  if (!is.null(right)) {
    if (right >= max(vec, na.rm = TRUE)) {
      stop("Right should be less than maximum value", call. = FALSE)
    }
    if (right <= min(vec, na.rm = TRUE)) {
      stop("Right should be greater than minimum value", call. = FALSE)
    }
  }

  if (!is.null(left) && !is.null(right)) {
    if (left >= right) {
      stop("Right should be larger than left", call. = FALSE)
    }
  }

  l1 <- split(vec_new, cut(vec_new,
                           breaks = c(min(vec, na.rm = TRUE), left,
                                      right - 1e-10, max(vec, na.rm = TRUE)),
                           include.lowest = TRUE))

  l1 <- lapply(l1, function(x) data.frame(x = x))
  if (is.null(left)) l1 <- append(list(NULL), l1)
  if (is.null(right)) l1 <- append(l1, list(NULL))
  l1
}

#' @keywords internal
construct_fm <- function(lhs, rhs) {
  as.formula(paste0(paste0(lhs, collapse = " + "), "~ ", rhs))
}


#' @keywords internal
moments <- function(x, dist = c("gamma", "lognormal")) {

  dist <- match.arg(dist)
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  v <- s^2

  if (dist == "gamma") {
    scale <- m ^ 2 / s
    shape <- s / m
    return(list(scale = scale, shape = shape))
  }

  if (dist == "lognormal") {
    meanlog <- log(m ^ 2 / sqrt(v + m ^ 2))
    sdlog <- log(v / (m ^ 2) + 1)
    return(list(meanlog = meanlog, sdlog = sdlog))
  }
}

#' @keywords internal
dtrunc <- function(x, spec, a = -Inf, b = Inf, ...) {
  ###
  ### this function computes the density function defined by the spec argument
  ### for the vector of quantile values in x.  The random variable is truncated
  ### to be in the interval ( a, b )
  ###
  ### Arguments
  ### x = a numeric vector of quantiles
  ### spec = a character value for the name of the distribution (e.g., "norm")
  ### ... = other arguments passed to the corresponding density function
  ###
  if (a >= b)
    stop("argument a is greater than or equal to b")
  tt <- rep(0, length(x))
  g <- get(paste("d", spec, sep = ""), mode = "function")
  G <- get(paste("p", spec, sep = ""), mode = "function")
  G.a <- G(a, ...)
  G.b <- G(b, ...)
  if (G.a == G.b) {
    stop("Trunction interval is not inside the domain of the density function")
  }
  tt[x >= a & x <= b] <- g(x[x >= a & x <= b], ...) / (G(b, ...) - G(a, ...))
  tt
}

#' @keywords internal
ptrunc <- function(q, spec, a = -Inf, b = Inf, ...) {
  ###
  ### this function computes the distribution function defined by the spec
  ### argument for the vector of quantile values in x.  The random variable is
  ### truncated to be in the interval ( a, b )
  ###
  ### Arguments
  ### q = a numeric vector of quantiles
  ### spec = a character value for the name of the distribution (e.g., "norm")
  ### ... = other arguments passed to the corresponding density function
  ###
  if (a >= b)
    stop("argument a is greater than or equal to b")
  tt <- q
  aa <- rep(a, length(q))
  bb <- rep(b, length(q))
  G <- get(paste("p", spec, sep = ""), mode = "function")
  tt <- G(apply(cbind(apply(cbind(q, bb), 1, min), aa), 1, max), ...)
  tt <- tt - G(aa, ...)
  G.a <- G(aa, ...)
  G.b <- G(bb, ...)
  if (any(G.a == G.b)) {
    stop(
      "Trunction interval is not inside the domain of the distribution function"
    )
  }
  tt / (G(bb, ...) - G(aa, ...))
}

#' @keywords internal
color_blue <- function(x) {
  x[!is.na(x)] <- paste0("\033[", "3", "4m", x[!is.na(x)],
                         "\033[", "3", "9m")
  x
}


#' @keywords internal
color_red <- function(x) {
  x[!is.na(x)] <- paste0("\033[", "3", "1m", x[!is.na(x)],
                         "\033[", "3", "9m")
  x
}

#' @keywords internal
color_green <- function(x) {
  x[!is.na(x)] <- paste0("\033[", "3", "2m", x[!is.na(x)],
                         "\033[", "3", "9m")
  x
}
