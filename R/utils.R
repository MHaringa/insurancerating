#' Set reference group to the group with largest exposure
#'
#' @description Relevels a factor so that the category with the highest total
#' weight (e.g., exposure) becomes the reference (first) level.
#' This is useful in regression settings, where the first level of a factor
#' is taken as the baseline. In insurance applications, the group with the
#' largest exposure is often chosen as reference.
#'
#' @param x A factor (unordered). Character vectors should be converted to
#'   factor before use.
#' @param weight A numeric vector of the same length as \code{x},
#'   typically representing exposure or frequency weights.
#'
#' @author Martin Haringa
#'
#' @references Kaas, Rob & Goovaerts, Marc & Dhaene, Jan & Denuit, Michel.
#' (2008). Modern Actuarial Risk Theory: Using R.
#' \doi{doi:10.1007/978-3-540-70998-5.}
#'
#' @importFrom stats relevel
#'
#' @return A factor of the same length as \code{x}, with the reference level set
#'   to the group with the largest weight.
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
  if (!is.factor(x)) stop("`x` must be a factor")
  if (!is.numeric(weight)) stop("`weight` must be numeric")
  if (length(x) != length(weight)) {
    stop("`x` and `weight` must have the same length")
  }

  counts <- sort(tapply(weight, x, FUN = sum), decreasing = TRUE)
  xrelevel <- stats::relevel(x, ref = names(counts)[1])
  attr(xrelevel, "xoriginal") <- levels(x)
  xrelevel
}


#' Convert p-values into significance stars
#'
#' @param pval Numeric vector of p-values.
#'
#' @return Character vector of the same length as `pval`,
#'   containing `"***"`, `"**"`, `"*"`, `"."`, or `""`.
#'
#' @keywords internal
make_stars <- function(pval) {
  if (!is.numeric(pval)) stop("`pval` must be numeric")

  stars <- cut(
    pval,
    breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
    labels = c("***", "**", "*", ".", ""),
    right = TRUE
  )

  stars <- as.character(stars)

  stars[is.na(pval)] <- ""
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

  as.numeric(unlist(splits_vector))
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
#' @keywords internal
split_x_fn <- function(data, x, left = NULL, right = NULL) {

  vec <- data[[x]]
  vec_new <- vec

  if (!is.null(right)) {
    vec_new[vec_new > right] <- right
  }

  if (!is.null(left)) {
    vec_new[vec_new < left] <- left
  }

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


#' @importFrom mgcv predict.gam
#' @keywords internal
confint_gam <- function(model, newdata, level = 0.95) {

  # predict on link scale
  pred <- mgcv::predict.gam(model, newdata = newdata, type = "link",
                            se.fit = TRUE)

  # critical value
  alpha <- 1 - level
  z <- qnorm(1 - alpha/2)

  # intervals on link scale
  lwr_link <- pred$fit - z * pred$se.fit
  upr_link <- pred$fit + z * pred$se.fit

  # inverse link function (family-specific)
  linkinv <- model$family$linkinv

  # transform to response scale
  data.frame(
    x = newdata[,1, drop=FALSE],
    predicted = linkinv(pred$fit),
    lwr_95 = linkinv(lwr_link),
    upr_95 = linkinv(upr_link)
  )
}

#' @keywords internal
exposure_by_factor <- function(var1, model_data, exposure) {
  x <- model_data[!is.na(model_data[[var1]]), ]
  sums <- tapply(x[[exposure]], x[[var1]], sum, na.rm = TRUE)
  df <- data.frame(
    level = names(sums),
    exposure = as.numeric(sums),
    stringsAsFactors = FALSE
  )
  df$risk_factor <- var1
  df
}
