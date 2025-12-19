#' Get offset from model object
#'
#' @noRd
#'
#' @param model Must be of class glm
#'
#' @return Character string with offset term
#'
#' @keywords internal
get_offset <- function(model) {

  if (!inherits(model, "lm")) {
    stop("Input must be of class (g)lm", call. = FALSE)
  }

  nm1 <- names(attributes(model$terms)$dataClasses)
  n_offsets <- sum(lengths(regmatches(nm1, gregexpr("offset", nm1))))

  if (n_offsets > 1) {
    stop("Length of offset-terms must be equal to 1", call. = FALSE)
  }

  if ("(offset)" %in% nm1) {
    deparse(as.list(model$call)$offset)
  } else {
    out <- sub("offset\\((.*)\\)$", "\\1", grep("offset", nm1, value = TRUE))
    if (identical(out, character(0))) {
      out <- NULL
    }
    out
  }
}


#' Remove offset term from formula
#'
#' @noRd
#'
#' @param formula Formula of class formula
#'
#' @keywords internal
remove_offset_formula <- function(formula) {

  if (!inherits(formula, "formula")) {
    stop("Input must be of class formula", call. = FALSE)
  }

  proc <- function(x) {
    if (length(x) == 1) return(x)
    if (x[[1]] == as.name("offset")) return(x[[1]])
    replace(x, -1, lapply(x[-1], proc))
  }

  update(proc(formula), . ~ . - offset)
}


#' Remove risk factor from formula
#'
#' @noRd
#'
#' @param fm Formula of class formula
#' @param remove_term Risk factor to remove
#'
#' @keywords internal
update_formula_remove <- function(fm, remove_term) {

  if (!inherits(fm, "formula")) {
    stop("Input must be of class formula.", call. = FALSE)
  }

  if (!is.character(remove_term)) {
    stop("Column must be a character.", call. = FALSE)
  }

  fm_new <- update(fm, paste("~ . -", remove_term))

  if (identical(fm, fm_new)) {
    warning("Column '", remove_term, "' must be in model call.\n")
  }

  fm_new
}

#' Create new offset-term and new formula
#'
#' @param offset_term String obtained from get_offset()
#' @param fm_no_offset Obtained from remove_offset_formula()
#' @param add_term Name of restricted risk factor to add
#'
#' @keywords internal
update_formula_add <- function(offset_term, fm_no_offset, add_term) {

  if (!inherits(fm_no_offset, "formula")) {
    stop("Input must be of class formula", call. = FALSE)
  }

  if (is.null(offset_term)) {
    new_offset <- paste0("log(", add_term, ")")
  }

  if (!is.null(offset_term)) {
    new_offset <- paste0("log(", add_term, ")", " + ", offset_term)
  }

  new_offset_term <- paste0("offset(", new_offset, ")")
  new_fm <- update(fm_no_offset, paste("~ . +", new_offset_term))
  list(new_fm, new_offset)
}

#' @keywords internal
cut_borders_df <- function(df, col) {

  if (!col %in% names(df)) stop("Column name must be available in data",
                                call. = FALSE)
  df_vec <- df[[col]]

  if (anyNA(df_vec)) {
    message("NAs detected in column ", col)
  }

  suppressWarnings({
    s1 <- substr(df_vec, start = 1, stop = 1)
    nchr <- if (is.factor(df_vec)) {
      nchar(levels(df_vec)[df_vec])
    } else {
      nchar(df_vec)
    }
    e1 <- substr(df_vec, start = nchr, stop = nchr)
    m <- substr(df_vec, start = 2, stop = nchr - 1)
    df$start_oc <- ifelse(s1 == "(", "open", ifelse(s1 == "[", "closed", NA))
    df$end_oc <- ifelse(e1 == ")", "open", ifelse(e1 == "]", "closed", NA))
    df$start_  <- as.numeric(gsub("^(.*?),.*", "\\1", m))
    df$end_ <- as.numeric(sub(".*,", "", m))
  })
  df$avg_ <- rowMeans(df[, c("start_", "end_")], na.rm = TRUE)
  return(df)
}

#' @keywords internal
cut_borders_model <- function(model, x_cut) {

  if (inherits(model, "glm")) {
    rf <- rating_table(model, signif_stars = FALSE)$df
  }

  if (inherits(model, "smooth")) {
    rf <- rating_table(model$model_out, signif_stars = FALSE)$df
  }

  if (inherits(model, "restricted")) {
    rf <- rating_table(model$model_out, signif_stars = FALSE)$df
  }

  colnames(rf)[3] <- c("estimate")
  rf_xcut <- rf[rf$risk_factor == x_cut, ]
  cut_borders_df(rf_xcut, "level")
}

#' @noRd
#'
#' @importFrom scam scam
#' @importFrom stats lm
#'
#' @keywords internal
fit_polynomial <- function(borders_model, x_org, degree = NULL, breaks = NULL,
                           smoothing = "spline", k = NULL, weights) {

  if (is.null(breaks)) {
    breaks <- seq(min(borders_model$start_), max(borders_model$end_),
                  length.out = nrow(borders_model))
  }

  # Take halfway points of breaks to fit polynomial
  breaks_min <- breaks[-length(breaks)]
  breaks_max <- breaks[-1]
  breaks_mid <- (breaks_min + breaks_max) / 2

  levels_borders <- levels(cut(breaks_min, breaks = unique(breaks),
                               include.lowest = TRUE, dig.lab = 9))

  # Checks
  valid_methods <- c("spline", "mpi", "mpd", "gam",
                     "cx","cv","micx","micv","mdcx","mdcv")
  if (!smoothing %in% valid_methods) {
    stop("Invalid smoothing: must be one of ",
         paste(valid_methods, collapse = ", "), call. = FALSE)
  }

  # Model fitten
  if (smoothing == "spline") {
    lm_poly <- lm(estimate ~ poly(avg_, degree = degree), data = borders_model)
  } else if (smoothing == "gam") {
    if (is.null(k)) {
      lm_poly <- mgcv::gam(estimate ~ s(avg_),
                           weights = weights,
                           data = borders_model)
    } else {
      lm_poly <- mgcv::gam(estimate ~ s(avg_, k = k),
                           weights = weights,
                           data = borders_model)
    }
  } else {
    if (is.null(k)) {
      lm_poly <- scam::scam(estimate ~ s(avg_, bs = smoothing),
                            weights = weights,
                            data = borders_model)
    } else {
      lm_poly <- scam::scam(estimate ~ s(avg_, k = k, bs = smoothing),
                            weights = weights,
                            data = borders_model)
    }
  }

  new_poly_df <- data.frame(avg_ = breaks_mid)
  poly_line <- data.frame(avg_ = breaks)
  poly_line$yhat <- as.numeric(predict(lm_poly, poly_line))

  new_poly_df$yhat <- as.numeric(predict(lm_poly, new_poly_df))
  new_poly_df$breaks_min <- breaks_min
  new_poly_df$breaks_max <- breaks_max
  new_poly_df$cuts <- levels_borders
  new_poly_df$risk_factor <- paste0(x_org, "_smooth")
  colnames(new_poly_df)[1] <- x_org
  new_colname_cat <- paste0(x_org, "_smooth")
  colnames(new_poly_df)[5] <- new_colname_cat
  new_poly_df <- cut_borders_df(new_poly_df, new_colname_cat)
  colnames(poly_line)[1] <- x_org

  new_rf <- new_poly_df[, c("risk_factor", new_colname_cat, "yhat")]
  colnames(new_rf)[2] <- "level"

  list(new_poly_df = new_poly_df, poly_line = poly_line, new_rf = new_rf)
}



#' Join new data to nearest data point
#'
#' @noRd
#'
#' @param dat data.frame with model data
#' @param reference data.frame with polynomial fit
#' @param x character string to join on
#'
#' @importFrom data.table data.table
#' @importFrom data.table setnames
#' @importFrom data.table setkeyv
#' @importFrom data.table foverlaps
#' @importFrom data.table setcolorder
#'
#' @keywords internal
join_to_nearest <- function(dat, reference, x) {
  ref <- data.table::data.table(reference)
  dat <- data.table::data.table(dat)

  # due to NSE notes in R CMD check
  end_oc <- lookup_start <- lookup_start2 <- start_oc <- idkey <- NULL

  ref[start_oc == "open", breaks_min := breaks_min + 1e-6][
    end_oc == "open", breaks_max := breaks_max - 1e-6][
      , c(x) := NULL]
  data.table::setnames(dat, x, "lookup_start")
  dat[, lookup_start2 := lookup_start]
  data.table::setkeyv(ref, c("breaks_min", "breaks_max"))
  data.table::setkeyv(dat, c("lookup_start", "lookup_start2"))

  if (anyNA(dat[["lookup_start"]])) {
    message("NAs detected in column ", x)
    dat[, idkey := seq_along(lookup_start)]
    dat2 <- dat[!is.na(lookup_start2)]
    fov <- data.table::foverlaps(dat2, ref, type = "within",
                                 mult = "all",
                                 nomatch = 0L)
    keycols <- names(dat)
    data.table::setkeyv(fov, keycols)
    data.table::setkeyv(dat, keycols)
    fov <- fov[dat][, idkey := NULL]
  }

  if (!anyNA(dat[["lookup_start"]])) {
    fov <- data.table::foverlaps(dat, ref, type = "within",
                                 mult = "all",
                                 nomatch = 0L)
  }

  fov[start_oc == "open", breaks_min := breaks_min - 1e-6][
    end_oc == "open", breaks_max := breaks_max + 1e-6][
      , lookup_start2 := NULL]
  data.table::setnames(fov, "lookup_start", x)
  data.table::setcolorder(fov, x)
  as.data.frame(fov)
}



#' Join restricted data to model data
#'
#' @noRd
#'
#' @param model_data data.frame with original model data
#' @param restrictions_df data.frame with two columns, column 1 must include
#'   the levels of the risk factor, and column 2 must include the new restricted
#'   coefficients.
#'
#' @importFrom dplyr left_join
#'
#' @keywords internal
add_restrictions_df <- function(model_data, restrictions_df) {

  rcol1 <- names(restrictions_df)[1]
  rcol2 <- names(restrictions_df)[2]

  if (!rcol1 %in% names(model_data)) {
    stop("Can't find column '", rcol1, "' in model call.", call. = FALSE)
  }

  if (ncol(restrictions_df) != 2) {
    stop("Number of columns must be equal to 2.", call. = FALSE)
  }

  if (length(unique(restrictions_df[[1]])) != nrow(restrictions_df)) {
    stop(rcol1, " in restricted data must have unique values.", call. = FALSE)
  }

  if (rcol2 %in% names(model_data)) {
    stop("Column '", rcol2,
         "' in restricted data must be different from existing columns.",
         call. = FALSE)
  }

  # bestaande (unieke) levels/waarden in de modeldata
  existing_levels  <- unique(model_data[[rcol1]])

  # levels/waarden in de restrictie-tabel
  restricted_levels <- unique(restrictions_df[[1]])

  # levels in data zonder restrictie
  levels_not_restricted <- setdiff(existing_levels, restricted_levels)

  # levels in restrictiedata die niet in de modeldata voorkomen
  restricted_not_present <- setdiff(restricted_levels, existing_levels)

  if (length(levels_not_restricted) > 0) {
    warning(
      "Levels in '", rcol1,
      "' in model data without restriction: ",
      paste(levels_not_restricted, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(restricted_not_present) > 0) {
    warning(
      "Levels in restriction data for '", rcol1,
      "' not present in model data: ",
      paste(restricted_not_present, collapse = ", "),
      call. = FALSE
    )
  }


  if (is.factor(model_data[[rcol1]])) {
    restrictions_df[[rcol1]] <- as.factor(restrictions_df[[rcol1]])
  }

  model_df_restrictions <- dplyr::left_join(model_data, restrictions_df,
                                            by = rcol1)

  if (anyNA(model_df_restrictions[[rcol2]])) {
    warning("Can't match all existing factor levels to new levels.\n")
  }

  model_df_restrictions
}


#' Create one data.frame from multiple restriction data.frames
#'
#' @noRd
#'
#' @param restricted_df data.frame with restrictions
#'
#' @keywords internal
restrict_df <- function(restricted_df) {
  restricted_df$risk_factor <- colnames(restricted_df)[2]
  colnames(restricted_df)[2] <- "yhat"
  colnames(restricted_df)[1] <- "level"
  restricted_df$level <- as.character(restricted_df$level)
  restricted_df
}


#' Default extrapolation break size based on existing tariff breaks
#'
#' Uses the median width of existing break intervals as a robust scale-aware
#' default for extrapolation discretisation.
#'
#' @param borders_model A data.frame with columns `breaks_min` and `breaks_max`.
#' @param factor Numeric scalar > 0. Multiplier applied to the median break width.
#'
#' @return Numeric scalar (> 0).
#'
#' @keywords internal
default_extrapolation_break_size <- function(borders_model, factor = 1) {
  if (!is.numeric(factor) || length(factor) != 1 || factor <= 0) {
    stop("'factor' must be a single positive numeric value.", call. = FALSE)
  }

  if (!all(c("breaks_min", "breaks_max") %in% names(borders_model))) {
    stop("borders_model must contain 'breaks_min' and 'breaks_max'.", call. = FALSE)
  }

  w <- borders_model$breaks_max - borders_model$breaks_min
  w <- w[is.finite(w) & w > 0]

  if (length(w) == 0) return(1)  # safe fallback

  as.numeric(stats::median(w) * factor)
}


#' Modify an existing smoothing curve by linear adjustments and extrapolation
#'
#' @description
#' Internal helper used by [`update_smoothing()`] to locally modify an already
#' fitted smoothing curve.
#'
#' The function operates directly on the discretised representation of a
#' smoothing curve (break intervals and fitted values) and allows:
#' \itemize{
#'   \item linear reshaping of the curve between two x-values (`x1`, `x2`);
#'   \item optional overriding of the curve at the interval boundaries;
#'   \item insertion of intermediate knot points through which the curve must pass;
#'   \item optional extrapolation beyond the original range using synthetic
#'         break intervals.
#' }
#'
#' This function does not refit any statistical model; it purely transforms the
#' smoothed curve deterministically and returns updated break-level
#' representations.
#'
#' @param borders_model A data.frame representing the discretised smoothing
#'   curve, typically `model$new` from a `"smooth"` object. Must contain at least
#'   the columns `breaks_min`, `breaks_max`, `avg_` and `yhat`.
#' @param x_org Character. Name of the original continuous risk factor (without
#'   the `"_smooth"` suffix).
#' @param x1,x2 Numeric. Start and end of the interval over which the smoothing
#'   curve should be modified. Must satisfy `x1 < x2`.
#' @param overwrite_y1,overwrite_y2 Optional numeric. If supplied, these values
#'   replace the smoothed values at `x1` and `x2`, respectively. If `NULL`, the
#'   existing smoothed values are used.
#' @param middle_x,middle_y Optional numeric vectors of equal length specifying
#'   intermediate knot points through which the modified curve must pass.
#' @param allow_extrapolation Logical. If `TRUE`, allows the smoothing curve to
#'   be extrapolated beyond its original range.
#' @param extrapolation_break_size Numeric scalar (> 0). Width of synthetic
#'   break intervals created when extrapolating. Must be a single positive
#'   value.
#'
#' @return
#' A list with the following components:
#' \describe{
#'   \item{new_poly_df}{Data frame with updated discretised smoothing curve,
#'   including break intervals and fitted values.}
#'   \item{poly_line}{Data frame suitable for plotting the updated curve.}
#'   \item{new_rf}{Data frame of updated smoothing coefficients in
#'   `rating_factors` format.}
#' }
#'
#' @keywords internal
#' @noRd
change_xy <- function(borders_model, x_org,
                      x1, x2,
                      overwrite_y1 = NULL, overwrite_y2 = NULL,
                      middle_x = c(), middle_y = c(),
                      allow_extrapolation,
                      extrapolation_break_size = NULL) {

  if (!is.numeric(extrapolation_break_size) ||
      length(extrapolation_break_size) != 1 ||
      !is.finite(extrapolation_break_size) ||
      extrapolation_break_size <= 0) {
    stop("'extrapolation_break_size' must be a single positive numeric value.",
         call. = FALSE)
  }

  breaks_min <- borders_model$breaks_min
  breaks_max <- borders_model$breaks_max
  breaks_mid <- borders_model$avg_
  breaks_mid <- breaks_mid[!is.na(breaks_mid)]
  yhat <- borders_model$yhat

  if (x1 >= x2) {
    stop("'x1' must be smaller than 'x2'.", call. = FALSE)
  }

  if (length(middle_x) != length(middle_y)) {
    stop("Lengths of 'middle_x' and 'middle_y' must be equal.", call. = FALSE)
  }

  if (x1 < min(breaks_min) && !allow_extrapolation) {
    stop(
      sprintf(
        "x1 (%s) is below minimum break (%s); set allow_extrapolation = TRUE if you want to extrapolate.",
        x1, min(breaks_min)
      ),
      call. = FALSE
    )
  }

  if (x2 > max(breaks_max) && !allow_extrapolation) {
    stop(
      sprintf(
        "x2 (%s) is above maximum break (%s); set allow_extrapolation = TRUE if you want to extrapolate.",
        x2, max(breaks_max)
      ),
      call. = FALSE
    )
  }

  ab_line <- function(x, x1, y1, x2, y2) {
    (y2 - y1) / (x2 - x1) * (x - x1) + y1
  }

  ## Linker staart extrapoleren
  if (allow_extrapolation && x1 < min(breaks_min)) {
    extra_breaks_min <- rev(seq(
      from = min(breaks_min) - extrapolation_break_size,
      to   = x1 - extrapolation_break_size,
      by   = -extrapolation_break_size
    ))

    if (length(extra_breaks_min) > 0 && min(extra_breaks_min) == x1 - extrapolation_break_size) {
      extra_breaks_min <- extra_breaks_min[-1]
    }

    extra_breaks_max <- extra_breaks_min + extrapolation_break_size
    extra_breaks_mid <- extra_breaks_min + extrapolation_break_size / 2

    # index die we gebruiken om de lijn te bepalen
    idx_max <- which(breaks_max >= x2)
    if (length(idx_max) == 0) {
      idx_max <- length(breaks_mid)
    } else {
      idx_max <- min(idx_max)
    }
    extrapolate_max_index <- max(min(idx_max, length(breaks_mid)), 2)

    extra_yhat <- vapply(
      extra_breaks_mid,
      ab_line,
      numeric(1),
      x1 = breaks_mid[1], y1 = yhat[1],
      x2 = breaks_mid[extrapolate_max_index], y2 = yhat[extrapolate_max_index]
    )

    breaks_min <- c(extra_breaks_min, breaks_min)
    breaks_max <- c(extra_breaks_max, breaks_max)
    breaks_mid <- c(extra_breaks_mid, breaks_mid)
    yhat       <- c(extra_yhat, yhat)
  }

  ## Rechter staart extrapoleren
  if (allow_extrapolation && x2 > max(breaks_max)) {
    extra_breaks_min <- seq(from = max(breaks_max), to = x2, by = extrapolation_break_size)
    if (length(extra_breaks_min) > 0 && max(extra_breaks_min) == x2) {
      extra_breaks_min <- extra_breaks_min[-length(extra_breaks_min)]
    }
    extra_breaks_max <- extra_breaks_min + extrapolation_break_size
    extra_breaks_mid <- extra_breaks_min + extrapolation_break_size / 2

    idx_min <- which(breaks_max >= x1)
    if (length(idx_min) == 0) {
      idx_min <- length(breaks_mid) - 1
    } else {
      idx_min <- min(idx_min)
    }
    extrapolate_min_index <- min(max(idx_min, 1), length(breaks_mid) - 1)

    extra_yhat <- vapply(
      extra_breaks_mid,
      ab_line,
      numeric(1),
      x1 = breaks_mid[extrapolate_min_index],     y1 = yhat[extrapolate_min_index],
      x2 = breaks_mid[length(breaks_mid)],        y2 = yhat[length(yhat)]
    )

    breaks_min <- c(breaks_min, extra_breaks_min)
    breaks_max <- c(breaks_max, extra_breaks_max)
    breaks_mid <- c(breaks_mid, extra_breaks_mid)
    yhat       <- c(yhat, extra_yhat)
  }

  unique_borders  <- unique(c(breaks_min, breaks_max))
  levels_borders  <- levels(cut(breaks_min, breaks = unique_borders,
                                include.lowest = TRUE, dig.lab = 9))

  min_index <- which(breaks_max >= x1)[1]
  max_index <- which(breaks_max >= x2)[1]

  if (is.na(min_index) || is.na(max_index)) {
    stop("x1/x2 not within the breaks after extrapolation.", call. = FALSE)
  }

  if (is.null(overwrite_y1)) {
    y1 <- yhat[min_index]
  } else {
    y1 <- overwrite_y1
    yhat[min_index] <- overwrite_y1
  }

  if (is.null(overwrite_y2)) {
    y2 <- yhat[max_index]
  } else {
    y2 <- overwrite_y2
    yhat[max_index] <- overwrite_y2
  }

  x_coord <- c(breaks_mid[min_index], middle_x, breaks_mid[max_index])
  y_coord <- c(y1,                 middle_y, y2)
  indices <- c(min_index, rep(NA_integer_, length(middle_x)), max_index)

  # bepaal in welke interval-index elke middle_x valt
  if (length(middle_x) > 0) {
    for (i in seq_along(middle_x)) {
      indices[i + 1] <- max(which(breaks_mid <= middle_x[i]))
    }
  }

  # nu alle segmenten lineair invullen
  for (i in seq_len(length(x_coord) - 1)) {
    from_idx <- indices[i] + 1
    to_idx   <- indices[i + 1]
    if (from_idx <= to_idx) {
      for (j in from_idx:to_idx) {
        yhat[j] <- ab_line(
          x  = breaks_mid[j],
          x1 = x_coord[i],     y1 = y_coord[i],
          x2 = x_coord[i + 1], y2 = y_coord[i + 1]
        )
      }
    }
  }

  new_poly_df <- data.frame(avg_ = breaks_mid)
  poly_line   <- data.frame(avg_ = breaks_mid, yhat = as.numeric(yhat))

  new_poly_df$yhat       <- as.numeric(yhat)
  new_poly_df$breaks_min <- breaks_min
  new_poly_df$breaks_max <- breaks_max
  new_poly_df$cuts       <- levels_borders
  new_poly_df$risk_factor <- paste0(x_org, "_smooth")
  colnames(new_poly_df)[1] <- x_org

  new_colname_cat <- paste0(x_org, "_smooth")
  colnames(new_poly_df)[5] <- new_colname_cat

  new_poly_df <- cut_borders_df(new_poly_df, new_colname_cat)
  colnames(poly_line)[1] <- x_org

  new_rf <- new_poly_df[, c("risk_factor", new_colname_cat, "yhat")]
  colnames(new_rf)[2] <- "level"

  list(new_poly_df = new_poly_df, poly_line = poly_line, new_rf = new_rf)
}



