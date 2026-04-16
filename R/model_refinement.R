# -----------------------------------------------------------------------------
# refinement_api.R
# -----------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.is_refinement <- function(x) {
  inherits(x, "rating_refinement")
}

.assert_refinement <- function(x) {
  if (!inherits(x, "rating_refinement")) {
    stop("Input must be of class 'rating_refinement'. Start with prepare_refinement().",
         call. = FALSE)
  }
}

.get_model_data <- function(model) {
  if (!inherits(model, "glm")) {
    stop("'model' must be of class glm.", call. = FALSE)
  }

  if (!is.null(model$data)) {
    return(model$data)
  }

  mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (!is.null(mf)) {
    return(as.data.frame(mf))
  }

  stop(
    "Could not retrieve model data from 'model'. Please pass 'data' explicitly to prepare_refinement().",
    call. = FALSE
  )
}

.get_rating_factors_df <- function(model) {
  rfdf <- rating_table(model, signif_stars = FALSE)$df
  colnames(rfdf)[3] <- "estimate"
  as.data.frame(rfdf)
}

.make_refinement <- function(base, steps = list(), legacy = list()) {
  structure(
    list(
      base = base,
      steps = steps,
      legacy = legacy
    ),
    class = "rating_refinement"
  )
}

.next_step_id <- function(ref) {
  paste0("step_", length(ref$steps) + 1)
}

.add_step <- function(ref, step) {
  .assert_refinement(ref)
  ref$steps[[length(ref$steps) + 1]] <- step
  ref
}

.find_step <- function(ref, type = NULL, variable = NULL, step = NULL) {
  .assert_refinement(ref)

  if (!is.null(step)) {
    if (!is.numeric(step) || length(step) != 1 || is.na(step)) {
      stop("'step' must be a single numeric index.", call. = FALSE)
    }
    step <- as.integer(step)
    if (step < 1 || step > length(ref$steps)) {
      stop("'step' is out of bounds.", call. = FALSE)
    }
    return(step)
  }

  idx <- seq_along(ref$steps)

  if (!is.null(type)) {
    idx <- idx[vapply(ref$steps[idx], function(s) identical(s$type, type), logical(1))]
  }

  if (!is.null(variable)) {
    idx <- idx[vapply(ref$steps[idx], function(s) identical(s$variable, variable), logical(1))]
  }

  if (length(idx) == 0) {
    msg <- "No matching refinement step found."
    if (!is.null(type) && !is.null(variable)) {
      msg <- paste0("No ", type, " step found for variable: ", variable)
    }
    stop(msg, call. = FALSE)
  }

  if (length(idx) > 1) {
    warning("Multiple matching refinement steps found; using the first one.", call. = FALSE)
  }

  idx[1]
}

.safe_unique_append <- function(x, values) {
  unique(c(x, values))
}

.replace_formula_term <- function(formula, old_term, new_term, offset_term = NULL) {
  fm_remove <- update_formula_remove(formula, old_term)
  fm_add <- update_formula_add(offset_term, fm_remove, new_term)
  list(
    formula = fm_add[[1]],
    formula_no_offset = fm_remove,
    offset = fm_add[[2]]
  )
}

.refinement_base_from_glm <- function(model, data = NULL) {
  if (!inherits(model, "glm")) {
    stop("'model' must be of class glm.", call. = FALSE)
  }

  data <- data %||% .get_model_data(model)

  list(
    model = model,
    data = data,
    formula = stats::formula(model),
    formula_no_offset = remove_offset_formula(stats::formula(model)),
    offset = get_offset(model),
    model_call = model$call,
    rating_factors = .get_rating_factors_df(model)
  )
}


# -----------------------------------------------------------------------------
# Coercion
# -----------------------------------------------------------------------------

as_refinement <- function(x, ...) {
  UseMethod("as_refinement")
}

#' @export
as_refinement.glm <- function(x, data = NULL, ...) {
  .make_refinement(
    base = .refinement_base_from_glm(x, data = data),
    steps = list(),
    legacy = list(source_class = class(x))
  )
}

#' @export
as_refinement.rating_refinement <- function(x, ...) {
  x
}

#' @export
as_refinement.smooth <- function(x, ...) {
  ref <- as_refinement(x$model_out)

  mgd_smt <- x$mgd_smt
  if (!is.null(mgd_smt) && length(mgd_smt) > 0) {
    for (pair in mgd_smt) {
      if (length(pair) == 2) {
        x_org <- sub("_smooth$", "", pair[1])
        x_cut <- sub("_smooth$", "", pair[2])

        ref$steps[[length(ref$steps) + 1]] <- list(
          id = .next_step_id(ref),
          type = "smoothing",
          variable = x_cut,
          x_cut = x_cut,
          x_org = x_org,
          degree = x$degree %||% NULL,
          breaks = NULL,
          smoothing = x$smoothing %||% "spline",
          k = NULL,
          weights = NULL,
          edit = NULL,
          legacy_import = TRUE
        )
      }
    }
  }

  ref$legacy$source_class <- class(x)
  ref$legacy$legacy_object <- x
  ref
}

#' @export
as_refinement.restricted <- function(x, ...) {
  ref <- as_refinement(x$model_out)

  rst_lst <- x$restrictions_lst
  if (!is.null(rst_lst) && length(rst_lst) > 0) {
    for (nm in names(rst_lst)) {
      obj <- rst_lst[[nm]]
      is_relativities <- is.list(obj) && !is.data.frame(obj)

      if (is_relativities) {
        ref$steps[[length(ref$steps) + 1]] <- list(
          id = .next_step_id(ref),
          type = "relativities",
          variable = x$base_risk_factor %||% nm,
          risk_factor = x$base_risk_factor %||% nm,
          risk_factor_split = x$risk_factor_split %||% NULL,
          relativities = obj,
          exposure = x$exposure %||% NULL,
          normalize = isTRUE(x$normalize),
          legacy_import = TRUE
        )
      } else {
        ref$steps[[length(ref$steps) + 1]] <- list(
          id = .next_step_id(ref),
          type = "restriction",
          variable = names(obj)[1],
          restrictions = obj,
          legacy_import = TRUE
        )
      }
    }
  }

  mgd_smt <- x$mgd_smt
  if (!is.null(mgd_smt) && length(mgd_smt) > 0) {
    for (pair in mgd_smt) {
      if (length(pair) == 2) {
        x_org <- sub("_smooth$", "", pair[1])
        x_cut <- sub("_smooth$", "", pair[2])

        already_present <- any(vapply(
          ref$steps,
          function(s) identical(s$type, "smoothing") && identical(s$x_cut, x_cut),
          logical(1)
        ))

        if (!already_present) {
          ref$steps[[length(ref$steps) + 1]] <- list(
            id = .next_step_id(ref),
            type = "smoothing",
            variable = x_cut,
            x_cut = x_cut,
            x_org = x_org,
            degree = NULL,
            breaks = NULL,
            smoothing = "spline",
            k = NULL,
            weights = NULL,
            edit = NULL,
            legacy_import = TRUE
          )
        }
      }
    }
  }

  ref$legacy$source_class <- class(x)
  ref$legacy$legacy_object <- x
  ref
}


# -----------------------------------------------------------------------------
# Public refinement API
# -----------------------------------------------------------------------------

#' Prepare a model refinement workflow
#'
#' @description
#' Start a refinement workflow for a fitted GLM. Refinement steps such as
#' smoothing, restrictions and expert-based relativities can be added
#' sequentially and are only applied once [refit()] is called.
#'
#' @param model Object of class `glm`.
#' @param data Optional data.frame. If `NULL`, the data are retrieved from the
#'   model object.
#'
#' @return Object of class `rating_refinement`.
#' @export
prepare_refinement <- function(model, data = NULL) {
  as_refinement(model, data = data)
}

#' @export
print.rating_refinement <- function(x, ...) {
  cat("<rating_refinement>\n")
  cat("Base model: ", paste(class(x$base$model), collapse = ", "), "\n", sep = "")
  cat("Steps: ", length(x$steps), "\n", sep = "")

  if (length(x$steps) > 0) {
    for (i in seq_along(x$steps)) {
      s <- x$steps[[i]]
      line <- paste0(i, ". ", s$type)
      if (!is.null(s$variable)) line <- paste0(line, " [", s$variable, "]")
      cat(line, "\n", sep = "")
    }
  }
  invisible(x)
}

#' @export
summary.rating_refinement <- function(object, ...) {
  out <- list(
    base_formula = object$base$formula,
    offset = object$base$offset,
    n_steps = length(object$steps),
    steps = lapply(object$steps, function(s) {
      s[intersect(names(s), c(
        "id", "type", "variable", "x_cut", "x_org", "smoothing",
        "risk_factor", "risk_factor_split", "normalize"
      ))]
    })
  )
  class(out) <- "summary.rating_refinement"
  out
}

#' @export
print.summary.rating_refinement <- function(x, ...) {
  cat("Refinement summary\n\n")
  cat("Base formula:\n")
  print(x$base_formula)
  cat("\nOffset: ", x$offset %||% "none", "\n", sep = "")
  cat("Steps: ", x$n_steps, "\n", sep = "")
  if (x$n_steps > 0) {
    for (i in seq_along(x$steps)) {
      cat(i, ". ", paste(names(x$steps[[i]]), x$steps[[i]], sep = "=", collapse = ", "), "\n", sep = "")
    }
  }
  invisible(x)
}


# -----------------------------------------------------------------------------
# New add_* functions
# -----------------------------------------------------------------------------

#' Add coefficient restrictions to a refinement workflow
#'
#' @description
#' Adds a restriction step to a `rating_refinement` object.
#'
#' @details
#' `add_restriction()` is part of the new refinement workflow and is intended
#' to be used in combination with `prepare_refinement()` and `refit()`.
#'
#' The legacy function `restrict_coef()` remains the only function that should
#' be applied directly to a model object.
#'
#' @param model Object of class `rating_refinement`.
#' @param restrictions data.frame with two columns containing restricted data.
#'
#' @return Object of class `rating_refinement`.
#' @export
add_restriction <- function(model, restrictions) {
  .assert_refinement(model)

  if (!is.data.frame(restrictions) || ncol(restrictions) != 2) {
    stop("'restrictions' must be a data.frame with exactly two columns.", call. = FALSE)
  }

  variable <- names(restrictions)[1]

  if (!variable %in% names(model$base$data)) {
    stop("Restriction variable '", variable, "' is not in refinement data.", call. = FALSE)
  }

  .add_step(model, list(
    id = .next_step_id(model),
    type = "restriction",
    variable = variable,
    restrictions = restrictions
  ))
}

#' @rdname add_restriction
#' @export
restrict_coef <- function(model, restrictions) {
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "restrict_coef()",
    with = "add_restriction()"
  )
  warning(
    "New workflow: prepare_refinement(model) |> add_restriction(...) |> refit()",
    call. = FALSE
  )
  ref <- prepare_refinement(model)
  ref <- add_restriction(ref, restrictions)
  ref
}

#' Add smoothing to a refinement workflow
#'
#' @description
#' Adds a smoothing step to a `rating_refinement` object.
#'
#' @details
#' `add_smoothing()` is part of the new refinement workflow and is intended
#' to be used in combination with `prepare_refinement()` and `refit()`.
#'
#' The legacy function `smooth_coef()` remains the only function that should
#' be applied directly to a model object.
#'
#' @param model Object of class `rating_refinement`.
#' @param x_cut column name with breaks/cut
#' @param x_org column name where x_cut is based on
#' @param degree order of polynomial
#' @param breaks numerical vector with new clusters for x
#' @param smoothing smoothing specification
#' @param k number of basis functions
#' @param weights weights column name, usually exposure
#'
#' @return Object of class `rating_refinement`.
#' @export
add_smoothing <- function(model, x_cut, x_org, degree = NULL, breaks = NULL,
                          smoothing = "spline", k = NULL, weights = NULL) {
  .assert_refinement(model)

  if (is.null(breaks) || !is.numeric(breaks)) {
    stop("'breaks' must be a numerical vector", call. = FALSE)
  }

  if (!x_cut %in% names(model$base$data)) {
    stop("'x_cut' is not present in refinement data.", call. = FALSE)
  }
  if (!x_org %in% names(model$base$data)) {
    stop("'x_org' is not present in refinement data.", call. = FALSE)
  }
  if (!is.null(weights) && !weights %in% names(model$base$data)) {
    stop("'weights' column is not present in refinement data.", call. = FALSE)
  }

  .add_step(model, list(
    id = .next_step_id(model),
    type = "smoothing",
    variable = x_cut,
    x_cut = x_cut,
    x_org = x_org,
    degree = degree,
    breaks = breaks,
    smoothing = smoothing,
    k = k,
    weights = weights,
    edit = NULL
  ))
}

#' @rdname add_smoothing
#' @export
smooth_coef <- function(model, x_cut, x_org, degree = NULL, breaks = NULL,
                        smoothing = "spline", k = NULL, weights = NULL) {
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "smooth_coef()",
    with = "add_smoothing()"
  )

  warning(
    "New workflow: prepare_refinement(model) |> add_smoothing(...) |> refit()",
    call. = FALSE
  )

  ref <- prepare_refinement(model)
  add_smoothing(
    ref,
    x_cut = x_cut,
    x_org = x_org,
    degree = degree,
    breaks = breaks,
    smoothing = smoothing,
    k = k,
    weights = weights
  )
}

#' Edit an existing smoothing step in a refinement workflow
#'
#' @description
#' `edit_smoothing()` modifies a previously added smoothing step in a
#' `rating_refinement` object. The actual smoothing is only re-applied when
#' [refit()] is called.
#'
#' @param model Object of class `rating_refinement`, `smooth` or `restricted`.
#' @param variable Character. The `x_cut` variable of the smoothing step to edit.
#' @param step Optional numeric index of the step to edit.
#' @param x1,x2 Numeric. Start and end of the interval over which the smoothing
#'   should be modified.
#' @param overwrite_y1,overwrite_y2 Optional numeric. Overrides for the smoothed
#'   values at `x1` and `x2`.
#' @param knots_x,knots_y Optional numeric vectors of equal length.
#' @param allow_extrapolation Logical.
#' @param extrapolation_break_size Numeric scalar (> 0) or `NULL`.
#'
#' @return Object of class `rating_refinement`.
#' @export
edit_smoothing <- function(model,
                           variable = NULL,
                           step = NULL,
                           x1, x2,
                           overwrite_y1 = NULL, overwrite_y2 = NULL,
                           knots_x = NULL, knots_y = NULL,
                           allow_extrapolation = FALSE,
                           extrapolation_break_size = NULL) {
  if (inherits(model, c("smooth", "restricted"))) {
    lifecycle::deprecate_warn(
      when = "0.9.0",
      what = "edit_smoothing(model = <smooth/restricted>)",
      with = "prepare_refinement() |> add_smoothing() |> edit_smoothing() |> refit()"
    )
    model <- as_refinement(model)
  }

  .assert_refinement(model)

  if (is.null(knots_x)) knots_x <- numeric()
  if (is.null(knots_y)) knots_y <- numeric()

  if (length(knots_x) != length(knots_y)) {
    stop("Lengths of 'knots_x' and 'knots_y' must be equal.", call. = FALSE)
  }

  idx <- .find_step(model, type = "smoothing", variable = variable, step = step)

  model$steps[[idx]]$edit <- utils::modifyList(
    model$steps[[idx]]$edit %||% list(),
    list(
      x1 = x1,
      x2 = x2,
      overwrite_y1 = overwrite_y1,
      overwrite_y2 = overwrite_y2,
      knots_x = knots_x,
      knots_y = knots_y,
      allow_extrapolation = allow_extrapolation,
      extrapolation_break_size = extrapolation_break_size
    )
  )

  model
}

#' @rdname edit_smoothing
#' @importFrom utils tail
#' @export
update_smoothing <- function(model,
                             x1, x2,
                             overwrite_y1 = NULL, overwrite_y2 = NULL,
                             knots_x = NULL, knots_y = NULL,
                             allow_extrapolation = FALSE,
                             extrapolation_break_size = NULL) {
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "update_smoothing()",
    with = "edit_smoothing()"
  )

  ref <- if (.is_refinement(model)) model else as_refinement(model)

  smooth_steps <- which(vapply(
    ref$steps,
    function(s) identical(s$type, "smoothing"),
    logical(1)
  ))

  if (length(smooth_steps) == 0) {
    stop("No smoothing step found. Precede update_smoothing() with smooth_coef() or add_smoothing().",
         call. = FALSE)
  }

  edit_smoothing(
    model = ref,
    step = tail(smooth_steps, 1),
    x1 = x1,
    x2 = x2,
    overwrite_y1 = overwrite_y1,
    overwrite_y2 = overwrite_y2,
    knots_x = knots_x,
    knots_y = knots_y,
    allow_extrapolation = allow_extrapolation,
    extrapolation_break_size = extrapolation_break_size
  )
}

#' Add expert-based relativities to a refinement workflow
#'
#' @description
#' Adds a relativities step to a `rating_refinement` object.
#'
#' @details
#' `add_relativities()` is only available within the new refinement workflow
#' and should be used in combination with `prepare_refinement()` and `refit()`.
#'
#' @param model Object of class `rating_refinement`.
#' @param risk_factor Character string. Name of existing risk factor in the model.
#' @param risk_factor_split Character string. More granular split column.
#' @param relativities Named list of data.frames.
#' @param exposure Character string. Exposure column.
#' @param normalize Logical.
#'
#' @return Object of class `rating_refinement`.
#' @export
add_relativities <- function(model,
                             risk_factor,
                             risk_factor_split,
                             relativities,
                             exposure,
                             normalize = TRUE) {
  .assert_refinement(model)

  if (!risk_factor %in% names(model$base$data)) {
    stop("'risk_factor' column is not present in refinement data.", call. = FALSE)
  }
  if (!risk_factor_split %in% names(model$base$data)) {
    stop("'risk_factor_split' column is not present in refinement data.", call. = FALSE)
  }
  if (!exposure %in% names(model$base$data)) {
    stop("'exposure' column is not present in refinement data.", call. = FALSE)
  }

  .check_relativities_list(relativities)

  .add_step(model, list(
    id = .next_step_id(model),
    type = "relativities",
    variable = risk_factor,
    risk_factor = risk_factor,
    risk_factor_split = risk_factor_split,
    relativities = relativities,
    exposure = exposure,
    normalize = normalize
  ))
}





# -----------------------------------------------------------------------------
# Execution state
# -----------------------------------------------------------------------------

.make_exec_state <- function(ref) {
  list(
    data = ref$base$data,
    formula = ref$base$formula,
    formula_no_offset = ref$base$formula_no_offset,
    offset = ref$base$offset,
    rating_factors = ref$base$rating_factors,
    model_call = ref$base$model_call,
    model_out = ref$base$model,
    restrictions_lst = list(),
    rf_restricted_df = NULL,
    new_rf = NULL,
    new_col_nm = character(),
    old_col_nm = character(),
    mgd_rst = list(),
    mgd_smt = list(),
    borders = NULL,
    new = NULL,
    new_line = NULL,
    degree = NULL,
    smoothing = NULL,
    relativities_df = NULL,
    normalize = NULL,
    exposure = NULL,
    base_risk_factor = NULL,
    risk_factor_split = NULL,
    display_risk_factor = NULL,
    model_risk_factor = NULL
  )
}


# -----------------------------------------------------------------------------
# Apply refinement steps
# -----------------------------------------------------------------------------

.apply_restriction_step <- function(state, step) {
  restrictions <- step$restrictions
  variable <- names(restrictions)[1]
  restricted_df <- restrict_df(restrictions)

  fm_replace <- .replace_formula_term(
    formula = state$formula_no_offset,
    old_term = variable,
    new_term = names(restrictions)[2],
    offset_term = state$offset
  )

  state$formula <- fm_replace$formula
  state$formula_no_offset <- fm_replace$formula_no_offset
  state$offset <- fm_replace$offset
  state$data <- add_restrictions_df(state$data, restrictions)
  state$restrictions_lst[[variable]] <- restrictions

  if (is.null(state$rf_restricted_df)) {
    state$rf_restricted_df <- restricted_df
  } else {
    state$rf_restricted_df <- rbind(state$rf_restricted_df, restricted_df)
  }

  nrst <- setdiff(names(restrictions), unique(state$rating_factors$risk_factor))
  orst <- setdiff(names(restrictions), state$new_col_nm)
  state$mgd_rst <- append(state$mgd_rst, list(unique(c(orst, nrst))))

  state$new_col_nm <- .safe_unique_append(
    state$new_col_nm,
    setdiff(names(restrictions), unique(state$rating_factors$risk_factor))
  )
  state$old_col_nm <- .safe_unique_append(
    state$old_col_nm,
    setdiff(names(restrictions), state$new_col_nm)
  )

  state
}

.apply_smoothing_step <- function(state, step) {
  x_cut <- step$x_cut
  x_org <- step$x_org
  degree <- step$degree
  breaks <- step$breaks
  smoothing <- step$smoothing
  k <- step$k
  weights <- step$weights

  borders_x_cut <- cut_borders_model(state$model_out, x_cut)

  if (is.null(degree)) {
    degree <- nrow(borders_x_cut) - 1
  }

  if (smoothing %in% c("mpi", "mpd", "cx", "cv", "micx", "micv", "mdcx",
                       "mdcv", "gam")) {
    if (is.null(weights)) {
      exposur0 <- rep(1, nrow(borders_x_cut))
    } else {
      exposur0 <- aggregate(
        list(exposure = state$data[[weights]]),
        by = list(x = state$data[[x_cut]]),
        FUN = sum,
        na.rm = TRUE,
        na.action = NULL
      )[, 2]
    }
  } else {
    exposur0 <- NULL
  }

  fit_poly <- fit_polynomial(
    borders_model = borders_x_cut,
    x_org = x_org,
    degree = degree,
    breaks = breaks,
    smoothing = smoothing,
    k = k,
    weights = exposur0
  )

  df_poly <- fit_poly[["new_poly_df"]]
  df_poly_line <- fit_poly[["poly_line"]]
  df_new_rf <- fit_poly[["new_rf"]]

  if (!is.null(step$edit)) {
    edit <- step$edit

    ebreak <- edit$extrapolation_break_size
    if (is.null(ebreak)) {
      ebreak <- default_extrapolation_break_size(df_poly, factor = 1)
    }

    changed <- change_xy(
      borders_model = df_poly,
      x_org = x_org,
      x1 = edit$x1,
      x2 = edit$x2,
      overwrite_y1 = edit$overwrite_y1,
      overwrite_y2 = edit$overwrite_y2,
      middle_x = edit$knots_x %||% numeric(),
      middle_y = edit$knots_y %||% numeric(),
      allow_extrapolation = isTRUE(edit$allow_extrapolation),
      extrapolation_break_size = ebreak
    )

    df_poly <- changed[["new_poly_df"]]
    df_poly_line <- changed[["poly_line"]]
    df_new_rf <- changed[["new_rf"]]
  }

  state$mgd_smt <- append(
    state$mgd_smt,
    list(c(paste0(x_org, "_smooth"), paste0(x_cut, "_smooth")))
  )

  state$old_col_nm <- .safe_unique_append(state$old_col_nm, paste0(x_org, "_smooth"))
  state$new_col_nm <- .safe_unique_append(state$new_col_nm, paste0(x_cut, "_smooth"))

  fm_replace <- .replace_formula_term(
    formula = state$formula_no_offset,
    old_term = x_cut,
    new_term = paste0(x_cut, "_smooth"),
    offset_term = state$offset
  )

  state$formula <- fm_replace$formula
  state$formula_no_offset <- fm_replace$formula_no_offset
  state$offset <- fm_replace$offset

  state$data <- join_to_nearest(state$data, df_poly, x_org)
  names(state$data)[names(state$data) == "yhat"] <- paste0(x_cut, "_smooth")

  if (is.null(state$new_rf)) {
    state$new_rf <- df_new_rf
  } else {
    keep <- state$new_rf$risk_factor != paste0(x_org, "_smooth")
    state$new_rf <- rbind(state$new_rf[keep, , drop = FALSE], df_new_rf)
  }

  state$borders <- borders_x_cut
  state$new <- df_poly
  state$new_line <- df_poly_line
  state$degree <- degree
  state$smoothing <- smoothing

  state
}

.apply_relativities_step <- function(state, step) {
  risk_factor <- step$risk_factor
  risk_factor_split <- step$risk_factor_split
  relativities <- step$relativities
  exposure <- step$exposure
  normalize <- isTRUE(step$normalize)

  df_new <- state$data
  rfdf <- state$rating_factors

  if (!risk_factor %in% names(df_new)) {
    stop("risk_factor column: ", risk_factor, " is not in the model data.", call. = FALSE)
  }
  if (!risk_factor_split %in% names(df_new)) {
    stop("risk_factor_split column: ", risk_factor_split, " is not in the model data.", call. = FALSE)
  }
  if (!exposure %in% names(df_new)) {
    stop("exposure column: ", exposure, " is not in the model data.", call. = FALSE)
  }
  if (!risk_factor %in% unique(rfdf$risk_factor)) {
    stop("'", risk_factor, "' is not present as a risk factor in the model.", call. = FALSE)
  }

  rel_levels <- names(relativities)
  model_levels <- rfdf$level[rfdf$risk_factor == risk_factor]
  missing_levels <- setdiff(rel_levels, model_levels)
  if (length(missing_levels) > 0) {
    stop(
      "The following levels in 'relativities' are not present in risk_factor '",
      risk_factor, "': ", paste(missing_levels, collapse = ", "),
      call. = FALSE
    )
  }

  rel_df <- .build_relativities_df(relativities)

  exposure_df <- stats::aggregate(
    df_new[[exposure]],
    by = list(
      level = df_new[[risk_factor]],
      new_level = df_new[[risk_factor_split]]
    ),
    FUN = sum,
    na.rm = TRUE
  )
  names(exposure_df)[3] <- "exposure"

  rel_df <- merge(
    rel_df,
    exposure_df,
    by = c("level", "new_level"),
    all.x = TRUE,
    sort = FALSE
  )

  if (any(is.na(rel_df$exposure))) {
    miss <- unique(rel_df$new_level[is.na(rel_df$exposure)])
    stop(
      "No matching exposure found in model data for the following new levels in '",
      risk_factor_split, "': ", paste(miss, collapse = ", "),
      call. = FALSE
    )
  }

  if (normalize) {
    rel_df <- .normalize_relativities(rel_df)
  } else {
    rel_df$relativity_final <- rel_df$relativity
  }

  base_df <- rfdf[rfdf$risk_factor == risk_factor, c("level", "estimate")]
  names(base_df)[2] <- "estimate_base"

  rel_df <- merge(
    rel_df,
    base_df,
    by = "level",
    all.x = TRUE,
    sort = FALSE
  )

  rel_df$estimate <- rel_df$estimate_base * rel_df$relativity_final

  new_rf_name <- paste0(risk_factor, "_rel")
  display_rf_name <- risk_factor_split

  map_unsplit <- rfdf[rfdf$risk_factor == risk_factor, c("level", "estimate")]
  names(map_unsplit) <- c(risk_factor, "estimate_base")

  map_split <- rel_df[, c("level", "new_level", "estimate")]
  names(map_split)[names(map_split) == "new_level"] <- risk_factor_split

  df_restricted <- df_new
  df_restricted$row_id__tmp <- seq_len(nrow(df_restricted))

  df_restricted <- merge(
    df_restricted,
    map_unsplit,
    by = risk_factor,
    all.x = TRUE,
    sort = FALSE
  )

  df_restricted <- merge(
    df_restricted,
    map_split,
    by.x = c(risk_factor, risk_factor_split),
    by.y = c("level", risk_factor_split),
    all.x = TRUE,
    sort = FALSE
  )

  df_restricted[[new_rf_name]] <- ifelse(
    !is.na(df_restricted$estimate),
    df_restricted$estimate,
    df_restricted$estimate_base
  )

  df_restricted <- df_restricted[order(df_restricted$row_id__tmp), ]
  rownames(df_restricted) <- NULL
  df_restricted$estimate <- NULL
  df_restricted$estimate_base <- NULL
  df_restricted$row_id__tmp <- NULL

  unsplit_levels <- setdiff(
    unique(as.character(df_restricted[[risk_factor]])),
    names(relativities)
  )

  if (length(unsplit_levels) > 0) {
    unsplit_df <- data.frame(
      level = unsplit_levels,
      yhat = map_unsplit$estimate_base[
        match(unsplit_levels, map_unsplit[[risk_factor]])
      ],
      risk_factor = rep(display_rf_name, length(unsplit_levels)),
      stringsAsFactors = FALSE
    )
  } else {
    unsplit_df <- data.frame(
      level = character(0),
      yhat = numeric(0),
      risk_factor = character(0),
      stringsAsFactors = FALSE
    )
  }

  split_df_display <- rel_df[, c("new_level", "estimate")]
  names(split_df_display) <- c("level", "yhat")
  split_df_display$risk_factor <- rep(display_rf_name, nrow(split_df_display))
  split_df_display$level <- as.character(split_df_display$level)

  restricted_df_new <- rbind(
    unsplit_df[, c("level", "yhat", "risk_factor")],
    split_df_display[, c("level", "yhat", "risk_factor")]
  )

  restricted_df_new <- unique(restricted_df_new)
  rownames(restricted_df_new) <- NULL

  if (is.null(state$rf_restricted_df)) {
    state$rf_restricted_df <- restricted_df_new
  } else {
    state$rf_restricted_df <- unique(rbind(state$rf_restricted_df, restricted_df_new))
  }
  rownames(state$rf_restricted_df) <- NULL

  fm_replace <- .replace_formula_term(
    formula = state$formula_no_offset,
    old_term = risk_factor,
    new_term = new_rf_name,
    offset_term = state$offset
  )

  state$formula <- fm_replace$formula
  state$formula_no_offset <- fm_replace$formula_no_offset
  state$offset <- fm_replace$offset
  state$data <- df_restricted

  state$restrictions_lst[[new_rf_name]] <- relativities
  state$mgd_rst <- append(state$mgd_rst, list(c(risk_factor, new_rf_name)))
  state$new_col_nm <- .safe_unique_append(state$new_col_nm, c(new_rf_name, display_rf_name))
  state$old_col_nm <- .safe_unique_append(state$old_col_nm, risk_factor)

  state$relativities_df <- rel_df
  state$normalize <- normalize
  state$exposure <- exposure
  state$base_risk_factor <- risk_factor
  state$risk_factor_split <- risk_factor_split
  state$display_risk_factor <- display_rf_name
  state$model_risk_factor <- new_rf_name

  state
}

.apply_refinement_step <- function(state, step) {
  switch(
    step$type,
    restriction = .apply_restriction_step(state, step),
    smoothing = .apply_smoothing_step(state, step),
    relativities = .apply_relativities_step(state, step),
    stop("Unknown refinement step type: ", step$type, call. = FALSE)
  )
}


# -----------------------------------------------------------------------------
# Plot bridge
# -----------------------------------------------------------------------------

.preview_to_legacy_object <- function(state, last_step_type = NULL) {

  has_restriction <- length(state$restrictions_lst) > 0 ||
    !is.null(state$rf_restricted_df) ||
    !is.null(state$relativities_df)

  has_smoothing <- !is.null(state$new_rf) && nrow(state$new_rf) > 0

  # ---------------------------------------------------------------------------
  # Only smoothing -> return smooth object
  # ---------------------------------------------------------------------------
  if (!has_restriction && has_smoothing) {
    st <- list(
      formula_restricted = state$formula,
      formula_removed = state$formula_no_offset,
      data_restricted = state$data,
      fm_no_offset = state$formula_no_offset,
      offset = state$offset,
      borders = state$borders,
      new = state$new,
      new_line = state$new_line,
      model_call = state$model_call,
      rating_factors = as.data.frame(state$rating_factors),
      restrictions_lst = state$restrictions_lst,
      new_rf = state$new_rf,
      degree = state$degree,
      model_out = state$model_out,
      new_col_nm = state$new_col_nm,
      old_col_nm = state$old_col_nm,
      mgd_rst = state$mgd_rst,
      mgd_smt = state$mgd_smt,
      smoothing = state$smoothing
    )
    attr(st, "class") <- "smooth"
    attr(st, "has_smoothing") <- TRUE
    attr(st, "last_smoothing_step") <- "add_smoothing"
    return(st)
  }

  # ---------------------------------------------------------------------------
  # Any restriction/relativities present -> return restricted object
  # and include smoothing output inside rf_restricted_df
  # ---------------------------------------------------------------------------
  rf_restricted_df <- state$rf_restricted_df

  if (has_smoothing) {
    if (is.null(rf_restricted_df)) {
      rf_restricted_df <- state$new_rf
    } else {
      rf_restricted_df <- unique(rbind(rf_restricted_df, state$new_rf))
      rownames(rf_restricted_df) <- NULL
    }
  }

  rt <- list(
    formula_restricted = state$formula,
    formula_removed = state$formula_no_offset,
    data_restricted = state$data,
    fm_no_offset = state$formula_no_offset,
    offset = state$offset,
    rating_factors = state$rating_factors,
    restrictions_lst = state$restrictions_lst,
    rf_restricted_df = rf_restricted_df,
    model_call = state$model_call,
    model_out = state$model_out,
    new_col_nm = state$new_col_nm,
    old_col_nm = state$old_col_nm,
    mgd_rst = state$mgd_rst,
    mgd_smt = state$mgd_smt,
    relativities_df = state$relativities_df,
    normalize = state$normalize,
    exposure = state$exposure,
    base_risk_factor = state$base_risk_factor,
    risk_factor_split = state$risk_factor_split,
    display_risk_factor = state$display_risk_factor,
    model_risk_factor = state$model_risk_factor
  )

  attr(rt, "class") <- "restricted"
  attr(rt, "has_smoothing") <- has_smoothing
  attr(rt, "last_smoothing_step") <- if (has_smoothing) "add_smoothing" else NULL
  rt
}

preview_refinement <- function(ref) {
  .assert_refinement(ref)

  if (length(ref$steps) == 0) {
    stop("No refinement steps available.", call. = FALSE)
  }

  state <- .make_exec_state(ref)
  last_type <- NULL

  for (step in ref$steps) {
    state <- .apply_refinement_step(state, step)
    last_type <- step$type
  }

  list(
    state = state,
    legacy_plot_object = .preview_to_legacy_object(state, last_step_type = last_type)
  )
}




# -----------------------------------------------------------------------------
# Legacy executor reused by refit()
# -----------------------------------------------------------------------------

.legacy_add_restriction_on_legacy_object <- function(model, restrictions) {
  if (!inherits(model, c("smooth", "restricted"))) {
    stop("Internal error: model must be smooth/restricted.", call. = FALSE)
  }

  fm <- model$formula_restricted
  offset_term <- model$offset
  fm_no_offset <- model$formula_removed
  df_new <- model$data_restricted
  model_call <- model$model_call
  model_out <- model$model_out

  rfdf <- model$rating_factors
  rst_lst <- model$restrictions_lst
  rst_lst[[names(restrictions)[1]]] <- restrictions
  restricted_df <- restrict_df(restrictions)
  new_col_nm <- model$new_col_nm
  old_col_nm <- model$old_col_nm
  mgd_rst <- model$mgd_rst
  mgd_smt <- model$mgd_smt

  if (inherits(model, "restricted")) {
    restricted_df <- rbind(model$rf_restricted_df, restricted_df)
  }

  if (inherits(model, "smooth")) {
    restricted_df <- rbind(model$new_rf, restricted_df)
  }

  fm_remove <- update_formula_remove(fm_no_offset, names(restrictions)[1])
  fm_add <- update_formula_add(offset_term, fm_remove, names(restrictions)[2])
  df_restricted <- add_restrictions_df(df_new, restrictions)

  nrst <- unique(setdiff(names(restrictions), unique(rfdf$risk_factor)))
  orst <- unique(setdiff(names(restrictions), new_col_nm))
  mgd_rst <- append(mgd_rst, list(unique(c(orst, nrst))))

  new_col_nm <- unique(append(new_col_nm,
                              setdiff(names(restrictions),
                                      unique(rfdf$risk_factor))))
  old_col_nm <- unique(append(old_col_nm, setdiff(names(restrictions),
                                                  new_col_nm)))

  rt <- list(
    formula_restricted = fm_add[[1]],
    formula_removed = fm_remove,
    data_restricted = df_restricted,
    fm_no_offset = fm_no_offset,
    offset = fm_add[[2]],
    rating_factors = rfdf,
    restrictions_lst = rst_lst,
    rf_restricted_df = restricted_df,
    model_call = model_call,
    model_out = model_out,
    new_col_nm = new_col_nm,
    old_col_nm = old_col_nm,
    mgd_rst = mgd_rst,
    mgd_smt = mgd_smt
  )
  attr(rt, "class") <- "restricted"
  attr(rt, "has_smoothing") <- FALSE
  attr(rt, "last_smoothing_step") <- NULL
  rt
}

.legacy_refit_glm <- function(x, intercept_only = FALSE, ...) {
  if (!inherits(x, c("restricted", "smooth"))) {
    stop("Input must be of class 'restricted' or 'smooth'.", call. = FALSE)
  }

  if (isTRUE(intercept_only)) {
    andere <- attr(stats::terms.formula(x$formula_removed), "term.labels")
    if (length(andere) > 0) {
      tot_rf <- x$rating_factors
      df <- tot_rf[tot_rf$risk_factor %in% andere, ]
      rf_mult <- names(table(df$risk_factor)[table(df$risk_factor) > 1])
      rf_single <- names(table(df$risk_factor)[table(df$risk_factor) == 1])

      if (length(rf_mult) > 0) {
        df1 <- df[df$risk_factor %in% rf_mult, ]
        mult_lst <- split(df1, df1$risk_factor)

        for (i in seq_along(mult_lst)) {
          risk_factor_name <- unique(mult_lst[[i]]$risk_factor)
          names(mult_lst[[i]])[names(mult_lst[[i]]) == "level"] <- risk_factor_name
          names(mult_lst[[i]])[names(mult_lst[[i]]) == "estimate"] <- paste0(
            risk_factor_name, "_rst99"
          )
          mult_lst[[i]]$risk_factor <- NULL
          x <- .legacy_add_restriction_on_legacy_object(x, mult_lst[[i]])
        }
      }

      if (length(rf_single) > 0) {
        df2 <- df[df$risk_factor %in% rf_single, ]
        sng_lst <- split(df2, df2$risk_factor)

        for (i in seq_along(sng_lst)) {
          formula_removed <- x$formula_removed
          rf_name <- unique(sng_lst[[i]]$risk_factor)
          rf_est <- unique(sng_lst[[i]]$estimate)
          formula_removed <- update(formula_removed, paste("~ . -", rf_name))
          add_offset <- paste0(rf_name, " * log(", rf_est, ")")
          newoffset <- paste0(x$offset, " + ", add_offset)
          newoffsetterm <- paste0("offset(", newoffset, ")")
          formula_restricted <- update(formula_removed, paste("~ . + ", newoffsetterm))
          x$offset <- newoffset
          x$formula_restricted <- formula_restricted
          x$formula_removed <- formula_removed
        }
      }
    }
  }

  lst_call <- as.list(x$model_call)

  lst <- list(
    formula = x$formula_restricted,
    data = x$data_restricted,
    offset = NULL
  )

  y <- eval(as.call(utils::modifyList(lst_call, lst)))

  y$call$formula <- lst$formula
  y$call$data <- quote(df_new)

  offweights <- NULL
  if (!is.null(lst_call$weights)) {
    offweights <- append(offweights, as.character(lst_call$weights))
  }
  if (!is.null(lst_call$offset)) {
    offweights <- append(offweights, as.character(lst_call$offset)[2])
  }

  if (inherits(x, "smooth")) {
    attr(y, "new_rf") <- x[["new_rf"]]
    attr(y, "class") <- append(class(y), "refitsmooth")
  }

  if (inherits(x, "restricted")) {
    attr(y, "new_rf_rst") <- x[["rf_restricted_df"]]
    attr(y, "class") <- append(class(y), "refitrestricted")
  }

  rf <- x$rating_factors
  rf2 <- unique(rf$risk_factor[rf$risk_factor != "(Intercept)"])

  rf_single <- names(which(table(rf$risk_factor) == 1))
  rf_single <- setdiff(rf_single, "(Intercept)")
  rf_single_rows <- rf[rf$risk_factor %in% rf_single, ]

  attr(y, "new_col_nm") <- x$new_col_nm
  attr(y, "old_col_nm") <- x$old_col_nm
  attr(y, "rf") <- rf2
  attr(y, "mgd_smt") <- x$mgd_smt
  attr(y, "mgd_rst") <- x$mgd_rst
  attr(y, "offweights") <- offweights
  attr(y, "continuous_factors") <- rf_single_rows
  attr(y, "intercept_only") <- isTRUE(intercept_only)
  y
}


# -----------------------------------------------------------------------------
# Refit
# -----------------------------------------------------------------------------

.fit_refined_glm <- function(state, intercept_only = FALSE, ...) {
  legacy_obj <- .preview_to_legacy_object(state)
  .legacy_refit_glm(legacy_obj, intercept_only = intercept_only, ...)
}

#' Refit a prepared refinement workflow
#'
#' @description
#' Applies all collected refinement steps and refits the underlying GLM in one
#' call.
#'
#' @param x Object of class `rating_refinement`.
#' @param intercept_only Logical. Default `FALSE`.
#' @param ... Other arguments.
#'
#' @return Object of class `glm`.
#' @export
refit <- function(x, intercept_only = FALSE, ...) {
  .assert_refinement(x)

  state <- .make_exec_state(x)

  if (length(x$steps) == 0) {
    warning("No refinement steps were added; returning refit of original model.", call. = FALSE)
  }

  for (step in x$steps) {
    state <- .apply_refinement_step(state, step)
  }

  .fit_refined_glm(state, intercept_only = intercept_only, ...)
}

#' Refit a GLM model or refinement workflow
#'
#' @description
#' Backwards-compatible wrapper. Prefer using [refit()] for the new
#' refinement workflow.
#'
#' @param x Object of class `rating_refinement`, `restricted` or `smooth`.
#' @param intercept_only Logical.
#' @param ... Other arguments.
#'
#' @return Object of class `glm`.
#' @export
refit_glm <- function(x, intercept_only = FALSE, ...) {
  if (inherits(x, "rating_refinement")) {
    lifecycle::deprecate_warn("0.9.0", "refit_glm()", "refit()")
    return(refit(x, intercept_only = intercept_only, ...))
  }

  if (!inherits(x, c("restricted", "smooth"))) {
    stop("Input must be of class 'rating_refinement', 'restricted' or 'smooth'.",
         call. = FALSE)
  }

  .legacy_refit_glm(x, intercept_only = intercept_only, ...)
}

#' @rdname refit_glm
#' @export
update_glm <- function(x, intercept_only = FALSE, ...) {
  lifecycle::deprecate_warn("0.8.0", "update_glm()", "refit_glm()")
  refit_glm(x, intercept_only = intercept_only, ...)
}


# -----------------------------------------------------------------------------
# Plot selection helpers
# -----------------------------------------------------------------------------

.select_plot_step <- function(ref, variable = NULL, step = NULL) {
  .assert_refinement(ref)

  if (!is.null(step)) {
    if (!is.numeric(step) || length(step) != 1 || is.na(step)) {
      stop("'step' must be a single numeric index.", call. = FALSE)
    }

    step <- as.integer(step)

    if (step < 1 || step > length(ref$steps)) {
      stop("'step' is out of bounds.", call. = FALSE)
    }

    return(step)
  }

  if (!is.null(variable)) {
    matches <- which(vapply(
      ref$steps,
      function(s) identical(s$variable, variable),
      logical(1)
    ))

    if (length(matches) == 0) {
      stop("No refinement step found for variable: ", variable, call. = FALSE)
    }

    if (length(matches) > 1) {
      stop(
        "Multiple refinement steps found for variable: ",
        variable,
        ". Use `step =` instead.",
        call. = FALSE
      )
    }

    return(matches)
  }

  if (length(ref$steps) == 1) {
    return(1L)
  }

  stop(
    "Multiple refinement steps found. Use `variable =` or `step =` to select one.",
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# Preview refinement up to a selected step
# -----------------------------------------------------------------------------

preview_refinement <- function(ref, upto = length(ref$steps)) {
  .assert_refinement(ref)

  if (length(ref$steps) == 0) {
    stop("No refinement steps available.", call. = FALSE)
  }

  if (!is.numeric(upto) || length(upto) != 1 || is.na(upto)) {
    stop("'upto' must be a single numeric step index.", call. = FALSE)
  }

  upto <- as.integer(upto)

  if (upto < 1 || upto > length(ref$steps)) {
    stop("'upto' is out of bounds.", call. = FALSE)
  }

  state <- .make_exec_state(ref)

  for (i in seq_len(upto)) {
    state <- .apply_refinement_step(state, ref$steps[[i]])
  }

  list(
    state = state,
    step = ref$steps[[upto]]
  )
}


# -----------------------------------------------------------------------------
# autoplot for refinement workflow
# -----------------------------------------------------------------------------

#' Automatically create a ggplot for objects obtained from refinement
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Takes an object produced by `add_restriction()` or `add_relativities()`
#' and creates a plot comparing the adjusted coefficients with the original
#' coefficients obtained from the model.
#'
#' For objects produced by `add_relativities()`, original levels that are split
#' into new levels are removed from the connected original line and from the
#' x-axis. Instead, the original level is shown as a horizontal blue segment
#' spanning all child categories, with the original level label centred above
#' the segment.
#'
#' @param object Object produced by `add_restriction()` or `add_relativities()`.
#' @param variable Optional character string specifying the risk factor to plot.
#'   If `NULL` (default), all available variables in the refinement object are
#'   shown. If specified, only the selected risk factor is plotted.
#'
#' @param step Optional integer specifying which refinement step to plot.
#'   This is mainly relevant when multiple refinement steps have been applied
#'   (e.g. multiple calls to `add_smoothing()`, `add_restriction()`, or
#'   `add_relativities()`).
#'
#'   - If `NULL` (default), the latest refinement step is shown.
#'   - If specified, the corresponding step in the refinement sequence is used.
#'
#'   This makes it possible to inspect intermediate refinement stages before
#'   calling `refit()`.
#' @param remove_underscores Logical; if `TRUE`, underscores are replaced by
#'   spaces in the x-axis label. Default is `FALSE`.
#' @param rotate_angle Optional numeric value for the angle of x-axis labels.
#' @param custom_theme Optional list passed to `ggplot2::theme()`.
#' @param ... Additional plotting arguments passed to ggplot2 geoms.
#'
#' @return A `ggplot2` object.
#'
#' @author Martin Haringa
#'
#' @import ggplot2
#' @importFrom dplyr left_join
#'
#' @export
autoplot.rating_refinement <- function(object,
                                       variable = NULL,
                                       step = NULL,
                                       remove_underscores = FALSE,
                                       rotate_angle = NULL,
                                       custom_theme = NULL,
                                       ...) {
  .assert_refinement(object)

  if (length(object$steps) == 0) {
    stop("No refinement steps available to plot.", call. = FALSE)
  }

  idx <- .select_plot_step(object, variable = variable, step = step)
  preview <- preview_refinement(object, upto = idx)

  selected_step <- preview$step
  state <- preview$state

  if (identical(selected_step$type, "smoothing")) {
    return(
      .plot_smoothing_step(
        state = state,
        step = selected_step,
        ...
      )
    )
  }

  if (selected_step$type %in% c("restriction", "relativities")) {
    return(
      .plot_restriction_step(
        state = state,
        step = selected_step,
        remove_underscores = remove_underscores,
        rotate_angle = rotate_angle,
        custom_theme = custom_theme,
        ...
      )
    )
  }

  stop("No plot available for this refinement step.", call. = FALSE)
}


# -----------------------------------------------------------------------------
# Internal plotting helpers
# -----------------------------------------------------------------------------

.plot_palette_ir <- function() {
  list(
    frequency        = "#2C7FB8",
    average_severity = "#41AB5D",
    risk_premium     = "#F28E2B",
    loss_ratio       = "#8C6BB1",
    average_premium  = "#2CB1A1",
    bg_bar           = "#E6E6E6"
  )
}

.plot_grid_theme_ir <- function() {
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.grid.major = ggplot2::element_line(color = "#F2F2F2", linewidth = 0.4),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border     = ggplot2::element_blank(),
    axis.text.y.right  = ggplot2::element_text(color = "#9E9E9E", size = 8),
    axis.title.y.right = ggplot2::element_text(color = "#9E9E9E", size = 9),
    axis.title.y       = ggplot2::element_text(size = 10)
  )
}


# -----------------------------------------------------------------------------
# Plot smoothing step
# -----------------------------------------------------------------------------

.plot_smoothing_step <- function(state, step, ...) {
  rf2 <- state$borders
  new <- state$new
  new_line <- state$new_line

  if (is.null(rf2) || is.null(new) || is.null(new_line)) {
    stop("No smoothing plot data found for this step.", call. = FALSE)
  }

  pal <- .plot_palette_ir()
  grid_theme <- .plot_grid_theme_ir()

  rf2_start_open   <- rf2[rf2$start_oc == "open", , drop = FALSE]
  rf2_start_closed <- rf2[rf2$start_oc == "closed", , drop = FALSE]
  rf2_end_open     <- rf2[rf2$end_oc == "open", , drop = FALSE]
  rf2_end_closed   <- rf2[rf2$end_oc == "closed", , drop = FALSE]

  new_start_open   <- new[new$start_oc == "open", , drop = FALSE]
  new_start_closed <- new[new$start_oc == "closed", , drop = FALSE]
  new_end_open     <- new[new$end_oc == "open", , drop = FALSE]
  new_end_closed   <- new[new$end_oc == "closed", , drop = FALSE]

  x_name <- names(new_line)[1]
  names(new_line)[names(new_line) == x_name] <- "col1"

  ggplot2::ggplot(data = rf2) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = start_,
        y = estimate,
        xend = end_,
        yend = estimate,
        color = "Model fit"
      ),
      linewidth = 0.8,
      ...
    ) +
    ggplot2::geom_segment(
      data = new,
      ggplot2::aes(
        x = breaks_min,
        y = yhat,
        xend = breaks_max,
        yend = yhat,
        color = "New cluster"
      ),
      linewidth = 0.8,
      ...
    ) +
    ggplot2::geom_line(
      data = new_line,
      ggplot2::aes(
        x = col1,
        y = yhat
      ),
      linewidth = 0.5,
      linetype = "dashed",
      color = "#4D4D4D",
      ...
    ) +
    ggplot2::geom_point(
      data = rf2_start_closed,
      ggplot2::aes(x = start_, y = estimate, color = "Model fit"),
      shape = 16,
      size = 2.2,
      ...
    ) +
    ggplot2::geom_point(
      data = rf2_end_closed,
      ggplot2::aes(x = end_, y = estimate, color = "Model fit"),
      shape = 16,
      size = 2.2,
      ...
    ) +
    ggplot2::geom_point(
      data = rf2_start_open,
      ggplot2::aes(x = start_, y = estimate, color = "Model fit"),
      shape = 21,
      fill = "white",
      stroke = 0.7,
      size = 2.2,
      ...
    ) +
    ggplot2::geom_point(
      data = rf2_end_open,
      ggplot2::aes(x = end_, y = estimate, color = "Model fit"),
      shape = 21,
      fill = "white",
      stroke = 0.7,
      size = 2.2,
      ...
    ) +
    ggplot2::geom_point(
      data = new_start_closed,
      ggplot2::aes(x = start_, y = yhat, color = "New cluster"),
      shape = 16,
      size = 2.2,
      ...
    ) +
    ggplot2::geom_point(
      data = new_end_closed,
      ggplot2::aes(x = end_, y = yhat, color = "New cluster"),
      shape = 16,
      size = 2.2,
      ...
    ) +
    ggplot2::geom_point(
      data = new_start_open,
      ggplot2::aes(x = start_, y = yhat, color = "New cluster"),
      shape = 21,
      fill = "white",
      stroke = 0.7,
      size = 2.2,
      ...
    ) +
    ggplot2::geom_point(
      data = new_end_open,
      ggplot2::aes(x = end_, y = yhat, color = "New cluster"),
      shape = 21,
      fill = "white",
      stroke = 0.7,
      size = 2.2,
      ...
    ) +
    ggplot2::labs(
      x = x_name,
      y = "Estimated relativity"
    ) +
    ggplot2::scale_colour_manual(
      name = NULL,
      values = c(
        "Model fit" = pal$frequency,
        "New cluster" = pal$risk_premium
      ),
      labels = c(
        "Model fit" = "Original fit",
        "New cluster" = "Clustered fit"
      )
    ) +
    ggplot2::theme_minimal() +
    grid_theme
}


# -----------------------------------------------------------------------------
# Plot restriction / relativities step
# -----------------------------------------------------------------------------

.plot_restriction_step <- function(state,
                                   step,
                                   remove_underscores = FALSE,
                                   rotate_angle = NULL,
                                   custom_theme = NULL,
                                   ...) {

  pal <- .plot_palette_ir()
  grid_theme <- .plot_grid_theme_ir()

  if (identical(step$type, "relativities")) {
    rel_df <- state$relativities_df
    rf <- as.data.frame(state$rating_factors)
    base_rf <- state$base_risk_factor
    display_rf <- state$display_risk_factor

    if (is.null(rel_df) || is.null(base_rf) || is.null(display_rf)) {
      stop("No relativities plot data found for this step.", call. = FALSE)
    }

    rel_df <- as.data.frame(rel_df)

    rf_base <- rf[rf$risk_factor == base_rf, c("level", "estimate"), drop = FALSE]
    names(rf_base)[2] <- "Coef"
    rf_base$type <- "Original fit"

    rel_plot <- rel_df[, c("new_level", "estimate"), drop = FALSE]
    names(rel_plot) <- c("level", "Coef")
    rel_plot$type <- "New relativities"

    replaced_levels <- unique(rel_df$level)

    rf_base_line <- rf_base[!rf_base$level %in% replaced_levels, , drop = FALSE]

    unsplit_levels <- setdiff(rf_base$level, unique(rel_df$level))
    if (length(unsplit_levels) > 0) {
      unsplit_plot <- rf_base[rf_base$level %in% unsplit_levels, c("level", "Coef"), drop = FALSE]
      unsplit_plot$type <- "New relativities"
      rel_plot <- rbind(rel_plot, unsplit_plot)
    }

    lvl_order <- unique(c(
      as.character(rf_base_line$level),
      as.character(rel_plot$level)
    ))

    x_lookup <- data.frame(
      level = lvl_order,
      x_num = seq_along(lvl_order),
      stringsAsFactors = FALSE
    )

    rf_base_line$level <- as.character(rf_base_line$level)
    rel_plot$level <- as.character(rel_plot$level)

    rf_base_line <- dplyr::left_join(rf_base_line, x_lookup, by = "level")
    rel_plot <- dplyr::left_join(rel_plot, x_lookup, by = "level")

    segments_list <- list()

    for (lvl in replaced_levels) {
      child_lvls <- as.character(rel_df$new_level[rel_df$level == lvl])
      child_lvls <- child_lvls[child_lvls %in% lvl_order]

      if (length(child_lvls) == 0) next

      child_pos <- x_lookup$x_num[match(child_lvls, x_lookup$level)]
      child_pos <- child_pos[!is.na(child_pos)]

      if (length(child_pos) == 0) next

      y_val <- rf_base$Coef[rf_base$level == lvl][1]

      segments_list[[as.character(lvl)]] <- data.frame(
        x_start = min(child_pos),
        x_end   = max(child_pos),
        y       = y_val,
        label   = as.character(lvl),
        stringsAsFactors = FALSE
      )
    }

    segments_df <- NULL
    if (length(segments_list) > 0) {
      segments_df <- do.call(rbind, segments_list)
    }

    x_lab <- display_rf
    if (remove_underscores) {
      x_lab <- gsub("_", " ", x_lab)
    }

    p <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = rf_base_line,
        ggplot2::aes(x = x_num, y = Coef, color = type, group = type),
        linewidth = 0.8,
        ...
      ) +
      ggplot2::geom_point(
        data = rf_base_line,
        ggplot2::aes(x = x_num, y = Coef, color = type),
        shape = 21,
        fill = "white",
        stroke = 0.7,
        size = 2.4,
        ...
      ) +
      ggplot2::geom_line(
        data = rel_plot,
        ggplot2::aes(x = x_num, y = Coef, color = type, group = type),
        linewidth = 0.8,
        ...
      ) +
      ggplot2::geom_point(
        data = rel_plot,
        ggplot2::aes(x = x_num, y = Coef, color = type),
        shape = 21,
        fill = "white",
        stroke = 0.7,
        size = 2.4,
        ...
      )

    if (!is.null(segments_df) && nrow(segments_df) > 0) {
      p <- p +
        ggplot2::geom_segment(
          data = segments_df,
          ggplot2::aes(
            x = x_start,
            xend = x_end,
            y = y,
            yend = y
          ),
          color = pal$frequency,
          linewidth = 0.9
        ) +
        ggplot2::geom_segment(
          data = segments_df,
          ggplot2::aes(
            x = x_start,
            xend = x_start,
            y = y,
            yend = y - 0.012
          ),
          color = pal$frequency,
          linewidth = 0.8
        ) +
        ggplot2::geom_segment(
          data = segments_df,
          ggplot2::aes(
            x = x_end,
            xend = x_end,
            y = y,
            yend = y - 0.012
          ),
          color = pal$frequency,
          linewidth = 0.8
        ) +
        ggplot2::geom_text(
          data = segments_df,
          ggplot2::aes(
            x = (x_start + x_end) / 2,
            y = y,
            label = label
          ),
          vjust = -0.7,
          color = pal$frequency,
          size = 3.2
        )
    }

    p <- p +
      ggplot2::scale_x_continuous(
        breaks = x_lookup$x_num,
        labels = x_lookup$level
      ) +
      ggplot2::scale_colour_manual(
        values = c(
          "Original fit" = pal$frequency,
          "New relativities" = pal$risk_premium
        ),
        name = NULL
      ) +
      ggplot2::labs(
        x = x_lab,
        y = "Relativity"
      ) +
      ggplot2::theme_minimal() +
      grid_theme

    if (!is.null(rotate_angle)) {
      p <- p +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = rotate_angle, hjust = 1)
        )
    }

    if (!is.null(custom_theme)) {
      p <- p + do.call(ggplot2::theme, custom_theme)
    }

    return(p)
  }

  if (identical(step$type, "restriction")) {
    naam_rst <- step$restrictions
    name <- names(naam_rst)[1]

    rf <- as.data.frame(state$rating_factors)
    naam_rf <- rf[rf$risk_factor == name, , drop = FALSE]
    naam_rf <- naam_rf[, c("level", "estimate"), drop = FALSE]

    names(naam_rst)[names(naam_rst) == name] <- "level"
    naam_rf <- matchColClasses(naam_rst, naam_rf)

    koppel <- dplyr::left_join(naam_rst, naam_rf, by = "level")

    restricted_name <- names(naam_rst)[2]
    unrestricted_name <- names(naam_rf)[2]

    koppel_restricted <- data.frame(
      level = koppel$level,
      type = "Adjusted fit",
      Coef = koppel[[restricted_name]],
      stringsAsFactors = FALSE
    )

    koppel_unrestricted <- data.frame(
      level = koppel$level,
      type = "Original fit",
      Coef = koppel[[unrestricted_name]],
      stringsAsFactors = FALSE
    )

    koppel_long <- rbind(koppel_unrestricted, koppel_restricted)
    lvl_order <- unique(koppel$level)

    x_lookup <- data.frame(
      level = lvl_order,
      x_num = seq_along(lvl_order),
      stringsAsFactors = FALSE
    )

    koppel_long <- dplyr::left_join(koppel_long, x_lookup, by = "level")

    x_lab <- name
    if (remove_underscores) {
      x_lab <- gsub("_", " ", x_lab)
    }

    p <- ggplot2::ggplot(
      data = koppel_long,
      ggplot2::aes(x = x_num, y = Coef, color = type, group = type)
    ) +
      ggplot2::geom_line(linewidth = 0.8, ...) +
      ggplot2::geom_point(
        shape = 21,
        fill = "white",
        stroke = 0.7,
        size = 2.4,
        ...
      ) +
      ggplot2::scale_x_continuous(
        breaks = x_lookup$x_num,
        labels = x_lookup$level
      ) +
      ggplot2::scale_colour_manual(
        values = c(
          "Original fit" = pal$frequency,
          "Adjusted fit" = pal$risk_premium
        ),
        name = NULL
      ) +
      ggplot2::labs(
        x = x_lab,
        y = "Relativity"
      ) +
      ggplot2::theme_minimal() +
      grid_theme

    if (!is.null(rotate_angle)) {
      p <- p +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = rotate_angle, hjust = 1)
        )
    }

    if (!is.null(custom_theme)) {
      p <- p + do.call(ggplot2::theme, custom_theme)
    }

    return(p)
  }

  stop("No restriction/relativities plot available for this step.", call. = FALSE)
}



