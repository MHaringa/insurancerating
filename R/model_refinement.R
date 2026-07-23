
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
  rfdf <- rating_table(model, significance = FALSE)$df
  colnames(rfdf)[3] <- "estimate"
  as.data.frame(rfdf)
}

.refinement_model_variables <- function(model, data) {
  model_terms <- tryCatch(stats::terms(model), error = function(e) NULL)
  variables <- if (is.null(model_terms)) {
    character()
  } else {
    all.vars(stats::delete.response(model_terms))
  }

  if (!is.null(model$call$weights)) {
    variables <- c(variables, all.vars(model$call$weights))
  }
  if (!is.null(model$call$offset)) {
    variables <- c(variables, all.vars(model$call$offset))
  }

  intersect(unique(variables), names(data))
}

.refinement_problem_counts <- function(data, variables, rows = NULL) {
  values <- if (is.null(rows)) {
    data[variables]
  } else {
    data[rows, variables, drop = FALSE]
  }

  missing <- vapply(values, function(x) sum(is.na(x)), integer(1))
  non_finite <- vapply(
    values,
    function(x) {
      if (is.numeric(x)) {
        sum(!is.finite(x) & !is.na(x))
      } else {
        0L
      }
    },
    integer(1)
  )

  list(
    missing = missing[missing > 0L],
    non_finite = non_finite[non_finite > 0L]
  )
}

.format_refinement_counts <- function(counts) {
  paste0("- ", names(counts), ": ", unname(counts))
}

.stop_refinement_row_mismatch <- function(model, data, model_rows) {
  data_rows <- nrow(data)
  model_variables <- .refinement_model_variables(model, data)
  na_action <- model$na.action
  if (is.null(na_action)) {
    na_action <- tryCatch(stats::na.action(model), error = function(e) NULL)
  }
  omitted_rows <- if (is.null(na_action)) integer() else as.integer(na_action)
  omitted_rows <- omitted_rows[
    is.finite(omitted_rows) & omitted_rows >= 1L & omitted_rows <= data_rows
  ]
  omitted_rows <- unique(omitted_rows)

  omission_matches <- data_rows > model_rows &&
    length(omitted_rows) == data_rows - model_rows
  omitted_problems <- if (omission_matches && length(model_variables) > 0L) {
    .refinement_problem_counts(data, model_variables, omitted_rows)
  } else {
    list(missing = integer(), non_finite = integer())
  }

  if (omission_matches &&
      (length(omitted_problems$missing) > 0L ||
       length(omitted_problems$non_finite) > 0L)) {
    issue <- if (length(omitted_problems$missing) > 0L &&
                 length(omitted_problems$non_finite) > 0L) {
      "missing or non-finite model inputs"
    } else if (length(omitted_problems$missing) > 0L) {
      "missing values"
    } else {
      "non-finite numeric values"
    }
    omitted_text <- if (length(omitted_rows) == 1L) {
      "1 observation appears"
    } else {
      paste(length(omitted_rows), "observations appear")
    }
    lines <- c(
      paste0(
        "The model was fitted on ", model_rows,
        " observations, but `data` contains ", data_rows, " rows."
      ),
      paste0(
        omitted_text,
        " to have been omitted during model fitting ",
        "because of ", issue, "."
      ),
      paste0(
        "The fitted model frame no longer contains these observations; ",
        "`lm()`, `glm()` and related model functions remove omitted rows ",
        "before storing the model frame."
      )
    )
    if (length(omitted_problems$missing) > 0L) {
      lines <- c(
        lines,
        "Variables containing missing values in the omitted rows:",
        .format_refinement_counts(omitted_problems$missing)
      )
    }
    if (length(omitted_problems$non_finite) > 0L) {
      lines <- c(
        lines,
        "Numeric variables containing non-finite values in the omitted rows:",
        .format_refinement_counts(omitted_problems$non_finite)
      )
    }
    lines <- c(
      lines,
      paste0(
        "`prepare_refinement()` requires `data` to contain exactly the ",
        "observations used to fit the model. Remove the omitted observations, ",
        "refit the model with an explicit missing-value strategy, or supply ",
        "the model frame used during fitting."
      )
    )
    stop(paste(lines, collapse = "\n"), call. = FALSE)
  }

  all_problems <- if (length(model_variables) > 0L) {
    .refinement_problem_counts(data, model_variables)
  } else {
    list(missing = integer(), non_finite = integer())
  }
  lines <- c(
    paste0(
      "The model frame contains ", model_rows,
      " rows, while `data` contains ", data_rows, " rows."
    ),
    paste0(
      "The fitted model frame contains only observations retained by the ",
      "model-fitting function."
    ),
    paste0(
      "The supplied data does not appear to be the same data used to fit the ",
      "model. Missing values, subsetting, filtering, or row removal during ",
      "model fitting may have caused this difference."
    )
  )
  if (length(all_problems$non_finite) > 0L) {
    lines <- c(
      lines,
      "Numeric model variables containing non-finite values in `data`:",
      .format_refinement_counts(all_problems$non_finite)
    )
  }
  stop(paste(lines, collapse = "\n"), call. = FALSE)
}

.validate_refinement_data <- function(model, data) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.", call. = FALSE)
  }

  model_frame <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  model_rows <- if (is.null(model_frame)) stats::nobs(model) else nrow(model_frame)
  if (!is.null(model_rows) && nrow(data) != model_rows) {
    .stop_refinement_row_mismatch(model, data, model_rows)
  }

  needed <- all.vars(stats::formula(model))

  if (!is.null(model$call$weights)) {
    needed <- unique(c(needed, all.vars(model$call$weights)))
  }

  if (!is.null(model$call$offset)) {
    needed <- unique(c(needed, all.vars(model$call$offset)))
  }

  missing <- setdiff(needed, names(data))
  if (length(missing) > 0) {
    stop(
      "'data' is missing model column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  as.data.frame(data)
}

.is_single_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

.assert_column_name <- function(x, arg, data) {
  if (!.is_single_string(x)) {
    stop("'", arg, "' must be a single non-empty character string.", call. = FALSE)
  }
  if (!x %in% names(data)) {
    stop("'", arg, "' column is not present in refinement data.", call. = FALSE)
  }
  invisible(TRUE)
}

.assert_optional_column_name <- function(x, arg, data) {
  if (is.null(x)) {
    return(invisible(TRUE))
  }
  .assert_column_name(x, arg, data)
}

.assert_single_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop("'", arg, "' must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

.assert_single_numeric <- function(x, arg, allow_null = TRUE, positive = FALSE,
                                   whole = FALSE) {
  if (is.null(x) && allow_null) {
    return(invisible(TRUE))
  }
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || !is.finite(x)) {
    stop("'", arg, "' must be a single finite numeric value.", call. = FALSE)
  }
  if (positive && x <= 0) {
    stop("'", arg, "' must be greater than 0.", call. = FALSE)
  }
  if (whole && x != as.integer(x)) {
    stop("'", arg, "' must be a whole number.", call. = FALSE)
  }
  invisible(TRUE)
}

.allowed_smoothing_methods <- c(
  "spline", "mpi", "mpd", "gam", "cx", "cv", "micx", "micv", "mdcx", "mdcv"
)

.assert_smoothing_interval_levels <- function(model, model_variable) {
  borders <- suppressMessages(cut_borders_model(model, model_variable))
  ok <- nrow(borders) > 0 &&
    all(is.finite(borders$start_)) &&
    all(is.finite(borders$end_)) &&
    all(is.finite(borders$avg_))

  if (!ok) {
    stop(
      "'model_variable' must be a grouped numeric variable with interval-style ",
      "levels, such as levels created by cut().",
      call. = FALSE
    )
  }

  invisible(TRUE)
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

  data <- .validate_refinement_data(model, data %||% .get_model_data(model))

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
#' @param data Optional data.frame containing exactly the observations retained
#'   in the fitted GLM and all required model variables. If model fitting omitted
#'   rows because of missing values, supply the retained model data rather than
#'   the original unfiltered data. If `NULL`, the data are retrieved from the
#'   model object.
#'
#' @return Object of class `rating_refinement`.
#' @export
prepare_refinement <- function(model, data = NULL) {
  as_refinement(model, data = data)
}

#' @keywords internal
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
        "id", "type", "variable", "x_cut", "x_org", "model_variable",
        "source_variable", "split_variable", "smoothing", "risk_factor",
        "risk_factor_split", "normalize"
      ))]
    })
  )
  class(out) <- "summary.rating_refinement"
  out
}


#' @keywords internal
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
#' Fixes selected model levels to user-supplied relativities in a refinement
#' workflow. This is useful when the fitted GLM coefficients need to be adjusted
#' before the final tariff is refitted, for example to apply expert judgement,
#' enforce a business rule, remove an implausible local effect, or make a tariff
#' structure easier to explain.
#'
#' @details
#' `add_restriction()` stores a restriction step on a `rating_refinement`
#' object. It does not refit the GLM immediately. The restrictions are applied
#' when [refit()] is called.
#'
#' The `restrictions` data frame identifies the model variable to restrict by
#' its first column. The second column contains the relativities that should be
#' used for those levels in the refined model. New code should use this function
#' after [prepare_refinement()]; the deprecated [restrict_coef()] wrapper is
#' only kept for backwards compatibility.
#'
#' The restriction table may contain all levels of the model variable, or only
#' the levels that need a manual adjustment. If only a subset is supplied, the
#' missing levels are automatically filled with their current fitted GLM
#' relativities. This makes it possible to fix one level explicitly while keeping
#' the other levels at their already estimated values.
#'
#' @param model Object of class `rating_refinement`, usually created with
#'   [prepare_refinement()].
#' @param restrictions Data frame with exactly two columns. The first column
#'   must have the same name as the model variable to restrict and contains the
#'   levels to adjust. The second column contains the replacement relativities.
#'   Levels that are not supplied are filled with the currently fitted GLM
#'   relativities.
#'
#' @author Martin Haringa
#'
#' @return Object of class `rating_refinement`.
#'
#' @examples
#' portfolio <- data.frame(
#'   claims = c(1, 2, 1, 3, 2, 4),
#'   exposure = rep(1, 6),
#'   postal_area = factor(c("A", "B", "C", "A", "B", "C"))
#' )
#'
#' model <- glm(
#'   claims ~ postal_area + offset(log(exposure)),
#'   family = poisson(),
#'   data = portfolio
#' )
#'
#' restrictions <- data.frame(
#'   postal_area = "C",
#'   relativity = 1.10
#' )
#'
#' refined <- prepare_refinement(model, data = portfolio) |>
#'   add_restriction(restrictions)
#'
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

  restrictions <- .complete_restrictions_from_model(model, restrictions)

  .add_step(model, list(
    id = .next_step_id(model),
    type = "restriction",
    variable = variable,
    restrictions = restrictions
  ))
}

.complete_restrictions_from_model <- function(model, restrictions) {
  variable <- names(restrictions)[1]
  value_col <- names(restrictions)[2]

  if (length(unique(restrictions[[variable]])) != nrow(restrictions)) {
    stop("`", variable, "` in `restrictions` must have unique values.",
         call. = FALSE)
  }

  if (!is.numeric(restrictions[[value_col]]) ||
      any(!is.finite(restrictions[[value_col]]))) {
    stop("The second column of `restrictions` must contain finite numeric relativities.",
         call. = FALSE)
  }

  rf <- model$base$rating_factors
  rf_var <- rf[rf$risk_factor == variable, c("level", "estimate"), drop = FALSE]

  if (nrow(rf_var) == 0) {
    stop(
      "Restriction variable '", variable,
      "' is not a rating factor in the fitted GLM.",
      call. = FALSE
    )
  }

  model_levels <- as.character(rf_var$level)
  supplied_levels <- as.character(restrictions[[variable]])
  unknown_levels <- setdiff(supplied_levels, model_levels)

  if (length(unknown_levels) > 0) {
    stop(
      "Level(s) in `restrictions` not found in model variable `",
      variable,
      "`: ",
      paste(unknown_levels, collapse = ", "),
      call. = FALSE
    )
  }

  full <- data.frame(
    level = model_levels,
    value = as.numeric(rf_var$estimate),
    stringsAsFactors = FALSE
  )

  idx <- match(supplied_levels, full$level)
  full$value[idx] <- restrictions[[value_col]]

  out <- stats::setNames(
    data.frame(full$level, full$value, stringsAsFactors = FALSE),
    c(variable, value_col)
  )

  out
}


#' Deprecated restriction helper
#'
#' @description
#' `restrict_coef()` is deprecated as of version 0.9.0. Use
#' [add_restriction()] instead.
#'
#' \preformatted{
#' prepare_refinement(model) |>
#'   add_restriction(...) |>
#'   refit()
#' }
#'
#' @param model A fitted model object.
#' @param restrictions data.frame with exactly two columns.
#'
#' @return A legacy restricted object. New code should use
#'   [prepare_refinement()], [add_restriction()], and [refit()].
#'
#' @seealso [add_restriction()], [prepare_refinement()], [refit()]
#'
#' @export
#' @keywords internal
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


#' Smooth grouped tariff relativities
#'
#' @description
#' Replace the estimated relativities of a grouped numeric model variable by a
#' smooth tariff curve. In actuarial pricing this can be useful for ordered risk
#' factors such as age, vehicle age, insured value or bonus-malus years, where
#' independently estimated factor levels may show sampling variation that is
#' not considered suitable for the final tariff structure.
#'
#' @details
#' `add_smoothing()` stores a smoothing specification on a
#' `rating_refinement` object. It does not immediately refit the pricing GLM.
#' The original GLM contains `model_variable`, usually a factor created by
#' grouping a continuous risk factor. `source_variable` identifies the original
#' numeric variable represented by those groups.
#'
#' The smoother is estimated from the fitted GLM relativities at the midpoint of
#' each model interval. Consequently, the amount of information available to
#' the smoother is primarily determined by the number of grouped model levels,
#' rather than by the number of individual portfolio records. Exposure or
#' another volume measure can be supplied through `weights` so that model levels
#' with more portfolio volume have greater influence on the fitted curve.
#'
#' The fitted curve is evaluated using `breaks` and converted back to a grouped
#' tariff variable. The original model term is replaced by that smoothed tariff
#' variable when [refit()] is called. The usual sequence is therefore
#' [prepare_refinement()], `add_smoothing()`, optionally [edit_smoothing()], and
#' finally [refit()].
#'
#' ## Smoothing methods
#'
#' The available methods represent different assumptions about the shape of the
#' tariff effect:
#'
#' \describe{
#'   \item{`"spline"`}{Fits an unconstrained global polynomial through the
#'   grouped GLM relativities. `degree` determines its order. A low degree is
#'   generally easier to interpret and less sensitive near the boundaries;
#'   higher degrees can follow more local variation but may introduce
#'   oscillation.}
#'   \item{`"gam"`}{Fits an unconstrained penalized smooth with [mgcv::gam()].
#'   This allows the tariff curve to adapt to the observed pattern while the
#'   smoothing penalty limits unnecessary variation.}
#'   \item{`"mpi"` and `"mpd"`}{Fit monotone increasing and monotone decreasing
#'   P-splines, respectively.}
#'   \item{`"cx"` and `"cv"`}{Fit convex and concave P-splines, respectively.}
#'   \item{`"micx"` and `"micv"`}{Fit monotone increasing curves that are,
#'   respectively, convex and concave.}
#'   \item{`"mdcx"` and `"mdcv"`}{Fit monotone decreasing curves that are,
#'   respectively, convex and concave.}
#' }
#'
#' The shape-constrained methods are fitted with [scam::scam()]. A constraint
#' should reflect an actuarial or pricing assumption that is defensible for the
#' risk factor; it should not be selected solely because it produces a smoother
#' visual result.
#'
#' ## Basis dimension and polynomial degree
#'
#' For `"gam"` and the shape-constrained methods, `k` specifies the basis
#' dimension. It controls the maximum flexibility available to the smooth, but
#' it is not the final effective degrees of freedom of the fitted curve. For a
#' penalized GAM, the estimated smoothing penalty can reduce the effective
#' degrees of freedom below this maximum.
#'
#' A smaller `k` restricts the curve to broad movements. A larger `k` permits
#' more local variation, but requires enough distinct grouped covariate values
#' and may be unstable when only a few tariff levels are available. If `k` is
#' `NULL`, an effective default basis dimension of 10 is used. The function
#' checks this dimension against the grouped values before fitting and reports
#' the observed number of unique values when the requested complexity is not
#' feasible.
#'
#' For `"spline"`, `degree` has the corresponding complexity role. A polynomial
#' of degree \eqn{d} requires at least \eqn{d + 1} unique grouped values. When
#' `degree` is omitted, the existing behaviour uses the highest degree supported
#' by the grouped model points.
#'
#' The deprecated [smooth_coef()] wrapper remains available for backwards
#' compatibility.
#'
#' @param model Object of class `rating_refinement`, usually created with
#'   [prepare_refinement()].
#' @param model_variable Character string. Existing grouped or binned variable
#'   in the GLM. This is the model term that will be replaced by a smoothed
#'   tariff factor. The column must not contain missing values; remove or impute
#'   missing values before adding the smoothing step.
#' @param source_variable Character string. Original numeric portfolio variable
#'   underlying `model_variable`. Its name is also used for the resulting
#'   smoothed tariff variable.
#' @param degree Optional single whole number. Polynomial degree, used by
#'   `smoothing = "spline"`. The degree must be feasible for the number of unique
#'   grouped model points.
#' @param breaks Numeric vector with the tariff segment boundaries to use after
#'   smoothing. These boundaries determine the final tariff segmentation, not
#'   the number of portfolio observations used to estimate the curve. Values
#'   must be finite and strictly increasing.
#' @param smoothing Character string selecting the smoothing method. Available
#'   values are `"spline"`, `"gam"`, `"mpi"`, `"mpd"`, `"cx"`, `"cv"`,
#'   `"micx"`, `"micv"`, `"mdcx"` and `"mdcv"`. See Details for the shape
#'   restrictions represented by these codes.
#' @param k Optional single positive whole number. Basis dimension for smoothing
#'   methods other than `"spline"`. It sets the maximum flexibility available
#'   to the smooth and is not necessarily equal to its estimated effective
#'   degrees of freedom. `NULL` uses an effective default of 10. The basis
#'   dimension cannot exceed the number of unique grouped covariate values
#'   available for fitting.
#' @param weights Optional character string. Numeric volume column, usually
#'   exposure, used to weight the grouped GLM relativities during smoothing.
#' @param tariff_class,rating_variable Deprecated. Use `model_variable` and
#'   `source_variable` instead.
#' @param x_cut,x_org Deprecated. Use `model_variable` and `source_variable`
#'   instead.
#'
#' @author Martin Haringa
#'
#' @return An object of class `rating_refinement` containing the stored
#'   smoothing specification. The pricing GLM is not fitted again until
#'   [refit()] is called.
#'
#' @seealso [prepare_refinement()], [edit_smoothing()], [refit()],
#'   [risk_factor_gam()]
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' age_policyholder_frequency <- risk_factor_gam(
#'   data = MTPL,
#'   claim_count = "nclaims",
#'   risk_factor = "age_policyholder",
#'   exposure = "exposure"
#' )
#'
#' age_segments_freq <- derive_tariff_segments(age_policyholder_frequency)
#'
#' dat <- MTPL |>
#'   add_tariff_segments(age_segments_freq, name = "age_policyholder_freq_cat") |>
#'   mutate(across(where(is.character), as.factor)) |>
#'   mutate(across(where(is.factor), ~ set_reference_level(., exposure)))
#'
#' freq <- glm(
#'   nclaims ~ bm + age_policyholder_freq_cat,
#'   offset = log(exposure),
#'   family = poisson(),
#'   data = dat
#' )
#'
#' sev <- glm(
#'   amount ~ zip,
#'   weights = nclaims,
#'   family = Gamma(link = "log"),
#'   data = dat |> filter(amount > 0)
#' )
#'
#' premium_df <- dat |>
#'   add_prediction(freq, sev) |>
#'   mutate(premium = pred_nclaims_freq * pred_amount_sev)
#'
#' burn_unrestricted <- glm(
#'   premium ~ zip + bm + age_policyholder_freq_cat,
#'   weights = exposure,
#'   family = Gamma(link = "log"),
#'   data = premium_df
#' )
#'
#' ref <- prepare_refinement(burn_unrestricted) |>
#'   add_smoothing(
#'     model_variable = "age_policyholder_freq_cat",
#'     source_variable = "age_policyholder",
#'     breaks = seq(18, 95, 5),
#'     smoothing = "gam",
#'     k = 6,
#'     weights = "exposure"
#'   )
#'
#' # Limit the visible range without changing the fitted smoothing curve.
#' autoplot(ref, x_max = 80)
#' }
#'
#' @export
add_smoothing <- function(model, model_variable = NULL, source_variable = NULL,
                          degree = NULL, breaks = NULL, smoothing = "spline",
                          k = NULL, weights = NULL, tariff_class = NULL,
                          rating_variable = NULL, x_cut = NULL, x_org = NULL) {
  .assert_refinement(model)

  if (!is.null(tariff_class)) {
    lifecycle::deprecate_warn(
      when = "0.9.0",
      what = "add_smoothing(tariff_class)",
      with = "add_smoothing(model_variable)"
    )
    if (!is.null(model_variable)) {
      stop("Use only one of 'model_variable' and deprecated 'tariff_class'.", call. = FALSE)
    }
    model_variable <- tariff_class
  }

  if (!is.null(rating_variable)) {
    lifecycle::deprecate_warn(
      when = "0.9.0",
      what = "add_smoothing(rating_variable)",
      with = "add_smoothing(source_variable)"
    )
    if (!is.null(source_variable)) {
      stop("Use only one of 'source_variable' and deprecated 'rating_variable'.", call. = FALSE)
    }
    source_variable <- rating_variable
  }

  if (!is.null(x_cut)) {
    lifecycle::deprecate_warn(
      when = "0.9.0",
      what = "add_smoothing(x_cut)",
      with = "add_smoothing(model_variable)"
    )
    if (!is.null(model_variable)) {
      stop("Use only one of 'model_variable' and deprecated 'x_cut'.", call. = FALSE)
    }
    model_variable <- x_cut
  }

  if (!is.null(x_org)) {
    lifecycle::deprecate_warn(
      when = "0.9.0",
      what = "add_smoothing(x_org)",
      with = "add_smoothing(source_variable)"
    )
    if (!is.null(source_variable)) {
      stop("Use only one of 'source_variable' and deprecated 'x_org'.", call. = FALSE)
    }
    source_variable <- x_org
  }

  if (!is.numeric(breaks) || length(breaks) == 0 ||
      anyNA(breaks) || any(!is.finite(breaks))) {
    stop("'breaks' must be a numeric vector with finite values.", call. = FALSE)
  }
  if (length(unique(breaks)) < 2) {
    stop("'breaks' must contain at least two distinct values.", call. = FALSE)
  }
  if (is.unsorted(breaks, strictly = TRUE)) {
    stop("'breaks' must be strictly increasing.", call. = FALSE)
  }

  .assert_column_name(model_variable, "model_variable", model$base$data)
  .assert_column_name(source_variable, "source_variable", model$base$data)
  .assert_optional_column_name(weights, "weights", model$base$data)
  model_variable_missing <- sum(is.na(model$base$data[[model_variable]]))
  if (model_variable_missing > 0L) {
    missing_label <- if (model_variable_missing == 1L) {
      "missing value"
    } else {
      "missing values"
    }
    stop(
      "The `model_variable` column `", model_variable, "` contains ",
      model_variable_missing, " ", missing_label, ". Smoothing cannot be ",
      "applied to missing values; remove or impute them first.",
      call. = FALSE
    )
  }
  .assert_smoothing_interval_levels(model$base$model, model_variable)
  .assert_single_numeric(degree, "degree", allow_null = TRUE, positive = TRUE, whole = TRUE)
  .assert_single_numeric(k, "k", allow_null = TRUE, positive = TRUE, whole = TRUE)

  if (!.is_single_string(smoothing) || !smoothing %in% .allowed_smoothing_methods) {
    stop(
      "'smoothing' must be one of: ",
      paste(.allowed_smoothing_methods, collapse = ", "),
      call. = FALSE
    )
  }

  if (smoothing %in% c(
    "spline", "gam", "mpi", "mpd", "cx", "cv", "micx", "micv", "mdcx", "mdcv"
  )) {
    borders_model <- cut_borders_model(model$base$model, model_variable)
    .validate_smoothing_complexity(
      covariates = borders_model["avg_"],
      source_variable = source_variable,
      smoothing = smoothing,
      k = k,
      degree = degree,
      response = borders_model$estimate
    )
  }

  .add_step(model, list(
    id = .next_step_id(model),
    type = "smoothing",
    variable = model_variable,
    x_cut = model_variable,
    x_org = source_variable,
    model_variable = model_variable,
    source_variable = source_variable,
    tariff_class = model_variable,
    rating_variable = source_variable,
    degree = degree,
    breaks = breaks,
    smoothing = smoothing,
    k = k,
    weights = weights,
    edit = NULL
  ))
}


#' Deprecated smoothing helper
#'
#' @description
#' `smooth_coef()` is deprecated as of version 0.9.0. Use
#' [add_smoothing()] instead.
#'
#' \preformatted{
#' prepare_refinement(model) |>
#'   add_smoothing(...) |>
#'   refit()
#' }
#'
#' @seealso [add_smoothing()], [prepare_refinement()], [refit()]
#'
#' @param model A fitted model object.
#' @param x_cut Deprecated model variable used in the GLM.
#' @param x_org Deprecated source variable used to fit the smoothing curve.
#' @param degree Deprecated polynomial degree.
#' @param breaks Deprecated smoothing break points.
#' @param smoothing Deprecated smoothing type.
#' @param k Deprecated spline basis dimension.
#' @param weights Deprecated weights column.
#'
#' @return A legacy smooth object. New code should use [prepare_refinement()],
#'   [add_smoothing()], and [refit()].
#'
#' @export
#' @keywords internal
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
    model_variable = x_cut,
    source_variable = x_org,
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
#' Manually adjusts a smoothing step that was previously added with
#' [add_smoothing()]. This is intended for actuarial review of a smoothed tariff
#' curve, for example to flatten an unstable segment, align the end points of an
#' interval, or add extra control points where expert judgement should guide the
#' curve.
#' The adjusted smoothing is applied when [refit()] is called.
#'
#' @details
#' Use `model_variable` or `step` to identify the smoothing step to edit. The
#' interval from `from` to `to` defines the part of the source variable range
#' that should be changed. `from_value` and `to_value` can be used to force the
#' curve values at the interval boundaries. `control_positions` and
#' `control_values` add additional points that the edited curve should follow
#' inside the interval.
#'
#' @param model Object of class `rating_refinement`, usually created with
#'   [prepare_refinement()]. Legacy `smooth` and `restricted` objects are still
#'   accepted for backwards compatibility.
#' @param model_variable Character string. The `model_variable` of the smoothing
#'   step to edit. Required when more than one smoothing step exists and `step`
#'   is not supplied.
#' @param step Optional numeric index of the smoothing step to edit.
#' @param from,to Numeric values giving the start and end of the source-variable
#'   interval to modify.
#' @param from_value,to_value Optional numeric values used to override the
#'   smoothed curve value at `from` and `to`.
#' @param control_positions,control_values Optional numeric vectors of equal
#'   length. These define additional points that the edited smoothing curve
#'   should pass through.
#' @param allow_extrapolation Logical. Whether edits may extend beyond the
#'   observed source-variable range.
#' @param extrapolation_step Optional positive numeric scalar used to set the
#'   spacing of extra break points when extrapolation is allowed.
#'
#' @author Martin Haringa
#'
#' @return Object of class `rating_refinement`.
#'
#' @examples
#' set.seed(42)
#' driver_age <- rep(seq(20, 59), each = 4)
#' exposure <- rep(1, length(driver_age))
#' age_band <- cut(
#'   driver_age,
#'   breaks = c(18, 30, 40, 50, 60),
#'   include.lowest = TRUE
#' )
#' expected_claims <- exp(
#'   -1.7 + 0.018 * (driver_age - 20) + 0.0006 * (driver_age - 40)^2
#' )
#' portfolio <- data.frame(
#'   claims = rpois(length(driver_age), exposure * expected_claims),
#'   exposure = exposure,
#'   driver_age = driver_age,
#'   age_band = age_band
#' )
#'
#' model <- glm(
#'   claims ~ age_band + offset(log(exposure)),
#'   family = poisson(),
#'   data = portfolio
#' )
#'
#' refined <- prepare_refinement(model, data = portfolio) |>
#'   add_smoothing(
#'     model_variable = "age_band",
#'     source_variable = "driver_age",
#'     breaks = c(18, 30, 40, 50, 60),
#'     degree = 2,
#'     weights = "exposure"
#'   ) |>
#'   edit_smoothing(
#'     model_variable = "age_band",
#'     from = 30,
#'     to = 50,
#'     from_value = 1.00,
#'     to_value = 1.10,
#'     control_positions = c(40),
#'     control_values = c(1.05)
#'   )
#'
#' refined_model <- refit(refined)
#'
#' @export
edit_smoothing <- function(model,
                           model_variable = NULL,
                           step = NULL,
                           from, to,
                           from_value = NULL, to_value = NULL,
                           control_positions = NULL,
                           control_values = NULL,
                           allow_extrapolation = FALSE,
                           extrapolation_step = NULL) {
  if (inherits(model, c("smooth", "restricted"))) {
    lifecycle::deprecate_warn(
      when = "0.9.0",
      what = "edit_smoothing(model = <smooth/restricted>)",
      with = "prepare_refinement() |> add_smoothing() |> edit_smoothing() |> refit()"
    )
    model <- as_refinement(model)
  }

  .assert_refinement(model)

  .assert_single_numeric(from, "from", allow_null = FALSE)
  .assert_single_numeric(to, "to", allow_null = FALSE)
  if (from >= to) {
    stop("'from' must be smaller than 'to'.", call. = FALSE)
  }
  .assert_single_numeric(from_value, "from_value", allow_null = TRUE)
  .assert_single_numeric(to_value, "to_value", allow_null = TRUE)
  .assert_single_logical(allow_extrapolation, "allow_extrapolation")
  .assert_single_numeric(extrapolation_step, "extrapolation_step",
                         allow_null = TRUE, positive = TRUE)

  if (is.null(control_positions)) control_positions <- numeric()
  if (is.null(control_values)) control_values <- numeric()

  if (!is.numeric(control_positions) || anyNA(control_positions) ||
      any(!is.finite(control_positions))) {
    stop("'control_positions' must be a numeric vector with finite values.",
         call. = FALSE)
  }
  if (!is.numeric(control_values) || anyNA(control_values) ||
      any(!is.finite(control_values))) {
    stop("'control_values' must be a numeric vector with finite values.",
         call. = FALSE)
  }
  if (length(control_positions) != length(control_values)) {
    stop("'control_positions' and 'control_values' must have the same length.",
         call. = FALSE)
  }

  idx <- .find_step(model, type = "smoothing",
                    variable = model_variable,
                    step = step)

  if (length(control_positions) > 0 &&
      any(control_positions <= from | control_positions >= to)) {
    stop("'control_positions' must lie between 'from' and 'to'.",
         call. = FALSE)
  }

  model$steps[[idx]]$edit <- utils::modifyList(
    model$steps[[idx]]$edit %||% list(),
    list(
      from = from,
      to = to,
      from_value = from_value,
      to_value = to_value,
      control_positions = control_positions,
      control_values = control_values,
      allow_extrapolation = allow_extrapolation,
      extrapolation_step = extrapolation_step
    )
  )

  model
}


#' Add expert-based relativities to a refinement workflow
#'
#' @description
#' Splits an existing model variable into more detailed tariff segments using
#' supplied relativities. This is useful when the GLM is fitted on a coarser
#' rating factor for credibility or stability, but the final tariff needs a
#' more detailed split that is based on portfolio exposure, expert judgement or
#' externally agreed relativities.
#'
#' @details
#' `model_variable` is the variable already used in the GLM. `split_variable` is
#' the more detailed variable in the portfolio data that will be used to split
#' one or more levels of `model_variable`. The `relativities` argument should be
#' a named list describing those splits, usually built with [relativities()] and
#' [split_level()].
#'
#' The step is stored on the `rating_refinement` object and is applied when
#' [refit()] is called. When `normalize = TRUE`, the supplied relativities are
#' normalised using exposure so that the refined split keeps the original level
#' effect on average. This helps prevent an expert split from unintentionally
#' changing the total premium level for the original model group.
#'
#' **When to use**
#'
#' `add_relativities()` is intended for refinement within an already reasonably
#' homogeneous GLM segment. It redistributes an existing coefficient across
#' sublevels using exposure-weighted relativities, while preserving the overall
#' level of the original coefficient. This is useful for mild heterogeneity,
#' commercial refinement, monotonic tariff differentiation, or expert-based
#' segmentation within a stable risk group where the original GLM coefficient is
#' broadly representative.
#'
#' **Limitations**
#'
#' The method is not a substitute for creating a separate risk segment when the
#' original GLM coefficient is itself distorted. For example, suppose a broad
#' industry segment contains many relatively stable businesses, but a few
#' chemical companies drive most of the losses while representing little
#' exposure. The fitted industry coefficient may then be dominated by the
#' chemical companies' experience. Applying exposure-weighted relativities inside
#' that segment may barely reduce the coefficient for the large exposure group,
#' because the original coefficient is already pulled upward by the outlier
#' subgroup.
#'
#' In that situation it is often better to create a separate GLM factor level,
#' derive a separate tariff segment, or apply explicit segmentation or
#' acceptation rules, instead of relying only on `add_relativities()`.
#'
#' @param model Object of class `rating_refinement`, usually created with
#'   [prepare_refinement()].
#' @param model_variable Character string. Existing variable in the GLM. Levels
#'   of this variable can be split into more detailed tariff segments.
#' @param split_variable Character string. More granular portfolio variable that
#'   defines the detailed groups inside `model_variable`.
#' @param relativities Named list of data frames, usually created with
#'   [relativities()] and [split_level()].
#' @param exposure Character string. Exposure column used for weighting and,
#'   when requested, normalisation.
#' @param normalize Logical. If `TRUE`, normalise the supplied relativities by
#'   exposure within each split model level.
#'
#' @author Martin Haringa
#'
#' @return Object of class `rating_refinement`.
#'
#' @examples
#' portfolio <- data.frame(
#'   claims = c(1, 2, 1, 3, 2, 4),
#'   exposure = rep(1, 6),
#'   construction = factor(c("residential", "commercial", "residential",
#'                           "commercial", "residential", "commercial")),
#'   construction_detail = factor(c("flat", "shop", "house",
#'                                  "office", "flat", "shop"))
#' )
#'
#' model <- glm(
#'   claims ~ construction + offset(log(exposure)),
#'   family = poisson(),
#'   data = portfolio
#' )
#'
#' relativities <- relativities(
#'   split_level(
#'     "residential",
#'     new_levels = c("flat", "house"),
#'     relativities = c(0.95, 1.05)
#'   )
#' )
#'
#' refined <- prepare_refinement(model, data = portfolio) |>
#'   add_relativities(
#'     model_variable = "construction",
#'     split_variable = "construction_detail",
#'     relativities = relativities,
#'     exposure = "exposure"
#'   )
#'
#' @export
add_relativities <- function(model,
                             model_variable,
                             split_variable,
                             relativities,
                             exposure,
                             normalize = TRUE) {
  .assert_refinement(model)

  .assert_column_name(model_variable, "model_variable", model$base$data)
  .assert_column_name(split_variable, "split_variable", model$base$data)
  .assert_column_name(exposure, "exposure", model$base$data)
  .assert_single_logical(normalize, "normalize")

  .check_relativities(relativities)

  .add_step(model, list(
    id = .next_step_id(model),
    type = "relativities",
    variable = model_variable,
    model_variable = model_variable,
    split_variable = split_variable,
    risk_factor = model_variable,
    risk_factor_split = split_variable,
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

    ebreak <- edit$extrapolation_step
    if (is.null(ebreak)) {
      ebreak <- default_extrapolation_break_size(df_poly, factor = 1)
    }

    changed <- change_xy(
      borders_model = df_poly,
      x_org = x_org,
      x1 = edit$from,
      x2 = edit$to,
      overwrite_y1 = edit$from_value,
      overwrite_y2 = edit$to_value,
      middle_x = edit$control_positions %||% numeric(),
      middle_y = edit$control_values %||% numeric(),
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
  refined_data <- x$data_restricted

  weights <- NULL
  if (!is.null(lst_call$weights)) {
    weights <- eval(lst_call$weights, envir = refined_data, enclos = parent.frame())
  }

  glm_args <- c(
    list(
      formula = x$formula_restricted,
      family = x$model_out$family,
      data = refined_data,
      offset = NULL
    ),
    list(...)
  )

  if (!is.null(weights)) {
    glm_args$weights <- weights
  }

  y <- do.call(
    stats::glm,
    glm_args
  )

  y$call$formula <- x$formula_restricted
  y$call$data <- quote(refined_data)

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

  restriction_map <- NULL

  if (!is.null(x$mgd_rst) && length(x$mgd_rst) > 0) {
    rst_pairs <- lapply(x$mgd_rst, function(z) {
      z <- unique(as.character(z))
      if (length(z) < 2) {
        return(NULL)
      }

      data.frame(
        source_var = z[1],
        risk_factor = z[2],
        stringsAsFactors = FALSE
      )
    })

    rst_pairs <- rst_pairs[!vapply(rst_pairs, is.null, logical(1))]

    if (length(rst_pairs) > 0) {
      restriction_map <- unique(do.call(rbind, rst_pairs))
      rownames(restriction_map) <- NULL
    }
  }

  attr(y, "new_col_nm") <- x$new_col_nm
  attr(y, "old_col_nm") <- x$old_col_nm
  attr(y, "rf") <- rf2
  attr(y, "mgd_smt") <- x$mgd_smt
  attr(y, "mgd_rst") <- x$mgd_rst
  attr(y, "restriction_map") <- restriction_map
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
#' Applies the refinement steps stored in a `rating_refinement` object and
#' returns a refitted GLM. This is the final step in the refinement workflow
#' after [prepare_refinement()], [add_smoothing()], [add_restriction()] or
#' [add_relativities()] have been used to define the proposed tariff structure.
#'
#' @details
#' Refinement steps are not applied to the fitted model immediately. They are
#' collected on the `rating_refinement` object so they can be inspected first,
#' for example with [autoplot.rating_refinement()]. `refit()` then applies the
#' steps in order, updates the model formula and data, and calls [stats::glm()]
#' with the original model family and any additional arguments passed through
#' `...`.
#'
#' With `intercept_only = FALSE`, the refined GLM is fitted with the remaining
#' free model terms that are still present after applying the refinement steps.
#' With `intercept_only = TRUE`, remaining original model effects are fixed as
#' offsets based on the existing fitted relativities. The refit then estimates
#' only the intercept. This can be useful when the relative tariff structure
#' should remain fixed and only the overall premium level should be recalibrated.
#'
#' @param object Object of class `rating_refinement`, usually created with
#'   [prepare_refinement()].
#' @param intercept_only Logical. If `FALSE` (default), fit the refined model
#'   with remaining model terms still free. If `TRUE`, keep remaining existing
#'   relativities fixed as offsets and estimate only the intercept.
#' @param ... Additional arguments passed to [stats::glm()], such as `control`.
#'
#' @author Martin Haringa
#'
#' @return A refitted object of class `glm`. The returned model also stores
#'   attributes used by [rating_table()] and [rating_grid()] to recognise
#'   refined rating factors, fixed relativities and smoothing metadata.
#'
#' @examples
#' zip_df <- data.frame(
#'   zip = c(0, 1, 2, 3),
#'   zip_adj = c(0.8, 0.9, 1.0, 1.2)
#' )
#'
#' model <- glm(
#'   nclaims ~ zip + offset(log(exposure)),
#'   family = poisson(),
#'   data = MTPL
#' )
#'
#' refined_model <- prepare_refinement(model) |>
#'   add_restriction(zip_df) |>
#'   refit(intercept_only = TRUE)
#'
#' @export
refit <- function(object, intercept_only = FALSE, ...) {
  .assert_refinement(object)
  .assert_single_logical(intercept_only, "intercept_only")

  state <- .make_exec_state(object)

  if (length(object$steps) == 0) {
    warning("No refinement steps were added; returning refit of original model.", call. = FALSE)
  }

  for (step in object$steps) {
    state <- .apply_refinement_step(state, step)
  }

  .fit_refined_glm(state, intercept_only = intercept_only, ...)
}

#' Deprecated refit wrapper
#'
#' @description
#' `refit_glm()` is deprecated as of version 0.9.0. Use [refit()] instead.
#'
#' @param x Object of class `rating_refinement`, `restricted` or `smooth`.
#' @param intercept_only Logical.
#' @param ... Other arguments.
#'
#' @return Object of class `glm`.
#'
#' @export
#' @keywords internal
refit_glm <- function(x, intercept_only = FALSE, ...) {
  lifecycle::deprecate_warn("0.9.0", "refit_glm()", "refit()")
  .assert_single_logical(intercept_only, "intercept_only")

  if (inherits(x, "rating_refinement")) {
    return(refit(x, intercept_only = intercept_only, ...))
  }

  if (!inherits(x, c("restricted", "smooth"))) {
    stop("Input must be of class 'rating_refinement', 'restricted' or 'smooth'.",
         call. = FALSE)
  }

  .legacy_refit_glm(x, intercept_only = intercept_only, ...)
}

#' Deprecated alias for `refit_glm()`
#'
#' @description
#' `update_glm()` is deprecated as of version 0.8.0. Use [refit()] for the new
#' refinement workflow.
#'
#' @inheritParams refit_glm
#' @return See [refit_glm()].
#'
#' @export
#' @keywords internal
update_glm <- function(x, intercept_only = FALSE, ...) {
  lifecycle::deprecate_warn("0.8.0", "update_glm()", "refit()")
  refit_glm(x, intercept_only = intercept_only, ...)
}
