
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.is_refinement <- function(x) {
  inherits(x, "rating_refinement")
}

.assert_refinement <- function(x) {
  if (!inherits(x, "rating_refinement")) {
    if (inherits(x, "glm")) {
      model_type <- if (inherits(x, c("refitrestricted", "refitsmooth"))) {
        "a refitted GLM returned by `refit()`"
      } else {
        "a fitted GLM"
      }
      stop(
        "Refinement steps cannot be added to or edited on ", model_type, ". ",
        "Call `prepare_refinement()` first and retain the returned ",
        "`rating_refinement` object. Use `refit()` to create a fitted GLM ",
        "without replacing the refinement specification.",
        call. = FALSE
      )
    }
    stop(
      "Input must be a `rating_refinement` object created with ",
      "`prepare_refinement()`.",
      call. = FALSE
    )
  }
  invisible(TRUE)
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

.closest_refinement_value <- function(value, choices) {
  choices <- unique(as.character(choices))
  choices <- choices[!is.na(choices) & nzchar(choices)]
  if (length(choices) == 0L) {
    return(NULL)
  }

  value_cmp <- tolower(trimws(as.character(value)))
  choices_cmp <- tolower(trimws(choices))
  distances <- as.numeric(utils::adist(value_cmp, choices_cmp))
  best <- which.min(distances)
  scale <- max(nchar(value_cmp), nchar(choices_cmp[best]), 1L)

  if (distances[best] <= 2L || distances[best] / scale <= 0.20) {
    choices[best]
  } else {
    NULL
  }
}

.assert_column_name <- function(x, arg, data) {
  if (!.is_single_string(x)) {
    stop("'", arg, "' must be a single non-empty character string.", call. = FALSE)
  }
  if (!x %in% names(data)) {
    suggestion <- .closest_refinement_value(x, names(data))
    message <- paste0(
      "Column `", x, "`, supplied through `", arg,
      "`, was not found in the refinement data."
    )
    if (!is.null(suggestion)) {
      message <- paste0(message, " Did you mean `", suggestion, "`?")
    }
    stop(message, call. = FALSE)
  }
  invisible(TRUE)
}

.assert_optional_column_name <- function(x, arg, data) {
  if (is.null(x)) {
    return(invisible(TRUE))
  }
  .assert_column_name(x, arg, data)
}

.assert_smoothing_model_variable <- function(model, model_variable) {
  if (!.is_single_string(model_variable)) {
    stop(
      "'model_variable' must be a single non-empty character string.",
      call. = FALSE
    )
  }

  model_terms <- unique(as.character(model$base$rating_factors$risk_factor))
  model_terms <- setdiff(model_terms, "(Intercept)")
  if (!model_variable %in% model_terms) {
    suggestion <- .closest_refinement_value(model_variable, model_terms)
    message <- paste0(
      "Variable `", model_variable, "`, supplied through `model_variable`, ",
      "is not a model term in the GLM used by `prepare_refinement()`."
    )
    if (!is.null(suggestion)) {
      message <- paste0(message, " Did you mean `", suggestion, "`?")
    }
    stop(message, call. = FALSE)
  }

  if (!model_variable %in% names(model$base$data)) {
    stop(
      "Model term `", model_variable, "` is not available as a column in ",
      "the refinement data.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.assert_restriction_relativities <- function(restrictions, value_col) {
  values <- restrictions[[value_col]]
  if (!is.numeric(values)) {
    value_type <- class(values)[1]
    stop(
      "The relativity column `", value_col, "` must be numeric, but it is ",
      value_type, ". Supply relativities as numeric values, for example `1` ",
      "instead of `\"1\"`.",
      call. = FALSE
    )
  }

  invalid <- sum(!is.finite(values))
  if (invalid > 0L) {
    value_label <- if (invalid == 1L) "value" else "values"
    stop(
      "The relativity column `", value_col, "` must contain finite numeric ",
      "relativities, but it contains ", invalid, " missing or non-finite ",
      value_label, ".",
      call. = FALSE
    )
  }

  invisible(TRUE)
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

.smoothing_method_codes <- c(
  spline = "spline",
  poly = "poly",
  gam = "gam",
  increasing = "mpi",
  decreasing = "mpd",
  convex = "cx",
  concave = "cv",
  increasing_convex = "micx",
  increasing_concave = "micv",
  decreasing_convex = "mdcx",
  decreasing_concave = "mdcv"
)

.smoothing_method_aliases <- c(
  mpi = "increasing",
  mpd = "decreasing",
  cx = "convex",
  cv = "concave",
  micx = "increasing_convex",
  micv = "increasing_concave",
  mdcx = "decreasing_convex",
  mdcv = "decreasing_concave"
)

.allowed_smoothing_methods <- c(
  names(.smoothing_method_codes),
  names(.smoothing_method_aliases)
)

.resolve_smoothing_method <- function(smoothing) {
  canonical <- if (smoothing %in% names(.smoothing_method_aliases)) {
    unname(.smoothing_method_aliases[[smoothing]])
  } else {
    smoothing
  }

  list(
    method = canonical,
    code = unname(.smoothing_method_codes[[canonical]])
  )
}

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

.validate_smoothing_source_and_breaks <- function(data, source_variable, breaks,
                                                  model, model_variable) {
  source <- data[[source_variable]]

  if (!is.numeric(source)) {
    stop(
      "The `source_variable` column `", source_variable,
      "` must be numeric to apply smoothing.",
      call. = FALSE
    )
  }

  missing_count <- sum(is.na(source))
  non_finite_count <- sum(!is.finite(source) & !is.na(source))
  if (missing_count > 0L || non_finite_count > 0L) {
    details <- character()
    if (missing_count > 0L) {
      details <- c(
        details,
        paste0(
          missing_count, " missing ",
          if (missing_count == 1L) "value" else "values"
        )
      )
    }
    if (non_finite_count > 0L) {
      details <- c(
        details,
        paste0(
          non_finite_count, " non-finite ",
          if (non_finite_count == 1L) "value" else "values",
          " (`Inf` or `-Inf`)"
        )
      )
    }

    stop(
      "The `source_variable` column `", source_variable, "` contains ",
      paste(details, collapse = " and "), ". Smoothing requires finite ",
      "numeric values; remove or impute these values first.",
      call. = FALSE
    )
  }

  source_range <- range(source)
  break_range <- range(breaks)
  below <- sum(source < break_range[1])
  above <- sum(source > break_range[2])
  if (below > 0L || above > 0L) {
    outside <- c(
      if (below > 0L) paste0(below, " below the first break"),
      if (above > 0L) paste0(above, " above the last break")
    )
    stop(
      "`breaks` do not cover all values in the `source_variable` column `",
      source_variable, "`. The observed range is ",
      format(source_range[1], trim = TRUE), " to ",
      format(source_range[2], trim = TRUE), ", while `breaks` cover ",
      format(break_range[1], trim = TRUE), " to ",
      format(break_range[2], trim = TRUE), ". There are ",
      paste(outside, collapse = " and "),
      ". Extend `breaks` so every portfolio observation is assigned to a ",
      "tariff segment.",
      call. = FALSE
    )
  }

  borders <- suppressMessages(cut_borders_model(model, model_variable))
  model_range <- c(min(borders$start_), max(borders$end_))
  if (break_range[1] < model_range[1] || break_range[2] > model_range[2]) {
    formatted_model_range <- format(
      model_range,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
    warning(
      "The supplied `breaks` extend beyond the fitted GLM range (",
      formatted_model_range[1], "\u2013", formatted_model_range[2], "). ",
      "New intervals outside this range are based on extrapolation rather ",
      "than observed model estimates. Use `edit_smoothing()` to adjust the ",
      "extrapolated part of the smoothing curve.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.make_refinement <- function(base, steps = list(), legacy = list()) {
  package_version <- tryCatch(
    as.character(utils::packageVersion("insurancerating")),
    error = function(e) NA_character_
  )

  structure(
    list(
      base = base,
      steps = steps,
      legacy = legacy,
      metadata = list(
        package = "insurancerating",
        package_version = package_version,
        created_at = Sys.time()
      )
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

.replace_refinement_offset <- function(formula_no_offset, offset_term,
                                       old_term, new_term) {
  old_offset <- paste0("log(", old_term, ")")
  new_offset <- paste0("log(", new_term, ")")

  if (is.null(offset_term) ||
      !grepl(old_offset, offset_term, fixed = TRUE)) {
    stop(
      "Restricted model variable `", old_term,
      "` is not present in the current refinement offset.",
      call. = FALSE
    )
  }

  offset_new <- sub(old_offset, new_offset, offset_term, fixed = TRUE)
  formula_new <- update(
    formula_no_offset,
    paste0("~ . + offset(", offset_new, ")")
  )

  list(
    formula = formula_new,
    formula_no_offset = formula_no_offset,
    offset = offset_new
  )
}

.restriction_variable_map <- function(steps) {
  restriction_steps <- Filter(
    function(step) identical(step$type, "restriction") &&
      is.data.frame(step$restrictions) &&
      ncol(step$restrictions) == 2L,
    steps
  )

  if (length(restriction_steps) == 0L) {
    return(data.frame(
      source_variable = character(),
      restricted_variable = character(),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, lapply(restriction_steps, function(step) {
    data.frame(
      source_variable = names(step$restrictions)[1],
      restricted_variable = names(step$restrictions)[2],
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

.resolve_relativities_source <- function(model, model_variable) {
  restriction_map <- .restriction_variable_map(model$steps)

  explicit_restricted <- which(
    restriction_map$restricted_variable == model_variable
  )
  if (length(explicit_restricted) > 0L) {
    mapping <- restriction_map[
      utils::tail(explicit_restricted, 1L),
      ,
      drop = FALSE
    ]
    return(list(
      requested_model_variable = model_variable,
      source_model_variable = mapping$source_variable,
      effective_model_variable = model_variable
    ))
  }

  restricted_source <- which(
    restriction_map$source_variable == model_variable
  )
  if (length(restricted_source) > 0L) {
    mapping <- restriction_map[
      utils::tail(restricted_source, 1L),
      ,
      drop = FALSE
    ]
    return(list(
      requested_model_variable = model_variable,
      source_model_variable = model_variable,
      effective_model_variable = mapping$restricted_variable
    ))
  }

  list(
    requested_model_variable = model_variable,
    source_model_variable = model_variable,
    effective_model_variable = model_variable
  )
}

.stop_missing_relativities_source <- function(model, model_variable) {
  restriction_map <- .restriction_variable_map(model$steps)
  choices <- unique(c(
    names(model$base$data),
    restriction_map$restricted_variable
  ))
  suggestion <- .closest_refinement_value(model_variable, choices)
  message <- paste0(
    "Column `", model_variable, "`, supplied through `model_variable`, was ",
    "not found in the refinement data and does not identify a restricted ",
    "variable created by an earlier `add_restriction()` step."
  )
  if (!is.null(suggestion)) {
    message <- paste0(message, " Did you mean `", suggestion, "`?")
  }
  stop(message, call. = FALSE)
}

.closest_refinement_level <- function(level, choices) {
  .closest_refinement_value(level, choices)
}

.stop_missing_split_levels <- function(levels, split_variable, choices) {
  entries <- vapply(levels, function(level) {
    suggestion <- .closest_refinement_level(level, choices)
    line <- paste0("- `", level, "`")
    if (!is.null(suggestion)) {
      line <- paste0(line, ". Did you mean `", suggestion, "`?")
    }
    line
  }, character(1))

  noun <- if (length(levels) == 1L) "level" else "levels"
  verb <- if (length(levels) == 1L) "does" else "do"
  stop(
    "The following ", noun, " supplied in `relativities` ", verb,
    " not occur in `split_variable` `", split_variable, "`:\n",
    paste(entries, collapse = "\n"),
    call. = FALSE
  )
}

.stop_missing_model_levels <- function(levels, model_variable, choices) {
  entries <- vapply(levels, function(level) {
    suggestion <- .closest_refinement_level(level, choices)
    line <- paste0("- `", level, "`")
    if (!is.null(suggestion)) {
      line <- paste0(line, ". Did you mean `", suggestion, "`?")
    }
    line
  }, character(1))

  noun <- if (length(levels) == 1L) "category" else "categories"
  verb <- if (length(levels) == 1L) "does" else "do"
  stop(
    "The following ", noun, " supplied in `relativities` ", verb,
    " not occur in `model_variable` `", model_variable, "`:\n",
    paste(entries, collapse = "\n"),
    call. = FALSE
  )
}

.validate_relativities_levels <- function(data, source_model_variable,
                                          split_variable, relativities) {
  rel_df <- .build_relativities_df(relativities)
  model_values <- unique(as.character(data[[source_model_variable]]))
  model_values <- model_values[!is.na(model_values)]
  missing_model_levels <- setdiff(
    unique(as.character(rel_df$level)),
    model_values
  )

  if (length(missing_model_levels) > 0L) {
    .stop_missing_model_levels(
      missing_model_levels,
      source_model_variable,
      model_values
    )
  }

  split_values <- unique(as.character(data[[split_variable]]))
  split_values <- split_values[!is.na(split_values)]
  missing_levels <- setdiff(unique(as.character(rel_df$new_level)), split_values)

  if (length(missing_levels) > 0L) {
    .stop_missing_split_levels(missing_levels, split_variable, split_values)
  }

  observed_pairs <- unique(data.frame(
    level = as.character(data[[source_model_variable]]),
    new_level = as.character(data[[split_variable]]),
    stringsAsFactors = FALSE
  ))
  requested_keys <- paste(rel_df$level, rel_df$new_level, sep = "\r")
  observed_keys <- paste(
    observed_pairs$level,
    observed_pairs$new_level,
    sep = "\r"
  )
  missing_pairs <- !requested_keys %in% observed_keys

  if (any(missing_pairs)) {
    invalid <- rel_df[missing_pairs, c("level", "new_level"), drop = FALSE]
    entries <- vapply(seq_len(nrow(invalid)), function(i) {
      parent <- invalid$level[i]
      level <- invalid$new_level[i]
      parent_choices <- observed_pairs$new_level[
        observed_pairs$level == parent & !is.na(observed_pairs$level)
      ]
      suggestion <- .closest_refinement_level(level, parent_choices)
      line <- paste0("- `", level, "` within `", parent, "`")
      if (!is.null(suggestion)) {
        line <- paste0(line, ". Did you mean `", suggestion, "`?")
      }
      line
    }, character(1))

    stop(
      "The following `split_variable` levels supplied in `relativities` do ",
      "not occur within their specified `model_variable` levels:\n",
      paste(entries, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.relativities_base_coefficients <- function(state, effective_model_variable) {
  if (!is.null(state$rf_restricted_df) &&
      effective_model_variable %in%
        unique(state$rf_restricted_df$risk_factor)) {
    out <- state$rf_restricted_df[
      state$rf_restricted_df$risk_factor == effective_model_variable,
      c("level", "yhat"),
      drop = FALSE
    ]
    names(out)[2] <- "estimate"
    return(out)
  }

  state$rating_factors[
    state$rating_factors$risk_factor == effective_model_variable,
    c("level", "estimate"),
    drop = FALSE
  ]
}

.current_refinement_coefficients <- function(state, model_variable) {
  sources <- list(state$rf_restricted_df, state$new_rf, state$rating_factors)
  value_columns <- c("yhat", "yhat", "estimate")

  for (i in seq_along(sources)) {
    source <- sources[[i]]
    if (is.null(source) || !is.data.frame(source) ||
        !all(c("risk_factor", "level", value_columns[i]) %in% names(source))) {
      next
    }
    rows <- source$risk_factor == model_variable
    if (!any(rows)) {
      next
    }
    out <- source[rows, c("level", value_columns[i]), drop = FALSE]
    names(out)[2] <- "relativity"
    out$level <- as.character(out$level)
    rownames(out) <- NULL
    return(out)
  }

  NULL
}

.refinement_term_for_variable <- function(steps, model_variable) {
  model_term <- model_variable

  for (step in steps) {
    if (identical(step$type, "relativities")) {
      output_variable <- step$output_variable %||% step$display_risk_factor %||%
        step$split_variable %||% step$risk_factor_split
      if (identical(output_variable, model_variable)) {
        model_term <- step$derived_model_variable %||%
          paste0(step$source_model_variable %||% step$model_variable, "_rel")
      }
    } else if (identical(step$type, "restriction") &&
               identical(step$variable, model_variable)) {
      model_term <- names(step$restrictions)[2]
    } else if (identical(step$type, "shrinkage") &&
               identical(step$model_variable, model_variable)) {
      model_term <- step$derived_model_variable
    } else if (identical(step$type, "rebasing") &&
               identical(step$model_variable, model_variable)) {
      model_term <- step$derived_model_variable
    }
  }

  model_term
}

.shrinkage_offset_variable <- function(model) {
  offset <- tryCatch(get_offset(model), error = function(e) NULL)
  if (is.null(offset) || length(offset) != 1L) {
    return(NULL)
  }
  expression <- tryCatch(parse(text = offset)[[1]], error = function(e) NULL)
  if (!is.call(expression) || !identical(expression[[1]], as.name("log")) ||
      length(expression) != 2L || !is.symbol(expression[[2]])) {
    return(NULL)
  }
  as.character(expression[[2]])
}

.resolve_shrinkage_weight_spec <- function(model, weights) {
  data <- model$base$data

  if (!is.null(weights)) {
    if (!is.character(weights) || length(weights) != 1L || is.na(weights) ||
        !nzchar(weights)) {
      stop(
        "`weights` must be NULL, `\"equal\"`, or one column name from the refinement data.",
        call. = FALSE
      )
    }
    if (identical(weights, "equal")) {
      return(list(type = "equal", label = "equal across levels"))
    }
    if (!weights %in% names(data)) {
      stop(
        "Weight column `", weights,
        "` was not found in the refinement data.",
        call. = FALSE
      )
    }
    return(list(type = "column", column = weights, label = weights,
                inferred = FALSE))
  }

  weight_call <- model$base$model_call$weights
  if (!is.null(weight_call)) {
    return(list(
      type = "model_weights",
      expression = weight_call,
      label = paste(deparse(weight_call), collapse = " "),
      inferred = TRUE
    ))
  }

  offset_variable <- .shrinkage_offset_variable(model$base$model)
  if (!is.null(offset_variable) && offset_variable %in% names(data)) {
    return(list(
      type = "column",
      column = offset_variable,
      label = offset_variable,
      inferred = TRUE,
      inferred_from = "model offset"
    ))
  }

  stop(
    "No unambiguous shrinkage weight could be derived from the fitted GLM. ",
    "Supply a numeric column through `weights`, or use `weights = \"equal\"` ",
    "to give every risk-factor level the same weight.",
    call. = FALSE
  )
}

.validate_shrinkage_row_weights <- function(values, label) {
  if (!is.numeric(values) || anyNA(values) || any(!is.finite(values)) ||
      any(values < 0)) {
    stop(
      "Shrinkage weights from `", label,
      "` must be finite, non-missing, non-negative numeric values.",
      call. = FALSE
    )
  }
  if (!any(values > 0)) {
    stop(
      "Shrinkage weights from `", label,
      "` must contain at least one positive value.",
      call. = FALSE
    )
  }
  values
}

.shrinkage_level_weights <- function(state, model_variable, coefficients,
                                     weight_spec) {
  if (identical(weight_spec$type, "equal")) {
    return(rep(1, nrow(coefficients)))
  }

  row_weights <- if (identical(weight_spec$type, "column")) {
    state$data[[weight_spec$column]]
  } else {
    tryCatch(
      eval(
        weight_spec$expression,
        envir = state$data,
        enclos = environment(stats::formula(state$model_out))
      ),
      error = function(e) {
        stop(
          "The model weights `", weight_spec$label,
          "` could not be evaluated in the refinement data: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }
  row_weights <- .validate_shrinkage_row_weights(
    row_weights,
    weight_spec$label
  )
  if (length(row_weights) != nrow(state$data)) {
    stop(
      "Shrinkage weights from `", weight_spec$label,
      "` must have one value for every row in the refinement data.",
      call. = FALSE
    )
  }

  levels <- as.character(state$data[[model_variable]])
  if (anyNA(levels)) {
    stop(
      "Model variable `", model_variable, "` contains ", sum(is.na(levels)),
      " missing value(s). Shrinkage requires every observation to be assigned ",
      "to a risk-factor level.",
      call. = FALSE
    )
  }
  totals <- tapply(row_weights, levels, sum)
  out <- unname(totals[match(coefficients$level, names(totals))])
  out[is.na(out)] <- 0
  out
}

.calculate_shrinkage <- function(state, step) {
  model_variable <- step$model_variable
  effective_model_term <- step$effective_model_term %||% model_variable
  coefficients <- if (!identical(effective_model_term, model_variable)) {
    .current_refinement_coefficients(state, effective_model_term)
  } else {
    NULL
  }
  coefficients <- coefficients %||%
    .current_refinement_coefficients(state, model_variable)
  if (is.null(coefficients) || nrow(coefficients) < 2L) {
    stop(
      "`model_variable` `", model_variable,
      "` must identify a categorical risk factor with at least two current relativities.",
      call. = FALSE
    )
  }
  if (anyDuplicated(coefficients$level)) {
    stop(
      "Risk factor `", model_variable,
      "` contains duplicate coefficient levels in the current refinement.",
      call. = FALSE
    )
  }
  if (!is.numeric(coefficients$relativity) ||
      anyNA(coefficients$relativity) ||
      any(!is.finite(coefficients$relativity)) ||
      any(coefficients$relativity <= 0)) {
    stop(
      "Current relativities for `", model_variable,
      "` must be finite, non-missing and greater than zero.",
      call. = FALSE
    )
  }

  observed_levels <- unique(as.character(state$data[[model_variable]]))
  observed_levels <- observed_levels[!is.na(observed_levels)]
  missing_levels <- setdiff(observed_levels, coefficients$level)
  if (length(missing_levels) > 0L) {
    stop(
      "No current relativity is available for level(s) of `", model_variable,
      "`: ", paste(missing_levels, collapse = ", "),
      call. = FALSE
    )
  }

  level_weights <- .shrinkage_level_weights(
    state,
    model_variable,
    coefficients,
    step$weight_spec
  )
  if (!any(level_weights > 0)) {
    stop(
      "The aggregated shrinkage weights for `", model_variable,
      "` must contain at least one positive level weight.",
      call. = FALSE
    )
  }

  credibility <- step$credibility
  centre <- exp(stats::weighted.mean(
    log(coefficients$relativity),
    level_weights
  ))
  unscaled <- exp(
    credibility * log(coefficients$relativity) +
      (1 - credibility) * log(centre)
  )
  original_mean <- stats::weighted.mean(
    coefficients$relativity,
    level_weights
  )
  unscaled_mean <- stats::weighted.mean(unscaled, level_weights)
  normalization_factor <- original_mean / unscaled_mean
  adjusted <- unscaled * normalization_factor

  data.frame(
    level = coefficients$level,
    original_relativity = coefficients$relativity,
    weight = level_weights,
    adjusted_relativity = adjusted,
    stringsAsFactors = FALSE,
    row.names = NULL
  ) |>
    structure(
      centre = centre,
      normalization_factor = normalization_factor,
      original_weighted_mean = original_mean,
      adjusted_weighted_mean = stats::weighted.mean(adjusted, level_weights)
    )
}

.calculate_rebasing <- function(state, step) {
  model_variable <- step$model_variable
  effective_model_term <- step$effective_model_term %||% model_variable
  coefficients <- if (!identical(effective_model_term, model_variable)) {
    .current_refinement_coefficients(state, effective_model_term)
  } else {
    NULL
  }
  coefficients <- coefficients %||%
    .current_refinement_coefficients(state, model_variable)

  if (is.null(coefficients) || nrow(coefficients) < 1L) {
    stop(
      "`model_variable` `", model_variable,
      "` must identify a categorical risk factor with current relativities.",
      call. = FALSE
    )
  }
  if (anyDuplicated(coefficients$level)) {
    stop(
      "Risk factor `", model_variable,
      "` contains duplicate coefficient levels in the current refinement.",
      call. = FALSE
    )
  }
  if (!is.numeric(coefficients$relativity) ||
      anyNA(coefficients$relativity) ||
      any(!is.finite(coefficients$relativity)) ||
      any(coefficients$relativity <= 0)) {
    stop(
      "Current relativities for `", model_variable,
      "` must be finite, non-missing and greater than zero.",
      call. = FALSE
    )
  }

  observed_levels <- unique(as.character(state$data[[model_variable]]))
  observed_levels <- observed_levels[!is.na(observed_levels)]
  missing_levels <- setdiff(observed_levels, coefficients$level)
  if (length(missing_levels) > 0L) {
    stop(
      "No current relativity is available for level(s) of `", model_variable,
      "`: ", paste(missing_levels, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(step$reference_level)) {
    reference_level <- step$reference_level
    if (!reference_level %in% coefficients$level) {
      suggestion <- .closest_refinement_level(
        reference_level,
        coefficients$level
      )
      message <- paste0(
        "Reference level `", reference_level, "` does not occur in risk factor `",
        model_variable, "`."
      )
      if (!is.null(suggestion)) {
        message <- paste0(message, " Did you mean `", suggestion, "`?")
      }
      stop(message, call. = FALSE)
    }
    level_weights <- rep(NA_real_, nrow(coefficients))
    method <- "explicit"
  } else {
    level_weights <- .shrinkage_level_weights(
      state,
      model_variable,
      coefficients,
      step$weight_spec
    )
    if (!any(level_weights > 0)) {
      stop(
        "The aggregated rebasing weights for `", model_variable,
        "` must contain at least one positive level weight.",
        call. = FALSE
      )
    }
    largest <- which(level_weights == max(level_weights))
    reference_level <- coefficients$level[largest[1L]]
    method <- "largest_weight"
  }

  reference_relativity <- coefficients$relativity[
    match(reference_level, coefficients$level)
  ]
  rebased <- coefficients$relativity / reference_relativity

  data.frame(
    level = coefficients$level,
    original_relativity = coefficients$relativity,
    weight = level_weights,
    rebased_relativity = rebased,
    stringsAsFactors = FALSE,
    row.names = NULL
  ) |>
    structure(
      reference_level = reference_level,
      reference_relativity = reference_relativity,
      method = method
    )
}

.resolve_restriction_context <- function(model, variable, before_step = NULL) {
  if (is.null(before_step)) {
    before_step <- length(model$steps) + 1L
  }

  prior_indices <- seq_along(model$steps)
  prior_indices <- prior_indices[prior_indices < before_step]
  derived_indices <- prior_indices[vapply(
    model$steps[prior_indices],
    function(step) {
      (identical(step$type, "relativities") && identical(
          step$output_variable %||% step$display_risk_factor %||%
            step$split_variable %||% step$risk_factor_split,
          variable
        )) ||
        (identical(step$type, "shrinkage") &&
           identical(step$model_variable, variable)) ||
        (identical(step$type, "rebasing") &&
           identical(step$model_variable, variable))
    },
    logical(1)
  )]

  if (length(derived_indices) == 0L) {
    return(NULL)
  }

  derived_index <- utils::tail(derived_indices, 1L)
  derived_step <- model$steps[[derived_index]]

  state <- .make_exec_state(model)
  if (before_step > 1L) {
    for (i in seq_len(before_step - 1L)) {
      state <- .apply_refinement_step(state, model$steps[[i]])
    }
  }

  coefficients <- .current_refinement_coefficients(state, variable)
  if (is.null(coefficients)) {
    return(NULL)
  }
  names(coefficients)[2] <- "estimate"

  if (nrow(coefficients) == 0L) {
    return(NULL)
  }
  if (anyDuplicated(coefficients$level)) {
    stop(
      "The current refinement contains multiple relativities for level(s) of `",
      variable, "`. Resolve the preceding relativity specification before ",
      "adding a restriction.",
      call. = FALSE
    )
  }

  context <- list(
    coefficients = coefficients,
    model_term = .refinement_term_for_variable(
      model$steps[prior_indices],
      variable
    ),
    derived_from_step = derived_step$id,
    replace_refinement_offset = TRUE
  )

  if (identical(derived_step$type, "relativities")) {
    source_model_variable <- derived_step$source_model_variable %||%
      derived_step$model_variable %||%
      derived_step$risk_factor
    context$derived_source_model_variable <- source_model_variable
    context$derived_split_levels <- names(derived_step$relativities)
    context$derived_from_relativities <- TRUE
  }

  context
}

.validate_restriction_replacement <- function(model, replaces, variable,
                                                before_step = NULL) {
  if (is.null(replaces)) {
    return(invisible(TRUE))
  }
  if (!.is_single_string(replaces)) {
    stop("`replaces` must be NULL or one non-empty character string.",
         call. = FALSE)
  }
  if (identical(replaces, variable)) {
    stop(
      "`replaces` must identify the existing model variable that the new ",
      "risk factor replaces; it cannot equal the new risk factor `", variable,
      "`.",
      call. = FALSE
    )
  }

  if (is.null(before_step)) {
    before_step <- length(model$steps) + 1L
  }
  state <- .make_exec_state(model)
  prior_indices <- seq_along(model$steps)
  prior_indices <- prior_indices[prior_indices < before_step]
  if (length(prior_indices) > 0L) {
    for (i in prior_indices) {
      state <- .apply_refinement_step(state, model$steps[[i]])
    }
  }

  term_labels <- attr(stats::terms(state$formula_no_offset), "term.labels")
  related_terms <- term_labels[vapply(
    term_labels,
    function(term) {
      expression <- tryCatch(
        stats::as.formula(paste("~", term)),
        error = function(e) NULL
      )
      !is.null(expression) && replaces %in% all.vars(expression)
    },
    logical(1)
  )]

  if (!replaces %in% term_labels) {
    if (length(related_terms) > 0L) {
      stop(
        "Model variable `", replaces, "` is not a standalone main-effect term ",
        "in the current refinement formula. It occurs within: ",
        paste0("`", related_terms, "`", collapse = ", "), ". ",
        "Replace transformed terms or interactions by revising the model ",
        "specification explicitly.",
        call. = FALSE
      )
    }
    suggestion <- .closest_refinement_value(replaces, term_labels)
    message <- paste0(
      "Model variable `", replaces,
      "`, supplied through `replaces`, is not an active standalone term in ",
      "the current refinement formula."
    )
    if (!is.null(suggestion)) {
      message <- paste0(message, " Did you mean `", suggestion, "`?")
    }
    stop(message, call. = FALSE)
  }

  interaction_terms <- setdiff(related_terms, replaces)
  if (length(interaction_terms) > 0L) {
    stop(
      "Model variable `", replaces, "` also occurs in interaction term(s): ",
      paste0("`", interaction_terms, "`", collapse = ", "), ". ",
      "`add_restriction(replaces = ...)` only replaces a standalone main ",
      "effect. Revise the interaction structure explicitly before refitting.",
      call. = FALSE
    )
  }

  invisible(TRUE)
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
          split_variable = x$risk_factor_split %||% NULL,
          output_variable = x$output_variable %||%
            x$display_risk_factor %||% x$risk_factor_split %||% NULL,
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
#' Create an editable refinement specification from a fitted pricing GLM.
#' Smoothing, coefficient restrictions, shrinkage, rebasing and sublevel
#' relativities can then be added in a defined order. These steps do not alter the fitted GLM
#' until [refit()] is called.
#'
#' @details
#' `prepare_refinement()` creates a persistent refinement specification. This
#' object contains the original GLM, the corresponding model data and the
#' ordered smoothing, restriction, shrinkage, rebasing and relativity steps.
#' Retain this object
#' during actuarial review so that assumptions can be inspected, revised and
#' applied again in the same order.
#'
#' ## Actuarial interpretation
#'
#' Preparing a refinement does not change coefficients, fitted values or the
#' tariff structure. It separates the original statistical model from
#' subsequent actuarial adjustments. Each adjustment remains an explicit step
#' rather than being embedded directly in transformed data or overwritten model
#' coefficients. This supports comparison between the unrestricted model and
#' alternative refinement specifications.
#'
#' [refit()] applies the stored specification and returns a fitted GLM for model
#' diagnostics, prediction and tariff reporting. The returned GLM is a result,
#' not an editable refinement specification. Functions such as
#' [add_smoothing()], [edit_smoothing()], [add_restriction()], [add_shrinkage()],
#' [add_rebasing()] and [add_relativities()] therefore accept a
#' `rating_refinement` object and do not
#' accept an ordinary or refitted GLM directly.
#'
#' A practical iterative workflow therefore keeps both objects:
#'
#' \preformatted{
#' refinement <- prepare_refinement(model) |>
#'   add_smoothing(...)
#'
#' fitted_model <- refit(refinement)
#'
#' refinement <- refinement |>
#'   edit_smoothing(...)
#'
#' fitted_model <- refit(refinement)
#' }
#'
#' `prepare_refinement()` is normally required only once for such an iteration.
#' Calling it on a model returned by `refit()` deliberately starts a new
#' refinement workflow with the already refined model as its baseline; it does
#' not recover the earlier smoothing or restriction steps for further editing.
#'
#' @param model Object of class `glm`.
#' @param data Optional data.frame containing exactly the observations retained
#'   in the fitted GLM and all required model variables. If model fitting omitted
#'   rows because of missing values, supply the retained model data rather than
#'   the original unfiltered data. If `NULL`, the data are retrieved from the
#'   model object.
#'
#' @author Martin Haringa
#'
#' @return A `rating_refinement` object containing the original GLM, retained
#'   model data and ordered refinement specification. No GLM is fitted again
#'   until [refit()] is called.
#'
#' @seealso [summary.rating_refinement()], [add_smoothing()],
#'   [edit_smoothing()], [add_restriction()], [add_shrinkage()],
#'   [add_rebasing()], [add_relativities()], [refit()]
#'
#' @examples
#' portfolio <- data.frame(
#'   claims = c(1, 2, 1, 3, 2, 4),
#'   exposure = rep(1, 6),
#'   risk_class = factor(c("A", "B", "A", "B", "A", "B"))
#' )
#'
#' model <- glm(
#'   claims ~ risk_class + offset(log(exposure)),
#'   family = poisson(),
#'   data = portfolio
#' )
#'
#' refinement <- prepare_refinement(model, data = portfolio) |>
#'   add_restriction(data.frame(
#'     risk_class = "B",
#'     risk_class_restricted = 1.15
#'   ))
#'
#' summary(refinement)
#'
#' fitted_model <- refit(refinement)
#'
#' # Retain and revise the specification rather than editing fitted_model.
#' refinement <- refinement |>
#'   add_restriction(data.frame(
#'     risk_class = "B",
#'     risk_class_restricted = 1.10
#'   ))
#'
#' updated_model <- refit(refinement)
#' @export
prepare_refinement <- function(model, data = NULL) {
  supported <- inherits(
    model,
    c("glm", "rating_refinement", "smooth", "restricted")
  )
  if (!supported) {
    if (!is.data.frame(model)) {
      stop(
        "`model` must be a fitted `glm` object. You supplied an object of ",
        "class `", paste(class(model), collapse = "/"), "`.",
        call. = FALSE
      )
    }
    stop(
      "`model` must be a fitted `glm` object, not a data frame. Fit the ",
      "model first and then call `prepare_refinement(model)`.",
      call. = FALSE
    )
  }

  as_refinement(model, data = data)
}

#' @keywords internal
#' @export
print.rating_refinement <- function(x, ...) {
  family_name <- tools::toTitleCase(x$base$model$family$family)
  link_name <- x$base$model$family$link

  cat("<rating_refinement>\n")
  cat(
    "Base model: ", family_name, " GLM (", link_name, " link)\n",
    sep = ""
  )
  cat("Steps: ", length(x$steps), "\n", sep = "")

  if (length(x$steps) > 0) {
    for (i in seq_along(x$steps)) {
      cat(.format_refinement_step(x$steps[[i]], i), "\n", sep = "")
    }
  }
  invisible(x)
}

.format_refinement_time <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return("unknown")
  }
  format(as.POSIXct(x), "%Y-%m-%d %H:%M:%S %Z")
}

.refinement_step_details <- function(step) {
  if (identical(step$type, "restriction")) {
    restrictions <- step$restrictions
    if (is.null(restrictions) || ncol(restrictions) < 2L) {
      return(NA_character_)
    }
    values <- paste0(
      as.character(restrictions[[1]]), " = ",
      format(as.numeric(restrictions[[2]]), trim = TRUE),
      collapse = "; "
    )
    if (!is.null(step$replaces)) {
      values <- paste0("replaces ", step$replaces, "; ", values)
    }
    return(values)
  }

  if (identical(step$type, "smoothing")) {
    details <- character()
    if (!is.null(step$breaks)) {
      details <- c(details, paste0(
        length(step$breaks) - 1L, " intervals over ",
        format(min(step$breaks), trim = TRUE), " to ",
        format(max(step$breaks), trim = TRUE)
      ))
    }
    if (!is.null(step$edit)) {
      details <- c(details, paste0(nrow(step$edit), " edited values"))
    }
    return(if (length(details) == 0L) NA_character_ else {
      paste(details, collapse = "; ")
    })
  }

  if (identical(step$type, "relativities")) {
    specification <- .build_relativities_df(step$relativities)
    parents <- unique(as.character(specification$level))
    return(paste0(
      length(parents), " parent level",
      if (length(parents) == 1L) "" else "s",
      " split: ", paste(parents, collapse = ", ")
    ))
  }

  if (identical(step$type, "shrinkage")) {
    source <- step$weight_spec$label %||% step$weights %||% "unknown"
    if (isTRUE(step$weight_spec$inferred)) {
      source <- paste0(
        source,
        " (derived from ",
        step$weight_spec$inferred_from %||% "model weights",
        ")"
      )
    }
    return(paste0(
      "credibility = ", format(step$credibility, trim = TRUE),
      "; weights = ", source,
      "; weighted mean preserved"
    ))
  }

  if (identical(step$type, "rebasing")) {
    source <- if (identical(step$method, "explicit")) {
      "explicit reference"
    } else {
      paste0(
        "largest weight: ",
        step$weight_spec$label %||% step$weights %||% "unknown"
      )
    }
    return(paste0(
      "reference = ", step$reference_level,
      "; original relativity = ",
      format(step$reference_relativity, trim = TRUE),
      "; selection = ", source,
      "; relative level ratios preserved"
    ))
  }

  NA_character_
}

.refinement_steps_table <- function(steps) {
  if (length(steps) == 0L) {
    return(data.frame(
      step = integer(), type = character(), variable = character(),
      description = character(), details = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(seq_along(steps), function(i) {
    step <- steps[[i]]
    variable <- if (identical(step$type, "smoothing")) {
      step$model_variable %||% step$x_cut %||% step$variable
    } else if (identical(step$type, "relativities")) {
      step$output_variable %||% step$display_risk_factor %||%
        step$split_variable %||% step$risk_factor_split
    } else {
      step$variable %||% NA_character_
    }
    description <- sub(
      "^  [0-9]+\\. ", "", .format_refinement_step(step, i)
    )
    data.frame(
      step = i,
      type = step$type %||% "unknown",
      variable = variable %||% NA_character_,
      description = description,
      details = .refinement_step_details(step),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Summarise a prepared refinement specification
#'
#' @description
#' Describe the original GLM and the ordered actuarial adjustments stored in a
#' `rating_refinement` object before [refit()] is called. The summary records
#' what will be applied; it does not compare fitted predictions because the
#' refined GLM has not yet been estimated.
#'
#' @param object A `rating_refinement` object.
#' @param ... Currently unused.
#'
#' @return An object of class `summary.rating_refinement` containing model and
#' package metadata together with a data frame describing the refinement steps
#' in their evaluation order.
#'
#' @seealso [prepare_refinement()], [refit()], [audit_refinement()]
#' @keywords internal
#' @export
summary.rating_refinement <- function(object, ...) {
  .check_dots_empty(...)
  metadata <- object$metadata %||% list()
  out <- list(
    package = metadata$package %||% "insurancerating",
    package_version = metadata$package_version %||% NA_character_,
    created_at = metadata$created_at %||% NA,
    base_formula = object$base$formula,
    family = object$base$model$family$family,
    link = object$base$model$family$link,
    observations = nrow(object$base$data),
    offset = object$base$offset,
    n_steps = length(object$steps),
    steps = .refinement_steps_table(object$steps)
  )
  class(out) <- "summary.rating_refinement"
  out
}


#' @keywords internal
#' @export
print.summary.rating_refinement <- function(x, ...) {
  cat("Refinement specification\n\n")
  cat("Package: ", x$package, " ", x$package_version, "\n", sep = "")
  cat("Created: ", .format_refinement_time(x$created_at), "\n", sep = "")
  cat("Observations: ", format(x$observations, big.mark = ","), "\n", sep = "")
  cat("Family: ", x$family, " (", x$link, " link)\n", sep = "")
  cat("Base formula:\n  ")
  cat(paste(deparse(x$base_formula), collapse = "\n  "))
  cat("\nOffset: ", x$offset %||% "none", "\n", sep = "")
  cat("\nRefinement steps: ", x$n_steps, "\n", sep = "")
  if (x$n_steps == 0L) {
    cat("  None\n")
  } else {
    for (i in seq_len(nrow(x$steps))) {
      cat("  ", x$steps$step[i], ". ", x$steps$description[i], "\n", sep = "")
      if (!is.na(x$steps$details[i])) {
        cat("     ", x$steps$details[i], "\n", sep = "")
      }
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
#' Fix selected risk-factor levels at user-supplied relativities before the
#' refined pricing GLM is fitted. This can be appropriate when sampling
#' variation produces an implausible local effect, when an actuarial assumption
#' is supported by additional information, or when a documented tariff
#' constraint must be applied consistently.
#'
#' @details
#' `add_restriction()` stores a restriction step on a `rating_refinement`
#' object. It does not alter the fitted GLM immediately. The restriction is
#' evaluated in the recorded step order and applied when [refit()] is called.
#' Retain the refinement object when reviewing or revising the specification.
#'
#' The `restrictions` data frame identifies the risk factor to restrict by its
#' first column. This may be a variable from the original GLM or a tariff factor
#' created by an earlier refinement step. The second column contains the
#' relativities used for those levels in the refined model.
#'
#' ## Actuarial interpretation
#'
#' The restriction table may contain all levels of the model variable, or only
#' the levels that need a manual adjustment. If only a subset is supplied, the
#' missing levels are automatically filled with their current effective
#' relativities at that point in the refinement workflow. These may be the
#' original fitted GLM relativities or values produced by preceding refinement
#' steps. This makes it possible to change one level explicitly while fixing all
#' other levels at their current values.
#'
#' Levels that were not observed when the GLM was fitted can also be supplied.
#' Such a level has no coefficient estimate from the model data. Its relativity
#' is therefore an explicit tariff assumption, for example based on expert
#' judgement, external experience or a planned extension of the tariff. Existing
#' levels that are not supplied remain fixed at their fitted relativities.
#'
#' With `allow_new_levels = TRUE`, which is the default, these new tariff levels
#' are retained in the refinement metadata and subsequently shown by
#' [rating_table()]. An informational message identifies every newly added
#' level, its supplied relativity and the fact that it was not observed in the
#' model data. Set `allow_new_levels = FALSE` when the restriction table should
#' be checked strictly against the levels observed by the fitted model, for
#' example to detect spelling errors in level names.
#'
#' A variable that is present in the refinement data but was not included in the
#' fitted GLM can be added with `allow_new_risk_factors = TRUE`. In that case all
#' observed levels must have a supplied relativity. The new factor is applied as
#' a fixed tariff factor during [refit()]; its effects are not estimated from the
#' model data. This can be appropriate when an external classification or expert
#' assumption must be incorporated, such as a hail zone derived from geographic
#' information.
#'
#' `allow_new_risk_factors` does not create the portfolio variable itself. The
#' refinement data must already contain a column assigning every observation to
#' a level. This is required to apply the supplied relativities to individual
#' records.
#'
#' ## Replacing an existing model variable
#'
#' A new fixed tariff factor can either supplement the fitted GLM or replace an
#' existing model variable. Supply `replaces` when the new factor represents an
#' alternative tariff classification for an effect already present in the
#' model. During [refit()], the named existing term is removed and the supplied
#' fixed relativities are inserted in its place. With `replaces = NULL`, the new
#' factor is added alongside the existing model terms, which preserves the
#' previous behaviour.
#'
#' Supplying `replaces` is itself an explicit request to add the new risk factor,
#' so `allow_new_risk_factors = TRUE` does not also need to be supplied. The
#' replacement relationship is retained in the ordered refinement metadata and
#' is shown by `print()`, `summary()` and [audit_refinement()]. This makes clear
#' that the new factor substitutes for an earlier model effect rather than
#' adding further multiplicative differentiation.
#'
#' `replaces` is intentionally limited to a standalone main-effect term in the
#' current refinement formula. A variable used in an interaction or transformed
#' expression cannot be removed unambiguously through this argument. Such model
#' structures should be revised explicitly before the refinement is prepared.
#' This argument is therefore not a general-purpose facility for deleting model
#' terms.
#'
#' ## Updating an existing restriction
#'
#' A later call to `add_restriction()` for the same risk factor and the same
#' restricted model variable updates the restriction already stored in the
#' refinement. Relativities supplied in the later call replace the previously
#' stored values for those levels. Restrictions for levels that are not supplied
#' again are retained.
#'
#' The existing and new values are first combined and the resulting restriction
#' table is then validated as one specification. This is useful when an
#' actuarial assumption is revised during model refinement: only the affected
#' levels need to be supplied again, while the remaining tariff assumptions
#' stay unchanged. The restriction step keeps its original position in the
#' workflow, so subsequent steps such as [add_relativities()] use the revised
#' restricted coefficients.
#'
#' The second column must retain the same name when an existing restriction is
#' updated, because that name identifies the restricted model variable used by
#' [refit()]. A message reports levels whose previously supplied relativity is
#' changed.
#'
#' ## Restricting a factor created by add_relativities()
#'
#' An `output_variable` introduced by an earlier [add_relativities()] step is
#' already part of the ordered refinement specification. It is therefore not
#' treated as a new external risk factor and does not require
#' `allow_new_risk_factors = TRUE`. `add_restriction()` identifies the preceding
#' relativity step from its stored metadata and replaces the corresponding
#' derived tariff effect during [refit()].
#'
#' When only one level of such a refined variable is supplied, that level receives
#' the new relativity and every other level is fixed at the relativity produced
#' by `add_relativities()`. Mathematically, the resulting restriction therefore
#' covers all current levels. Only the explicitly supplied level changes. This
#' is useful when actuarial review supports a local adjustment but the remaining
#' expert split should not be re-estimated.
#'
#' Refinement order remains material. A restriction added after
#' `add_relativities()` operates on the derived split relativities. A
#' restriction added before `add_relativities()` instead changes the coefficient
#' basis from which the split is derived.
#'
#' @param model Object of class `rating_refinement`, created with
#'   [prepare_refinement()]. A fitted GLM, including a model returned by
#'   [refit()], is not accepted directly; retain and modify the corresponding
#'   refinement specification instead.
#' @param restrictions Data frame with exactly two columns. The first column
#'   must have the same name as the risk factor to restrict and contains the
#'   levels to adjust. This can also be the `output_variable` from an earlier
#'   [add_relativities()] step. The second column contains the replacement
#'   relativities. Levels that are not supplied are fixed at their current
#'   effective relativities.
#' @param allow_new_levels Logical. If `TRUE` (default), `restrictions` may
#'   contain levels that were not observed in the model data. Their supplied
#'   relativities are treated as explicit tariff assumptions rather than model
#'   estimates. If `FALSE`, an unknown level results in an error.
#' @param allow_new_risk_factors Logical. If `FALSE` (default), the first column
#'   of `restrictions` must identify a variable included in the fitted GLM or a
#'   tariff factor created by an earlier refinement step. Set this to `TRUE` to
#'   add an external variable that is present in the refinement data but absent
#'   from both the model and preceding refinement steps. All observed levels
#'   must then have supplied relativities, which are treated as fixed tariff
#'   assumptions.
#' @param replaces `NULL` (default) or a character string naming an existing
#'   standalone model term that the new fixed risk factor replaces. During
#'   [refit()], this term is removed before the restricted relativity column is
#'   added. Supplying `replaces` also provides the explicit opt-in required for
#'   a new risk factor; `allow_new_risk_factors = TRUE` is then unnecessary.
#'   Existing terms used in transformations or interactions cannot be replaced
#'   through this argument.
#'
#' @author Martin Haringa
#'
#' @return A `rating_refinement` object containing the stored restriction
#'   specification. The pricing GLM is not fitted again until [refit()] is
#'   called.
#'
#' @seealso [prepare_refinement()], [add_smoothing()], [add_shrinkage()],
#'   [add_rebasing()], [add_relativities()], [refit()], [rating_table()]
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
#'   postal_area = c("C", "D"),
#'   relativity = c(1.10, 1.20)
#' )
#'
#' refined <- prepare_refinement(model, data = portfolio) |>
#'   add_restriction(restrictions)
#'
#' # Postal area D was not observed in the portfolio. Its relativity is an
#' # explicit tariff assumption and becomes available after refitting.
#' refined_model <- refit(refined)
#' rating_table(refined_model, exposure = FALSE)
#'
#' # A factor absent from the fitted GLM can replace an existing model term.
#' # The portfolio must already assign every observation to a hail zone.
#' portfolio$hail_zone <- factor(c("low", "high", "low", "high", "low", "high"))
#' hail_restrictions <- data.frame(
#'   hail_zone = c("low", "high"),
#'   hail_relativity = c(1.00, 1.20)
#' )
#'
#' prepare_refinement(model, data = portfolio) |>
#'   add_restriction(
#'     hail_restrictions,
#'     replaces = "postal_area"
#'   )
#' # During refit(), hail_zone replaces postal_area rather than supplementing it.
#'
#' # Without `replaces`, a new fixed factor supplements the existing terms.
#' # A later actuarial review changes only the relativity for the low hail zone.
#' # The high-zone relativity remains 1.20 and the existing step is updated.
#' revised_hail_restrictions <- data.frame(
#'   hail_zone = "low",
#'   hail_relativity = 1.10
#' )
#'
#' hail_refinement <- prepare_refinement(model, data = portfolio) |>
#'   add_restriction(
#'     hail_restrictions,
#'     allow_new_risk_factors = TRUE
#'   ) |>
#'   add_restriction(revised_hail_restrictions)
#'
#' refit(hail_refinement)
#'
#' @export
add_restriction <- function(model, restrictions, allow_new_levels = TRUE,
                            allow_new_risk_factors = FALSE, replaces = NULL) {
  allow_new_levels_missing <- missing(allow_new_levels)
  allow_new_risk_factors_missing <- missing(allow_new_risk_factors)
  replaces_missing <- missing(replaces)

  .assert_refinement(model)

  if (!is.data.frame(restrictions) || ncol(restrictions) != 2) {
    stop("'restrictions' must be a data.frame with exactly two columns.", call. = FALSE)
  }

  variable <- names(restrictions)[1]
  value_col <- names(restrictions)[2]
  if (length(unique(restrictions[[variable]])) != nrow(restrictions)) {
    stop("`", variable, "` in `restrictions` must have unique values.",
         call. = FALSE)
  }
  if (anyNA(restrictions[[variable]])) {
    stop("The first column of `restrictions` must not contain missing levels.",
         call. = FALSE)
  }
  .assert_restriction_relativities(restrictions, value_col)

  requested_levels <- as.character(restrictions[[variable]])
  updated_restrictions <- list()
  restriction_steps <- which(vapply(
    model$steps,
    function(step) {
      identical(step$type, "restriction") &&
        identical(step$variable, variable)
    },
    logical(1)
  ))

  if (length(restriction_steps) > 1) {
    stop(
      "Multiple restriction steps are stored for risk factor `", variable,
      "`. Combine these restrictions into one specification before updating it.",
      call. = FALSE
    )
  }

  existing_step <- if (length(restriction_steps) == 1) {
    model$steps[[restriction_steps]]
  } else {
    NULL
  }
  if (!is.null(existing_step)) {
    existing_replaces <- existing_step$replaces %||% NULL
    if (replaces_missing || is.null(replaces)) {
      replaces <- existing_replaces
    } else if (!is.null(existing_replaces) &&
               !identical(replaces, existing_replaces)) {
      stop(
        "Risk factor `", variable, "` already replaces `", existing_replaces,
        "`. Rebuild the refinement specification to replace a different model ",
        "variable.",
        call. = FALSE
      )
    }
  }
  if (!is.null(replaces) && !.is_single_string(replaces)) {
    stop("`replaces` must be NULL or one non-empty character string.",
         call. = FALSE)
  }
  if (!is.null(replaces)) {
    if (!allow_new_risk_factors_missing && !isTRUE(allow_new_risk_factors)) {
      stop(
        "Supplying `replaces` adds a new fixed risk factor and therefore ",
        "conflicts with `allow_new_risk_factors = FALSE`. Omit ",
        "`allow_new_risk_factors` or set it to `TRUE`.",
        call. = FALSE
      )
    }
    allow_new_risk_factors <- TRUE
  }
  restriction_context <- .resolve_restriction_context(
    model,
    variable,
    before_step = if (length(restriction_steps) == 1L) {
      restriction_steps
    } else {
      NULL
    }
  )

  if (!is.null(existing_step)) {
    existing_value_col <- names(existing_step$restrictions)[2]
    incoming_value_col <- names(restrictions)[2]

    if (!identical(incoming_value_col, existing_value_col)) {
      stop(
        "Risk factor `", variable, "` already has a restriction stored as `",
        existing_value_col, "`. Use `", existing_value_col,
        "` as the second column to update that restriction.",
        call. = FALSE
      )
    }

    if (allow_new_levels_missing) {
      allow_new_levels <- existing_step$allow_new_levels %||% TRUE
    }
    if (allow_new_risk_factors_missing) {
      allow_new_risk_factors <-
        existing_step$allow_new_risk_factors %||% FALSE
    }

    existing <- existing_step$restrictions
    existing_levels <- as.character(existing[[variable]])
    incoming_levels <- as.character(restrictions[[variable]])
    supplied_before <- existing_step$supplied_levels %||% existing_levels
    matching <- match(incoming_levels, existing_levels)
    previously_supplied <- incoming_levels %in% supplied_before
    changed <- !is.na(matching) & previously_supplied &
      existing[[existing_value_col]][matching] !=
        restrictions[[incoming_value_col]]

    if (any(changed)) {
      updated_restrictions <- lapply(which(changed), function(i) {
        list(
          level = incoming_levels[i],
          old = existing[[existing_value_col]][matching[i]],
          new = restrictions[[incoming_value_col]][i]
        )
      })
    }

    replace <- !is.na(matching)
    existing[[existing_value_col]][matching[replace]] <-
      restrictions[[incoming_value_col]][replace]

    if (any(!replace)) {
      additions <- stats::setNames(
        data.frame(
          incoming_levels[!replace],
          restrictions[[incoming_value_col]][!replace],
          stringsAsFactors = FALSE
        ),
        c(variable, incoming_value_col)
      )
      existing <- rbind(existing, additions)
    }

    restrictions <- existing
  }

  .assert_single_logical(allow_new_levels, "allow_new_levels")
  .assert_single_logical(allow_new_risk_factors, "allow_new_risk_factors")

  if (!variable %in% names(model$base$data) &&
      is.null(restriction_context)) {
    stop(
      "Risk factor `", variable, "` is not present in the refinement data. ",
      "Add a column assigning each observation to a level before adding this ",
      "tariff factor. `allow_new_risk_factors = TRUE` can only be used when ",
      "that column is available.",
      call. = FALSE
    )
  }

  completed <- .complete_restrictions_from_model(
    model,
    restrictions,
    allow_new_levels = allow_new_levels,
    allow_new_risk_factors = allow_new_risk_factors,
    current_coefficients = restriction_context$coefficients %||% NULL
  )

  if (!is.null(replaces)) {
    if (!isTRUE(completed$new_risk_factor)) {
      stop(
        "`replaces` can only be used when `", variable,
        "` is a new fixed risk factor. Existing model or refinement variables ",
        "already replace their own active term when restricted.",
        call. = FALSE
      )
    }
    .validate_restriction_replacement(
      model,
      replaces = replaces,
      variable = variable,
      before_step = if (length(restriction_steps) == 1L) {
        restriction_steps
      } else {
        NULL
      }
    )
  }

  if (length(updated_restrictions) > 0) {
    for (update in updated_restrictions) {
      message(
        "Updated existing restriction for `", variable, " = \"",
        update$level, "\"`: ",
        format(update$old, trim = TRUE),
        " -> ",
        format(update$new, trim = TRUE)
      )
    }
  }

  previous_new_levels <- existing_step$new_levels %||% character()
  added_new_levels <- setdiff(completed$new_levels, previous_new_levels)
  if (length(added_new_levels) > 0L) {
    level_values <- completed$restrictions[[value_col]][
      match(added_new_levels, completed$restrictions[[variable]])
    ]
    if (length(added_new_levels) == 1L) {
      message(
        "Added new level `", added_new_levels, "` to risk factor `", variable,
        "` with relativity ", format(level_values, trim = TRUE),
        ". This level was not observed in the model data."
      )
    } else {
      level_details <- paste0(
        "`", added_new_levels, "` (relativity ",
        format(level_values, trim = TRUE), ")"
      )
      message(
        "Added new levels to risk factor `", variable, "`: ",
        paste(level_details, collapse = ", "),
        ". These levels were not observed in the model data."
      )
    }
  }

  restriction_step <- list(
    id = if (is.null(existing_step)) {
      .next_step_id(model)
    } else {
      existing_step$id
    },
    type = "restriction",
    variable = variable,
    restrictions = completed$restrictions,
    supplied_levels = unique(c(
      existing_step$supplied_levels %||% character(),
      requested_levels
    )),
    new_levels = completed$new_levels,
    allow_new_levels = allow_new_levels,
    new_risk_factor = completed$new_risk_factor,
    allow_new_risk_factors = allow_new_risk_factors,
    replaces = replaces,
    model_term = existing_step$model_term %||%
      restriction_context$model_term %||%
      variable,
    replace_refinement_offset =
      existing_step$replace_refinement_offset %||%
      isTRUE(restriction_context$replace_refinement_offset) %||%
      isTRUE(restriction_context$derived_from_relativities),
    derived_from_step = existing_step$derived_from_step %||%
      restriction_context$derived_from_step %||%
      NULL,
    derived_source_model_variable =
      existing_step$derived_source_model_variable %||%
      restriction_context$derived_source_model_variable %||%
      NULL,
    derived_split_levels = existing_step$derived_split_levels %||%
      restriction_context$derived_split_levels %||%
      NULL
  )

  if (is.null(existing_step)) {
    restriction_step$supplied_levels <- requested_levels
    return(.add_step(model, restriction_step))
  }

  model$steps[[restriction_steps]] <- restriction_step
  model
}

.complete_restrictions_from_model <- function(model, restrictions,
                                              allow_new_levels = TRUE,
                                              allow_new_risk_factors = FALSE,
                                              current_coefficients = NULL) {
  variable <- names(restrictions)[1]
  value_col <- names(restrictions)[2]

  if (length(unique(restrictions[[variable]])) != nrow(restrictions)) {
    stop("`", variable, "` in `restrictions` must have unique values.",
         call. = FALSE)
  }
  if (anyNA(restrictions[[variable]])) {
    stop("The first column of `restrictions` must not contain missing levels.",
         call. = FALSE)
  }

  .assert_restriction_relativities(restrictions, value_col)

  if (is.null(current_coefficients)) {
    rf <- model$base$rating_factors
    rf_var <- rf[
      rf$risk_factor == variable,
      c("level", "estimate"),
      drop = FALSE
    ]
  } else {
    rf_var <- current_coefficients[, c("level", "estimate"), drop = FALSE]
  }

  if (nrow(rf_var) == 0) {
    if (!isTRUE(allow_new_risk_factors)) {
      stop(
        "Risk factor `", variable, "` is present in the refinement data but ",
        "was not included in the fitted model. To add it as an ",
        "expert-specified fixed tariff factor, set ",
        "`allow_new_risk_factors = TRUE`.",
        call. = FALSE
      )
    }

    missing_data_levels <- sum(is.na(model$base$data[[variable]]))
    if (missing_data_levels > 0) {
      stop(
        "New risk factor `", variable, "` contains ", missing_data_levels,
        " missing value(s) in the refinement data. Assign every observation ",
        "to a level before adding this tariff factor.",
        call. = FALSE
      )
    }
    if (any(restrictions[[value_col]] <= 0)) {
      stop(
        "Relativities for new risk factor `", variable,
        "` must be greater than zero because they are applied on the log scale.",
        call. = FALSE
      )
    }

    data_levels <- unique(as.character(model$base$data[[variable]]))
    data_levels <- data_levels[!is.na(data_levels)]
    supplied_levels <- as.character(restrictions[[variable]])
    missing_levels <- setdiff(data_levels, supplied_levels)
    new_levels <- setdiff(supplied_levels, data_levels)

    if (length(missing_levels) > 0) {
      stop(
        "Every observed level of new risk factor `", variable,
        "` must have a supplied relativity. Missing level(s): ",
        paste(missing_levels, collapse = ", "),
        call. = FALSE
      )
    }

    if (length(new_levels) > 0 && !isTRUE(allow_new_levels)) {
      stop(
        "Level(s) in `restrictions` not found in refinement variable `",
        variable,
        "`: ",
        paste(new_levels, collapse = ", "),
        ". Set `allow_new_levels = TRUE` to include them.",
        call. = FALSE
      )
    }

    ordered_levels <- c(data_levels, new_levels)
    idx <- match(ordered_levels, supplied_levels)
    out <- stats::setNames(
      data.frame(
        ordered_levels,
        restrictions[[value_col]][idx],
        stringsAsFactors = FALSE
      ),
      c(variable, value_col)
    )

    return(list(
      restrictions = out,
      new_levels = new_levels,
      new_risk_factor = TRUE
    ))
  }

  model_levels <- as.character(rf_var$level)
  supplied_levels <- as.character(restrictions[[variable]])
  unknown_levels <- setdiff(supplied_levels, model_levels)

  if (length(unknown_levels) > 0 && !isTRUE(allow_new_levels)) {
    stop(
      "Level(s) in `restrictions` not found in model variable `",
      variable,
      "`: ",
      paste(unknown_levels, collapse = ", "),
      call. = FALSE
    )
  }

  full <- data.frame(
    level = c(model_levels, unknown_levels),
    value = c(as.numeric(rf_var$estimate), rep(NA_real_, length(unknown_levels))),
    stringsAsFactors = FALSE
  )

  idx <- match(supplied_levels, full$level)
  full$value[idx] <- restrictions[[value_col]]

  out <- stats::setNames(
    data.frame(full$level, full$value, stringsAsFactors = FALSE),
    c(variable, value_col)
  )

  list(
    restrictions = out,
    new_levels = unknown_levels,
    new_risk_factor = FALSE
  )
}


#' Shrink categorical tariff relativities towards a common level
#'
#' @description
#' Reduce differences between the relativities of one categorical risk factor
#' before the refined GLM is fitted. `add_shrinkage()` combines each current
#' relativity with a central level on the logarithmic scale. Extreme
#' relativities move further in absolute terms, while their ordering is
#' retained.
#'
#' @details
#' Shrinkage can be used when the direction of a fitted risk-factor pattern is
#' credible, but the difference between its highest and lowest relativities is
#' considered too large for the available experience or the intended tariff.
#' It is a structured actuarial adjustment rather than a new statistical fit.
#'
#' For level \eqn{i}, the unnormalised adjusted relativity is
#'
#' \deqn{
#' \tilde{r}_i = \exp\{Z \log(r_i) + (1-Z)\log(c)\},
#' }
#'
#' where \eqn{r_i} is the current relativity, \eqn{Z} is `credibility`, and
#' \eqn{c} is the weighted geometric centre. A credibility of 1 leaves the
#' relativities unchanged. A credibility of 0 removes the differences between
#' levels.
#'
#' The adjusted relativities are subsequently rescaled so that their weighted
#' arithmetic mean equals the weighted arithmetic mean before shrinkage. With
#' portfolio weights such as exposure or claim count, this prevents shrinkage
#' itself from changing the weighted level of the risk factor. The final GLM
#' refit may still change the intercept or other fitted quantities; use
#' [audit_refinement()] to assess that combined portfolio effect.
#'
#' ## Weight selection
#'
#' `weights = NULL` first uses explicit GLM weights when these were supplied
#' during model fitting. Otherwise, a single column in an offset of the form
#' `log(column)` is used. This commonly selects claim count for a weighted
#' severity GLM and exposure for a frequency or risk-premium GLM. If neither
#' source is unambiguous, the function asks for an explicit choice.
#'
#' Set `weights` to a column name to control the basis directly. For example,
#' exposure is generally appropriate for frequency or risk-premium
#' relativities, while claim count is generally appropriate for severity
#' relativities. Set `weights = "equal"` to give every risk-factor level the
#' same weight. In that case the equal-level mean is preserved, which does not
#' necessarily preserve the level of the observed portfolio.
#'
#' ## Interpretation
#'
#' `credibility` is a user-supplied refinement parameter. It should not be
#' interpreted as an automatically estimated Buhlmann or Buhlmann-Straub
#' credibility factor. Its value should be supported by portfolio stability,
#' validation over time and the intended degree of tariff differentiation.
#' The selected value and weighting basis are retained in the refinement
#' specification and shown by `summary()`.
#'
#' @param model A `rating_refinement` object created with
#'   [prepare_refinement()]. Shrinkage is applied to the current relativities at
#'   this point in the ordered refinement workflow.
#' @param model_variable Character string naming the categorical risk factor to
#'   shrink. This may also identify a tariff factor created by an earlier
#'   [add_relativities()] or [add_restriction()] step.
#' @param credibility Numeric scalar between 0 and 1. This is the weight given
#'   to the current risk-factor relativity. The remaining weight is assigned to
#'   the common centre. The default `0.9` retains 90 percent of the current
#'   logarithmic effect.
#' @param weights `NULL`, `"equal"`, or a character string naming a numeric,
#'   non-negative column in the refinement data. `NULL` derives the weighting
#'   basis from explicit model weights or a simple exposure offset. `"equal"`
#'   gives every level equal weight.
#'
#' @return A `rating_refinement` object containing an ordered shrinkage step.
#'   The returned object stores the original and adjusted relativities, level
#'   weights, inferred weight source and normalization information. The GLM is
#'   fitted only when [refit()] is called.
#'
#' @author Martin Haringa
#'
#' @seealso [prepare_refinement()], [add_smoothing()], [add_restriction()],
#'   [add_rebasing()], [add_relativities()], [refit()], [audit_refinement()]
#'
#' @examples
#' portfolio <- data.frame(
#'   claims = c(1, 2, 1, 3, 2, 4, 1, 5),
#'   exposure = c(1, 1, 1, 1, 2, 1, 1, 1),
#'   sector = factor(rep(c("Industry", "Office", "Retail", "Transport"), 2))
#' )
#'
#' model <- glm(
#'   claims ~ sector + offset(log(exposure)),
#'   family = poisson(),
#'   data = portfolio
#' )
#'
#' refinement <- prepare_refinement(model, data = portfolio) |>
#'   add_shrinkage(
#'     model_variable = "sector",
#'     credibility = 0.9,
#'     weights = "exposure"
#'   )
#'
#' summary(refinement)
#' refined_model <- refit(refinement)
#' rating_table(refined_model)
#'
#' # Use equal level weights explicitly when portfolio weighting is not wanted.
#' equal_level_refinement <- prepare_refinement(model, data = portfolio) |>
#'   add_shrinkage(
#'     model_variable = "sector",
#'     credibility = 0.8,
#'     weights = "equal"
#'   )
#'
#' @export
add_shrinkage <- function(model, model_variable, credibility = 0.9,
                          weights = NULL) {
  .assert_refinement(model)
  if (!is.character(model_variable) || length(model_variable) != 1L ||
      is.na(model_variable) || !nzchar(model_variable)) {
    stop("`model_variable` must be one non-empty character string.",
         call. = FALSE)
  }
  if (!is.numeric(credibility) || length(credibility) != 1L ||
      is.na(credibility) || !is.finite(credibility) || credibility < 0 ||
      credibility > 1) {
    stop("`credibility` must be one finite numeric value between 0 and 1.",
         call. = FALSE)
  }
  if (any(vapply(
    model$steps,
    function(step) identical(step$type, "shrinkage") &&
      identical(step$model_variable, model_variable),
    logical(1)
  ))) {
    stop(
      "Risk factor `", model_variable,
      "` already has a shrinkage step. Rebuild the refinement specification ",
      "with the revised `credibility` or `weights` value.",
      call. = FALSE
    )
  }

  state <- .make_exec_state(model)
  if (length(model$steps) > 0L) {
    for (step in model$steps) {
      state <- .apply_refinement_step(state, step)
    }
  }
  if (!model_variable %in% names(state$data)) {
    choices <- names(state$data)
    suggestion <- .closest_refinement_value(model_variable, choices)
    message <- paste0(
      "Column `", model_variable,
      "`, supplied through `model_variable`, was not found in the current refinement data."
    )
    if (!is.null(suggestion)) {
      message <- paste0(message, " Did you mean `", suggestion, "`?")
    }
    stop(message, call. = FALSE)
  }

  weight_spec <- .resolve_shrinkage_weight_spec(model, weights)
  effective_model_term <- .refinement_term_for_variable(
    model$steps,
    model_variable
  )
  formula_variables <- all.vars(state$formula_no_offset)
  offset_variables <- if (is.null(state$offset)) {
    character()
  } else {
    all.vars(tryCatch(parse(text = state$offset)[[1]],
                      error = function(e) quote(NULL)))
  }
  replace_offset <- effective_model_term %in% offset_variables
  if (!replace_offset && !effective_model_term %in% formula_variables) {
    stop(
      "Risk factor `", model_variable,
      "` is not an active term in the current refinement specification.",
      call. = FALSE
    )
  }

  derived_model_variable <- paste0(model_variable, "_shrunk")
  if (derived_model_variable %in% names(state$data)) {
    stop(
      "Generated shrinkage column `", derived_model_variable,
      "` already exists in the refinement data.",
      call. = FALSE
    )
  }

  shrinkage_step <- list(
    id = .next_step_id(model),
    type = "shrinkage",
    variable = model_variable,
    model_variable = model_variable,
    credibility = as.numeric(credibility),
    weights = weights,
    weight_spec = weight_spec,
    effective_model_term = effective_model_term,
    derived_model_variable = derived_model_variable,
    replace_refinement_offset = replace_offset
  )
  shrinkage_step$values <- .calculate_shrinkage(state, shrinkage_step)
  shrinkage_step$centre <- attr(shrinkage_step$values, "centre")
  shrinkage_step$normalization_factor <- attr(
    shrinkage_step$values,
    "normalization_factor"
  )
  shrinkage_step$original_weighted_mean <- attr(
    shrinkage_step$values,
    "original_weighted_mean"
  )
  shrinkage_step$adjusted_weighted_mean <- attr(
    shrinkage_step$values,
    "adjusted_weighted_mean"
  )

  .add_step(model, shrinkage_step)
}


#' Rebase categorical tariff relativities to a reference level
#'
#' @description
#' Rescale the current relativities of one categorical risk factor so that a
#' selected level has relativity 1. `add_rebasing()` is an ordered refinement
#' step: it uses the relativities available at that point in the workflow and
#' retains all ratios between levels.
#'
#' @details
#' Rebasing changes the numerical reference of a tariff factor, but does not
#' change its relative differentiation. If the current relativity of reference
#' level \eqn{j} is \eqn{r_j}, every level is transformed as
#'
#' \deqn{
#' r_i^{new} = \frac{r_i}{r_j}.
#' }
#'
#' The selected reference level therefore becomes 1, while the ratio between
#' any two levels remains unchanged. For example, relativities 0.8, 1.0 and 1.2
#' rebased to the first level become 1.0, 1.25 and 1.5. This is different from
#' [add_shrinkage()], which deliberately reduces the spread between levels.
#'
#' ## Selecting the reference level
#'
#' Supply `reference_level` when the tariff has an established reference class
#' or when governance requires a particular level to remain at 1. If
#' `reference_level = NULL`, the level with the largest aggregated weight is
#' selected. Ties are resolved by the order of the current factor levels.
#'
#' With `weights = NULL`, explicit GLM weights are used when available;
#' otherwise a single offset of the form `log(column)` is used. This commonly
#' selects claim count for a weighted severity GLM and exposure for a frequency
#' or risk-premium GLM. An explicit numeric column can be supplied when another
#' portfolio basis is required. `weights` is ignored when `reference_level` is
#' supplied because no automatic selection is then needed.
#'
#' ## Position in the refinement workflow
#'
#' Rebasing is generally applied after the step that creates the final tariff
#' levels. For example, [add_relativities()] may replace a broad level by
#' several sublevels; `add_rebasing()` can then select one of those resulting
#' sublevels as the new reference. It can also follow [add_shrinkage()] when the
#' shrunken relativities should be reported relative to an established level.
#'
#' `set_reference_level()` serves a different purpose. It changes the contrast
#' reference of a factor before fitting a GLM. `add_rebasing()` rescales current
#' tariff relativities inside an existing refinement specification. The
#' refinement step and selected reference are retained for review by
#' `summary()` and [audit_refinement()].
#'
#' @param model A `rating_refinement` object created with
#'   [prepare_refinement()]. Rebasing uses the current relativities at this
#'   point in the ordered workflow.
#' @param model_variable Character string naming the categorical risk factor to
#'   rebase. This may identify an original GLM factor or a tariff factor created
#'   by an earlier [add_relativities()], [add_restriction()] or
#'   [add_shrinkage()] step.
#' @param reference_level Optional single character value naming the level that
#'   should receive relativity 1. When `NULL`, the level with the largest
#'   aggregated weight is selected automatically.
#' @param weights `NULL` or a character string naming a numeric, non-negative
#'   column in the refinement data. The weights are used only when
#'   `reference_level = NULL`. `NULL` derives the basis from explicit model
#'   weights or a simple exposure offset.
#'
#' @return A `rating_refinement` object containing an ordered rebasing step.
#'   The step stores the original and rebased relativities, the selected
#'   reference level, its original relativity, the selection method and, when
#'   applicable, the aggregated level weights. The GLM is fitted only when
#'   [refit()] is called.
#'
#' @author Martin Haringa
#'
#' @seealso [prepare_refinement()], [set_reference_level()],
#'   [add_relativities()], [add_shrinkage()], [add_restriction()], [refit()],
#'   [audit_refinement()]
#'
#' @examples
#' portfolio <- data.frame(
#'   claims = c(1, 2, 1, 3, 2, 4, 1, 5),
#'   exposure = c(1, 1, 1, 1, 2, 1, 1, 1),
#'   sector = factor(rep(c("Industry", "Office", "Retail", "Transport"), 2))
#' )
#'
#' model <- glm(
#'   claims ~ sector + offset(log(exposure)),
#'   family = poisson(),
#'   data = portfolio
#' )
#'
#' # Keep Office as the explicit tariff reference after shrinkage.
#' refinement <- prepare_refinement(model, data = portfolio) |>
#'   add_shrinkage(
#'     model_variable = "sector",
#'     credibility = 0.9,
#'     weights = "exposure"
#'   ) |>
#'   add_rebasing(
#'     model_variable = "sector",
#'     reference_level = "Office"
#'   )
#'
#' summary(refinement)
#' refined_model <- refit(refinement)
#' rating_table(refined_model)
#'
#' # Omitting reference_level selects the level with the largest exposure.
#' exposure_reference <- prepare_refinement(model, data = portfolio) |>
#'   add_rebasing(
#'     model_variable = "sector",
#'     weights = "exposure"
#'   )
#'
#' @export
add_rebasing <- function(model, model_variable, reference_level = NULL,
                         weights = NULL) {
  .assert_refinement(model)
  if (!.is_single_string(model_variable)) {
    stop("`model_variable` must be one non-empty character string.",
         call. = FALSE)
  }
  if (!is.null(reference_level) && !.is_single_string(reference_level)) {
    stop("`reference_level` must be NULL or one non-empty character string.",
         call. = FALSE)
  }
  if (!is.null(reference_level) && !is.null(weights)) {
    warning(
      "`weights` is ignored because `reference_level` was supplied explicitly.",
      call. = FALSE
    )
  }
  if (is.null(reference_level) && identical(weights, "equal")) {
    stop(
      "`weights = \"equal\"` cannot identify a largest reference level. ",
      "Supply `reference_level` explicitly or use a portfolio weight column.",
      call. = FALSE
    )
  }
  if (any(vapply(
    model$steps,
    function(step) identical(step$type, "rebasing") &&
      identical(step$model_variable, model_variable),
    logical(1)
  ))) {
    stop(
      "Risk factor `", model_variable,
      "` already has a rebasing step. Rebuild the refinement specification ",
      "with the revised reference selection.",
      call. = FALSE
    )
  }

  state <- .make_exec_state(model)
  if (length(model$steps) > 0L) {
    for (step in model$steps) {
      state <- .apply_refinement_step(state, step)
    }
  }
  if (!model_variable %in% names(state$data)) {
    suggestion <- .closest_refinement_value(model_variable, names(state$data))
    message <- paste0(
      "Column `", model_variable,
      "`, supplied through `model_variable`, was not found in the current refinement data."
    )
    if (!is.null(suggestion)) {
      message <- paste0(message, " Did you mean `", suggestion, "`?")
    }
    stop(message, call. = FALSE)
  }

  weight_spec <- if (is.null(reference_level)) {
    .resolve_shrinkage_weight_spec(model, weights)
  } else {
    NULL
  }
  effective_model_term <- .refinement_term_for_variable(
    model$steps,
    model_variable
  )
  formula_variables <- all.vars(state$formula_no_offset)
  offset_variables <- if (is.null(state$offset)) {
    character()
  } else {
    all.vars(tryCatch(parse(text = state$offset)[[1]],
                      error = function(e) quote(NULL)))
  }
  replace_offset <- effective_model_term %in% offset_variables
  if (!replace_offset && !effective_model_term %in% formula_variables) {
    stop(
      "Risk factor `", model_variable,
      "` is not an active term in the current refinement specification.",
      call. = FALSE
    )
  }

  derived_model_variable <- paste0(model_variable, "_rebased")
  if (derived_model_variable %in% names(state$data)) {
    stop(
      "Generated rebasing column `", derived_model_variable,
      "` already exists in the refinement data.",
      call. = FALSE
    )
  }

  rebasing_step <- list(
    id = .next_step_id(model),
    type = "rebasing",
    variable = model_variable,
    model_variable = model_variable,
    reference_level = reference_level,
    weights = weights,
    weight_spec = weight_spec,
    effective_model_term = effective_model_term,
    derived_model_variable = derived_model_variable,
    replace_refinement_offset = replace_offset
  )
  rebasing_step$values <- .calculate_rebasing(state, rebasing_step)
  rebasing_step$reference_level <- attr(
    rebasing_step$values,
    "reference_level"
  )
  rebasing_step$reference_relativity <- attr(
    rebasing_step$values,
    "reference_relativity"
  )
  rebasing_step$method <- attr(rebasing_step$values, "method")

  .add_step(model, rebasing_step)
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
#' @param allow_new_levels Logical. If `TRUE` (default), restrictions may
#'   include tariff levels that were not observed when the model was fitted.
#'   See [add_restriction()].
#' @param allow_new_risk_factors Logical. Whether a fixed tariff factor that is
#'   available in the model data but absent from the fitted model may be added.
#'   The default is `TRUE` to preserve the historical behaviour of
#'   `restrict_coef()`. New code using [add_restriction()] requires an explicit
#'   opt-in because its default is `FALSE`.
#'
#' @return A `rating_refinement` object containing the restriction step. Call
#'   [refit()] to apply the restriction and return the refined GLM. New code
#'   should use [prepare_refinement()] followed by [add_restriction()] directly.
#'
#' @seealso [add_restriction()], [prepare_refinement()], [refit()]
#'
#' @export
#' @keywords internal
restrict_coef <- function(model, restrictions, allow_new_levels = TRUE,
                          allow_new_risk_factors = TRUE) {
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
  ref <- add_restriction(
    ref,
    restrictions,
    allow_new_levels = allow_new_levels,
    allow_new_risk_factors = allow_new_risk_factors
  )
  ref
}


#' Smooth grouped tariff relativities in a refinement workflow
#'
#' @description
#' Replace independently estimated relativities of an ordered, grouped model
#' variable with a smooth tariff curve. This can reduce sampling variation
#' between adjacent levels of risk factors such as age, vehicle age, insured
#' value or bonus-malus years while retaining the broad effect estimated by the
#' GLM.
#'
#' @details
#' `add_smoothing()` stores a smoothing specification on a
#' `rating_refinement` object. It does not alter the fitted GLM immediately.
#' The smoothing is evaluated in the recorded step order and applied when
#' [refit()] is called.
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
#' variable during refitting.
#'
#' ## Effect strength
#'
#' `effect_strength` adjusts the overall spread of the fitted smoothing curve
#' without estimating a different curve. For a smoothed relativity `r(x)` and
#' common centre `c`, the adjustment is `c * (r(x) / c)^a`, where `a` is
#' `effect_strength`. It is therefore a multiplication of deviations on the
#' logarithmic relativity scale, rather than a change in skewness or kurtosis.
#' A value of 1 retains the fitted smooth,
#' values between 0 and 1 flatten the effect, values above 1 strengthen it, and
#' 0 produces a constant effect. The adjusted values are subsequently
#' normalised so that the weighted arithmetic mean of the smoothed tariff
#' levels remains unchanged. The `weights` column is used for this
#' normalisation when supplied; otherwise, tariff levels receive equal weight.
#'
#' This adjustment changes the overall degree of tariff differentiation. It
#' does not selectively change only the upper or lower part of the curve. Use
#' [edit_smoothing()] with interval boundaries and control points when a local
#' part of the relationship requires a separate actuarial adjustment.
#' Monotonic ordering is retained, but curvature on the raw relativity scale
#' can change and should be reviewed when a convex or concave specification is
#' important.
#'
#' ## Actuarial interpretation
#'
#' Smoothing introduces a structural assumption: adjacent values of the source
#' variable are expected to have related tariff effects. The selected method,
#' basis dimension and breaks should therefore be assessed against exposure,
#' observed experience, coefficient uncertainty and stability over time. A
#' smooth curve should not be interpreted as evidence that the underlying risk
#' relationship is itself known without uncertainty.
#'
#' ## Smoothing methods
#'
#' The available methods represent different assumptions about the shape of the
#' tariff effect:
#'
#' \describe{
#'   \item{`"spline"`}{The general-purpose default. Fits an unconstrained
#'   penalized cubic regression spline. It is suitable when the tariff effect
#'   should be smooth but no monotonicity or curvature restriction is
#'   justified.}
#'   \item{`"poly"`}{Fits a global polynomial through the grouped GLM
#'   relativities. `degree` determines its order. A low degree gives a compact
#'   parametric trend; higher degrees can follow more local variation but may
#'   oscillate, particularly near the boundaries.}
#'   \item{`"increasing"` and `"decreasing"`}{Fit monotone smooths. These
#'   methods constrain the tariff effect to move in one direction, without
#'   imposing how quickly its slope changes. They are often the most directly
#'   interpretable constrained specifications when actuarial reasoning supports
#'   a consistently increasing or decreasing risk effect.}
#'   \item{`"convex"` and `"concave"`}{Constrain curvature but not direction.
#'   For a convex curve, the slope increases as the source variable increases;
#'   for a concave curve, the slope decreases. A convex curve may therefore be
#'   U-shaped and a concave curve may be inverted U-shaped. These are advanced
#'   choices when curvature itself has a defensible interpretation.}
#'   \item{`"increasing_convex"` and `"increasing_concave"`}{Fit increasing
#'   curves with an additional curvature constraint. An increasing convex
#'   effect rises at an increasing rate, for example when upper-tail risk causes
#'   marginal cost to accelerate. An increasing concave effect rises at a
#'   decreasing rate and gradually flattens, for example when risk cost rises
#'   with insured value but less than proportionally.}
#'   \item{`"decreasing_convex"` and `"decreasing_concave"`}{Fit decreasing
#'   curves with an additional curvature constraint. A decreasing convex effect
#'   becomes less steep and tends to flatten. A decreasing concave effect
#'   becomes progressively steeper.}
#'   \item{`"gam"`}{Fits an unconstrained thin-plate regression spline with
#'   [mgcv::gam()]. It is mainly intended as a flexible reference when comparing
#'   the general spline and shape-constrained specifications. It does not impose
#'   the actuarial shape assumptions represented by the constrained methods.}
#' }
#'
#' The shape-constrained methods are fitted with [scam::scam()]. Monotonicity
#' concerns the direction of the effect, whereas convexity and concavity concern
#' how its slope changes. In most tariff applications, a directional assumption
#' is easier to substantiate than a curvature assumption. A constraint
#' should reflect an actuarial or pricing assumption that is defensible for the
#' risk factor; it should not be selected solely because it produces a smoother
#' visual result. The combined monotonicity and curvature methods are advanced
#' specifications and are most appropriate when both assumptions can be
#' supported independently.
#'
#' The former short codes `"mpi"`, `"mpd"`, `"cx"`, `"cv"`, `"micx"`,
#' `"micv"`, `"mdcx"` and `"mdcv"` remain accepted as compatibility aliases.
#' New code should use the readable method names above. Both forms produce the
#' same smoothing specification.
#'
#' ## Basis dimension and polynomial degree
#'
#' For `"spline"`, `"gam"` and the shape-constrained methods, `k` specifies the
#' basis dimension. It controls the maximum flexibility available to the smooth,
#' but it is not the final effective degrees of freedom of the fitted curve.
#' The estimated smoothing penalty can reduce the effective degrees of freedom
#' below this maximum.
#'
#' A smaller `k` restricts the curve to broad movements. A larger `k` permits
#' more local variation, but requires enough distinct grouped covariate values
#' and may be unstable when only a few tariff levels are available. If `k` is
#' `NULL`, the function uses the smaller of 10 and the number of unique grouped
#' model points. Spline, GAM and shape-constrained smoothing require at least
#' three unique grouped values. The function checks this dimension before
#' fitting and reports the observed number of unique values when the requested
#' complexity is not feasible.
#'
#' For `"poly"`, `degree` has the corresponding complexity role. A polynomial
#' of degree \eqn{d} requires at least \eqn{d + 1} unique grouped values. When
#' `degree` is omitted, the existing behaviour uses the highest degree supported
#' by the grouped model points. In practice, an explicit low degree is generally
#' preferable when a stable global trend is intended.
#'
#' `degree` is only accepted for `smoothing = "poly"`. Conversely, `k` is only
#' accepted for `"spline"`, `"gam"` and the shape-constrained methods. This
#' separation prevents a complexity argument from being supplied but silently
#' ignored.
#'
#' The deprecated [smooth_coef()] wrapper remains available for backwards
#' compatibility.
#'
#' @param model Object of class `rating_refinement`, created with
#'   [prepare_refinement()]. A fitted GLM, including a model returned by
#'   [refit()], is not accepted directly; retain and modify the corresponding
#'   refinement specification instead.
#' @param model_variable Character string. Existing grouped or binned variable
#'   in the GLM. This is the model term that will be replaced by a smoothed
#'   tariff factor. The column must not contain missing values; remove or impute
#'   missing values before adding the smoothing step.
#' @param source_variable Character string. Original numeric portfolio variable
#'   underlying `model_variable`. Its name is also used for the resulting
#'   smoothed tariff variable. The column must contain only finite, non-missing
#'   numeric values.
#' @param breaks Numeric vector with the tariff segment boundaries to use after
#'   smoothing. These boundaries determine the final tariff segmentation, not
#'   the number of portfolio observations used to estimate the curve. Values
#'   must be finite, strictly increasing and cover every observed value of
#'   `source_variable`. Boundaries outside the interval range represented by
#'   `model_variable` are allowed, but produce a warning because the resulting
#'   relativities rely on extrapolation beyond the fitted GLM levels. This
#'   argument is required.
#' @param smoothing Character string selecting the smoothing method. Available
#'   values are `"spline"` (default), `"poly"`, `"gam"`, `"increasing"`,
#'   `"decreasing"`, `"convex"`, `"concave"`, `"increasing_convex"`,
#'   `"increasing_concave"`, `"decreasing_convex"` and
#'   `"decreasing_concave"`. The former short SCOP codes remain accepted as
#'   compatibility aliases. See Details for the statistical interpretation and
#'   shape restrictions.
#' @param k Optional single positive whole number. Basis dimension for smoothing
#'   methods `"spline"`, `"gam"`, `"increasing"`, `"decreasing"`,
#'   `"convex"`, `"concave"` and the combined direction-curvature methods. It
#'   sets the maximum
#'   flexibility available to the smooth and is not necessarily equal to its
#'   estimated effective degrees of freedom. `NULL` uses the smaller of 10 and
#'   the number of unique grouped model points. At least three unique grouped
#'   values are required. The basis dimension cannot exceed the number of
#'   unique grouped covariate values available for fitting.
#' @param degree Optional single whole number. Polynomial degree, used only by
#'   `smoothing = "poly"`. The degree must be feasible for the number of unique
#'   grouped model points.
#' @param weights Optional character string. Numeric volume column, usually
#'   exposure, used to weight the grouped GLM relativities during smoothing.
#' @param effect_strength Non-negative finite numeric scalar controlling the
#'   spread of the fitted smoothing effect on the logarithmic relativity scale.
#'   The default `1` leaves the smoothing unchanged. Values below 1 flatten the
#'   complete effect and values above 1 make it steeper. After adjustment, the
#'   weighted arithmetic mean is restored; see Details.
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
#' @seealso [prepare_refinement()], [edit_smoothing()], [add_restriction()],
#'   [add_shrinkage()], [add_rebasing()], [add_relativities()], [refit()],
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
#' age_segments_freq <- derive_tariff_segments(
#'   age_policyholder_frequency,
#'   segmentation_penalty = 10,
#'   seed = 1
#' )
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
#'     breaks = c(seq(18, 93, 5), 95),
#'     smoothing = "spline",
#'     k = 6,
#'     weights = "exposure",
#'     effect_strength = 1.1
#'   )
#'
#' # When the tariff effect must not decrease, use the readable constrained
#' # method name. The former value "mpi" remains accepted for compatibility.
#' increasing_ref <- prepare_refinement(burn_unrestricted) |>
#'   add_smoothing(
#'     model_variable = "age_policyholder_freq_cat",
#'     source_variable = "age_policyholder",
#'     breaks = c(seq(18, 93, 5), 95),
#'     smoothing = "increasing",
#'     k = 6,
#'     weights = "exposure"
#'   )
#'
#' # Limit the visible range without changing the fitted smoothing curve.
#' autoplot(ref, x_max = 80, y_max = 1.5)
#' }
#'
#' @export
add_smoothing <- function(model, model_variable = NULL, source_variable = NULL,
                          breaks, smoothing = "spline", k = NULL,
                          degree = NULL, weights = NULL, effect_strength = 1,
                          tariff_class = NULL, rating_variable = NULL,
                          x_cut = NULL, x_org = NULL) {
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

  if (missing(breaks)) {
    stop(
      "`breaks` is required and must contain the tariff segment boundaries.",
      call. = FALSE
    )
  }

  if (!.is_single_string(smoothing) ||
      !smoothing %in% .allowed_smoothing_methods) {
    stop(
      "'smoothing' must be one of: ",
      paste(names(.smoothing_method_codes), collapse = ", "),
      ". The former aliases ",
      paste(names(.smoothing_method_aliases), collapse = ", "),
      " are also accepted for compatibility.",
      call. = FALSE
    )
  }
  smoothing_spec <- .resolve_smoothing_method(smoothing)
  smoothing <- smoothing_spec$method
  smoothing_code <- smoothing_spec$code
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

  .assert_smoothing_model_variable(model, model_variable)
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
  .validate_smoothing_source_and_breaks(
    data = model$base$data,
    source_variable = source_variable,
    breaks = breaks,
    model = model$base$model,
    model_variable = model_variable
  )
  .assert_single_numeric(degree, "degree", allow_null = TRUE, positive = TRUE, whole = TRUE)
  .assert_single_numeric(k, "k", allow_null = TRUE, positive = TRUE, whole = TRUE)
  if (!is.numeric(effect_strength) || length(effect_strength) != 1L ||
      is.na(effect_strength) || !is.finite(effect_strength) ||
      effect_strength < 0) {
    stop(
      "`effect_strength` must be one finite non-negative numeric value.",
      call. = FALSE
    )
  }

  basis_methods <- c(
    "spline", "gam", "mpi", "mpd", "cx", "cv", "micx", "micv", "mdcx",
    "mdcv"
  )
  if (!is.null(degree) && !identical(smoothing_code, "poly")) {
    stop(
      "'degree' is only used with `smoothing = \"poly\"`.",
      call. = FALSE
    )
  }
  if (!is.null(k) && !smoothing_code %in% basis_methods) {
    stop(
      "'k' is only used with `smoothing = \"spline\"`, `\"gam\"` or a ",
      "shape-constrained smoothing method.",
      call. = FALSE
    )
  }
  if (!is.null(k) && smoothing_code %in% basis_methods && k < 3) {
    stop(
      "'k' must be at least 3 for `smoothing = \"", smoothing, "\"`.",
      call. = FALSE
    )
  }

  if (smoothing_code %in% c("poly", basis_methods)) {
    borders_model <- cut_borders_model(model$base$model, model_variable)
    complexity <- .validate_smoothing_complexity(
      covariates = borders_model["avg_"],
      source_variable = source_variable,
      smoothing = smoothing_code,
      k = k,
      degree = degree,
      response = borders_model$estimate
    )
    if (smoothing_code %in% basis_methods && is.null(k)) {
      k <- complexity$k
    }
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
    smoothing_code = smoothing_code,
    k = k,
    weights = weights,
    effect_strength = as.numeric(effect_strength),
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

#' Edit a smoothing curve in a refinement workflow
#'
#' @description
#' Modify the overall effect strength or a specified interval of a smoothing
#' curve previously added with [add_smoothing()]. The function can replace the
#' stored strength, fix boundary values and introduce internal control points.
#'
#' @details
#' `edit_smoothing()` stores an edit on the selected smoothing step of a
#' `rating_refinement` object. It does not alter the fitted GLM immediately.
#' The edited curve is evaluated in the recorded step order and applied when
#' [refit()] is called.
#'
#' `effect_strength` updates the overall strength stored by [add_smoothing()].
#' It is not multiplied by the previous value. For example, changing the value
#' from 1.1 to 1.2 recalculates the current smoothing specification with a
#' strength of 1.2; it does not multiply 1.1 by 1.2. If
#' `effect_strength` is `NULL`, the value already stored on the smoothing step
#' is retained. Local edits are calculated first and the selected effect
#' strength is then applied to the resulting complete curve.
#'
#' Use `model_variable` or `step` to identify the smoothing step to edit. The
#' interval from `from` to `to` defines the part of the source variable range
#' that should be changed. Both may be omitted when only `effect_strength` is
#' updated. `from_value` and `to_value` can be used to force the
#' curve values at the interval boundaries. `control_positions` and
#' `control_values` add additional points that the edited curve should follow
#' inside the interval.
#'
#' ## Actuarial interpretation
#'
#' The edited interval is an explicit tariff assumption layered on the
#' statistically fitted smoothing curve. It should be supported by an actuarial
#' rationale and reviewed against exposure, observed experience and the
#' continuity of adjacent segments. The edit does not add information to sparse
#' parts of the portfolio and should not be interpreted as a new model estimate.
#'
#' Keep the `rating_refinement` object, call [refit()] to assess the current
#' specification, edit that same refinement object, and call [refit()] again.
#' The previously fitted GLM remains unchanged. This retains the order and
#' content of manual adjustments as part of the reproducible refinement
#' specification.
#'
#' @param model Object of class `rating_refinement`, created with
#'   [prepare_refinement()] and containing an existing smoothing step. Ordinary
#'   and refitted GLMs are not accepted directly. Legacy `smooth` and
#'   `restricted` objects are still accepted for backwards compatibility.
#' @param model_variable Character string. The `model_variable` of the smoothing
#'   step to edit. Required when more than one smoothing step exists and `step`
#'   is not supplied.
#' @param step Optional numeric index of the smoothing step to edit.
#' @param from,to Optional numeric values giving the start and end of the
#'   source-variable interval to modify. Supply both for a local curve edit, or
#'   omit both when only `effect_strength` is changed.
#' @param from_value,to_value Optional numeric values used to override the
#'   smoothed curve value at `from` and `to`.
#' @param control_positions,control_values Optional numeric vectors of equal
#'   length. These define additional points that the edited smoothing curve
#'   should pass through.
#' @param allow_extrapolation Logical. Whether edits may extend beyond the
#'   observed source-variable range.
#' @param extrapolation_step Optional positive numeric scalar used to set the
#'   spacing of extra break points when extrapolation is allowed.
#' @param effect_strength Optional non-negative finite numeric scalar replacing
#'   the effect strength stored on the smoothing step. `NULL` retains the
#'   existing value. The update is non-cumulative; see Details.
#'
#' @author Martin Haringa
#'
#' @return A `rating_refinement` object containing the edited smoothing
#'   specification. The pricing GLM is not fitted again until [refit()] is
#'   called.
#'
#' @seealso [prepare_refinement()], [add_smoothing()], [add_restriction()],
#'   [add_shrinkage()], [add_rebasing()], [add_relativities()], [refit()]
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
#' refinement <- prepare_refinement(model, data = portfolio) |>
#'   add_smoothing(
#'     model_variable = "age_band",
#'     source_variable = "driver_age",
#'     breaks = c(18, 30, 40, 50, 60),
#'     weights = "exposure"
#'   )
#'
#' # Fit and inspect the initial smoothing specification.
#' initial_model <- refit(refinement)
#'
#' # Edit the retained specification and fit it again.
#' refinement <- refinement |>
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
#' refined_model <- refit(refinement)
#'
#' # Retain the smoothing shape and strengthen its complete effect. This
#' # replaces the stored value; it is not multiplied by an earlier strength.
#' refinement <- refinement |>
#'   edit_smoothing(
#'     model_variable = "age_band",
#'     effect_strength = 1.15
#'   )
#'
#' @export
edit_smoothing <- function(model,
                           model_variable = NULL,
                           step = NULL,
                           from = NULL, to = NULL,
                           from_value = NULL, to_value = NULL,
                           control_positions = NULL,
                           control_values = NULL,
                           allow_extrapolation = FALSE,
                           extrapolation_step = NULL,
                           effect_strength = NULL) {
  if (inherits(model, c("smooth", "restricted"))) {
    lifecycle::deprecate_warn(
      when = "0.9.0",
      what = "edit_smoothing(model = <smooth/restricted>)",
      with = "prepare_refinement() |> add_smoothing() |> edit_smoothing() |> refit()"
    )
    model <- as_refinement(model)
  }

  .assert_refinement(model)

  .assert_single_numeric(from, "from", allow_null = TRUE)
  .assert_single_numeric(to, "to", allow_null = TRUE)
  if (xor(is.null(from), is.null(to))) {
    stop("Supply both `from` and `to`, or omit both.", call. = FALSE)
  }
  if (!is.null(from) && from >= to) {
    stop("'from' must be smaller than 'to'.", call. = FALSE)
  }
  .assert_single_numeric(from_value, "from_value", allow_null = TRUE)
  .assert_single_numeric(to_value, "to_value", allow_null = TRUE)
  .assert_single_logical(allow_extrapolation, "allow_extrapolation")
  .assert_single_numeric(extrapolation_step, "extrapolation_step",
                         allow_null = TRUE, positive = TRUE)
  if (!is.null(effect_strength) &&
      (!is.numeric(effect_strength) || length(effect_strength) != 1L ||
       is.na(effect_strength) || !is.finite(effect_strength) ||
       effect_strength < 0)) {
    stop(
      "`effect_strength` must be NULL or one finite non-negative numeric value.",
      call. = FALSE
    )
  }

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

  has_interval_edit <- !is.null(from)
  has_interval_values <- !is.null(from_value) || !is.null(to_value) ||
    length(control_positions) > 0L || length(control_values) > 0L ||
    isTRUE(allow_extrapolation) || !is.null(extrapolation_step)
  if (!has_interval_edit && has_interval_values) {
    stop(
      "Supply `from` and `to` when specifying local smoothing edits or ",
      "extrapolation settings.",
      call. = FALSE
    )
  }
  if (!has_interval_edit && is.null(effect_strength)) {
    stop(
      "Supply `effect_strength` or define a local interval with `from` and `to`.",
      call. = FALSE
    )
  }

  if (has_interval_edit && length(control_positions) > 0 &&
      any(control_positions <= from | control_positions >= to)) {
    stop("'control_positions' must lie between 'from' and 'to'.",
         call. = FALSE)
  }

  if (has_interval_edit) {
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
  }
  if (!is.null(effect_strength)) {
    model$steps[[idx]]$effect_strength <- as.numeric(effect_strength)
  }

  model
}


#' Add sublevel relativities to a refinement workflow
#'
#' @description
#' Divide one or more levels of an existing GLM risk factor into more detailed
#' tariff levels using supplied relativities. This can be appropriate when the
#' GLM is estimated on a coarser factor for statistical stability, while a
#' documented actuarial segmentation is required within sufficiently
#' homogeneous model levels.
#'
#' @details
#' `add_relativities()` stores a relativity step on a `rating_refinement`
#' object. It does not alter the fitted GLM immediately. The split is evaluated
#' in the recorded step order and applied when [refit()] is called.
#'
#' `model_variable` is the variable already used in the GLM. `split_variable` is
#' the more detailed variable in the portfolio data that will be used to split
#' one or more levels of `model_variable`. The `relativities` argument should be
#' a named list describing those splits, usually built with [relativities()] and
#' [split_level()]. `output_variable` names the resulting hybrid tariff factor:
#' levels included in `relativities` are represented by their detailed
#' `split_variable` level, while all other levels retain their
#' `model_variable` level.
#'
#' Levels of `model_variable` that are not included in `relativities` retain
#' their existing model coefficient. In [rating_table()], exposure for these
#' retained levels is aggregated from `model_variable`, while exposure for the
#' newly split levels is aggregated from `split_variable` within the specified
#' parent model level. Omitting a model level from `relativities` therefore
#' means that the level remains unsplit; it is not treated as an incomplete
#' specification.
#'
#' `add_relativities()` validates the supplied sublevel names against the
#' observed values of `split_variable` before storing the refinement step. A
#' misspelled or incorrectly spaced category or sublevel therefore produces an
#' immediate error, with a suggestion when a closely matching observed value is
#' available. It also verifies that each sublevel occurs within its specified
#' parent category of `model_variable`.
#'
#' When `normalize = TRUE`, the supplied relativities are normalised using
#' exposure so that their exposure-weighted mean equals one within the split
#' model level. They then redistribute the existing model coefficient across
#' the sublevels without changing its exposure-weighted average. With
#' `normalize = FALSE`, the supplied relativities are applied directly.
#'
#' ## Step order and restrictions
#'
#' If `model_variable` was restricted in an earlier [add_restriction()] step,
#' the restricted coefficients are automatically used as the basis for the
#' derived relativities. The user can continue to supply the original model
#' variable; no additional argument is needed. Supplying the restricted
#' variable explicitly gives the same coefficient basis and does not apply the
#' restriction a second time. Refinement steps are order-dependent, so a
#' restriction added after `add_relativities()` does not affect an earlier
#' relativity step. Once the restricted coefficients have been used to derive
#' the final split, [rating_table()] reports `output_variable` as the tariff
#' factor and does not also show the intermediate restricted variable.
#'
#' Conversely, [add_restriction()] can be called after `add_relativities()` to
#' adjust selected levels of the derived `output_variable`. The output variable
#' is then recognised as an existing refinement factor; users do not need to
#' set `allow_new_risk_factors = TRUE`. Levels omitted from the restriction
#' table are fixed at the relativities calculated by this step.
#'
#' ## Appropriate use
#'
#' `add_relativities()` is intended for refinement within an already reasonably
#' homogeneous GLM segment. It redistributes an existing coefficient across
#' sublevels using exposure-weighted relativities, while preserving the overall
#' level of the original coefficient when normalisation is used. Appropriate
#' applications include mild residual heterogeneity, monotonic tariff
#' differentiation and expert-based segmentation within a stable risk group
#' where the original GLM coefficient remains broadly representative.
#'
#' ## Limitations
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
#' @param model Object of class `rating_refinement`, created with
#'   [prepare_refinement()]. A fitted GLM, including a model returned by
#'   [refit()], is not accepted directly; retain and modify the corresponding
#'   refinement specification instead.
#' @param model_variable Character string. Existing variable in the GLM, or a
#'   restricted version created by an earlier [add_restriction()] step. Levels
#'   of the underlying model variable can be split into more detailed tariff
#'   segments. When an earlier restriction exists, its coefficients are used
#'   automatically.
#' @param split_variable Character string. More granular portfolio variable that
#'   defines the detailed groups inside `model_variable`.
#' @param output_variable Character string naming the resulting hybrid tariff
#'   factor. The default appends `_refined` to `model_variable`. A more
#'   application-specific name, such as `sbi_tariff_segment`, can make the
#'   intended tariff use clearer in model output and reporting. The name must
#'   not overwrite an existing column in the refinement data.
#' @param relativities Named list of data frames, usually created with
#'   [relativities()] and [split_level()].
#' @param exposure Character string. Exposure column used for weighting and,
#'   when requested, normalisation.
#' @param normalize Logical. If `TRUE`, normalise the supplied relativities by
#'   exposure within each split model level.
#'
#' @author Martin Haringa
#'
#' @return A `rating_refinement` object containing the stored relativity
#'   specification. The pricing GLM is not fitted again until [refit()] is
#'   called.
#'
#' @seealso [prepare_refinement()], [relativities()], [split_level()],
#'   [add_restriction()], [add_shrinkage()], [add_rebasing()],
#'   [add_smoothing()], [refit()], [rating_table()]
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
#'   ),
#'   split_level(
#'     "commercial",
#'     new_levels = c("shop", "office"),
#'     relativities = c(1.10, 0.90)
#'   )
#' )
#'
#' refined <- prepare_refinement(model, data = portfolio) |>
#'   add_relativities(
#'     model_variable = "construction",
#'     split_variable = "construction_detail",
#'     output_variable = "construction_tariff_segment",
#'     relativities = relativities,
#'     exposure = "exposure"
#'   )
#'
#' # A subsequent restriction can revise one derived level. The remaining
#' # tariff-segment levels are fixed at the relativities calculated above.
#' refined <- refined |>
#'   add_restriction(data.frame(
#'     construction_tariff_segment = "flat",
#'     construction_tariff_segment_restricted = 1.00
#'   ))
#'
#' refined_model <- refit(refined)
#' rating_table(refined_model, exposure = FALSE)
#'
#' @export
add_relativities <- function(model,
                             model_variable,
                             split_variable,
                             relativities,
                             exposure,
                             normalize = TRUE,
                             output_variable = paste0(model_variable, "_refined")) {
  .assert_refinement(model)

  if (!.is_single_string(model_variable)) {
    stop(
      "'model_variable' must be a single non-empty character string.",
      call. = FALSE
    )
  }
  if (!.is_single_string(output_variable)) {
    stop(
      "'output_variable' must be a single non-empty character string.",
      call. = FALSE
    )
  }
  prior_output_variables <- vapply(model$steps, function(step) {
    step$output_variable %||% ""
  }, character(1))
  if (output_variable %in% c(names(model$base$data), prior_output_variables)) {
    stop(
      "Column `", output_variable, "`, supplied through `output_variable`, ",
      "already exists in the refinement data. Choose a new name for the ",
      "refined tariff factor.",
      call. = FALSE
    )
  }
  resolved_source <- .resolve_relativities_source(model, model_variable)
  if (!resolved_source$source_model_variable %in% names(model$base$data)) {
    .stop_missing_relativities_source(model, model_variable)
  }
  .assert_column_name(split_variable, "split_variable", model$base$data)
  .assert_column_name(exposure, "exposure", model$base$data)
  .assert_single_logical(normalize, "normalize")

  .check_relativities(relativities)
  .validate_relativities_levels(
    data = model$base$data,
    source_model_variable = resolved_source$source_model_variable,
    split_variable = split_variable,
    relativities = relativities
  )

  .add_step(model, list(
    id = .next_step_id(model),
    type = "relativities",
    variable = model_variable,
    model_variable = model_variable,
    requested_model_variable = resolved_source$requested_model_variable,
    source_model_variable = resolved_source$source_model_variable,
    effective_model_variable = resolved_source$effective_model_variable,
    derived_model_variable = paste0(
      resolved_source$source_model_variable,
      "_rel"
    ),
    split_variable = split_variable,
    output_variable = output_variable,
    display_risk_factor = output_variable,
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
    original_formula = ref$base$formula,
    refinement_steps = ref$steps,
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
    effect_strength = NULL,
    relativities_df = NULL,
    relativities_base_df = NULL,
    normalize = NULL,
    exposure = NULL,
    base_risk_factor = NULL,
    risk_factor_split = NULL,
    output_variable = NULL,
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
  model_term <- step$model_term %||% variable
  restricted_df <- restrict_df(restrictions)

  if (isTRUE(step$new_risk_factor)) {
    formula_no_offset <- state$formula_no_offset
    if (!is.null(step$replaces)) {
      formula_no_offset <- update_formula_remove(
        formula_no_offset,
        step$replaces
      )
    }
    fm_add <- update_formula_add(
      offset_term = state$offset,
      fm_no_offset = formula_no_offset,
      add_term = names(restrictions)[2]
    )
    fm_replace <- list(
      formula = fm_add[[1]],
      formula_no_offset = formula_no_offset,
      offset = fm_add[[2]]
    )
  } else if (isTRUE(step$replace_refinement_offset)) {
    fm_replace <- .replace_refinement_offset(
      formula_no_offset = state$formula_no_offset,
      offset_term = state$offset,
      old_term = model_term,
      new_term = names(restrictions)[2]
    )
  } else {
    fm_replace <- .replace_formula_term(
      formula = state$formula_no_offset,
      old_term = model_term,
      new_term = names(restrictions)[2],
      offset_term = state$offset
    )
  }

  state$formula <- fm_replace$formula
  state$formula_no_offset <- fm_replace$formula_no_offset
  state$offset <- fm_replace$offset
  if (isTRUE(step$replace_refinement_offset) &&
      !is.null(step$derived_source_model_variable)) {
    source_values <- as.character(
      state$data[[step$derived_source_model_variable]]
    )
    split_values <- as.character(state$data[[variable]])
    effective_levels <- ifelse(
      source_values %in% (step$derived_split_levels %||% character()),
      split_values,
      source_values
    )
    restriction_values <- restrictions[[2]][match(
      effective_levels,
      as.character(restrictions[[1]])
    )]
    if (anyNA(restriction_values)) {
      missing_levels <- unique(effective_levels[is.na(restriction_values)])
      stop(
        "No restriction is available for refined level(s) of `", variable,
        "`: ", paste(missing_levels, collapse = ", "),
        call. = FALSE
      )
    }
    state$data[[names(restrictions)[2]]] <- restriction_values
  } else {
    state$data <- add_restrictions_df(
      state$data,
      restrictions,
      allow_new_levels = isTRUE(step$allow_new_levels)
    )
  }
  state$restrictions_lst[[variable]] <- restrictions

  if (is.null(state$rf_restricted_df)) {
    state$rf_restricted_df <- restricted_df
  } else {
    if (isTRUE(step$replace_refinement_offset)) {
      state$rf_restricted_df <- state$rf_restricted_df[
        state$rf_restricted_df$risk_factor != variable,
        ,
        drop = FALSE
      ]
    }
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

.apply_shrinkage_step <- function(state, step) {
  model_variable <- step$model_variable
  derived_model_variable <- step$derived_model_variable
  values <- .calculate_shrinkage(state, step)

  if (derived_model_variable %in% names(state$data)) {
    stop(
      "Generated shrinkage column `", derived_model_variable,
      "` already exists in the current refinement data.",
      call. = FALSE
    )
  }
  matched <- match(
    as.character(state$data[[model_variable]]),
    values$level
  )
  if (anyNA(matched)) {
    missing_levels <- unique(as.character(
      state$data[[model_variable]][is.na(matched)]
    ))
    stop(
      "No adjusted relativity is available for level(s) of `", model_variable,
      "`: ", paste(missing_levels, collapse = ", "),
      call. = FALSE
    )
  }
  state$data[[derived_model_variable]] <- values$adjusted_relativity[matched]

  if (isTRUE(step$replace_refinement_offset)) {
    fm_replace <- .replace_refinement_offset(
      formula_no_offset = state$formula_no_offset,
      offset_term = state$offset,
      old_term = step$effective_model_term,
      new_term = derived_model_variable
    )
  } else {
    fm_replace <- .replace_formula_term(
      formula = state$formula_no_offset,
      old_term = step$effective_model_term,
      new_term = derived_model_variable,
      offset_term = state$offset
    )
  }
  state$formula <- fm_replace$formula
  state$formula_no_offset <- fm_replace$formula_no_offset
  state$offset <- fm_replace$offset

  display <- data.frame(
    level = values$level,
    yhat = values$adjusted_relativity,
    risk_factor = rep(model_variable, nrow(values)),
    stringsAsFactors = FALSE
  )
  if (!is.null(state$rf_restricted_df)) {
    state$rf_restricted_df <- state$rf_restricted_df[
      state$rf_restricted_df$risk_factor != model_variable,
      ,
      drop = FALSE
    ]
  }
  state$rf_restricted_df <- unique(rbind(state$rf_restricted_df, display))
  rownames(state$rf_restricted_df) <- NULL

  restrictions <- stats::setNames(
    data.frame(values$level, values$adjusted_relativity,
               stringsAsFactors = FALSE),
    c(model_variable, derived_model_variable)
  )
  state$restrictions_lst[[derived_model_variable]] <- restrictions
  state$mgd_rst <- append(
    state$mgd_rst,
    list(c(model_variable, derived_model_variable))
  )
  state$new_col_nm <- .safe_unique_append(
    state$new_col_nm,
    derived_model_variable
  )
  state$old_col_nm <- .safe_unique_append(
    state$old_col_nm,
    model_variable
  )
  state$shrinkage_df <- values

  state
}

.apply_rebasing_step <- function(state, step) {
  model_variable <- step$model_variable
  derived_model_variable <- step$derived_model_variable
  values <- .calculate_rebasing(state, step)

  if (derived_model_variable %in% names(state$data)) {
    stop(
      "Generated rebasing column `", derived_model_variable,
      "` already exists in the current refinement data.",
      call. = FALSE
    )
  }
  matched <- match(as.character(state$data[[model_variable]]), values$level)
  if (anyNA(matched)) {
    missing_levels <- unique(as.character(
      state$data[[model_variable]][is.na(matched)]
    ))
    stop(
      "No rebased relativity is available for level(s) of `", model_variable,
      "`: ", paste(missing_levels, collapse = ", "),
      call. = FALSE
    )
  }
  state$data[[derived_model_variable]] <- values$rebased_relativity[matched]

  if (isTRUE(step$replace_refinement_offset)) {
    fm_replace <- .replace_refinement_offset(
      formula_no_offset = state$formula_no_offset,
      offset_term = state$offset,
      old_term = step$effective_model_term,
      new_term = derived_model_variable
    )
  } else {
    fm_replace <- .replace_formula_term(
      formula = state$formula_no_offset,
      old_term = step$effective_model_term,
      new_term = derived_model_variable,
      offset_term = state$offset
    )
  }
  state$formula <- fm_replace$formula
  state$formula_no_offset <- fm_replace$formula_no_offset
  state$offset <- fm_replace$offset

  display <- data.frame(
    level = values$level,
    yhat = values$rebased_relativity,
    risk_factor = rep(model_variable, nrow(values)),
    stringsAsFactors = FALSE
  )
  if (!is.null(state$rf_restricted_df)) {
    state$rf_restricted_df <- state$rf_restricted_df[
      state$rf_restricted_df$risk_factor != model_variable,
      ,
      drop = FALSE
    ]
  }
  state$rf_restricted_df <- unique(rbind(state$rf_restricted_df, display))
  rownames(state$rf_restricted_df) <- NULL

  restrictions <- stats::setNames(
    data.frame(values$level, values$rebased_relativity,
               stringsAsFactors = FALSE),
    c(model_variable, derived_model_variable)
  )
  state$restrictions_lst[[derived_model_variable]] <- restrictions
  state$mgd_rst <- append(
    state$mgd_rst,
    list(c(model_variable, derived_model_variable))
  )
  state$new_col_nm <- .safe_unique_append(
    state$new_col_nm,
    derived_model_variable
  )
  state$old_col_nm <- .safe_unique_append(
    state$old_col_nm,
    model_variable
  )
  state$rebasing_df <- values

  state
}

.smoothing_effect_weights <- function(data, smooth, source_variable,
                                      weights = NULL) {
  if (is.null(weights)) {
    return(rep(1, nrow(smooth)))
  }

  boundaries <- c(smooth$breaks_min[1], smooth$breaks_max)
  interval <- cut(
    data[[source_variable]],
    breaks = boundaries,
    include.lowest = TRUE,
    labels = FALSE
  )
  if (anyNA(interval)) {
    stop(
      "Cannot normalise `effect_strength` because some `", source_variable,
      "` values are outside the smoothing breaks.",
      call. = FALSE
    )
  }

  weight_values <- data[[weights]]
  if (!is.numeric(weight_values) || anyNA(weight_values) ||
      any(!is.finite(weight_values)) || any(weight_values < 0)) {
    stop(
      "The smoothing `weights` column `", weights,
      "` must contain finite, non-negative values when `effect_strength` ",
      "is used.",
      call. = FALSE
    )
  }

  interval_weights <- numeric(nrow(smooth))
  totals <- rowsum(weight_values, interval, reorder = FALSE)
  interval_weights[as.integer(rownames(totals))] <- totals[, 1]
  interval_weights
}

.apply_smoothing_effect_strength <- function(data, smooth, line, new_rf,
                                             source_variable, weights,
                                             effect_strength) {
  if (isTRUE(all.equal(effect_strength, 1))) {
    return(list(smooth = smooth, line = line, new_rf = new_rf))
  }

  all_values <- c(smooth$yhat, line$yhat)
  if (anyNA(all_values) || any(!is.finite(all_values)) ||
      any(all_values <= 0)) {
    stop(
      "`effect_strength` can only be applied when all smoothed relativities ",
      "are finite and greater than zero.",
      call. = FALSE
    )
  }

  level_weights <- .smoothing_effect_weights(
    data = data,
    smooth = smooth,
    source_variable = source_variable,
    weights = weights
  )
  if (sum(level_weights) <= 0) {
    stop(
      "`effect_strength` requires at least one smoothing level with positive ",
      "weight.",
      call. = FALSE
    )
  }

  centre <- exp(stats::weighted.mean(log(smooth$yhat), level_weights))
  original_mean <- stats::weighted.mean(smooth$yhat, level_weights)
  unscaled <- centre * (smooth$yhat / centre)^effect_strength
  normalization_factor <- original_mean /
    stats::weighted.mean(unscaled, level_weights)

  transform <- function(x) {
    centre * (x / centre)^effect_strength * normalization_factor
  }
  smooth$yhat <- transform(smooth$yhat)
  line$yhat <- transform(line$yhat)
  new_rf$yhat <- smooth$yhat

  attr(smooth, "effect_strength") <- effect_strength
  attr(smooth, "effect_centre") <- centre
  attr(smooth, "effect_normalization_factor") <- normalization_factor

  list(smooth = smooth, line = line, new_rf = new_rf)
}

.apply_smoothing_step <- function(state, step) {
  x_cut <- step$x_cut
  x_org <- step$x_org
  degree <- step$degree
  breaks <- step$breaks
  smoothing_spec <- .resolve_smoothing_method(step$smoothing %||% "spline")
  smoothing <- step$smoothing_code %||% smoothing_spec$code
  k <- step$k
  weights <- step$weights
  effect_strength <- step$effect_strength %||% 1

  borders_x_cut <- cut_borders_model(state$model_out, x_cut)

  if (identical(smoothing, "poly") && is.null(degree)) {
    degree <- nrow(borders_x_cut) - 1
  }

  if (smoothing %in% c("spline", "poly", "mpi", "mpd", "cx", "cv", "micx",
                       "micv", "mdcx", "mdcv", "gam")) {
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

  fit_poly <- fit_smoothing_curve(
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

  strengthened <- .apply_smoothing_effect_strength(
    data = state$data,
    smooth = df_poly,
    line = df_poly_line,
    new_rf = df_new_rf,
    source_variable = x_org,
    weights = weights,
    effect_strength = effect_strength
  )
  df_poly <- strengthened$smooth
  df_poly_line <- strengthened$line
  df_new_rf <- strengthened$new_rf

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
  state$effect_strength <- effect_strength

  state
}

.apply_relativities_step <- function(state, step) {
  requested_model_variable <- step$requested_model_variable %||%
    step$model_variable %||% step$risk_factor
  source_model_variable <- step$source_model_variable %||%
    requested_model_variable
  effective_model_variable <- step$effective_model_variable %||%
    requested_model_variable
  risk_factor_split <- step$risk_factor_split
  output_variable <- step$output_variable %||% step$display_risk_factor %||%
    risk_factor_split
  relativities <- step$relativities
  exposure <- step$exposure
  normalize <- isTRUE(step$normalize)

  df_new <- state$data
  base_coefficients <- .relativities_base_coefficients(
    state,
    effective_model_variable
  )

  if (!source_model_variable %in% names(df_new)) {
    stop(
      "Source model variable `", source_model_variable,
      "` is not in the refinement data.",
      call. = FALSE
    )
  }
  if (!risk_factor_split %in% names(df_new)) {
    stop("risk_factor_split column: ", risk_factor_split, " is not in the model data.", call. = FALSE)
  }
  if (!exposure %in% names(df_new)) {
    stop("exposure column: ", exposure, " is not in the model data.", call. = FALSE)
  }
  if (nrow(base_coefficients) == 0L) {
    stop(
      "No coefficient information is available for effective model variable `",
      effective_model_variable, "`.",
      call. = FALSE
    )
  }

  rel_levels <- names(relativities)
  model_levels <- base_coefficients$level
  missing_levels <- setdiff(rel_levels, model_levels)
  if (length(missing_levels) > 0) {
    stop(
      "The following levels in 'relativities' are not present in risk_factor '",
      source_model_variable, "': ", paste(missing_levels, collapse = ", "),
      call. = FALSE
    )
  }

  rel_df <- .build_relativities_df(relativities)

  exposure_df <- stats::aggregate(
    df_new[[exposure]],
    by = list(
      level = df_new[[source_model_variable]],
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
    missing_rows <- is.na(rel_df$exposure)
    invalid <- rel_df[missing_rows, c("level", "new_level"), drop = FALSE]
    stop(
      "The following `split_variable` levels do not occur within their ",
      "specified `model_variable` levels: ",
      paste0(invalid$level, " -> ", invalid$new_level, collapse = ", "),
      call. = FALSE
    )
  }

  if (normalize) {
    rel_df <- .normalize_relativities(rel_df)
  } else {
    rel_df$relativity_final <- rel_df$relativity
  }

  base_df <- base_coefficients
  names(base_df)[2] <- "estimate_base"

  rel_df <- merge(
    rel_df,
    base_df,
    by = "level",
    all.x = TRUE,
    sort = FALSE
  )

  rel_df$estimate <- rel_df$estimate_base * rel_df$relativity_final

  new_rf_name <- paste0(source_model_variable, "_rel")
  display_rf_name <- output_variable

  map_unsplit <- base_coefficients
  names(map_unsplit) <- c(source_model_variable, "estimate_base")

  map_split <- rel_df[, c("level", "new_level", "estimate")]
  names(map_split)[names(map_split) == "new_level"] <- risk_factor_split

  df_restricted <- df_new
  df_restricted$row_id__tmp <- seq_len(nrow(df_restricted))

  df_restricted[[output_variable]] <- as.character(
    df_restricted[[source_model_variable]]
  )
  for (i in seq_len(nrow(rel_df))) {
    replace <-
      as.character(df_restricted[[source_model_variable]]) == rel_df$level[i] &
      as.character(df_restricted[[risk_factor_split]]) == rel_df$new_level[i]
    df_restricted[[output_variable]][replace] <- rel_df$new_level[i]
  }

  df_restricted <- merge(
    df_restricted,
    map_unsplit,
    by = source_model_variable,
    all.x = TRUE,
    sort = FALSE
  )

  df_restricted <- merge(
    df_restricted,
    map_split,
    by.x = c(source_model_variable, risk_factor_split),
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
    unique(as.character(df_restricted[[source_model_variable]])),
    names(relativities)
  )

  if (length(unsplit_levels) > 0) {
    unsplit_df <- data.frame(
      level = unsplit_levels,
      yhat = map_unsplit$estimate_base[
        match(unsplit_levels, map_unsplit[[source_model_variable]])
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

  if (!identical(effective_model_variable, source_model_variable) &&
      !is.null(state$rf_restricted_df)) {
    state$rf_restricted_df <- state$rf_restricted_df[
      state$rf_restricted_df$risk_factor != effective_model_variable,
      ,
      drop = FALSE
    ]
    if (nrow(state$rf_restricted_df) == 0L) {
      state$rf_restricted_df <- NULL
    }
  }

  if (is.null(state$rf_restricted_df)) {
    state$rf_restricted_df <- restricted_df_new
  } else {
    state$rf_restricted_df <- unique(rbind(state$rf_restricted_df, restricted_df_new))
  }
  rownames(state$rf_restricted_df) <- NULL

  if (identical(effective_model_variable, source_model_variable)) {
    fm_replace <- .replace_formula_term(
      formula = state$formula_no_offset,
      old_term = source_model_variable,
      new_term = new_rf_name,
      offset_term = state$offset
    )
  } else {
    fm_replace <- .replace_refinement_offset(
      formula_no_offset = state$formula_no_offset,
      offset_term = state$offset,
      old_term = effective_model_variable,
      new_term = new_rf_name
    )
  }

  state$formula <- fm_replace$formula
  state$formula_no_offset <- fm_replace$formula_no_offset
  state$offset <- fm_replace$offset
  state$data <- df_restricted

  state$restrictions_lst[[new_rf_name]] <- relativities
  state$mgd_rst <- append(
    state$mgd_rst,
    list(c(source_model_variable, new_rf_name))
  )
  state$new_col_nm <- .safe_unique_append(state$new_col_nm, c(new_rf_name, display_rf_name))
  state$old_col_nm <- .safe_unique_append(
    state$old_col_nm,
    source_model_variable
  )

  state$relativities_df <- rel_df
  state$relativities_base_df <- base_coefficients
  state$normalize <- normalize
  state$exposure <- exposure
  state$base_risk_factor <- source_model_variable
  state$risk_factor_split <- risk_factor_split
  state$output_variable <- output_variable
  state$display_risk_factor <- display_rf_name
  state$model_risk_factor <- new_rf_name

  state
}

.apply_refinement_step <- function(state, step) {
  switch(
    step$type,
    restriction = .apply_restriction_step(state, step),
    shrinkage = .apply_shrinkage_step(state, step),
    rebasing = .apply_rebasing_step(state, step),
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
      original_formula = state$original_formula,
      refinement_steps = state$refinement_steps,
      refinement_created_at = state$refinement_created_at,
      new_col_nm = state$new_col_nm,
      old_col_nm = state$old_col_nm,
      mgd_rst = state$mgd_rst,
      mgd_smt = state$mgd_smt,
      smoothing = state$smoothing,
      effect_strength = state$effect_strength
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
    original_formula = state$original_formula,
    refinement_steps = state$refinement_steps,
    refinement_created_at = state$refinement_created_at,
    new_col_nm = state$new_col_nm,
    old_col_nm = state$old_col_nm,
    mgd_rst = state$mgd_rst,
    mgd_smt = state$mgd_smt,
    relativities_df = state$relativities_df,
    normalize = state$normalize,
    exposure = state$exposure,
    base_risk_factor = state$base_risk_factor,
    risk_factor_split = state$risk_factor_split,
    output_variable = state$output_variable,
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
    original_formula = model$original_formula %||% model_out$formula,
    refinement_steps = model$refinement_steps %||% NULL,
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
          mult_lst[[i]] <- mult_lst[[i]][c(
            risk_factor_name,
            paste0(risk_factor_name, "_rst99")
          )]
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

  family_call <- call(
    x$model_out$family$family,
    link = x$model_out$family$link
  )
  y$call[[1]] <- quote(glm)
  y$call$formula <- x$formula_restricted
  y$call$family <- family_call
  y$call$data <- quote(refined_data)
  y$call$offset <- NULL

  offweights <- NULL
  if (!is.null(lst_call$weights)) {
    offweights <- append(offweights, as.character(lst_call$weights))
  }
  if (!is.null(lst_call$offset)) {
    offweights <- append(offweights, as.character(lst_call$offset)[2])
  }

  if (inherits(x, "smooth")) {
    attr(y, "new_rf") <- x[["new_rf"]]
  }

  if (inherits(x, "restricted")) {
    attr(y, "new_rf_rst") <- x[["rf_restricted_df"]]
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
  attr(y, "original_formula") <- x$original_formula %||% x$model_out$formula
  attr(y, "refinement_steps") <- x$refinement_steps %||% list()
  attr(y, "refinement_base_model") <- x$model_out
  attr(y, "refinement_package") <- "insurancerating"
  attr(y, "refinement_package_version") <- tryCatch(
    as.character(utils::packageVersion("insurancerating")),
    error = function(e) NA_character_
  )
  attr(y, "refinement_created_at") <- x$refinement_created_at %||% Sys.time()
  attr(y, "refinement_refitted_at") <- Sys.time()

  refinement_classes <- c(
    if (inherits(x, "restricted")) "refitrestricted",
    if (inherits(x, "smooth") || isTRUE(attr(x, "has_smoothing"))) "refitsmooth"
  )
  class(y) <- unique(c(refinement_classes, class(y)))
  y
}


# -----------------------------------------------------------------------------
# Refit
# -----------------------------------------------------------------------------

.fit_refined_glm <- function(state, intercept_only = FALSE, ...) {
  legacy_obj <- .preview_to_legacy_object(state)
  .legacy_refit_glm(legacy_obj, intercept_only = intercept_only, ...)
}

#' Fit a prepared refinement specification
#'
#' @description
#' Apply the ordered steps stored in a `rating_refinement` object and fit the
#' resulting pricing GLM. This evaluates the current refinement specification;
#' it may be called repeatedly while smoothing, restrictions, shrinkage,
#' rebasing or sublevel relativities are being reviewed.
#'
#' @details
#' `refit()` applies the stored steps in their recorded order, constructs the
#' required tariff variables and offsets, updates the model formula and calls
#' [stats::glm()] with the original model family. Additional fitting arguments
#' can be supplied through `...`.
#'
#' ## Actuarial interpretation
#'
#' The refitted model represents the combined effect of the original GLM
#' structure and the explicit actuarial assumptions stored in the refinement.
#' Its coefficients and predictions should be assessed against exposure,
#' observed experience, model diagnostics and the unrestricted model. A refit
#' does not establish that a manual restriction or curve edit is statistically
#' estimated; it applies that assumption as specified.
#'
#' ## Intercept-only recalibration
#'
#' With `intercept_only = FALSE`, the refined GLM is fitted with the remaining
#' free model terms that are still present after applying the refinement steps.
#' With `intercept_only = TRUE`, remaining original model effects are fixed as
#' offsets based on their existing fitted relativities. Only the intercept is
#' then estimated. Consequently, relative differences between those fixed
#' effects remain unchanged while the overall expected premium level is
#' recalibrated to the supplied model data.
#'
#' In practical actuarial work, `intercept_only = TRUE` is generally suitable
#' for a controlled actuarial or commercial refinement of an accepted tariff
#' structure. Examples include a small manual restriction, a limited curve
#' adjustment or a final calibration in which the relativities of unaffected
#' risk factors should remain unchanged.
#'
#' Use `intercept_only = FALSE` when the refinement forms part of substantive
#' model development. The remaining free model terms are then estimated again,
#' allowing the GLM to account for dependence between risk factors and find a
#' new joint statistical optimum conditional on the fixed refinement steps.
#' Coefficients of risk factors that were not directly refined may therefore
#' also change.
#'
#' ## Model result and further refinement
#'
#' Printing the returned model first shows the original and refitted formulas,
#' the model family, whether an intercept-only refit was used, and a concise
#' description of every restriction, smoothing, shrinkage or relativity step. This is
#' followed by the regular `glm` output with the model call, coefficients,
#' degrees of freedom, deviance and AIC. The object continues to inherit from
#' `glm`, so standard methods such as [stats::predict.glm()] and
#' [summary.glm()] remain available.
#'
#' The returned GLM is a fitted result, not an editable refinement
#' specification. Retain the original `rating_refinement` object when further
#' changes may be required. Passing the refitted GLM to [prepare_refinement()]
#' starts a new workflow from that model and does not reconstruct the earlier
#' sequence of refinement steps.
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
#' @return A fitted object inheriting from `glm`. Compatibility classes
#'   `refitrestricted`, `refitsmooth`, or both are added when relevant. The
#'   object stores refinement metadata used by [rating_table()] and
#'   [rating_grid()] to identify fixed relativities, smoothed variables and
#'   derived tariff factors.
#'
#' @seealso [prepare_refinement()], [add_smoothing()], [edit_smoothing()],
#'   [add_restriction()], [add_shrinkage()], [add_rebasing()],
#'   [add_relativities()], [rating_table()], [rating_grid()],
#'   [audit_refinement()]
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
#' refinement <- prepare_refinement(model) |>
#'   add_restriction(zip_df)
#'
#' refined_model <- refit(refinement, intercept_only = TRUE)
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

  state$refinement_created_at <- object$metadata$created_at %||% Sys.time()
  .fit_refined_glm(state, intercept_only = intercept_only, ...)
}


# -----------------------------------------------------------------------------
# Refit model printing
# -----------------------------------------------------------------------------

.format_refinement_step <- function(step, index) {
  if (identical(step$type, "restriction")) {
    target <- names(step$restrictions)[2]
    detail <- paste0(
      "Restriction: ", step$variable, " -> ", target,
      " (", nrow(step$restrictions), " levels)"
    )
    if (isTRUE(step$new_risk_factor)) {
      detail <- paste0(detail, " [expert-specified new risk factor]")
      if (!is.null(step$replaces)) {
        detail <- paste0(detail, " [replaces ", step$replaces, "]")
      }
    } else if (length(step$new_levels %||% character()) > 0) {
      detail <- paste0(
        detail,
        " [new level",
        if (length(step$new_levels) > 1) "s" else "",
        ": ",
        paste(step$new_levels, collapse = ", "),
        "]"
      )
    }
  } else if (identical(step$type, "smoothing")) {
    model_variable <- step$model_variable %||% step$x_cut %||% step$variable
    source_variable <- step$source_variable %||% step$x_org
    smoothing <- .resolve_smoothing_method(step$smoothing %||% "spline")$method
    detail <- paste0(
      "Smoothing: ", model_variable,
      if (!is.null(source_variable)) paste0(" from ", source_variable) else "",
      " (method: ", smoothing
    )
    if (!is.null(step$k)) {
      detail <- paste0(detail, ", k: ", step$k)
    } else if (!is.null(step$degree)) {
      detail <- paste0(detail, ", degree: ", step$degree)
    }
    detail <- paste0(
      detail,
      ", effect strength: ",
      format(step$effect_strength %||% 1, trim = TRUE)
    )
    detail <- paste0(detail, ")")
  } else if (identical(step$type, "relativities")) {
    model_variable <- step$model_variable %||% step$risk_factor %||% step$variable
    effective_model_variable <- step$effective_model_variable %||%
      model_variable
    split_variable <- step$split_variable %||% step$risk_factor_split
    output_variable <- step$output_variable %||% step$display_risk_factor %||%
      split_variable
    detail <- paste0(
      "Relativities: ", model_variable,
      if (!identical(effective_model_variable, model_variable)) {
        paste0(" using restricted coefficients from ", effective_model_variable)
      } else {
        ""
      },
      if (!is.null(split_variable)) paste0(" split by ", split_variable) else "",
      if (!is.null(output_variable)) paste0(" -> ", output_variable) else "",
      " (normalised: ", if (isTRUE(step$normalize)) "yes" else "no", ")"
    )
  } else if (identical(step$type, "shrinkage")) {
    weight_label <- step$weight_spec$label %||% step$weights %||% "unknown"
    if (isTRUE(step$weight_spec$inferred)) {
      weight_label <- paste0(
        weight_label,
        " derived from ",
        step$weight_spec$inferred_from %||% "model weights"
      )
    }
    detail <- paste0(
      "Shrinkage: ", step$model_variable,
      " (credibility: ", format(step$credibility, trim = TRUE),
      ", weights: ", weight_label,
      ", weighted mean preserved)"
    )
  } else if (identical(step$type, "rebasing")) {
    selection <- if (identical(step$method, "explicit")) {
      "explicit"
    } else {
      paste0(
        "largest weight using ",
        step$weight_spec$label %||% step$weights %||% "unknown"
      )
    }
    detail <- paste0(
      "Rebasing: ", step$model_variable,
      " (reference: ", step$reference_level,
      ", selection: ", selection,
      ", original reference relativity: ",
      format(step$reference_relativity, trim = TRUE),
      ")"
    )
  } else {
    detail <- paste0("Refinement step: ", step$type %||% "unknown")
  }

  paste0("  ", index, ". ", detail)
}

.print_refitted_glm <- function(x, ...) {
  original_formula <- attr(x, "original_formula")
  refined_formula <- stats::formula(x)
  steps <- attr(x, "refinement_steps")

  cat("Refined generalized linear model\n\n")
  cat("Original formula:\n  ")
  cat(paste(deparse(original_formula %||% refined_formula), collapse = "\n  "))
  cat("\n\nRefitted formula:\n  ")
  cat(paste(deparse(refined_formula), collapse = "\n  "))
  cat(
    "\n\nFamily: ",
    x$family$family,
    " (link: ",
    x$family$link,
    ")\n",
    sep = ""
  )
  cat(
    "Intercept-only refit: ",
    if (isTRUE(attr(x, "intercept_only"))) "yes" else "no",
    "\n",
    sep = ""
  )

  if (length(steps) > 0) {
    cat("Refinement steps:\n")
    for (i in seq_along(steps)) {
      cat(.format_refinement_step(steps[[i]], i), "\n", sep = "")
    }
  } else {
    cat("Refinement steps: metadata unavailable (legacy object)\n")
  }

  cat("\n")
  glm_object <- x
  class(glm_object) <- setdiff(
    class(glm_object),
    c("refitrestricted", "refitsmooth")
  )
  print(glm_object, ...)
  invisible(x)
}

#' @export
#' @noRd
print.refitrestricted <- function(x, ...) {
  .print_refitted_glm(x, ...)
}

#' @export
#' @noRd
print.refitsmooth <- function(x, ...) {
  .print_refitted_glm(x, ...)
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
