# -----------------------------------------------------------------------------
# Rating table helpers
# -----------------------------------------------------------------------------

#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom stats terms
#' @importFrom utils stack
#'
#' @keywords internal
.collect_refit_new_rf <- function(model) {

  out <- list()

  x_rst <- attr(model, "new_rf_rst")
  x_smt <- attr(model, "new_rf")

  if (!is.null(x_rst) && nrow(x_rst) > 0) {
    out[[length(out) + 1]] <- x_rst
  }

  if (!is.null(x_smt) && nrow(x_smt) > 0) {
    out[[length(out) + 1]] <- x_smt
  }

  if (length(out) == 0) {
    return(NULL)
  }

  x <- do.call(rbind, out)
  x <- unique(x)
  rownames(x) <- NULL
  x
}


#' @keywords internal
.normalize_level_key <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.numeric(x)) {
    x <- format(x, trim = TRUE, scientific = FALSE, digits = 15)
    x <- sub("\\.?0+$", "", x)
    x[x == ""] <- "0"
    return(x)
  }

  trimws(as.character(x))
}


#' @keywords internal
.rating_table_reference_level <- function(model, risk_factor) {
  steps <- attr(model, "refinement_steps")

  if (!is.null(steps) && length(steps) > 0L) {
    rebasing_steps <- Filter(function(step) {
      identical(step$type, "rebasing") &&
        identical(step$model_variable, risk_factor) &&
        !is.null(step$reference_level)
    }, steps)

    if (length(rebasing_steps) > 0L) {
      return(as.character(
        rebasing_steps[[length(rebasing_steps)]]$reference_level
      ))
    }
  }

  model_frame <- tryCatch(
    stats::model.frame(model),
    error = function(e) NULL
  )
  if (!is.null(model_frame) && risk_factor %in% names(model_frame) &&
      is.factor(model_frame[[risk_factor]])) {
    contrast_matrix <- tryCatch(
      stats::contrasts(model_frame[[risk_factor]]),
      error = function(e) NULL
    )
    if (!is.null(contrast_matrix)) {
      zero_rows <- which(rowSums(abs(contrast_matrix)) == 0)
      if (length(zero_rows) == 1L) {
        return(rownames(contrast_matrix)[zero_rows])
      }
    }
  }

  xlevels <- model$xlevels[[risk_factor]]
  if (!is.null(xlevels) && length(xlevels) > 0L) {
    return(as.character(xlevels[[1L]]))
  }

  NULL
}


#' @keywords internal
.resolve_rating_table_order_model <- function(order_model, model_names) {
  if (is.null(order_model)) {
    return(1L)
  }

  if (!is.character(order_model) || length(order_model) != 1L ||
      is.na(order_model) || !nzchar(order_model)) {
    stop(
      "`order_model` must be NULL or the name of one supplied model.",
      call. = FALSE
    )
  }

  requested <- sub("^est_", "", order_model)
  index <- match(requested, model_names)
  if (is.na(index)) {
    stop(
      "Model `", order_model, "` supplied through `order_model` was not found. ",
      "Choose one of: ", paste(model_names, collapse = ", "), ".",
      call. = FALSE
    )
  }

  index
}


#' @keywords internal
.rating_table_ordered_levels <- function(model, risk_factor) {
  model_frame <- tryCatch(
    stats::model.frame(model),
    error = function(e) NULL
  )
  if (is.null(model_frame) || !risk_factor %in% names(model_frame) ||
      !is.ordered(model_frame[[risk_factor]])) {
    return(NULL)
  }
  as.character(levels(model_frame[[risk_factor]]))
}


#' @keywords internal
.validate_level_order_by_risk_factor <- function(x, risk_factors) {
  if (is.null(x)) {
    return(character())
  }

  allowed <- c(
    "model", "alphabetical", "estimate_ascending", "estimate_descending"
  )
  if (!is.character(x) || length(x) == 0L || is.null(names(x)) ||
      anyNA(x) || anyNA(names(x)) || any(!nzchar(names(x))) ||
      anyDuplicated(names(x))) {
    stop(
      "`level_order_by_risk_factor` must be a named character vector with ",
      "one unique risk-factor name for every ordering override.",
      call. = FALSE
    )
  }
  invalid_values <- setdiff(unique(x), allowed)
  if (length(invalid_values) > 0L) {
    stop(
      "Unknown ordering in `level_order_by_risk_factor`: ",
      paste(invalid_values, collapse = ", "), ". Choose from: ",
      paste(allowed, collapse = ", "), ".",
      call. = FALSE
    )
  }
  unknown_factors <- setdiff(names(x), risk_factors)
  if (length(unknown_factors) > 0L) {
    stop(
      "Risk factor(s) in `level_order_by_risk_factor` were not found in the ",
      "rating table: ", paste(unknown_factors, collapse = ", "), ".",
      call. = FALSE
    )
  }
  x
}


#' @keywords internal
.parse_rating_table_numeric_levels <- function(levels) {
  levels <- trimws(as.character(levels))
  number_pattern <- paste0(
    "^[+-]?(?:",
    "(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?",
    "|Inf)$"
  )

  parse_number <- function(value) {
    value <- trimws(value)
    if (!grepl(number_pattern, value, perl = TRUE)) {
      return(NA_real_)
    }
    suppressWarnings(as.numeric(value))
  }

  lower <- upper <- rep(NA_real_, length(levels))

  for (i in seq_along(levels)) {
    level <- levels[[i]]
    first <- substr(level, 1L, 1L)
    last <- substr(level, nchar(level), nchar(level))
    is_interval <- first %in% c("(", "[") && last %in% c(")", "]")

    if (is_interval) {
      inside <- substr(level, 2L, nchar(level) - 1L)
      bounds <- strsplit(inside, ",", fixed = TRUE)[[1L]]
      if (length(bounds) != 2L) {
        return(NULL)
      }
      lower[[i]] <- parse_number(bounds[[1L]])
      upper[[i]] <- parse_number(bounds[[2L]])
    } else {
      value <- parse_number(level)
      lower[[i]] <- value
      upper[[i]] <- value
    }
  }

  if (anyNA(lower) || anyNA(upper) || any(lower > upper)) {
    return(NULL)
  }

  data.frame(
    lower = lower,
    upper = upper,
    original_order = seq_along(levels)
  )
}


#' @keywords internal
.order_rating_table <- function(data, model_tables, models, model_names,
                                reference_first, level_order,
                                level_order_by_risk_factor,
                                numeric_level_order,
                                risk_factor_order, order_model_index) {
  if (nrow(data) == 0L) {
    return(data)
  }

  data$risk_factor <- as.character(data$risk_factor)
  data$level <- as.character(data$level)

  selected_table <- model_tables[[order_model_index]]
  selected_risk_factors <- unique(as.character(selected_table$risk_factor))
  all_risk_factors <- unique(data$risk_factor)

  if (identical(risk_factor_order, "alphabetical")) {
    non_intercept <- sort(setdiff(all_risk_factors, "(Intercept)"))
    ordered_risk_factors <- c(
      intersect("(Intercept)", all_risk_factors),
      non_intercept
    )
  } else {
    ordered_risk_factors <- unique(c(selected_risk_factors, all_risk_factors))
    if ("(Intercept)" %in% ordered_risk_factors) {
      ordered_risk_factors <- c(
        "(Intercept)",
        setdiff(ordered_risk_factors, "(Intercept)")
      )
    }
  }

  reference_levels <- stats::setNames(
    rep(NA_character_, length(ordered_risk_factors)),
    ordered_risk_factors
  )
  level_order_by_risk_factor <- .validate_level_order_by_risk_factor(
    level_order_by_risk_factor,
    setdiff(ordered_risk_factors, "(Intercept)")
  )
  ordered_rows <- integer()

  for (risk_factor in ordered_risk_factors) {
    rows <- which(data$risk_factor == risk_factor)
    if (length(rows) == 0L) {
      next
    }

    current_levels <- data$level[rows]
    source_model_index <- order_model_index
    if (!risk_factor %in% selected_table$risk_factor) {
      containing_models <- which(vapply(model_tables, function(table) {
        risk_factor %in% table$risk_factor
      }, logical(1)))
      if (length(containing_models) > 0L) {
        source_model_index <- containing_models[[1L]]
      }
    }
    source_table <- model_tables[[source_model_index]]
    model_levels <- as.character(
      source_table$level[source_table$risk_factor == risk_factor]
    )
    model_levels <- unique(c(model_levels, current_levels))

    numeric_levels <- if (identical(numeric_level_order, "ascending")) {
      .parse_rating_table_numeric_levels(current_levels)
    } else {
      NULL
    }
    ordered_factor_levels <- .rating_table_ordered_levels(
      models[[source_model_index]],
      risk_factor
    )
    has_override <- risk_factor %in% names(level_order_by_risk_factor)
    selected_level_order <- if (has_override) {
      unname(level_order_by_risk_factor[[risk_factor]])
    } else {
      level_order
    }
    preserve_reference_position <- FALSE

    if (!is.null(numeric_levels)) {
      numeric_order <- with(
        numeric_levels,
        order(lower, upper, original_order)
      )
      level_sequence <- current_levels[numeric_order]
      preserve_reference_position <- TRUE
    } else if (has_override) {
      if (identical(selected_level_order, "alphabetical")) {
        level_sequence <- sort(unique(current_levels))
      } else if (selected_level_order %in%
                 c("estimate_ascending", "estimate_descending")) {
        decreasing <- identical(selected_level_order, "estimate_descending")
        estimate_column <- paste0("est_", model_names[[source_model_index]])
        estimates <- data[[estimate_column]][rows]
        sort_values <- if (decreasing) -estimates else estimates
        local_order <- order(sort_values, seq_along(rows), na.last = TRUE)
        level_sequence <- current_levels[local_order]
      } else {
        level_sequence <- model_levels
      }
      preserve_reference_position <- TRUE
    } else if (!is.null(ordered_factor_levels)) {
      level_sequence <- c(
        intersect(ordered_factor_levels, current_levels),
        setdiff(current_levels, ordered_factor_levels)
      )
      preserve_reference_position <- TRUE
    } else if (identical(selected_level_order, "alphabetical")) {
      level_sequence <- sort(unique(current_levels))
    } else if (selected_level_order %in%
               c("estimate_ascending", "estimate_descending")) {
      decreasing <- identical(selected_level_order, "estimate_descending")
      estimate_column <- paste0("est_", model_names[[source_model_index]])
      estimates <- data[[estimate_column]][rows]
      sort_values <- if (decreasing) -estimates else estimates
      local_order <- order(sort_values, seq_along(rows), na.last = TRUE)
      level_sequence <- current_levels[local_order]
      preserve_reference_position <- TRUE
    } else {
      level_sequence <- model_levels
    }

    reference_level <- .rating_table_reference_level(
      models[[source_model_index]],
      risk_factor
    )
    if (!is.null(reference_level) && reference_level %in% current_levels) {
      reference_levels[[risk_factor]] <- reference_level
      if (isTRUE(reference_first) && !preserve_reference_position) {
        level_sequence <- c(
          reference_level,
          setdiff(level_sequence, reference_level)
        )
      }
    }

    row_order <- match(level_sequence, current_levels)
    row_order <- row_order[!is.na(row_order)]
    remaining <- setdiff(seq_along(rows), row_order)
    ordered_rows <- c(ordered_rows, rows[c(row_order, remaining)])
  }

  out <- data[ordered_rows, , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "reference_levels") <- reference_levels[!is.na(reference_levels)]
  out
}


#' @keywords internal
.get_restriction_map <- function(model) {
  out <- attr(model, "restriction_map")
  if (is.null(out)) {
    return(NULL)
  }

  if (!is.data.frame(out)) {
    return(NULL)
  }

  needed <- c("source_var", "risk_factor")
  if (!all(needed %in% names(out))) {
    return(NULL)
  }

  out$source_var <- as.character(out$source_var)
  out$risk_factor <- as.character(out$risk_factor)
  unique(out[, needed, drop = FALSE])
}


#' @keywords internal
.get_relativities_exposure_spec <- function(model, risk_factor) {
  steps <- attr(model, "refinement_steps")
  if (is.null(steps) || length(steps) == 0L) {
    return(NULL)
  }

  matches <- vapply(steps, function(step) {
    identical(step$type, "relativities") &&
      identical(
        step$output_variable %||% step$display_risk_factor %||%
          step$split_variable %||% step$risk_factor_split,
        risk_factor
      )
  }, logical(1))

  if (!any(matches)) {
    return(NULL)
  }

  step <- steps[[utils::tail(which(matches), 1L)]]
  source_variable <- step$source_model_variable %||%
    step$model_variable %||% step$risk_factor
  split_variable <- step$split_variable %||% step$risk_factor_split

  if (is.null(source_variable) || is.null(split_variable) ||
      is.null(step$relativities)) {
    return(NULL)
  }

  list(
    source_variable = source_variable,
    split_variable = split_variable,
    relativities = step$relativities
  )
}


#' @keywords internal
.aggregate_relativities_exposure <- function(model_data, exposure_col,
                                             risk_factor, output_levels,
                                             spec) {
  source_variable <- spec$source_variable
  split_variable <- spec$split_variable

  if (!all(c(source_variable, split_variable) %in% names(model_data))) {
    return(NULL)
  }

  rel_df <- .build_relativities_df(spec$relativities)
  output_levels <- unique(as.character(output_levels))
  pieces <- lapply(output_levels, function(output_level) {
    split_rows <- rel_df[rel_df$new_level == output_level, , drop = FALSE]

    if (nrow(split_rows) > 0L) {
      selected <- rep(FALSE, nrow(model_data))
      for (i in seq_len(nrow(split_rows))) {
        selected <- selected |
          (as.character(model_data[[source_variable]]) == split_rows$level[i] &
             as.character(model_data[[split_variable]]) ==
               split_rows$new_level[i])
      }
    } else {
      selected <- as.character(model_data[[source_variable]]) == output_level
    }

    data.frame(
      level = output_level,
      exposure_value = sum(model_data[[exposure_col]][selected], na.rm = TRUE),
      risk_factor = risk_factor,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, pieces)
  names(out)[names(out) == "exposure_value"] <- exposure_col
  out
}


#' @keywords internal
.map_exposure_source_var <- function(model, risk_factor, model_data_names) {

  rf <- as.character(risk_factor)

  # 1. explicit restriction map from refit object
  # IMPORTANT: this must come before exact-match lookup, because the refit
  # data often also contains the restricted column itself (e.g. zip_rst),
  # while exposure should still be aggregated on the original source variable
  # (e.g. zip).
  rst_map <- .get_restriction_map(model)
  if (!is.null(rst_map)) {
    hit <- rst_map[rst_map$risk_factor == rf, , drop = FALSE]
    if (nrow(hit) > 0) {
      src <- hit$source_var[1]
      if (!is.na(src) && src %in% model_data_names) {
        return(src)
      }
    }
  }

  # 2. hybrid factor created by add_relativities()
  relativity_spec <- .get_relativities_exposure_spec(model, rf)
  if (!is.null(relativity_spec) &&
      all(c(
        relativity_spec$source_variable,
        relativity_spec$split_variable
      ) %in% model_data_names)) {
    return(relativity_spec$source_variable)
  }

  # 3. exact match
  if (rf %in% model_data_names) {
    return(rf)
  }

  # 4. fallback for legacy naming conventions
  rf_base <- sub("_rst99$", "", rf)
  rf_base <- sub("_rst$", "", rf_base)
  if (rf_base %in% model_data_names) {
    return(rf_base)
  }

  # 5. smoothing fallback
  rf_base <- sub("_smooth$", "", rf)
  if (rf_base %in% model_data_names) {
    return(rf_base)
  }

  NULL
}


#' @keywords internal
.resolve_rating_table_model_data <- function(model, model_data = NULL) {
  if (!is.null(model_data)) {
    return(as.data.frame(model_data))
  }

  if (!is.null(model$data)) {
    return(as.data.frame(model$data))
  }

  NULL
}


#' @keywords internal
.resolve_rating_table_model_data_name <- function(model, model_data = NULL) {
  if (!is.null(model_data)) {
    return(deparse(substitute(model_data)))
  }

  if (!is.null(model$data)) {
    return("model$data")
  }

  NULL
}


#' @keywords internal
.infer_exposure_col <- function(model, model_data) {

  if (is.null(model_data)) {
    return(NULL)
  }

  candidates <- character(0)

  # 1. weights from model call
  w_call <- tryCatch(model$call$weights, error = function(e) NULL)

  if (!is.null(w_call)) {
    w_chr <- deparse(w_call)
    if (length(w_chr) == 1 && w_chr %in% names(model_data)) {
      candidates <- c(candidates, w_chr)
    }
  }

  # 2. refit metadata
  offweights <- attr(model, "offweights")

  if (!is.null(offweights)) {
    offweights <- offweights[offweights %in% names(model_data)]
    if (length(offweights) > 0) {
      candidates <- c(candidates, offweights)
    }
  }

  candidates <- unique(candidates)

  if (length(candidates) == 0) {
    return(NULL)
  }

  # keep only numeric columns
  candidates <- candidates[vapply(
    candidates,
    function(nm) is.numeric(model_data[[nm]]),
    logical(1)
  )]

  if (length(candidates) == 0) {
    return(NULL)
  }

  # strong preference for a column literally called "exposure"
  if ("exposure" %in% candidates) {
    return("exposure")
  }

  candidates[1]
}


#' @keywords internal
.resolve_exposure_spec <- function(model, model_data, exposure = TRUE,
                                   exposure_output = NULL) {

  if (isFALSE(exposure)) {
    return(list(
      use_exposure = FALSE,
      exposure_col = NULL,
      exposure_out = NULL
    ))
  }

  if (isTRUE(exposure)) {
    exposure_col <- .infer_exposure_col(model, model_data)
  } else if (is.character(exposure) && length(exposure) == 1) {
    exposure_col <- exposure
  } else {
    stop(
      "`exposure` must be TRUE, FALSE, or a single character string.",
      call. = FALSE
    )
  }

  if (is.null(exposure_col)) {
    return(list(
      use_exposure = FALSE,
      exposure_col = NULL,
      exposure_out = NULL
    ))
  }

  if (is.null(model_data)) {
    return(list(
      use_exposure = FALSE,
      exposure_col = NULL,
      exposure_out = NULL
    ))
  }

  if (!exposure_col %in% names(model_data)) {
    stop(
      "Exposure column '", exposure_col, "' is not present in model data.",
      call. = FALSE
    )
  }

  if (!is.numeric(model_data[[exposure_col]])) {
    stop(
      "Exposure column '", exposure_col, "' must be numeric.",
      call. = FALSE
    )
  }

  exposure_out <- if (is.null(exposure_output)) exposure_col else exposure_output

  list(
    use_exposure = TRUE,
    exposure_col = exposure_col,
    exposure_out = exposure_out
  )
}


#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom stats terms
#' @importFrom utils stack
#'
#' @keywords internal
.rating_table_one_model <- function(model,
                                    model_data = NULL,
                                    exposure = TRUE,
                                    exposure_output = NULL,
                                    colname = "estimate",
                                    exponentiate = TRUE,
                                    round_exposure = 0) {

  # ---------------------------------------------------------------------------
  # Block pre-refit workflow objects
  # ---------------------------------------------------------------------------
  if (inherits(model, "rating_refinement")) {
    stop(
      "Input is a 'rating_refinement' object. Call refit() first, then use rating_table().",
      call. = FALSE
    )
  }

  if (inherits(model, c("restricted", "smooth"))) {
    stop(
      "Model is not refitted yet. Please call refit_glm() first.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Acceptable classes
  # ---------------------------------------------------------------------------
  if (!inherits(model, c("glm", "refitsmooth", "refitrestricted"))) {
    stop("Input must be a glm or a refit_glm()/refit() object.", call. = FALSE)
  }

  model_data_use <- .resolve_rating_table_model_data(model, model_data)
  model_data_name <- .resolve_rating_table_model_data_name(model, model_data)

  exposure_info <- .resolve_exposure_spec(
    model = model,
    model_data = model_data_use,
    exposure = exposure,
    exposure_output = exposure_output
  )

  use_exposure <- exposure_info$use_exposure
  exposure_col <- exposure_info$exposure_col
  exposure_out <- exposure_info$exposure_out

  # ---------------------------------------------------------------------------
  # Extract original xlevels from glm
  # ---------------------------------------------------------------------------
  xl <- model$xlevels
  xl_names <- character(0)

  xl_df <- data.frame(
    risk_factor = character(),
    level = character(),
    ind_values = character(),
    stringsAsFactors = FALSE
  )

  if (length(xl) > 0) {
    xl_names <- names(xl)
    tmp <- utils::stack(xl)
    tmp[] <- lapply(tmp, as.character)
    names(tmp) <- c("level", "risk_factor")
    tmp$ind_values <- paste0(tmp$risk_factor, tmp$level)
    xl_df <- rbind(xl_df, tmp)
  }

  # ---------------------------------------------------------------------------
  # Add refit() / refit_glm() attributes
  # IMPORTANT: collect BOTH new_rf_rst and new_rf
  # ---------------------------------------------------------------------------
  x_new <- NULL

  if (inherits(model, c("refitsmooth", "refitrestricted"))) {
    x_new <- .collect_refit_new_rf(model)

    if (!is.null(x_new) && nrow(x_new) > 0) {
      x_new$risk_factor <- as.character(x_new$risk_factor)
      x_new$level <- as.character(x_new$level)
      x_new$ind_values <- paste0(x_new$risk_factor, x_new$level)

      x2 <- x_new[, c("risk_factor", "level", "ind_values"), drop = FALSE]
      xl_df <- rbind(xl_df, x2)
      xl_names <- c(xl_names, unique(x_new$risk_factor))
    }
  }

  # ---------------------------------------------------------------------------
  # Exposure by factor level
  # ---------------------------------------------------------------------------
  if (isTRUE(use_exposure) && !is.null(model_data_use)) {

    model_data_use <- as.data.frame(model_data_use)

    rf_all <- unique(xl_df$risk_factor)

    rf_map <- data.frame(
      risk_factor = rf_all,
      source_var = vapply(
        rf_all,
        function(rf) {
          out <- .map_exposure_source_var(model, rf, names(model_data_use))
          if (is.null(out)) "" else out
        },
        character(1)
      ),
      stringsAsFactors = FALSE
    )

    rf_map_in  <- rf_map[rf_map$source_var != "", , drop = FALSE]
    rf_map_out <- rf_map[rf_map$source_var == "", , drop = FALSE]

    if (nrow(rf_map_out) > 0 && !is.null(model_data_name)) {
      message(
        paste(rf_map_out$risk_factor, collapse = ", "),
        " not in ",
        model_data_name
      )
    }

    if (nrow(rf_map_in) > 0) {
      listexp <- lapply(seq_len(nrow(rf_map_in)), function(i) {
        rf_i  <- rf_map_in$risk_factor[i]
        src_i <- rf_map_in$source_var[i]

        relativity_spec <- .get_relativities_exposure_spec(model, rf_i)
        if (!is.null(relativity_spec)) {
          mapped <- .aggregate_relativities_exposure(
            model_data = model_data_use,
            exposure_col = exposure_col,
            risk_factor = rf_i,
            output_levels = xl_df$level[xl_df$risk_factor == rf_i],
            spec = relativity_spec
          )
          if (!is.null(mapped)) {
            return(mapped)
          }
        }

        tmp <- stats::aggregate(
          model_data_use[[exposure_col]],
          by = list(level = as.character(model_data_use[[src_i]])),
          FUN = sum,
          na.rm = TRUE
        )

        names(tmp)[names(tmp) == "x"] <- exposure_col
        tmp$risk_factor <- rf_i
        tmp
      })

      dfexp <- if (length(listexp) > 0) do.call(rbind, listexp) else NULL

      if (!is.null(dfexp)) {
        dfexp$level <- as.character(dfexp$level)
        dfexp$risk_factor <- as.character(dfexp$risk_factor)

        if (!identical(exposure_col, exposure_out)) {
          names(dfexp)[names(dfexp) == exposure_col] <- exposure_out
        }

        xl_df$level <- as.character(xl_df$level)
        xl_df$risk_factor <- as.character(xl_df$risk_factor)

        xl_df <- dplyr::left_join(
          xl_df,
          dfexp,
          by = c("level", "risk_factor")
        )
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Coefficients and p-values
  # ---------------------------------------------------------------------------
  ret <- coefficients(summary(model))
  ret <- cbind(ind = rownames(ret), data.frame(ret, row.names = NULL))

  all_coefs <- stats::coef(model)

  if (length(all_coefs) != nrow(ret)) {
    coefs_df <- utils::stack(all_coefs)
    colnames(coefs_df)[colnames(coefs_df) == "values"] <- "Estimate"
    ret <- merge(x = coefs_df, y = ret, by = c("ind", "Estimate"), all.x = TRUE)
  }

  coef_vals <- coefficients(model)
  vals <- utils::stack(coef_vals)

  vals$pvalues <- as.numeric(ret[, 5])
  vals$pvalues <- ifelse(is.na(vals$pvalues), -9e9, vals$pvalues)
  vals$ind <- as.character(vals$ind)

  new_col_nm0 <- attr(model, "new_col_nm")

  if (inherits(model, c("refitsmooth", "refitrestricted")) &&
      !is.null(x_new) && nrow(x_new) > 0) {
    x_rf_names <- unique(x_new$risk_factor)
    new_col_nm0 <- unique(c(new_col_nm0, x_rf_names))
  }

  if (inherits(model, c("refitsmooth", "refitrestricted")) &&
      !is.null(x_new) && nrow(x_new) > 0) {
    x2 <- x_new[, c("yhat", "ind_values"), drop = FALSE]
    colnames(x2) <- c("values", "ind")
    x2$pvalues <- NA
    x2$values <- log(x2$values)
    vals <- rbind(vals, x2)
  }

  uit <- dplyr::full_join(xl_df, vals, by = c("ind_values" = "ind"))

  uit$values <- ifelse(
    is.na(uit$pvalues) &
      !(endsWith(uit$risk_factor, "_smooth") |
          uit$risk_factor %in% new_col_nm0),
    0,
    uit$values
  )

  int <- attr(stats::terms(model), "intercept")

  # Intercept
  intercept_condition <- int == 1 & uit$ind_values == "(Intercept)"
  uit$level[intercept_condition] <- "(Intercept)"
  uit$risk_factor[intercept_condition] <- "(Intercept)"

  # Continuous factors
  level_condition <- is.na(uit$level) & is.na(uit$risk_factor)
  uit$level[level_condition] <- uit$ind_values[level_condition]
  uit$risk_factor[is.na(uit$risk_factor)] <- uit$ind_values[is.na(uit$risk_factor)]

  # Exponentiate if needed
  if (isTRUE(exponentiate)) {
    uit$values <- exp(uit$values)
  }

  # Intercept first
  if (int == 1) {
    intercept_ix <- which(uit$risk_factor == "(Intercept)")
    uit <- uit[c(intercept_ix, setdiff(seq_len(nrow(uit)), intercept_ix)), , drop = FALSE]
  }

  row.names(uit) <- NULL

  # Select columns
  if (isTRUE(use_exposure) &&
      !is.null(exposure_out) &&
      exposure_out %in% names(uit)) {
    selected_columns <- c("risk_factor", "level", "values", exposure_out, "pvalues")
    selected_columns <- intersect(selected_columns, names(uit))
    uit <- uit[, selected_columns, drop = FALSE]
    uit[[exposure_out]] <- round(uit[[exposure_out]], round_exposure)
  } else {
    selected_columns <- c("risk_factor", "level", "values", "pvalues")
    selected_columns <- intersect(selected_columns, names(uit))
    uit <- uit[, selected_columns, drop = FALSE]
  }

  names(uit)[names(uit) == "values"] <- colname

  # p-values -> stars
  uit$pvalues[uit$pvalues < 0] <- NA
  uit$pvalues <- vapply(uit$pvalues, make_stars, FUN.VALUE = character(1))

  # Cleanup
  uit$risk_factor <- sub("_rst99$", "", uit$risk_factor)

  # intercept_only handling
  io <- attr(model, "intercept_only")
  cf <- attr(model, "continuous_factors")

  if (isTRUE(io) && !is.null(cf) && nrow(cf) > 0) {
    cf$pvalues <- NA_character_

    missing_cols <- setdiff(names(uit), names(cf))
    if (length(missing_cols) > 0) {
      for (nm in missing_cols) {
        cf[[nm]] <- NA
      }
    }

    cf <- cf[, names(uit), drop = FALSE]
    uit <- rbind(uit, cf)
  }

  attr(uit, "exposure_out") <- exposure_out
  attr(uit, "model_data_name") <- model_data_name

  uit
}


#' Present fitted pricing-model effects as a rating table
#'
#' @description
#' Extract coefficients from one or more fitted GLMs and organise them by risk
#' factor and level. Reference levels are made explicit, coefficients can be
#' expressed as multiplicative relativities, and portfolio exposure can be
#' attached to support actuarial review.
#'
#' @param ... One or more fitted `glm` objects, including models returned by
#'   [refit()]. Object expressions are used to construct the dynamic estimate
#'   column names.
#' @param model_data Optional data frame used to fit the models. If `NULL`,
#'   the function tries to use `model$data` for each supplied model.
#' @param exposure Logical or character string. If `TRUE`, exposure is added
#'   if it can be inferred from the model. If `FALSE`, no exposure is added.
#'   If a character string is supplied, it is interpreted as the exposure column
#'   name.
#' @param exposure_output Optional character string naming the exposure column
#'   in the output.
#'   If `NULL`, the original exposure column name is used.
#' @param exponentiate Logical. If `TRUE`, coefficients are
#'   exponentiated and shown as relativities. If `FALSE`, coefficients are shown
#'   on the model scale.
#' @param significance Logical. If `TRUE`, add a separate `signif_*` column for
#'   each model containing significance indicators based on coefficient
#'   p-values. The corresponding `est_*` columns remain numeric.
#' @param reference_first Logical. If `TRUE`, place the reference level first
#'   when the global ordering of a nominal risk factor is `"model"` or
#'   `"alphabetical"`. Numeric levels, ordered factors, estimate-based ordering
#'   and explicit per-factor overrides retain their selected order. For an
#'   ordinary GLM, the reference is obtained from the fitted factor contrasts.
#'   After [add_rebasing()], the selected rebasing level is used.
#' @param level_order Character string controlling the default order of nominal
#'   factor levels. `"estimate_descending"` (default) places the highest fitted
#'   effect first, `"estimate_ascending"` places the lowest first,
#'   `"model"` retains the fitted model order and `"alphabetical"` sorts labels.
#'   Numeric levels and explicitly ordered factors use their substantive order
#'   instead.
#' @param level_order_by_risk_factor Optional named character vector providing
#'   an ordering override for individual risk factors. Names identify risk
#'   factors and values must be `"model"`, `"alphabetical"`,
#'   `"estimate_ascending"` or `"estimate_descending"`. For example,
#'   `c(urbanisation = "model", sector = "estimate_descending")` preserves an
#'   ordinal urbanisation scale while ordering sector relativities from high to
#'   low. Numeric ordering still takes precedence.
#' @param numeric_level_order Character string controlling levels that are all
#'   recognisable as numbers or numeric intervals. `"ascending"` orders them by
#'   numeric value, or by the lower and then upper interval boundary, regardless
#'   of `level_order`. This correctly orders labels such as `(100,200]` and
#'   `(1000,2000]`. `"as_specified"` leaves these levels to `level_order`.
#'   Numeric ordering takes precedence over `reference_first`, so the reference
#'   level is not moved away from its numerical position.
#' @param risk_factor_order Character string controlling risk-factor order.
#'   `"model"` retains the order in the fitted model; `"alphabetical"` sorts
#'   risk-factor names. The intercept, when present, remains first.
#' @param order_model Optional character string naming the supplied model whose
#'   level order, reference levels and estimates are used for sorting. This is
#'   mainly relevant when several models are compared. If `NULL`, the first
#'   supplied model is used. Both `"frequency"` and `"est_frequency"` are
#'   accepted for a model object named `frequency`. If that model does not
#'   contain a particular risk factor, the first supplied model containing the
#'   factor provides its order and reference level.
#' @param round_exposure Non-negative number of digits used to round exposure.
#' @param exposure_name Deprecated. Use `exposure_output` instead.
#' @param signif_stars Deprecated. Use `significance` instead.
#'
#' @details
#' ## Coefficients and relativities
#'
#' The table contains one row per model term level. For factor variables,
#' the reference level is added explicitly with relativity `1` when
#' `exponentiate = TRUE`, or coefficient `0` when `exponentiate = FALSE`.
#' Numeric model terms are retained on the scale supplied by the fitted model
#' structure.
#'
#' Estimate columns are named from the supplied model expressions, for example
#' `est_frequency` for an object named `frequency`. When several models are
#' supplied, their effects are joined by risk factor and level.
#'
#' ## Actuarial interpretation
#'
#' With a log-link GLM, exponentiated coefficients represent conditional
#' multiplicative effects relative to the model reference level. They should be
#' interpreted together with the model specification and should not be confused
#' with the unadjusted observed measures returned by [factor_analysis()].
#'
#' Exposure by level provides context for the amount of portfolio information
#' supporting each fitted effect. Significance indicators describe evidence
#' conditional on the fitted model; they do not measure practical materiality,
#' temporal stability or suitability for direct tariff implementation.
#'
#' Comparing multiple models is useful for assessing changes between
#' unrestricted and refined specifications, or between alternative model
#' formulations. Comparable response definitions and coefficient scales remain
#' the responsibility of the analyst.
#'
#' ## Row order and reference levels
#'
#' By default, risk factors follow the model formula. Numeric levels and
#' intervals are shown from low to high, explicitly ordered factors retain their
#' factor-level sequence, and remaining nominal factors are shown from highest
#' to lowest fitted effect. This separates structural order from an ordering
#' used to compare tariff differentiation.
#'
#' `reference_first` applies only when a nominal factor uses model or
#' alphabetical order. It does not move the reference level ahead of a numeric,
#' ordinal or estimate-based sequence. The reference remains recorded in the
#' rating-table metadata, including a reference selected with [add_rebasing()].
#'
#' Alternative level ordering is useful for specific review tasks. Alphabetical
#' order supports lookup and export, while model order can retain a deliberately
#' specified factor sequence. Use `level_order_by_risk_factor` when nominal and
#' ordinal factors require different treatment in the same table. With several
#' models, `order_model` defines which fitted specification provides
#' estimate-based ordering. [as_gt()] and [autoplot.rating_table()] retain the
#' row order established here.
#'
#' Only a factor stored with `ordered = TRUE` is identified automatically as an
#' ordinal scale. A regular factor may also have deliberately arranged levels,
#' but that intention cannot be distinguished reliably from an arbitrary model
#' order. Use `level_order_by_risk_factor = c(variable = "model")` to preserve
#' that sequence explicitly.
#'
#' Numeric labels and intervals receive separate treatment because alphabetical
#' ordering can give an incorrect tariff sequence. With the default
#' `numeric_level_order = "ascending"`, a risk factor is sorted numerically only
#' when every displayed level is either a complete number or a valid interval
#' with two numeric boundaries. Mixed labels such as `"Industry 1"` remain
#' categorical. Set `numeric_level_order = "as_specified"` when the fitted model
#' order or another `level_order` should be retained deliberately.
#'
#' ## Significance indicators
#'
#' When `significance = TRUE`, every model receives its own `signif_*` column.
#' For example, models named `frequency` and `severity` produce
#' `est_frequency`, `signif_frequency`, `est_severity` and
#' `signif_severity`. Keeping estimates and indicators separate preserves the
#' numeric type of the fitted effects for subsequent calculations, filtering
#' and export.
#'
#' [as_gt()] combines each estimate with its corresponding significance
#' indicator for presentation and adds the significance thresholds as a source
#' note below the table. Reference levels generally have no separate
#' coefficient test and therefore have no significance indicator.
#'
#' `rating_table()` accepts fitted models only. A `rating_refinement`
#' specification must first be fitted with [refit()].
#'
#' @return A data frame with classes `"rating_table"`, legacy `"riskfactor"`
#' and `"data.frame"`. It can be inspected and manipulated directly with
#' ordinary data-frame operations. For backward compatibility, `x$df` returns
#' the same table without the package-specific class and metadata. The table
#' contains:
#' \describe{
#'   \item{risk_factor}{Model term or risk-factor name.}
#'   \item{level}{Factor level or term representation.}
#'   \item{`est_*`}{Coefficient or exponentiated relativity for each supplied
#'   model. The suffix is derived from the model expression.}
#'   \item{`signif_*`}{Optional significance indicator for each model.}
#'   \item{Exposure column}{Optional aggregated exposure, retaining the
#'   requested output name.}
#' }
#'
#' @author Martin Haringa
#'
#' @seealso [as_gt()] for grouped tabular presentation,
#'   [autoplot.rating_table()] for graphical comparison,
#'   [factor_analysis()] for observed portfolio experience, and [refit()] for
#'   fitting a refinement specification.
#'
#' @importFrom dplyr full_join
#' @importFrom utils stack
#' @importFrom stats coefficients
#'
#' @examples
#' df <- MTPL
#' df$zip <- as.factor(df$zip)
#'
#' freq <- glm(
#'   nclaims ~ bm + zip + offset(log(exposure)),
#'   family = poisson(),
#'   data = df
#' )
#'
#' fitted_effects <- rating_table(
#'   freq,
#'   model_data = df,
#'   exposure = "exposure"
#' )
#'
#' fitted_effects
#' head(fitted_effects)
#'
#' # The historical accessor remains available for existing code
#' identical(fitted_effects$df, as.data.frame(fitted_effects))
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   as_gt(fitted_effects)
#' }
#'
#' # Keep coefficients on the model scale instead of exponentiating
#' rating_table(
#'   freq,
#'   model_data = df,
#'   exposure = "exposure",
#'   exponentiate = FALSE
#' )
#'
#' # Significance is supplementary to exposure and stability assessment
#' rating_table(
#'   freq,
#'   model_data = df,
#'   exposure = "exposure",
#'   significance = TRUE
#' )
#'
#' # Compare two fitted models side by side
#' freq_simple <- glm(
#'   nclaims ~ bm + offset(log(exposure)),
#'   family = poisson(),
#'   data = df
#' )
#'
#' rating_table(
#'   freq_simple,
#'   freq,
#'   model_data = df,
#'   exposure = FALSE
#' )
#'
#' # Order all levels by fitted relativity
#' rating_table(
#'   freq,
#'   model_data = df,
#'   exposure = "exposure",
#'   level_order = "estimate_descending"
#' )
#'
#' @export
rating_table <- function(..., model_data = NULL, exposure = TRUE,
                         exposure_output = NULL,
                         exponentiate = TRUE, significance = FALSE,
                         reference_first = TRUE,
                         level_order = c(
                           "estimate_descending", "estimate_ascending",
                           "model", "alphabetical"
                         ),
                         level_order_by_risk_factor = NULL,
                         numeric_level_order = c("ascending", "as_specified"),
                         risk_factor_order = c("model", "alphabetical"),
                         order_model = NULL,
                         round_exposure = 0, exposure_name = NULL,
                         signif_stars = NULL) {

  mc <- match.call(expand.dots = FALSE)
  models <- list(...)

  if (!missing(exposure_name)) {
    lifecycle::deprecate_warn(
      when = "0.9.0",
      what = "rating_table(exposure_name)",
      with = "rating_table(exposure_output)"
    )
    if (!is.null(exposure_output)) {
      stop("Use only one of 'exposure_output' and deprecated 'exposure_name'.",
           call. = FALSE)
    }
    exposure_output <- exposure_name
  }

  if (!missing(signif_stars)) {
    lifecycle::deprecate_warn(
      when = "0.9.0",
      what = "rating_table(signif_stars)",
      with = "rating_table(significance)"
    )
    if (!missing(significance)) {
      stop("Use only one of 'significance' and deprecated 'signif_stars'.",
           call. = FALSE)
    }
    significance <- signif_stars
  }

  if (!is.logical(significance) || length(significance) != 1 || is.na(significance)) {
    stop("'significance' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(reference_first) || length(reference_first) != 1L ||
      is.na(reference_first)) {
    stop("`reference_first` must be TRUE or FALSE.", call. = FALSE)
  }
  level_order <- match.arg(level_order)
  numeric_level_order <- match.arg(numeric_level_order)
  risk_factor_order <- match.arg(risk_factor_order)

  if (length(models) == 0) {
    stop("At least one model must be supplied.", call. = FALSE)
  }

  bad_refinement <- vapply(models, inherits, logical(1), what = "rating_refinement")
  if (any(bad_refinement)) {
    stop(
      "rating_table() only works after refit(). One or more inputs are 'rating_refinement' objects.",
      call. = FALSE
    )
  }

  bad_legacy <- vapply(models, function(x) inherits(x, c("restricted", "smooth")), logical(1))
  if (any(bad_legacy)) {
    stop(
      "rating_table() only works after refit_glm()/refit(). One or more inputs are legacy 'restricted'/'smooth' objects.",
      call. = FALSE
    )
  }

  ok_classes <- vapply(
    models,
    function(x) inherits(x, c("glm", "refitsmooth", "refitrestricted")),
    logical(1)
  )
  if (!all(ok_classes)) {
    stop(
      "All inputs to rating_table() must be glm, refitsmooth or refitrestricted objects.",
      call. = FALSE
    )
  }

  cols <- .rating_table_model_names(models, mc)
  order_model_index <- .resolve_rating_table_order_model(order_model, cols)

  rf_list <- vector("list", length(models))
  exposure_out_nm <- NULL
  model_data_name_out <- NULL

  for (i in seq_along(models)) {
    df <- .rating_table_one_model(
      models[[i]],
      model_data = model_data,
      exposure = exposure,
      exposure_output = exposure_output,
      exponentiate = exponentiate,
      round_exposure = round_exposure
    )

    if (is.null(exposure_out_nm)) {
      exposure_out_nm <- attr(df, "exposure_out")
    }
    if (is.null(model_data_name_out)) {
      model_data_name_out <- attr(df, "model_data_name")
    }

    names(df)[names(df) == "estimate"] <- paste0("est_", cols[i])
    names(df)[names(df) == "pvalues"]  <- paste0("signif_", cols[i])

    rf_list[[i]] <- df
  }

  if (length(rf_list) == 1) {
    rf_fj <- rf_list[[1]]
  } else if (!is.null(exposure_out_nm)) {
    rf_fj <- Reduce(function(d1, d2) {
      dplyr::full_join(d1, d2, by = c("risk_factor", "level", exposure_out_nm))
    }, rf_list)

    keep_cols <- c(
      "risk_factor", "level",
      paste0("est_", cols),
      paste0("signif_", cols),
      exposure_out_nm
    )
    rf_fj <- rf_fj[, intersect(keep_cols, names(rf_fj)), drop = FALSE]

  } else {
    rf_fj <- Reduce(function(d1, d2) {
      dplyr::full_join(d1, d2, by = c("risk_factor", "level"))
    }, rf_list)

    keep_cols <- c(
      "risk_factor", "level",
      paste0("est_", cols),
      paste0("signif_", cols)
    )
    rf_fj <- rf_fj[, intersect(keep_cols, names(rf_fj)), drop = FALSE]
  }

  if (!isTRUE(significance)) {
    drop_cols <- paste0("signif_", cols)
    rf_fj <- rf_fj[, !(names(rf_fj) %in% drop_cols), drop = FALSE]
  }

  rf_fj <- .order_rating_table(
    data = rf_fj,
    model_tables = rf_list,
    models = models,
    model_names = cols,
    reference_first = reference_first,
    level_order = level_order,
    level_order_by_risk_factor = level_order_by_risk_factor,
    numeric_level_order = numeric_level_order,
    risk_factor_order = risk_factor_order,
    order_model_index = order_model_index
  )
  reference_levels <- attr(rf_fj, "reference_levels", exact = TRUE)

  rf_fj_stars <- NULL
  signif_levels <- NULL

  if (isTRUE(significance)) {
    signif_levels <-
      "Significance levels: *** p < 0.001; ** p < 0.01; * p < 0.05; . p < 0.1"

    rf_fj_stars <- rf_fj

    for (i in seq_along(cols)) {
      est_num  <- round(rf_fj_stars[[paste0("est_", cols[i])]], 6)
      est_char <- format(est_num, digits = 6, nsmall = 2)

      stars_char <- rf_fj_stars[[paste0("signif_", cols[i])]]
      stars_char[is.na(stars_char)] <- ""

      rf_fj_stars[[paste0("est_", cols[i])]] <-
        format(paste0(est_char, " ", stars_char), justify = "left")
    }

    drop_cols <- paste0("signif_", cols)
    rf_fj_stars <- rf_fj_stars[, !(names(rf_fj_stars) %in% drop_cols), drop = FALSE]
  }

  .new_rating_table(
    data = rf_fj,
    df_stars = rf_fj_stars,
    models = cols,
    exposure = exposure_out_nm,
    model_data = model_data_name_out,
    exponentiate = exponentiate,
    significance = significance,
    signif_levels = signif_levels,
    reference_levels = reference_levels,
    reference_first = reference_first,
    level_order = level_order,
    level_order_by_risk_factor = level_order_by_risk_factor,
    numeric_level_order = numeric_level_order,
    risk_factor_order = risk_factor_order,
    order_model = cols[[order_model_index]]
  )
}

.rating_table_metadata_names <- c(
  "df_stars", "models", "exposure", "model_data", "expon",
  "significance", "signif_stars", "signif_levels", "observed_experience",
  "reference_levels", "reference_first", "level_order",
  "level_order_by_risk_factor", "numeric_level_order", "risk_factor_order",
  "order_model"
)

.new_rating_table <- function(data, df_stars, models, exposure, model_data,
                              exponentiate, significance, signif_levels,
                              reference_levels = NULL,
                              reference_first = TRUE,
                              level_order = "estimate_descending",
                              level_order_by_risk_factor = NULL,
                              numeric_level_order = "ascending",
                              risk_factor_order = "model",
                              order_model = NULL) {
  out <- as.data.frame(data)
  attr(out, "df_stars") <- df_stars
  attr(out, "models") <- models
  attr(out, "exposure") <- exposure
  attr(out, "model_data") <- model_data
  attr(out, "expon") <- exponentiate
  attr(out, "significance") <- significance
  attr(out, "signif_stars") <- significance
  attr(out, "signif_levels") <- signif_levels
  attr(out, "observed_experience") <- NULL
  attr(out, "reference_levels") <- reference_levels
  attr(out, "reference_first") <- reference_first
  attr(out, "level_order") <- level_order
  attr(out, "level_order_by_risk_factor") <- level_order_by_risk_factor
  attr(out, "numeric_level_order") <- numeric_level_order
  attr(out, "risk_factor_order") <- risk_factor_order
  attr(out, "order_model") <- order_model
  class(out) <- c("rating_table", "riskfactor", "data.frame")
  out
}

.rating_table_data <- function(x) {
  if (!is.data.frame(x)) {
    return(x[["df"]])
  }

  out <- x
  for (name in .rating_table_metadata_names) {
    attr(out, name) <- NULL
  }
  class(out) <- "data.frame"
  out
}

.rating_table_metadata <- function(x, name) {
  if (is.data.frame(x)) {
    return(attr(x, name, exact = TRUE))
  }
  x[[name]]
}

#' Backward-compatible access to rating-table contents
#'
#' @description
#' A `rating_table` is a data frame. This method keeps the historical `x$df`
#' accessor available while allowing ordinary `$` access to table columns.
#' Package metadata is returned only when the requested name is not a column.
#'
#' @param x A `rating_table` object.
#' @param name Column or legacy component name.
#'
#' @return A table column, the underlying data frame for `name = "df"`, or a
#' stored metadata component.
#'
#' @keywords internal
#' @export
`$.rating_table` <- function(x, name) {
  if (identical(name, "df")) {
    return(.rating_table_data(x))
  }
  if (name %in% names(x)) {
    return(.subset2(x, name))
  }
  if (name %in% .rating_table_metadata_names) {
    return(.rating_table_metadata(x, name))
  }
  NULL
}


#' Add portfolio experience to a rating table
#'
#' @description
#' `add_portfolio_experience()` enriches a [rating_table()] object with observed
#' portfolio experience. When `data` is supplied, observed experience is
#' calculated automatically for all risk factors in the rating table, unless
#' `risk_factors` is specified. Existing [factor_analysis()] results can also be
#' supplied through `observed`.
#'
#' This makes it possible to compare fitted GLM relativities with observed
#' portfolio patterns in [autoplot.rating_table()]. The full observed output is
#' stored on the rating table, so [autoplot.rating_table()] can later switch
#' between metrics such as `"frequency"`, `"average_severity"` and
#' `"risk_premium"` without recalculating the summaries.
#'
#' The observed metric is scaled before plotting. With `scale = "reference"`
#' the metric is divided by the observed value of the model reference level. If
#' a clear reference level cannot be found, the metric is scaled to its mean.
#' With `scale = "mean"`, the metric is always scaled to its mean.
#'
#' @usage
#' add_portfolio_experience(x, ...)
#'
#' \method{add_portfolio_experience}{rating_table}(
#'   x,
#'   observed = NULL,
#'   data = NULL,
#'   risk_factors = NULL,
#'   claim_count = NULL,
#'   exposure = NULL,
#'   claim_amount = NULL,
#'   metric = NULL,
#'   label = "Observed experience",
#'   color = NULL,
#'   scale = c("reference", "mean"),
#'   experience = NULL,
#'   ...
#' )
#'
#' @aliases add_portfolio_experience.rating_table
#'
#' @param x A `rating_table` object returned by [rating_table()].
#' @param observed Optional [factor_analysis()] object or list of
#'   [factor_analysis()] objects. If supplied, these observed summaries are
#'   attached directly.
#' @param data Optional `data.frame`. If `observed = NULL`, observed experience
#'   is calculated from this data.
#' @param risk_factors Optional character vector. Risk factors for which
#'   observed experience should be calculated. If `NULL`, all risk factors in
#'   the rating table are used.
#' @param claim_count Optional character string. Claim count column used by
#'   [factor_analysis()].
#' @param exposure Optional character string. Exposure column used by
#'   [factor_analysis()].
#' @param claim_amount Optional character string. Claim amount column used by
#'   [factor_analysis()].
#' @param metric Optional character string. Default observed metric to plot.
#'   Common choices are `"frequency"`, `"severity"`/`"average_severity"` and
#'   `"risk_premium"`. The metric can also be overridden in
#'   [autoplot.rating_table()].
#' @param label Character; legend label for the observed experience line.
#' @param color Optional line color. If `NULL`, the internal risk premium color
#'   is used.
#' @param scale Character; scaling applied before plotting. One of
#'   `"reference"` or `"mean"`.
#' @param experience Deprecated alias for `observed`.
#' @param ... Unused.
#'
#' @return A `rating_table` object with observed portfolio experience attached.
#'
#' @author
#'   Martin Haringa
#'
#' @examples
#' df <- MTPL2
#' df$area <- as.factor(df$area)
#'
#' model <- glm(
#'   nclaims ~ area + offset(log(exposure)),
#'   family = poisson(),
#'   data = df
#' )
#'
#' rating_table(model, model_data = df, exposure = "exposure") |>
#'   add_portfolio_experience(
#'     data = df,
#'     claim_count = "nclaims",
#'     exposure = "exposure"
#'   ) |>
#'   autoplot(risk_factors = "area", metric = "frequency")
#'
#' observed <- factor_analysis(
#'   df,
#'   risk_factors = "area",
#'   claim_count = "nclaims",
#'   exposure = "exposure"
#' )
#'
#' rating_table(model, model_data = df, exposure = "exposure") |>
#'   add_portfolio_experience(observed = observed) |>
#'   autoplot(risk_factors = "area")
#'
#' @export
add_portfolio_experience <- function(x, ...) {
  UseMethod("add_portfolio_experience")
}

#' @export
add_portfolio_experience.rating_table <- function(x,
                                                  observed = NULL,
                                                  data = NULL,
                                                  risk_factors = NULL,
                                                  claim_count = NULL,
                                                  exposure = NULL,
                                                  claim_amount = NULL,
                                                  metric = NULL,
                                                  label = "Observed experience",
                                                  color = NULL,
                                                  scale = c("reference", "mean"),
                                                  experience = NULL,
                                                  ...) {
  .check_dots_empty(...)
  if (!is.null(experience)) {
    if (!is.null(observed)) {
      stop("Use only one of `observed` and deprecated `experience`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "add_portfolio_experience(experience)",
      "add_portfolio_experience(observed)"
    )
    observed <- experience
  }

  if (!is.null(observed) && !is.null(data)) {
    stop("Use only one of `observed` and `data`.", call. = FALSE)
  }
  if (!is.character(label) || length(label) != 1 || is.na(label)) {
    stop("`label` must be a single character string.", call. = FALSE)
  }
  if (!is.null(color) && (!is.character(color) || length(color) != 1 ||
                          is.na(color))) {
    stop("`color` must be NULL or a single character string.", call. = FALSE)
  }

  scale <- match.arg(scale)

  if (is.null(observed)) {
    observed <- calculate_rating_table_observed_experience(
      x = x,
      data = data,
      risk_factors = risk_factors,
      claim_count = claim_count,
      exposure = exposure,
      claim_amount = claim_amount
    )
  }

  observed_data <- normalize_rating_table_observed_experience(observed)
  default_metric <- resolve_rating_table_observed_metric(
    metric,
    observed_data,
    allow_null = TRUE
  )

  observed_experience <- list(
    data = observed_data,
    metric = default_metric,
    label = label,
    color = color,
    scale = scale
  )
  attr(x, "observed_experience") <- observed_experience

  x
}

#' Deprecated alias for `add_portfolio_experience()`
#'
#' @description
#' `add_observed_experience()` is deprecated. Use
#' [add_portfolio_experience()] instead.
#'
#' @inheritParams add_portfolio_experience
#'
#' @return See [add_portfolio_experience()].
#'
#' @author Martin Haringa
#'
#' @keywords internal
#' @export
add_observed_experience <- function(...) {
  lifecycle::deprecate_warn(
    "0.8.1",
    "add_observed_experience()",
    "add_portfolio_experience()"
  )
  add_portfolio_experience(...)
}

calculate_rating_table_observed_experience <- function(x,
                                                       data,
                                                       risk_factors = NULL,
                                                       claim_count = NULL,
                                                       exposure = NULL,
                                                       claim_amount = NULL) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be supplied as a data.frame when `observed = NULL`.",
         call. = FALSE)
  }

  table_risk_factors <- unique(as.character(.rating_table_data(x)$risk_factor))
  if (is.null(risk_factors)) {
    risk_factors <- intersect(table_risk_factors, names(data))
  } else if (!is.character(risk_factors) || length(risk_factors) == 0L ||
             anyNA(risk_factors)) {
    stop("`risk_factors` must be NULL or a non-empty character vector.",
         call. = FALSE)
  }

  unknown <- setdiff(risk_factors, table_risk_factors)
  if (length(unknown) > 0) {
    stop("Unknown risk factor(s) in `risk_factors`: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  missing_cols <- setdiff(risk_factors, names(data))
  if (length(missing_cols) > 0) {
    stop("Risk factor column(s) not found in `data`: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  lapply(
    risk_factors,
    function(rf) {
      factor_analysis(
        data = data,
        risk_factors = rf,
        claim_count = claim_count,
        claim_amount = claim_amount,
        exposure = exposure
      )
    }
  )
}

normalize_rating_table_observed_experience <- function(observed) {
  if (inherits(observed, "factor_analysis")) {
    observed <- list(observed)
  }
  if (!is.list(observed) || length(observed) == 0L) {
    stop("`observed` must be a factor_analysis object or a non-empty list of factor_analysis objects.",
         call. = FALSE)
  }
  if (!all(vapply(observed, inherits, logical(1), what = "factor_analysis"))) {
    stop("Every element of `observed` must be a factor_analysis object.",
         call. = FALSE)
  }

  observed <- lapply(observed, normalize_one_rating_table_observed_experience)
  all_names <- unique(unlist(lapply(observed, names)))
  observed <- lapply(
    observed,
    function(x) {
      missing <- setdiff(all_names, names(x))
      for (nm in missing) {
        x[[nm]] <- NA
      }
      x[, all_names, drop = FALSE]
    }
  )
  out <- do.call(rbind, observed)
  row.names(out) <- NULL
  out
}

normalize_one_rating_table_observed_experience <- function(experience) {
  xvar <- attr(experience, "xvar")
  if (length(xvar) == 0L || is.na(xvar[1])) {
    stop("The factor_analysis object does not store a risk factor name.",
         call. = FALSE)
  }
  risk_factor <- xvar[1]
  experience_df <- as.data.frame(experience)

  if (!risk_factor %in% names(experience_df)) {
    stop("The factor_analysis object does not contain its risk factor column.",
         call. = FALSE)
  }

  names(experience_df)[names(experience_df) == risk_factor] <- "level"
  experience_df$risk_factor <- risk_factor
  experience_df$level <- as.character(experience_df$level)

  col_map <- c(
    exposure = attr(experience, "exposure") %||% NA_character_,
    claim_count = attr(experience, "claim_count") %||%
      attr(experience, "nclaims") %||% NA_character_,
    claim_amount = attr(experience, "claim_amount") %||%
      attr(experience, "severity") %||% NA_character_,
    premium = attr(experience, "premium") %||% NA_character_
  )

  for (nm in names(col_map)) {
    old_nm <- unname(col_map[[nm]])
    if (!is.na(old_nm) && old_nm %in% names(experience_df) && old_nm != nm) {
      names(experience_df)[names(experience_df) == old_nm] <- nm
    }
  }

  keep <- intersect(
    c(
      "risk_factor", "level", "exposure", "claim_count", "claim_amount",
      "premium", "frequency", "average_severity", "risk_premium",
      "loss_ratio", "average_premium"
    ),
    names(experience_df)
  )
  experience_df[, keep, drop = FALSE]
}

resolve_rating_table_observed_metric <- function(metric,
                                                 observed_data,
                                                 allow_null = FALSE) {
  if (is.null(metric)) {
    if (isTRUE(allow_null)) {
      available <- intersect(
        c("frequency", "average_severity", "risk_premium",
          "loss_ratio", "average_premium"),
        names(observed_data)
      )
      if (length(available) == 0L) {
        return(NULL)
      }
      return(available[1])
    }
    stop("`metric` must be supplied when observed experience is attached.",
         call. = FALSE)
  }
  if (!is.character(metric) || length(metric) != 1L || is.na(metric)) {
    stop("`metric` must be NULL or a single character string.", call. = FALSE)
  }

  metric <- switch(
    metric,
    severity = "average_severity",
    metric
  )

  if (!metric %in% names(observed_data)) {
    stop("`metric` is not available in the attached observed experience.",
         call. = FALSE)
  }

  metric
}


# -----------------------------------------------------------------------------
# rating_table print / summary / dataframe
# -----------------------------------------------------------------------------

#' @export
print.rating_table <- function(x, ...) {
  print(.rating_table_data(x), ...)
  invisible(x)
}

#' @export
print.riskfactor <- print.rating_table

#' @export
as.data.frame.rating_table <- function(x, ...) {
  .rating_table_data(x)
}

#' @export
as.data.frame.riskfactor <- as.data.frame.rating_table

#' @export
summary.rating_table <- function(object, ...) {
  out <- list(
    models = .rating_table_metadata(object, "models"),
    exposure = .rating_table_metadata(object, "exposure"),
    model_data = .rating_table_metadata(object, "model_data"),
    exponentiate = .rating_table_metadata(object, "expon"),
    significance = isTRUE(.rating_table_metadata(object, "significance")) ||
      isTRUE(.rating_table_metadata(object, "signif_stars")),
    signif_stars = isTRUE(.rating_table_metadata(object, "significance")) ||
      isTRUE(.rating_table_metadata(object, "signif_stars")),
    n_rows = nrow(.rating_table_data(object))
  )
  class(out) <- c("summary.rating_table", "summary.riskfactor")
  out
}

#' @export
summary.riskfactor <- summary.rating_table

#' @export
print.summary.rating_table <- function(x, ...) {
  cat("rating_table summary\n\n")
  cat("Models: ", paste(x$models, collapse = ", "), "\n", sep = "")
  cat("Exposure column: ", if (is.null(x$exposure)) "none" else x$exposure, "\n", sep = "")
  cat("Model data: ", if (is.null(x$model_data)) "none" else x$model_data, "\n", sep = "")
  cat("Exponentiate: ", x$exponentiate, "\n", sep = "")
  cat("Significance: ", x$significance, "\n", sep = "")
  cat("Rows: ", x$n_rows, "\n", sep = "")
  invisible(x)
}

#' @export
print.summary.riskfactor <- print.summary.rating_table
