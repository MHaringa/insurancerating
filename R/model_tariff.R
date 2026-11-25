#' Create a tariff from a rating table
#'
#' @description
#' Select one model's base rate and relativities from [rating_table()] for
#' prediction on a portfolio. The result can be used with [add_prediction()]
#' or [stats::predict()] without retaining the fitted model.
#'
#' @param x A complete `rating_table` object with exponentiated estimates.
#' @param estimate Name of the estimate column to use. Required when the
#'   table compares several models; otherwise defaults to its only estimate.
#' @param segment_sources Named character vector. Names are tariff factors
#'   with numeric interval levels, and values are the corresponding numeric
#'   columns in the portfolio. For example, use
#'   `c(amount_band = "insured_amount")`. If the portfolio already contains
#'   the band column, it takes precedence and no mapping is needed.
#' @param unmatched How to handle portfolio values without a matching
#'   relativity: `"warn"` (default) warns and returns `NA` for affected rows;
#'   `"error"` stops. The diagnostic identifies the factors and values.
#' @param object A `rating_tariff` returned by `as_tariff()`.
#' @param newdata Data frame with the tariff factor columns, or their mapped
#'   numeric sources, and any required numeric offset columns.
#' @param type Prediction scale. Only `"response"` is supported.
#' @param ... Not used.
#'
#' @details
#' Predictions are the base rate multiplied by one relativity per factor.
#' Only log-link GLMs with an intercept and additive, treatment-coded
#' categorical effects are supported. Refinements represented by categorical
#' tariff factors are supported when their offset mappings can be resolved.
#' Simple offsets such as `log(exposure)` retain their original role as
#' multipliers. For example, a Poisson model with that offset predicts expected
#' claim counts for the supplied exposure, not a rate per exposure unit.
#' Predictions use the original response scale and do not add commercial
#' loadings or calculate confidence intervals. Use the fitted model directly
#' for continuous effects, interactions or more complicated offsets.
#'
#' Numeric sources are matched against the exact interval labels in the table.
#' For example, `(0,100]` includes 100 but not 0. Infinite bounds such as
#' `(250000,Inf]` are supported. Do not use rounded labels if they differ
#' from the tariff boundaries. Unknown categories, missing values in a factor
#' column and values outside the intervals follow `unmatched`. Missing columns,
#' non-numeric segment sources and overlapping intervals are errors.
#' The warning or error lists up to ten values per factor; its `unmatched`
#' field holds all unmatched values for use in a condition handler.
#'
#' @return `as_tariff()` returns a `rating_tariff` object.
#'   Its `predict()` method returns a numeric vector, in portfolio row order.
#'
#' @seealso [rating_table()], [add_prediction()]
#'
#' @examples
#' training <- data.frame(
#'   amount_band = factor(rep(c("(0,100]", "(100,200]"), each = 3)),
#'   risk_premium = c(90, 100, 110, 120, 135, 150)
#' )
#' model <- glm(risk_premium ~ amount_band, Gamma(link = "log"), data = training)
#' rt <- rating_table(model, exposure = FALSE)
#'
#' # The portfolio already contains the band.
#' add_prediction(training["amount_band"], as_tariff(rt),
#'                predictions = "risk_premium")
#'
#' # Derive the band from the numeric amount using the table's interval labels.
#' tariff <- as_tariff(rt, segment_sources = c(amount_band = "insured_amount"))
#' portfolio <- data.frame(insured_amount = c(100, 150))
#' add_prediction(portfolio, tariff, predictions = "risk_premium")
#'
#' @export
as_tariff <- function(x, estimate = NULL, segment_sources = NULL,
                      unmatched = c("warn", "error")) {
  unmatched <- match.arg(unmatched)
  if (!inherits(x, "rating_table")) {
    stop("`x` must be a complete object returned by rating_table().", call. = FALSE)
  }
  data <- .rating_table_data(x)
  estimates <- .rating_table_estimate_columns(x, data)
  if (is.null(estimate) && length(estimates) == 1L) estimate <- estimates[[1L]]
  if (!.is_single_string(estimate) || !estimate %in% estimates) {
    stop(
      "Choose one estimate column with `estimate`: ",
      paste(estimates, collapse = ", "), ".", call. = FALSE
    )
  }
  if (!isTRUE(.rating_table_metadata(x, "expon"))) {
    stop("Use rating_table(..., exponentiate = TRUE) to create a tariff.",
         call. = FALSE)
  }
  spec <- .rating_table_metadata(x, "tariff_info")[[estimate]]
  if (is.null(spec)) {
    stop("Tariff metadata is missing. Recreate the table with rating_table().",
         call. = FALSE)
  }
  if (!is.null(spec$error)) {
    stop("Cannot create a tariff from `", estimate, "`: ", spec$error,
         " Use add_prediction() with the fitted model instead.", call. = FALSE)
  }

  if (!all(c("risk_factor", "level") %in% names(data)) ||
      !is.numeric(data[[estimate]])) {
    stop("The rating table must include the risk factors, levels and numeric estimates.",
         call. = FALSE)
  }
  values <- data[[estimate]]
  # A comparison table has NA in rows belonging only to the other models.
  rows <- !is.na(values)
  factors <- data$risk_factor[rows]
  levels <- data$level[rows]
  if (anyNA(factors) || anyNA(levels) ||
      any(!is.finite(values[rows]) | values[rows] <= 0)) {
    stop("The base rate and all relativities must be finite and positive.",
         call. = FALSE)
  }
  tariff_levels <- split(as.character(levels), as.character(factors))
  if (!setequal(names(tariff_levels), names(spec$levels)) ||
      !all(vapply(names(spec$levels), function(factor) {
        levels <- tariff_levels[[factor]]
        !anyDuplicated(levels) && setequal(levels, spec$levels[[factor]])
      }, logical(1)))) {
    stop("The selected estimate must contain a complete tariff with one value ",
         "per factor level and one intercept.", call. = FALSE)
  }
  tariffs <- split(stats::setNames(values[rows], levels), as.character(factors))
  base <- unname(tariffs[["(Intercept)"]])
  tariffs[["(Intercept)"]] <- NULL

  if (!is.null(segment_sources)) {
    if (!is.character(segment_sources) || !is.null(dim(segment_sources)) ||
        is.null(names(segment_sources)) || anyNA(names(segment_sources)) ||
        anyNA(segment_sources) || any(!nzchar(names(segment_sources))) ||
        any(!nzchar(segment_sources)) || anyDuplicated(names(segment_sources))) {
      stop("`segment_sources` must be a named character vector mapping tariff ",
           "factors to numeric portfolio columns.", call. = FALSE)
    }
    unknown <- setdiff(names(segment_sources), names(tariffs))
    if (length(unknown)) {
      stop("Unknown tariff factor(s) in `segment_sources`: ",
           paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
  }
  intervals <- lapply(names(segment_sources), function(factor) {
    .tariff_intervals(names(tariffs[[factor]]), factor)
  })
  names(intervals) <- names(segment_sources)

  structure(list(base = base, relativities = tariffs, fields = spec$fields,
                 offsets = spec$offsets, response = spec$response,
                 segment_sources = segment_sources, intervals = intervals,
                 unmatched = unmatched),
            class = "rating_tariff")
}

.tariff_model_info <- function(model, table) {
  unsupported <- function(reason) list(error = reason)
  if (!inherits(model, "glm") || !identical(model$family$link, "log")) {
    return(unsupported("only log-link GLMs are supported."))
  }
  coefficients <- stats::coef(model)
  if (!"(Intercept)" %in% names(coefficients) ||
      any(!is.finite(coefficients)) || anyDuplicated(names(coefficients))) {
    return(unsupported("an intercept and finite, identifiable coefficients are required."))
  }
  terms <- stats::terms(model)
  factors <- attr(terms, "term.labels")
  if (!all(factors %in% names(model$xlevels)) ||
      !all(vapply(factors, function(term) is.symbol(str2lang(term)), logical(1))) ||
      !all(vapply(factors, function(factor) {
        contrast <- model$contrasts[[factor]]
        is.character(contrast) && length(contrast) == 1L &&
          contrast %in% c("contr.treatment", "contr.SAS")
      }, logical(1)))) {
    return(unsupported("only additive, treatment-coded categorical effects are supported."))
  }
  fields <- stats::setNames(factors, factors)
  offsets <- .tariff_offset_columns(model, terms)
  if (anyNA(offsets) || anyDuplicated(offsets)) {
    return(unsupported("offsets must be sums of distinct log(column) terms."))
  }

  # Refit stores the categorical labels and numeric offsets in separate columns.
  pairs <- c(attr(model, "mgd_smt"), attr(model, "mgd_rst"))
  derived <- attr(model, "new_col_nm")
  table_factors <- unique(table$risk_factor)
  external <- character()
  for (column in offsets) {
    matches <- unique(Filter(function(pair) {
      length(pair) == 2L && identical(pair[[2L]], column)
    }, pairs))
    if (!length(matches) && !column %in% derived) {
      external <- c(external, column)
      next
    }
    if (length(matches) != 1L) {
      return(unsupported(paste0("the refinement offset `", column,
                                "` has no unambiguous tariff factor.")))
    }
    field <- matches[[1L]][[1L]]
    factor <- intersect(c(column, field), table_factors)
    if (length(factor) != 1L || factor %in% names(fields)) {
      return(unsupported(paste0("the refinement offset `", column,
                                "` has no unambiguous tariff factor.")))
    }
    fields[[factor]] <- field
  }

  if (!setequal(table_factors, c("(Intercept)", names(fields)))) {
    return(unsupported("the table does not represent every model effect as a tariff factor."))
  }
  response <- all.vars(stats::formula(model)[[2L]])
  list(levels = split(table$level, table$risk_factor), fields = fields,
       offsets = external, response = if (length(response)) response[1L] else "tariff")
}

.tariff_offset_columns <- function(model, terms) {
  offsets <- lapply(attr(terms, "offset"), function(i) {
    attr(terms, "variables")[[i + 1L]][[2L]]
  })
  offset_arg <- stats::getCall(model)[["offset"]]
  if (!is.null(offset_arg)) offsets <- c(offsets, list(offset_arg))
  offsets <- unlist(lapply(offsets, .calibration_offset_terms), recursive = FALSE)
  vapply(offsets, function(term) {
    if (is.call(term) && length(term) == 2L && is.symbol(term[[2L]]) &&
        (identical(term[[1L]], quote(log)) || identical(term[[1L]], quote(base::log)))) {
      return(as.character(term[[2L]]))
    }
    NA_character_
  }, character(1))
}

.tariff_intervals <- function(levels, factor) {
  intervals <- lapply(levels, .parse_restriction_interval)
  invalid <- vapply(intervals, function(interval) {
    is.null(interval) || interval$lower >= interval$upper
  }, logical(1))
  if (any(invalid)) {
    stop("Tariff factor `", factor, "` needs exact numeric interval labels ",
         "such as (0,100]. Invalid label(s): ",
         paste(utils::head(levels[invalid], 5L), collapse = ", "), ".",
         call. = FALSE)
  }
  ordered <- order(vapply(intervals, `[[`, numeric(1), "lower"))
  intervals <- intervals[ordered]
  for (i in seq_len(length(intervals) - 1L)) {
    if (.restriction_intervals_overlap(intervals[[i]], intervals[[i + 1L]])) {
      stop("Tariff factor `", factor, "` has overlapping intervals: ",
           levels[ordered[i]], " and ", levels[ordered[i + 1L]], ".",
           call. = FALSE)
    }
  }
  data.frame(
    level = levels[ordered],
    lower = vapply(intervals, `[[`, numeric(1), "lower"),
    upper = vapply(intervals, `[[`, numeric(1), "upper"),
    lower_closed = vapply(intervals, `[[`, logical(1), "lower_closed"),
    upper_closed = vapply(intervals, `[[`, logical(1), "upper_closed")
  )
}

.tariff_segment_levels <- function(values, intervals, source) {
  if (!is.numeric(values) || !is.null(dim(values))) {
    stop("Segment source `", source, "` must be a numeric column.",
         call. = FALSE)
  }
  matched <- rep(NA_character_, length(values))
  finite <- is.finite(values)
  values <- values[finite]
  index <- findInterval(values, intervals$lower)
  hit <- pmax(index, 1L)
  # At an open lower bound, the preceding interval may include the value.
  index <- index - (index > 0L & values == intervals$lower[hit] &
                      !intervals$lower_closed[hit])
  hit <- pmax(index, 1L)
  outside <- index == 0L |
    (values > intervals$upper[hit]) |
    (values == intervals$upper[hit] & !intervals$upper_closed[hit])
  matched[finite] <- ifelse(outside, NA_character_, intervals$level[hit])
  matched
}

.tariff_missing_field <- function(data, field, factor, levels) {
  hint <- ""
  if (all(!vapply(lapply(levels, .parse_restriction_interval), is.null, logical(1)))) {
    candidates <- sub("_(cat|band|smooth|restricted)$", "", c(field, factor))
    available <- intersect(candidates, names(data))
    numeric <- available[vapply(data[available], is.numeric, logical(1))]
    source <- if (length(numeric) == 1L) numeric[[1L]] else "numeric_column"
    hint <- paste0(" Or set `segment_sources = c(",
                   deparse(as.name(factor), backtick = TRUE), " = ",
                   encodeString(source, quote = "\""), ")` in as_tariff().")
  }
  stop("Tariff factor `", factor, "` requires column `", field,
       "`. Supply its category labels in the portfolio.", hint, call. = FALSE)
}

.tariff_report_unmatched <- function(issues, policy) {
  details <- vapply(names(issues), function(factor) {
    entry <- issues[[factor]]
    values <- entry$values
    shown <- utils::head(values, 10L)
    labels <- ifelse(is.na(shown), "<NA>", encodeString(as.character(shown), quote = "\""))
    extra <- if (length(values) > length(shown)) {
      paste0(" (and ", length(values) - length(shown), " more values)")
    } else ""
    paste0("`", factor, "` from `", entry$column, "`: ",
           paste(labels, collapse = ", "), extra)
  }, character(1))
  message <- paste0("Unmatched tariff values: ", paste(details, collapse = "; "), ".")
  if (identical(policy, "error")) {
    stop(errorCondition(message, class = "tariff_unmatched_error", unmatched = issues))
  }
  warning(warningCondition(
    paste0(message, " Predictions for affected rows are NA."),
    class = "tariff_unmatched_warning", unmatched = issues
  ))
}

#' @rdname as_tariff
#' @export
predict.rating_tariff <- function(object, newdata, type = "response", ...) {
  match.arg(type, "response")
  .check_dots_empty(...)
  if (!is.data.frame(newdata)) stop("`newdata` must be a data frame.", call. = FALSE)
  if (anyDuplicated(names(newdata))) {
    stop("Portfolio column names must be unique.", call. = FALSE)
  }

  prediction <- rep(log(object$base), nrow(newdata))
  issues <- list()
  for (factor in names(object$relativities)) {
    weights <- object$relativities[[factor]]
    field <- object$fields[[factor]]
    if (field %in% names(newdata)) {
      column <- newdata[[field]]
      if (!is.atomic(column) || !is.null(dim(column))) {
        stop("Tariff column `", field, "` must contain category labels.",
             call. = FALSE)
      }
      labels <- as.character(column)
    } else if (factor %in% names(object$segment_sources)) {
      source <- object$segment_sources[[factor]]
      if (!source %in% names(newdata)) {
        stop("Tariff factor `", factor, "` requires category column `", field,
             "` or numeric segment source `", source, "`.", call. = FALSE)
      }
      field <- source
      column <- newdata[[source]]
      labels <- .tariff_segment_levels(column, object$intervals[[factor]], source)
    } else {
      .tariff_missing_field(newdata, field, factor, names(weights))
    }
    matched <- match(labels, names(weights))
    if (anyNA(matched)) {
      issues[[factor]] <- list(column = field, values = unique(column[is.na(matched)]))
    }
    prediction <- prediction + log(unname(weights[matched]))
  }
  for (offset in object$offsets) {
    column <- newdata[[offset]]
    if (!offset %in% names(newdata) || !is.numeric(column) ||
        !is.null(dim(column)) || any(!is.finite(column) | column <= 0)) {
      stop("Tariff offset column `", offset,
           "` is required and must contain finite positive numbers.", call. = FALSE)
    }
    prediction <- prediction + log(column)
  }
  prediction <- exp(prediction)
  known <- !is.na(prediction)
  if (any(!is.finite(prediction[known]) | prediction[known] <= 0)) {
    stop("The tariff produces predictions outside the positive finite numeric range.",
         call. = FALSE)
  }
  if (length(issues)) .tariff_report_unmatched(issues, object$unmatched)
  prediction
}
