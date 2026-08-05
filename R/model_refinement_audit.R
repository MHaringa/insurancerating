.audit_single_string <- function(x, name, allow_null = FALSE) {
  if (allow_null && is.null(x)) {
    return(invisible(TRUE))
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", name, "` must be a single non-empty character string.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.audit_offset_columns <- function(model, data) {
  offset <- tryCatch(get_offset(model), error = function(e) NULL)
  if (is.null(offset) || length(offset) != 1L) {
    return(character())
  }
  expression <- tryCatch(parse(text = offset)[[1]], error = function(e) NULL)
  if (is.null(expression)) {
    return(character())
  }
  intersect(all.vars(expression), names(data))
}

.audit_default_risk_factors <- function(model) {
  table <- tryCatch(
    rating_table(model, exposure = FALSE),
    error = function(e) NULL
  )
  if (is.null(table)) {
    return(character())
  }
  factors <- unique(as.character(as.data.frame(table)$risk_factor))
  setdiff(factors, "(Intercept)")
}

.audit_predictor_columns <- function(model, data) {
  terms <- stats::delete.response(stats::terms(model))
  intersect(unique(all.vars(terms)), names(data))
}

.audit_group_column <- function(model, risk_factor, grid) {
  restriction_map <- attr(model, "restriction_map", exact = TRUE)
  if (is.data.frame(restriction_map) &&
      all(c("source_var", "risk_factor") %in% names(restriction_map))) {
    matched <- restriction_map$source_var[
      restriction_map$risk_factor == risk_factor
    ]
    matched <- matched[matched %in% names(grid)]
    if (length(matched) > 0L) {
      return(utils::tail(matched, 1L))
    }
  }
  risk_factor
}

.audit_grid_key <- function(data, columns) {
  if (length(columns) == 0L) {
    return(rep("", nrow(data)))
  }
  values <- lapply(data[columns], function(x) {
    out <- as.character(x)
    out[is.na(out)] <- "<NA>"
    out
  })
  do.call(paste, c(values, sep = "\r"))
}

.audit_add_record_counts <- function(model, grid, exposure, group_by) {
  if ("count" %in% names(grid)) {
    return(grid)
  }
  count_grid <- rating_grid(model, group_by = group_by)
  if (!"count" %in% names(count_grid)) {
    grid$count <- NA_real_
    return(grid)
  }
  excluded <- unique(c("count", exposure))
  keys <- intersect(
    setdiff(names(grid), excluded),
    setdiff(names(count_grid), excluded)
  )
  index <- match(
    .audit_grid_key(grid, keys),
    .audit_grid_key(count_grid, keys)
  )
  grid$count <- count_grid$count[index]
  grid
}

.audit_weighted_mean <- function(x, weight) {
  valid <- is.finite(x) & is.finite(weight) & weight > 0
  if (!any(valid)) {
    return(NA_real_)
  }
  sum(x[valid] * weight[valid]) / sum(weight[valid])
}

.audit_change_ratio <- function(before, after) {
  ifelse(is.finite(before) & before != 0, (after - before) / before, NA_real_)
}

.audit_aggregate_level <- function(grid, risk_factor, group_column, exposure,
                                   scale) {
  level <- as.character(grid[[group_column]])
  levels <- unique(level[!is.na(level)])

  rows <- lapply(levels, function(value) {
    selected <- !is.na(level) & level == value
    point_weight <- if (!is.null(exposure)) {
      grid[[exposure]][selected]
    } else if ("count" %in% names(grid)) {
      grid$count[selected]
    } else {
      rep(1, sum(selected))
    }

    if (identical(scale, "per_exposure")) {
      denominator <- sum(grid[[exposure]][selected], na.rm = TRUE)
      before <- if (denominator > 0) {
        sum(grid$.prediction_before[selected], na.rm = TRUE) / denominator
      } else {
        NA_real_
      }
      after <- if (denominator > 0) {
        sum(grid$.prediction_after[selected], na.rm = TRUE) / denominator
      } else {
        NA_real_
      }
    } else {
      before <- .audit_weighted_mean(
        grid$.prediction_before[selected], point_weight
      )
      after <- .audit_weighted_mean(
        grid$.prediction_after[selected], point_weight
      )
    }

    data.frame(
      risk_factor = risk_factor,
      level = value,
      model_points = sum(selected),
      records = if ("count" %in% names(grid)) {
        sum(grid$count[selected], na.rm = TRUE)
      } else {
        NA_real_
      },
      exposure = if (!is.null(exposure)) {
        sum(grid[[exposure]][selected], na.rm = TRUE)
      } else {
        NA_real_
      },
      before = before,
      after = after,
      change = after - before,
      change_ratio = .audit_change_ratio(before, after),
      stringsAsFactors = FALSE
    )
  })

  if (length(rows) == 0L) {
    return(NULL)
  }
  do.call(rbind, rows)
}

#' Audit the effect of a fitted model refinement
#'
#' @description
#' Compare an unrestricted GLM with the model returned by [refit()] on the same
#' observed portfolio. The audit records the refinement specification and
#' quantifies how the fitted response or fitted rate changes for the portfolio
#' and for each final tariff-factor level.
#'
#' @details
#' Direct coefficient comparisons are generally not a sufficient refinement
#' audit. A coefficient can change because the intercept or another model term
#' changes, while the combined fitted value for a policy remains similar.
#' `audit_refinement()` therefore compares predictions from the original and
#' refined models on common observed model-point combinations.
#'
#' The model points are obtained with [rating_grid()]. Portfolio and level
#' results are weighted by the number of records or, when supplied, by
#' `exposure`. With `scale = "per_exposure"`, predictions that include an
#' exposure offset are divided by total exposure after aggregation. This gives
#' an exposure-weighted fitted rate rather than an unweighted average over
#' unique model points.
#'
#' The resulting measure should be named according to the model being audited.
#' For a frequency model it is normally a fitted frequency; for a severity
#' model it is a fitted average severity; and for a direct pure-premium model it
#' can be labelled `"risk_premium"`. A complete risk-premium comparison requires
#' either a direct risk-premium model or an explicit combination of frequency
#' and severity predictions.
#'
#' @param object A fitted model returned by [refit()]. Ordinary GLMs do not
#'   contain the stored baseline and refinement metadata required for the
#'   comparison.
#' @param exposure Optional character string naming the exposure column. With
#'   `scale = "auto"`, the function attempts to infer a single exposure column
#'   from the original model offset. Supply this argument explicitly when that
#'   interpretation is ambiguous.
#' @param risk_factors Optional character vector identifying final tariff
#'   factors for the level comparison. If `NULL`, the final factors reported by
#'   [rating_table()] and available in [rating_grid()] are used.
#' @param scale Character string. `"per_exposure"` compares fitted values per
#'   unit of exposure. `"response"` compares predictions on the response scale.
#'   `"auto"` selects `"per_exposure"` when one exposure variable can be
#'   identified from the model offset and otherwise selects `"response"`.
#' @param metric Optional character string used to describe the audited measure,
#'   for example `"risk_premium"`, `"frequency"` or `"average_severity"`.
#'   If `NULL`, the audit uses `"fitted_rate"` or `"fitted_response"`.
#'
#' @return An object of class `refinement_audit`. The object contains package
#'   and model metadata, the ordered refinement steps, portfolio-level results,
#'   results by risk factor and level, and the model points used in the
#'   calculation. Use [summary.refinement_audit()] for a concise audit report,
#'   `as.data.frame()` for the level results and `as_gt()` for a formatted table.
#'
#' @author Martin Haringa
#'
#' @seealso [prepare_refinement()], [refit()], [rating_grid()],
#'   [summary.rating_refinement()]
#'
#' @examples
#' portfolio <- data.frame(
#'   claims = c(1, 2, 1, 3, 2, 4),
#'   exposure = rep(1, 6),
#'   risk_class = factor(c("A", "B", "A", "B", "A", "B"))
#' )
#'
#' base_model <- glm(
#'   claims ~ risk_class + offset(log(exposure)),
#'   family = poisson(),
#'   data = portfolio
#' )
#'
#' refinement <- prepare_refinement(base_model, data = portfolio) |>
#'   add_restriction(data.frame(
#'     risk_class = "B",
#'     risk_class_restricted = 1.15
#'   ))
#'
#' summary(refinement)
#'
#' refined_model <- refit(refinement)
#' audit <- audit_refinement(
#'   refined_model,
#'   exposure = "exposure",
#'   metric = "frequency"
#' )
#'
#' summary(audit)
#' as.data.frame(audit)
#'
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   as_gt(audit)
#' }
#'
#' @export
audit_refinement <- function(object,
                             exposure = NULL,
                             risk_factors = NULL,
                             scale = c("auto", "response", "per_exposure"),
                             metric = NULL) {
  if (!inherits(object, c("refitrestricted", "refitsmooth"))) {
    stop("`object` must be a fitted model returned by `refit()`.",
         call. = FALSE)
  }
  scale <- match.arg(scale)
  .audit_single_string(exposure, "exposure", allow_null = TRUE)
  .audit_single_string(metric, "metric", allow_null = TRUE)
  if (!is.null(risk_factors) &&
      (!is.character(risk_factors) || anyNA(risk_factors) ||
       any(!nzchar(risk_factors)))) {
    stop("`risk_factors` must be NULL or a character vector of column names.",
         call. = FALSE)
  }

  base_model <- attr(object, "refinement_base_model", exact = TRUE)
  if (is.null(base_model) || !inherits(base_model, "glm")) {
    stop(
      "The original GLM is not stored on `object`. Refit the retained ",
      "`rating_refinement` object with the current package version before ",
      "creating an audit.",
      call. = FALSE
    )
  }
  refined_data <- object$data
  if (!is.data.frame(refined_data)) {
    stop("The refined model does not contain the portfolio data required for the audit.",
         call. = FALSE)
  }

  inferred_exposure <- .audit_offset_columns(base_model, refined_data)
  if (is.null(exposure) && length(inferred_exposure) == 1L) {
    exposure <- inferred_exposure
  }
  if (!is.null(exposure) && !exposure %in% names(refined_data)) {
    stop("Exposure column `", exposure, "` was not found in the refined model data.",
         call. = FALSE)
  }
  if (!is.null(exposure)) {
    values <- refined_data[[exposure]]
    if (!is.numeric(values) || anyNA(values) || any(!is.finite(values)) ||
        any(values < 0)) {
      stop("Exposure column `", exposure,
           "` must contain finite, non-missing, non-negative numeric values.",
           call. = FALSE)
    }
  }

  resolved_scale <- if (identical(scale, "auto")) {
    if (!is.null(exposure) && length(inferred_exposure) == 1L) {
      "per_exposure"
    } else {
      "response"
    }
  } else {
    scale
  }
  if (identical(resolved_scale, "per_exposure") && is.null(exposure)) {
    stop("`exposure` is required when `scale = \"per_exposure\"`.",
         call. = FALSE)
  }
  if (identical(resolved_scale, "per_exposure") &&
      sum(refined_data[[exposure]]) <= 0) {
    stop(
      "Exposure column `", exposure,
      "` must have a positive total for a per-exposure audit.",
      call. = FALSE
    )
  }

  if (is.null(risk_factors)) {
    risk_factors <- .audit_default_risk_factors(object)
  }
  group_columns <- vapply(
    risk_factors,
    function(risk_factor) {
      .audit_group_column(object, risk_factor, refined_data)
    },
    character(1)
  )
  missing_factors <- risk_factors[!group_columns %in% names(refined_data)]
  if (length(missing_factors) > 0L) {
    stop(
      "The following `risk_factors` are not available in the rating grid: ",
      paste(missing_factors, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(risk_factors) == 0L) {
    stop(
      "No final tariff factors could be identified for the level audit. ",
      "Supply `risk_factors` explicitly.",
      call. = FALSE
    )
  }

  prediction_columns <- unique(c(
    .audit_predictor_columns(base_model, refined_data),
    .audit_predictor_columns(object, refined_data),
    group_columns
  ))
  prediction_columns <- setdiff(prediction_columns, exposure)
  if (length(prediction_columns) == 0L) {
    stop("No predictor columns could be identified for the common rating grid.",
         call. = FALSE)
  }

  grid <- if (is.null(exposure)) {
    rating_grid(object, group_by = prediction_columns)
  } else {
    rating_grid(
      object,
      group_by = prediction_columns,
      exposure = exposure
    )
  }
  grid <- .audit_add_record_counts(
    object, grid, exposure, group_by = prediction_columns
  )
  grid$.prediction_before <- as.numeric(stats::predict(
    base_model, newdata = grid, type = "response"
  ))
  grid$.prediction_after <- as.numeric(stats::predict(
    object, newdata = grid, type = "response"
  ))

  impact_rows <- lapply(seq_along(risk_factors), function(i) {
    .audit_aggregate_level(
      grid,
      risk_factor = risk_factors[i],
      group_column = group_columns[i],
      exposure = exposure,
      scale = resolved_scale
    )
  })
  impact_rows <- impact_rows[!vapply(impact_rows, is.null, logical(1))]
  impact <- do.call(rbind, impact_rows)
  rownames(impact) <- NULL

  total_weight <- if (!is.null(exposure)) {
    grid[[exposure]]
  } else if ("count" %in% names(grid)) {
    grid$count
  } else {
    rep(1, nrow(grid))
  }
  if (identical(resolved_scale, "per_exposure")) {
    denominator <- sum(grid[[exposure]], na.rm = TRUE)
    before <- sum(grid$.prediction_before, na.rm = TRUE) / denominator
    after <- sum(grid$.prediction_after, na.rm = TRUE) / denominator
  } else {
    before <- .audit_weighted_mean(grid$.prediction_before, total_weight)
    after <- .audit_weighted_mean(grid$.prediction_after, total_weight)
  }
  portfolio <- data.frame(
    metric = metric %||% if (identical(resolved_scale, "per_exposure")) {
      "fitted_rate"
    } else {
      "fitted_response"
    },
    before = before,
    after = after,
    change = after - before,
    change_ratio = .audit_change_ratio(before, after),
    exposure = if (!is.null(exposure)) sum(grid[[exposure]], na.rm = TRUE) else NA_real_,
    records = if ("count" %in% names(grid)) sum(grid$count, na.rm = TRUE) else NA_real_,
    stringsAsFactors = FALSE
  )

  out <- list(
    metadata = list(
      package = attr(object, "refinement_package", exact = TRUE) %||%
        "insurancerating",
      package_version = attr(
        object, "refinement_package_version", exact = TRUE
      ) %||% NA_character_,
      created_at = attr(object, "refinement_created_at", exact = TRUE),
      refitted_at = attr(object, "refinement_refitted_at", exact = TRUE),
      audited_at = Sys.time()
    ),
    base_formula = stats::formula(base_model),
    refined_formula = stats::formula(object),
    family = object$family$family,
    link = object$family$link,
    intercept_only = isTRUE(attr(object, "intercept_only", exact = TRUE)),
    exposure = exposure,
    scale = resolved_scale,
    metric = portfolio$metric[1],
    steps = .refinement_steps_table(
      attr(object, "refinement_steps", exact = TRUE) %||% list()
    ),
    portfolio = portfolio,
    impact = impact,
    model_points = grid
  )
  class(out) <- "refinement_audit"
  out
}

#' Summarise a refinement audit
#'
#' @description
#' Return and print the provenance, ordered refinement steps, total portfolio
#' effect and the largest absolute level changes from [audit_refinement()].
#'
#' @param object A `refinement_audit` object.
#' @param top_n Non-negative whole number controlling how many level changes are
#'   included in the printed summary.
#' @param ... Currently unused.
#'
#' @return An object of class `summary.refinement_audit` containing the audit
#' metadata, formulas, steps, portfolio result and selected level impacts.
#'
#' @seealso [audit_refinement()]
#' @keywords internal
#' @export
summary.refinement_audit <- function(object, top_n = 10, ...) {
  .check_dots_empty(...)
  if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) ||
      !is.finite(top_n) || top_n < 0 || top_n != as.integer(top_n)) {
    stop("`top_n` must be a non-negative whole number.", call. = FALSE)
  }
  ordering <- order(abs(object$impact$change_ratio), decreasing = TRUE,
                    na.last = TRUE)
  selected <- object$impact[utils::head(ordering, as.integer(top_n)), , drop = FALSE]
  out <- list(
    metadata = object$metadata,
    base_formula = object$base_formula,
    refined_formula = object$refined_formula,
    family = object$family,
    link = object$link,
    intercept_only = object$intercept_only,
    exposure = object$exposure,
    scale = object$scale,
    metric = object$metric,
    steps = object$steps,
    portfolio = object$portfolio,
    impact = selected,
    total_levels = nrow(object$impact)
  )
  class(out) <- "summary.refinement_audit"
  out
}

#' @export
#' @noRd
print.refinement_audit <- function(x, ...) {
  print(summary(x), ...)
  invisible(x)
}

#' @export
#' @noRd
print.summary.refinement_audit <- function(x, ...) {
  portfolio <- x$portfolio[1, ]
  cat("Refinement audit\n\n")
  cat("Package: ", x$metadata$package, " ", x$metadata$package_version, "\n", sep = "")
  cat("Prepared: ", .format_refinement_time(x$metadata$created_at), "\n", sep = "")
  cat("Refitted: ", .format_refinement_time(x$metadata$refitted_at), "\n", sep = "")
  cat("Audited: ", .format_refinement_time(x$metadata$audited_at), "\n", sep = "")
  cat("Measure: ", x$metric, " (", x$scale, ")\n", sep = "")
  if (!is.null(x$exposure)) {
    cat("Exposure: ", x$exposure, "\n", sep = "")
  }
  cat("\nOriginal formula:\n  ")
  cat(paste(deparse(x$base_formula), collapse = "\n  "))
  cat("\nRefitted formula:\n  ")
  cat(paste(deparse(x$refined_formula), collapse = "\n  "))
  cat("\n\nRefinement steps: ", nrow(x$steps), "\n", sep = "")
  if (nrow(x$steps) == 0L) {
    cat("  None\n")
  } else {
    for (i in seq_len(nrow(x$steps))) {
      cat("  ", x$steps$step[i], ". ", x$steps$description[i], "\n", sep = "")
      if (!is.na(x$steps$details[i])) {
        cat("     ", x$steps$details[i], "\n", sep = "")
      }
    }
  }
  cat("\nPortfolio effect\n")
  cat("  Before: ", format(portfolio$before, digits = 6), "\n", sep = "")
  cat("  After:  ", format(portfolio$after, digits = 6), "\n", sep = "")
  cat("  Change: ", format(portfolio$change, digits = 6), " (",
      if (is.na(portfolio$change_ratio)) "NA" else {
        paste0(format(100 * portfolio$change_ratio, digits = 4), "%")
      }, ")\n", sep = "")

  if (nrow(x$impact) > 0L) {
    cat("\nLargest level changes (", nrow(x$impact), " of ",
        x$total_levels, ")\n", sep = "")
    displayed <- x$impact[, c(
      "risk_factor", "level", "before", "after", "change", "change_ratio"
    ), drop = FALSE]
    print(displayed, row.names = FALSE)
  }
  invisible(x)
}

#' @export
#' @noRd
as.data.frame.refinement_audit <- function(x, row.names = NULL, optional = FALSE,
                                           ...) {
  out <- as.data.frame(x$impact, stringsAsFactors = FALSE)
  rownames(out) <- row.names
  out
}

#' Present a refinement audit as a gt table
#'
#' @description
#' Format the risk-factor and level impact calculated by
#' [audit_refinement()] for a technical note or actuarial review.
#'
#' @param x A `refinement_audit` object.
#' @param locale Character string used for number formatting.
#' @param value_decimals Non-negative whole number for fitted values and
#'   absolute changes.
#' @param ratio_decimals Non-negative whole number for percentage changes.
#' @param title Optional table title.
#' @param subtitle Optional table subtitle. If `NULL`, package version and audit
#'   date are used.
#' @param ... Currently unused.
#'
#' @return A `gt_tbl` object.
#'
#' @seealso [audit_refinement()]
#' @keywords internal
#' @export
as_gt.refinement_audit <- function(x,
                                   locale = "nl-NL",
                                   value_decimals = 2,
                                   ratio_decimals = 1,
                                   title = "Refinement impact",
                                   subtitle = NULL,
                                   ...) {
  rlang::check_installed("gt")
  .check_dots_empty(...)
  .audit_single_string(locale, "locale")
  for (argument in c("value_decimals", "ratio_decimals")) {
    value <- get(argument)
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value < 0 || value != as.integer(value)) {
      stop("`", argument, "` must be a non-negative whole number.",
           call. = FALSE)
    }
  }
  .audit_single_string(title, "title", allow_null = TRUE)
  .audit_single_string(subtitle, "subtitle", allow_null = TRUE)

  table_data <- as.data.frame(x)
  out <- gt::gt(
    table_data,
    groupname_col = "risk_factor",
    row_group_as_column = TRUE,
    locale = locale
  )
  out <- gt::cols_label(
    out,
    risk_factor = "Risk factor",
    level = "Level",
    model_points = "Model points",
    records = "Records",
    exposure = "Exposure",
    before = "Before",
    after = "After",
    change = "Change",
    change_ratio = "Change (%)"
  )
  out <- gt::fmt_integer(out, columns = c("model_points", "records"),
                         locale = locale)
  out <- gt::fmt_number(
    out,
    columns = c("exposure", "before", "after", "change"),
    decimals = value_decimals,
    locale = locale
  )
  out <- gt::fmt_percent(out, columns = "change_ratio",
                         decimals = ratio_decimals, locale = locale)
  if (is.null(subtitle)) {
    subtitle <- paste0(
      x$metadata$package, " ", x$metadata$package_version,
      " | audited ", format(x$metadata$audited_at, "%Y-%m-%d")
    )
  }
  if (!is.null(title) || !is.null(subtitle)) {
    out <- gt::tab_header(out, title = title, subtitle = subtitle)
  }
  out
}
