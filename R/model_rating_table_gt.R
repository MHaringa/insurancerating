#' Present a rating table as a gt table
#'
#' @description
#' Create a formatted `gt` table from an object returned by [rating_table()].
#' Risk factors are presented as row groups, while fitted model effects are
#' shown as relativities or coefficients depending on the scale selected in
#' [rating_table()].
#'
#' @details
#' The first column of a `rating_table` identifies the model risk factor.
#' `as_gt()` uses this column as `groupname_col` and sets
#' `row_group_as_column = TRUE`. Levels belonging to the same risk factor are
#' therefore kept together in a compact format suitable for a tariff note,
#' model review or technical appendix.
#'
#' Risk-factor and level order are taken directly from [rating_table()]. Use
#' its `risk_factor_order`, `level_order`, `numeric_level_order`,
#' `reference_first` and `order_model` arguments to determine the order before
#' formatting the table.
#'
#' With `significance = TRUE`, significance stars are appended to the fitted
#' effects and the significance levels are shown below the table. This requires
#' an object originally created with `rating_table(significance = TRUE)`,
#' because p-value information is deliberately not retained when significance
#' is disabled during table construction. Significance stars are a statistical
#' diagnostic and should be interpreted together with exposure, effect size,
#' model stability and actuarial relevance.
#'
#' In the underlying `rating_table`, estimates and significance indicators are
#' stored in separate `est_*` and `signif_*` columns. `as_gt()` merges each pair
#' only for display. The estimates therefore remain numeric in the source
#' object, including when several models are presented in one table.
#'
#' @param significance Optional logical. If `NULL`, use the significance setting
#'   stored on `x`. If `TRUE`, append the stored significance stars to the model
#'   effects and add the significance-level note. If `FALSE`, show fitted
#'   effects without stars.
#' @param show_effect_spanner Optional logical. If `NULL`, show the
#'   `"Relativities"` or `"Coefficients"` spanner when multiple models are
#'   present and omit it for a single model. Use `TRUE` or `FALSE` to override
#'   this behaviour.
#' @param model_labels Optional character vector with display labels for the
#'   fitted models. By default, each model object name is used unchanged. An
#'   unnamed vector is matched to the model columns in their existing order. A
#'   named vector can map model object names to labels, for example
#'   `c(freq = "Frequency", sev = "Severity")`.
#' @param locale Character. Locale used to format model effects and exposure,
#'   for example `"nl-NL"` or `"en-US"`.
#' @param estimate_decimals Non-negative whole number. Number of decimals shown
#'   for fitted coefficients or relativities.
#' @param exposure_decimals Non-negative whole number. Number of decimals shown
#'   for the exposure column, when available.
#' @param missing_text Single character string used to display missing values.
#'   The default is an en dash (`"\\u2013"`) so structural missing values, such
#'   as exposure for the intercept, are visually distinct from observed zero
#'   values.
#' @param title Optional character. Table title. If `NULL`, no title is added.
#' @param subtitle Optional character. Table subtitle. If `NULL`, no subtitle is
#'   added.
#' @author Martin Haringa
#' @rdname as_gt
#'
#' @examples
#' portfolio <- MTPL
#' portfolio$zip <- as.factor(portfolio$zip)
#'
#' frequency_model <- glm(
#'   nclaims ~ bm + zip + offset(log(exposure)),
#'   family = poisson(),
#'   data = portfolio
#' )
#'
#' fitted_tariff <- rating_table(
#'   frequency_model,
#'   model_data = portfolio,
#'   exposure = "exposure",
#'   significance = TRUE
#' )
#'
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   as_gt(fitted_tariff)
#'   as_gt(fitted_tariff, model_labels = "Frequency model")
#'   as_gt(fitted_tariff, significance = FALSE, locale = "en-US")
#' }
#'
#' @export
as_gt.rating_table <- function(x,
                               significance = NULL,
                               show_effect_spanner = NULL,
                               model_labels = NULL,
                               locale = "nl-NL",
                               estimate_decimals = 3,
                               exposure_decimals = 0,
                               missing_text = "\u2013",
                               title = NULL,
                               subtitle = NULL,
                               ...) {
  rlang::check_installed("gt")
  .check_dots_empty(...)
  .validate_as_gt_rating_table(
    x = x,
    significance = significance,
    show_effect_spanner = show_effect_spanner,
    model_labels = model_labels,
    locale = locale,
    estimate_decimals = estimate_decimals,
    exposure_decimals = exposure_decimals,
    missing_text = missing_text,
    title = title,
    subtitle = subtitle
  )

  show_significance <- if (is.null(significance)) {
    isTRUE(.rating_table_metadata(x, "significance")) ||
      isTRUE(.rating_table_metadata(x, "signif_stars"))
  } else {
    significance
  }

  table_data <- .rating_table_data(x)
  estimate_cols <- .rating_table_estimate_columns(x, table_data)
  model_names <- .rating_table_metadata(x, "models")
  display_model_labels <- .resolve_rating_table_model_labels(
    model_names,
    model_labels
  )
  significance_cols <- .rating_table_significance_columns(x)
  exposure_col <- .rating_table_metadata(x, "exposure")

  display_cols <- c("risk_factor", "level", estimate_cols, exposure_col)
  if (show_significance) {
    display_cols <- c(display_cols, significance_cols)
    for (column in significance_cols) {
      table_data[[column]][is.na(table_data[[column]])] <- ""
    }
  }
  table_data <- table_data[, unique(display_cols), drop = FALSE]

  out <- gt::gt(
    data = table_data,
    groupname_col = "risk_factor",
    row_group_as_column = TRUE,
    locale = locale
  )

  labels <- c(
    risk_factor = "Risk factor",
    level = "Level"
  )
  for (i in seq_along(estimate_cols)) {
    labels[[estimate_cols[i]]] <- display_model_labels[i]
  }
  if (!is.null(exposure_col)) {
    labels[[exposure_col]] <- .rating_table_gt_label(exposure_col)
  }
  label_names <- intersect(names(labels), names(table_data))
  label_args <- stats::setNames(
    as.list(labels[label_names]),
    label_names
  )
  out <- do.call(gt::cols_label, c(list(.data = out), label_args))
  out <- gt::tab_stubhead(out, label = "Risk factor")

  use_effect_spanner <- if (is.null(show_effect_spanner)) {
    length(estimate_cols) > 1L
  } else {
    show_effect_spanner
  }
  if (use_effect_spanner) {
    out <- gt::tab_spanner(
      out,
      label = if (isTRUE(.rating_table_metadata(x, "expon"))) {
        "Relativities"
      } else {
        "Coefficients"
      },
      columns = estimate_cols
    )
  }
  out <- gt::fmt_number(
    out,
    columns = estimate_cols,
    decimals = estimate_decimals,
    locale = locale
  )
  if (!is.null(exposure_col)) {
    out <- gt::fmt_number(
      out,
      columns = exposure_col,
      decimals = exposure_decimals,
      locale = locale
    )
  }
  out <- gt::sub_missing(
    out,
    columns = gt::everything(),
    missing_text = .rating_table_gt_missing_text(missing_text)
  )

  if (show_significance) {
    for (i in seq_along(estimate_cols)) {
      out <- gt::cols_merge(
        out,
        columns = c(estimate_cols[i], significance_cols[i]),
        pattern = "{1} {2}"
      )
    }
    significance_note <- .rating_table_metadata(x, "signif_levels") %||%
      "Significance levels: *** p < 0.001; ** p < 0.01; * p < 0.05; . p < 0.1"
    out <- gt::tab_source_note(out, source_note = significance_note)
  }

  out <- gt::cols_align(
    out,
    align = "left",
    columns = "level"
  )
  out <- gt::cols_align(
    out,
    align = "right",
    columns = c(estimate_cols, exposure_col)
  )

  if (!is.null(title) || !is.null(subtitle)) {
    out <- gt::tab_header(out, title = title, subtitle = subtitle)
  }

  out
}

.validate_as_gt_rating_table <- function(x, significance,
                                         show_effect_spanner, model_labels,
                                         locale,
                                         estimate_decimals,
                                         exposure_decimals, missing_text,
                                         title, subtitle) {
  if (!inherits(x, "rating_table")) {
    stop("`x` must be an object returned by `rating_table()`.", call. = FALSE)
  }
  if (!is.null(significance)) {
    validate_single_logical(significance, "significance")
  }
  if (!is.null(show_effect_spanner)) {
    validate_single_logical(show_effect_spanner, "show_effect_spanner")
  }
  validate_single_character(locale, "locale")
  validate_decimal_count(estimate_decimals, "estimate_decimals")
  validate_decimal_count(exposure_decimals, "exposure_decimals")
  validate_single_character(missing_text, "missing_text")
  if (!is.null(title)) {
    validate_single_character(title, "title")
  }
  if (!is.null(subtitle)) {
    validate_single_character(subtitle, "subtitle")
  }
  table_data <- .rating_table_data(x)
  if (!is.data.frame(table_data)) {
    stop("The `rating_table` object does not contain tabular model output.",
         call. = FALSE)
  }

  required <- c("risk_factor", "level")
  missing_required <- setdiff(required, names(table_data))
  if (length(missing_required) > 0L) {
    stop(
      "Required rating-table column(s) missing: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  estimate_cols <- .rating_table_estimate_columns(x, table_data)
  if (length(estimate_cols) == 0L) {
    stop("The `rating_table` object contains no fitted model effects.",
         call. = FALSE)
  }
  .resolve_rating_table_model_labels(
    .rating_table_metadata(x, "models"),
    model_labels
  )
  exposure_col <- .rating_table_metadata(x, "exposure")
  if (!is.null(exposure_col) && !exposure_col %in% names(table_data)) {
    stop("The exposure column stored on `x` is not available in its data.",
         call. = FALSE)
  }

  show_significance <- if (is.null(significance)) {
    isTRUE(.rating_table_metadata(x, "significance")) ||
      isTRUE(.rating_table_metadata(x, "signif_stars"))
  } else {
    significance
  }
  if (show_significance) {
    significance_cols <- .rating_table_significance_columns(x)
    missing_significance <- setdiff(significance_cols, names(table_data))
    if (length(missing_significance) > 0L) {
      stop(
        "Significance information is not available in `x`. Recreate the ",
        "object with `rating_table(..., significance = TRUE)` or use ",
        "`as_gt(x, significance = FALSE)`.",
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

.rating_table_gt_label <- function(x) {
  tools::toTitleCase(gsub("_+", " ", x))
}

.resolve_rating_table_model_labels <- function(model_names, model_labels) {
  if (is.null(model_labels)) {
    return(model_names)
  }
  if (!is.character(model_labels) || anyNA(model_labels) ||
      any(!nzchar(model_labels))) {
    stop(
      "`model_labels` must be a character vector with non-empty labels.",
      call. = FALSE
    )
  }

  supplied_names <- names(model_labels)
  has_names <- !is.null(supplied_names) && any(nzchar(supplied_names))
  if (has_names) {
    if (any(!nzchar(supplied_names)) || anyDuplicated(supplied_names)) {
      stop(
        "A named `model_labels` vector must have one unique name for every ",
        "label.",
        call. = FALSE
      )
    }
    missing_models <- setdiff(model_names, supplied_names)
    unknown_models <- setdiff(supplied_names, model_names)
    if (length(missing_models) > 0L || length(unknown_models) > 0L) {
      stop(
        "Names in `model_labels` must match the model object names: ",
        paste(model_names, collapse = ", "), ".",
        call. = FALSE
      )
    }
    return(unname(model_labels[model_names]))
  }

  if (length(model_labels) != length(model_names)) {
    stop(
      "`model_labels` must contain exactly ", length(model_names),
      if (length(model_names) == 1L) " label." else " labels.",
      call. = FALSE
    )
  }
  unname(model_labels)
}

.rating_table_gt_missing_text <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x <- gsub("-", "&#45;", x, fixed = TRUE)
  gt::html(x)
}
