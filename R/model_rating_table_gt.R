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
#' With `significance = TRUE`, significance stars are appended to the fitted
#' effects and the significance levels are shown below the table. This requires
#' an object originally created with `rating_table(significance = TRUE)`,
#' because p-value information is deliberately not retained when significance
#' is disabled during table construction. Significance stars are a statistical
#' diagnostic and should be interpreted together with exposure, effect size,
#' model stability and actuarial relevance.
#'
#' @param significance Optional logical. If `NULL`, use the significance setting
#'   stored on `x`. If `TRUE`, append the stored significance stars to the model
#'   effects and add the significance-level note. If `FALSE`, show fitted
#'   effects without stars.
#' @param locale Character. Locale used to format model effects and exposure,
#'   for example `"nl-NL"` or `"en-US"`.
#' @param estimate_decimals Non-negative whole number. Number of decimals shown
#'   for fitted coefficients or relativities.
#' @param exposure_decimals Non-negative whole number. Number of decimals shown
#'   for the exposure column, when available.
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
#'   as_gt(fitted_tariff, significance = FALSE, locale = "en-US")
#' }
#'
#' @export
as_gt.rating_table <- function(x,
                               significance = NULL,
                               locale = "nl-NL",
                               estimate_decimals = 3,
                               exposure_decimals = 0,
                               title = NULL,
                               subtitle = NULL,
                               ...) {
  rlang::check_installed("gt")
  .check_dots_empty(...)
  .validate_as_gt_rating_table(
    x = x,
    significance = significance,
    locale = locale,
    estimate_decimals = estimate_decimals,
    exposure_decimals = exposure_decimals,
    title = title,
    subtitle = subtitle
  )

  show_significance <- if (is.null(significance)) {
    isTRUE(x$significance) || isTRUE(x$signif_stars)
  } else {
    significance
  }

  table_data <- as.data.frame(x$df)
  estimate_cols <- grep("^est_", names(table_data), value = TRUE)
  significance_cols <- paste0(
    "signif_",
    sub("^est_", "", estimate_cols)
  )
  exposure_col <- x$exposure

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
  for (column in estimate_cols) {
    labels[[column]] <- .rating_table_gt_label(sub("^est_", "", column))
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

  out <- gt::tab_spanner(
    out,
    label = if (isTRUE(x$expon)) "Relativities" else "Coefficients",
    columns = estimate_cols
  )
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

  if (show_significance) {
    for (i in seq_along(estimate_cols)) {
      out <- gt::cols_merge(
        out,
        columns = c(estimate_cols[i], significance_cols[i]),
        pattern = "{1} {2}"
      )
    }
    significance_note <- x$signif_levels %||%
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

.validate_as_gt_rating_table <- function(x, significance, locale,
                                         estimate_decimals,
                                         exposure_decimals, title, subtitle) {
  if (!inherits(x, "rating_table")) {
    stop("`x` must be an object returned by `rating_table()`.", call. = FALSE)
  }
  if (!is.null(significance)) {
    validate_single_logical(significance, "significance")
  }
  validate_single_character(locale, "locale")
  validate_decimal_count(estimate_decimals, "estimate_decimals")
  validate_decimal_count(exposure_decimals, "exposure_decimals")
  if (!is.null(title)) {
    validate_single_character(title, "title")
  }
  if (!is.null(subtitle)) {
    validate_single_character(subtitle, "subtitle")
  }
  if (!is.data.frame(x$df)) {
    stop("The `rating_table` object does not contain tabular model output.",
         call. = FALSE)
  }

  required <- c("risk_factor", "level")
  missing_required <- setdiff(required, names(x$df))
  if (length(missing_required) > 0L) {
    stop(
      "Required rating-table column(s) missing: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  estimate_cols <- grep("^est_", names(x$df), value = TRUE)
  if (length(estimate_cols) == 0L) {
    stop("The `rating_table` object contains no fitted model effects.",
         call. = FALSE)
  }
  if (!is.null(x$exposure) && !x$exposure %in% names(x$df)) {
    stop("The exposure column stored on `x` is not available in its data.",
         call. = FALSE)
  }

  show_significance <- if (is.null(significance)) {
    isTRUE(x$significance) || isTRUE(x$signif_stars)
  } else {
    significance
  }
  if (show_significance) {
    significance_cols <- paste0(
      "signif_",
      sub("^est_", "", estimate_cols)
    )
    missing_significance <- setdiff(significance_cols, names(x$df))
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
