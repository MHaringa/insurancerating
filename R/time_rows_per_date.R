#' Match event dates to active portfolio periods
#'
#' @description
#' Match event dates, such as claim dates or inspection dates, to policy records
#' that were active when the event occurred. The matched result contains the
#' portfolio characteristics and coverage information applicable on each event
#' date.
#'
#' @param portfolio A `data.frame` or `data.table` with portfolio rows and
#' active date intervals.
#' @param dates A `data.frame` or `data.table` with event or snapshot dates.
#' @param period_start Character string. Name of the portfolio column with
#' period start dates.
#' @param period_end Character string. Name of the portfolio column with period
#' end dates.
#' @param date Character string. Name of the date column in `dates`.
#' @param by Character vector with additional columns used to match `portfolio`
#' and `dates`, for example policy number or claim identifier.
#' @param unmatched Character string. Use `"drop"` to omit event dates for which
#'   no active portfolio row is found, or `"keep"` to retain them with missing
#'   portfolio information. The default is `"drop"`.
#' @param multiple_matches Character string controlling events that match
#'   multiple active portfolio rows. Use `"all"` to retain every match, or
#'   `"first"` or `"last"` to retain one matching row. The default is `"all"`.
#' @param nomatch,mult Deprecated technical argument names. Use `unmatched` and
#'   `multiple_matches` instead.
#'
#' @details
#' Claim and event files often contain an event date and policy identifier but
#' not the rating factors used at that point in time. The function performs an
#' interval match between those events and the portfolio history. Supplying a
#' policy identifier through `by` prevents an event from matching active periods
#' belonging to another policy.
#'
#' This is a temporal matching operation rather than a portfolio reduction. See
#' [merge_date_ranges()] for consolidating connected coverage periods and
#' [split_periods_to_months()] for expanding periods into monthly records.
#'
#' Multiple matches can be valid when one event relates to several concurrently
#' active coverages. They can also reveal overlapping or duplicated policy
#' periods. Use `multiple_matches = "all"` when every active record is relevant;
#' use `"first"` or `"last"` only when the source system defines which record
#' should take precedence.
#'
#' With `unmatched = "drop"`, events outside every applicable coverage period
#' are omitted. With `unmatched = "keep"`, they remain visible with missing
#' portfolio fields. Retaining unmatched events is generally preferable during
#' data-quality review because it makes gaps in the policy history explicit.
#'
#' The interval join is performed internally with [data.table::foverlaps()] on
#' local copies. Neither input is modified by reference. The output follows the
#' original order of `dates` and is always returned as a regular `data.frame`.
#'
#' @author Martin Haringa
#'
#' @import data.table
#' @importFrom lubridate is.Date
#'
#' @return A regular `data.frame` containing the event records and the portfolio
#'   information active on their dates. Event order is preserved. Depending on
#'   `multiple_matches`, one event can produce more than one output row.
#' @examples
#' portfolio <- data.frame(
#'   policy_id = c("P001", "P001", "P002"),
#'   coverage_start = as.Date(c("2024-01-01", "2025-01-01", "2025-01-01")),
#'   coverage_end = as.Date(c("2024-12-31", "2025-12-31", "2025-12-31")),
#'   sector = c("Retail", "Industry", "Services"),
#'   insured_amount = c(500000, 750000, 300000),
#'   earned_premium = c(900, 1250, 650)
#' )
#'
#' claims <- data.frame(
#'   claim_id = c("C001", "C002", "C003"),
#'   policy_id = c("P001", "P001", "P002"),
#'   claim_date = as.Date(c("2024-06-15", "2025-08-10", "2026-01-10")),
#'   claim_amount = c(12000, 45000, 8000)
#' )
#'
#' # Attach the policy characteristics that applied on each claim date.
#' active_rows_by_date(
#'   portfolio,
#'   claims,
#'   period_start = "coverage_start",
#'   period_end = "coverage_end",
#'   date = "claim_date",
#'   by = "policy_id"
#' )
#'
#' # Keep claims outside the available policy history for data-quality review.
#' active_rows_by_date(
#'   portfolio,
#'   claims,
#'   period_start = "coverage_start",
#'   period_end = "coverage_end",
#'   date = "claim_date",
#'   by = "policy_id",
#'   unmatched = "keep"
#' )
#'
#' @seealso [split_periods_to_months()], [merge_date_ranges()], [rating_grid()]
#'
#' @export
active_rows_by_date <- function(portfolio,
                                dates,
                                period_start,
                                period_end,
                                date,
                                by = NULL,
                                unmatched = c("drop", "keep"),
                                multiple_matches = c("all", "first", "last"),
                                nomatch = NULL,
                                mult = NULL) {
  .portfolio_row_id <- .date_row_id <- .event_date <- NULL

  unmatched_supplied <- !missing(unmatched)
  if (!missing(nomatch)) {
    if (unmatched_supplied) {
      stop("Use only one of `unmatched` and deprecated `nomatch`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "active_rows_by_date(nomatch)",
      "active_rows_by_date(unmatched)"
    )
    if (!is.null(nomatch) &&
        !(length(nomatch) == 1L && is.na(nomatch))) {
      stop("Deprecated `nomatch` must be NULL or NA.", call. = FALSE)
    }
    unmatched <- if (is.null(nomatch)) "drop" else "keep"
  }
  unmatched <- match.arg(unmatched)

  multiple_matches_supplied <- !missing(multiple_matches)
  if (!missing(mult)) {
    if (multiple_matches_supplied) {
      stop("Use only one of `multiple_matches` and deprecated `mult`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "active_rows_by_date(mult)",
      "active_rows_by_date(multiple_matches)"
    )
    multiple_matches <- mult
  }
  multiple_matches <- match.arg(multiple_matches)

  .time_validate_data_frame(portfolio, "`portfolio`")
  .time_validate_data_frame(dates, "`dates`")
  .time_validate_date_interval(portfolio, period_start, period_end,
                               "`portfolio`")
  .time_validate_columns(dates, date, "`date`")
  if (!lubridate::is.Date(dates[[date]])) {
    stop("`date` must refer to a Date column in `dates`.", call. = FALSE)
  }
  if (anyNA(dates[[date]])) {
    stop("`dates` must not contain missing values in `date`.", call. = FALSE)
  }
  if (is.null(by)) {
    by <- character(0)
  }
  .time_validate_columns(portfolio, by, "`by`")
  .time_validate_columns(dates, by, "`by`")
  missing_by <- c(
    vapply(by, function(column) sum(is.na(portfolio[[column]])), integer(1)),
    vapply(by, function(column) sum(is.na(dates[[column]])), integer(1))
  )
  if (any(missing_by > 0L)) {
    stop(
      "Columns supplied through `by` must not contain missing values in ",
      "`portfolio` or `dates`.",
      call. = FALSE
    )
  }
  portfolio_values <- setdiff(names(portfolio), c(by, period_start, period_end))
  date_values <- setdiff(names(dates), c(by, date))
  conflicting_cols <- intersect(portfolio_values, date_values)
  if (length(conflicting_cols) > 0L) {
    stop(
      "Non-matching columns must have distinct names in `portfolio` and ",
      "`dates`. Conflicting columns: ",
      paste(conflicting_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  portfolio_dt <- data.table::as.data.table(data.table::copy(portfolio))
  dates_dt <- data.table::as.data.table(data.table::copy(dates))

  portfolio_dt[, .portfolio_row_id := .I]
  dates_dt[, .date_row_id := .I]
  dates_dt[, .event_date := get(date)]

  lookup <- data.table::copy(dates_dt)
  data.table::setnames(lookup, old = date, new = period_start)
  lookup[, (period_end) := get(period_start)]

  key_cols <- c(by, period_start, period_end)
  data.table::setkeyv(portfolio_dt, key_cols)
  data.table::setkeyv(lookup, key_cols)

  ans <- data.table::foverlaps(
    lookup,
    portfolio_dt,
    by.x = key_cols,
    by.y = key_cols,
    type = "any",
    which = FALSE,
    nomatch = if (identical(unmatched, "keep")) NA else NULL,
    mult = multiple_matches
  )

  event_start <- paste0("i.", period_start)
  event_end <- paste0("i.", period_end)
  event_date <- ".event_date"
  if (event_start %in% names(ans)) ans[, (event_start) := NULL]
  if (event_end %in% names(ans)) ans[, (event_end) := NULL]
  if (event_date %in% names(ans)) {
    data.table::setnames(ans, old = event_date, new = date)
  }
  order_cols <- intersect(c(".date_row_id", ".portfolio_row_id"), names(ans))
  if (length(order_cols) > 0L) {
    data.table::setorderv(ans, order_cols, na.last = TRUE)
  }
  ans[, intersect(c(".portfolio_row_id", ".date_row_id"), names(ans)) := NULL]
  ans <- as.data.frame(ans, stringsAsFactors = FALSE)
  rownames(ans) <- NULL
  ans
}

#' Deprecated alias for `active_rows_by_date()`
#'
#' @description
#' `rows_per_date()` is deprecated as of version 0.9.0. Use
#' [active_rows_by_date()] instead.
#'
#' @inheritParams active_rows_by_date
#' @param df Deprecated. Use `portfolio` instead.
#' @param df_begin Deprecated NSE argument. Use `period_start` instead.
#' @param df_end Deprecated NSE argument. Use `period_end` instead.
#' @param dates_date Deprecated NSE argument. Use `date` instead.
#' @param ... Deprecated NSE join columns. Use `by` instead.
#'
#' @return See [active_rows_by_date()].
#'
#' @export
#' @keywords internal
rows_per_date <- function(df, dates, df_begin, df_end, dates_date, ...,
                          nomatch = NULL, mult = "all") {
  lifecycle::deprecate_warn("0.9.0", "rows_per_date()",
                            "active_rows_by_date()")

  active_rows_by_date(
    portfolio = df,
    dates = dates,
    period_start = deparse(substitute(df_begin)),
    period_end = deparse(substitute(df_end)),
    date = deparse(substitute(dates_date)),
    by = vapply(substitute(list(...))[-1], deparse, FUN.VALUE = character(1)),
    unmatched = if (is.null(nomatch)) "drop" else "keep",
    multiple_matches = mult
  )
}
