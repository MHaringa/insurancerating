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
#'   `"first"` or `"last"` to retain one. Matches for these two choices are
#'   ordered by period start, then period end, then original portfolio row
#'   order for ties. The default, `"all"`, returns matches for each event in
#'   original portfolio row order.
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
#' The interval join includes both the start and end date. Neither input is
#' modified. The output follows the original order of `dates` and is always
#' a regular `data.frame`.
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
  if (any(vapply(by, function(column) {
    anyNA(portfolio[[column]]) || anyNA(dates[[column]])
  }, logical(1)))) {
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

  key_cols <- c(by, period_start, period_end)
  if (anyDuplicated(key_cols) || date %in% by) {
    stop("Join keys and date columns must be distinct within each input.",
         call. = FALSE)
  }
  portfolio_cols <- .active_rows_as_columns(portfolio)
  date_cols <- .active_rows_as_columns(dates)
  if (anyDuplicated(names(portfolio_cols)) || anyDuplicated(names(date_cols))) {
    stop("Column names must be unique within each input.", call. = FALSE)
  }

  # Sort only the interval keys. Keep original row numbers for selecting output.
  keys <- paste0("key", seq_along(key_cols))
  periods <- data.table::as.data.table(stats::setNames(portfolio_cols[key_cols], keys))
  data.table::set(periods, j = "row_id", value = seq_len(nrow(portfolio)))
  data.table::setkeyv(periods, keys)
  event_date <- date_cols[[date]]
  events <- data.table::as.data.table(stats::setNames(
    c(date_cols[by], list(event_date, event_date)), keys
  ))

  ids <- data.table::foverlaps(
    events,
    periods,
    by.x = keys,
    by.y = keys,
    type = "any",
    which = TRUE,
    nomatch = NA,
    mult = multiple_matches
  )

  all_matches <- identical(multiple_matches, "all")
  event_id <- if (all_matches) ids$xid else seq_along(ids)
  portfolio_id <- periods$row_id[if (all_matches) ids$yid else ids]
  if (identical(unmatched, "drop")) {
    matched <- !is.na(portfolio_id)
    event_id <- event_id[matched]
    portfolio_id <- portfolio_id[matched]
  }
  if (all_matches) {
    ord <- order(event_id, portfolio_id, method = "radix")
    event_id <- event_id[ord]
    portfolio_id <- portfolio_id[ord]
  }

  event_keys <- lapply(date_cols[by], function(column) unname(column[event_id]))
  history <- lapply(portfolio_cols[setdiff(names(portfolio_cols), by)],
                    function(column) unname(column[portfolio_id]))
  event_fields <- lapply(date_cols[setdiff(names(date_cols), c(by, date))],
                         function(column) unname(column[event_id]))
  event_fields[[date]] <- unname(event_date[event_id])
  structure(c(event_keys, history, event_fields), class = "data.frame",
            row.names = .set_row_names(length(event_id)))
}


.active_rows_as_columns <- function(data) {
  # Retain data.table's expansion of matrix-like columns when present.
  if (any(vapply(data, function(column) !is.null(dim(column)), logical(1)))) {
    data <- data.table::as.data.table(data)
  }
  as.list(data)
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
