#' Find active portfolio rows for event dates
#'
#' @description
#' Matches event dates, such as claim dates or portfolio snapshot dates, to the
#' rows that were active in the portfolio on those dates.
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
#' @param nomatch When a row (with interval say, `[a,b]`) in x has no match in
#' y, nomatch=NA means NA is returned for y's non-by.y columns for that row of
#' x. nomatch=NULL (default) means no rows will be returned for that row of x.
#' @param mult When multiple rows in y match to the row in x, `mult` controls
#' which values are returned - "all" (default), "first" or "last".
#'
#' @details
#' This is useful when claim records or other dated events need the rating
#' factors, premium, exposure, or policy attributes that were active at the event
#' date. The function performs an interval join between event dates and
#' portfolio coverage periods, optionally within matching identifiers such as a
#' policy number.
#'
#' @author Martin Haringa
#'
#' @import data.table
#' @importFrom lubridate is.Date
#'
#' @return An object with the same class as `portfolio`.
#' @examples
#' library(lubridate)
#' portfolio <- data.frame(
#' begin1 = ymd(c("2014-01-01", "2014-01-01")),
#' end = ymd(c("2014-03-14", "2014-05-10")),
#' termination = ymd(c("2014-03-14", "2014-05-10")),
#' exposure = c(0.2025, 0.3583),
#' premium =  c(125, 150),
#' car_type = c("BMW", "TESLA"))
#'
#' ## Find active rows on different dates
#' dates0 <- data.frame(active_date = seq(ymd("2014-01-01"), ymd("2014-05-01"),
#' by = "months"))
#' active_rows_by_date(
#'   portfolio,
#'   dates0,
#'   period_start = "begin1",
#'   period_end = "end",
#'   date = "active_date"
#' )
#'
#' ## With extra identifiers (merge claim date with time interval in portfolio)
#' claim_dates <- data.frame(claim_date = ymd("2014-01-01"),
#' car_type = c("BMW", "VOLVO"))
#'
#' ### Only rows are returned that can be matched
#' active_rows_by_date(
#'   portfolio,
#'   claim_dates,
#'   period_start = "begin1",
#'   period_end = "end",
#'   date = "claim_date",
#'   by = "car_type"
#' )
#'
#' ### When row cannot be matched, NA is returned for that row
#' active_rows_by_date(
#'   portfolio,
#'   claim_dates,
#'   period_start = "begin1",
#'   period_end = "end",
#'   date = "claim_date",
#'   by = "car_type",
#'   nomatch = NA
#' )
#'
#' @export
active_rows_by_date <- function(portfolio,
                                dates,
                                period_start,
                                period_end,
                                date,
                                by = NULL,
                                nomatch = NULL,
                                mult = "all") {
  .portfolio_row_id <- .date_row_id <- .event_date <- NULL

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
  if (!is.null(nomatch) && !identical(nomatch, NA)) {
    stop("`nomatch` must be NULL or NA.", call. = FALSE)
  }
  if (!is.character(mult) || length(mult) != 1L ||
      !mult %in% c("all", "first", "last")) {
    stop("`mult` must be 'all', 'first', or 'last'.", call. = FALSE)
  }

  input_class <- class(portfolio)
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
    nomatch = nomatch,
    mult = mult
  )

  event_start <- paste0("i.", period_start)
  event_end <- paste0("i.", period_end)
  event_date <- ".event_date"
  if (event_start %in% names(ans)) ans[, (event_start) := NULL]
  if (event_end %in% names(ans)) ans[, (event_end) := NULL]
  if (event_date %in% names(ans)) {
    data.table::setnames(ans, old = event_date, new = date)
  }
  ans[, intersect(c(".portfolio_row_id", ".date_row_id"), names(ans)) := NULL]
  ans <- ans[order(get(date))]
  class(ans) <- input_class
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
    nomatch = nomatch,
    mult = mult
  )
}
