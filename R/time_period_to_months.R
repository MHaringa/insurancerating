#' Split periods into monthly intervals
#'
#' @description
#' Splits rows where the time period is longer than one month into multiple rows
#' with a time period of exactly one month each. Values in numeric columns (e.g.
#' exposure or premium) are divided proportionally over the months.
#'
#' This function uses **standard evaluation (SE)**: column names must be passed
#' as **character strings** (e.g. `begin = "begin_date"`).
#' The older function [period_to_months()] used non-standard evaluation (NSE) and
#' is deprecated as of version 0.8.0.
#'
#' @param df A `data.frame` or `data.table`.
#' @param begin Character string. Name of column in `df` with begin dates.
#' @param end Character string. Name of column in `df` with end dates.
#' @param cols Character vector with names of numeric columns in `df` to split.
#'
#' @return A `data.frame` with the same columns as in `df`, plus an `id` column.
#'
#' @details
#' In insurance portfolios it is common that rows relate to periods longer than
#' one month. This can be problematic when monthly exposures are needed.
#'
#' Since insurance premiums are assumed constant over months (and not depending
#' on the number of days per month), the function assumes each month has 30 days.
#'
#' @examples
#' library(lubridate)
#' portfolio <- data.frame(
#'   begin_date = ymd(c("2014-01-01", "2014-01-01")),
#'   end_date   = ymd(c("2014-03-14", "2014-05-10")),
#'   exposure   = c(0.2025, 0.3583),
#'   premium    = c(125, 150)
#' )
#'
#' # New SE interface
#' split_periods_to_months(portfolio,
#'   begin = "begin_date", end = "end_date",
#'   cols = c("premium", "exposure")
#' )
#'
#' # Old NSE interface (deprecated)
#' \dontrun{
#' period_to_months(portfolio, begin_date, end_date, premium, exposure)
#' }
#'
#' @author Martin Haringa
#' @import data.table
#' @importFrom lubridate is.Date ceiling_date floor_date
#' @export
split_periods_to_months <- function(df, begin, end, cols = NULL) {

  # remember input class
  input_class <- class(df)

  if (!begin %in% names(df) || !end %in% names(df)) {
    stop("`begin` and `end` must be column names in `df`.", call. = FALSE)
  }
  if (!lubridate::is.Date(df[[begin]]) || !lubridate::is.Date(df[[end]])) {
    stop("Columns `begin` and `end` must be Date objects.", call. = FALSE)
  }
  if (!is.null(cols) && !all(cols %in% names(df))) {
    stop("Numeric `cols` not found in data.", call. = FALSE)
  }

  # Create lookup table of months
  datum_begin <- seq(min(df[[begin]]), max(df[[end]]), by = "months")
  datum_end   <- lubridate::ceiling_date(datum_begin, unit = "months") - 1
  datum_begin <- lubridate::floor_date(datum_begin, unit = "months")
  lookup <- data.table::data.table(datum_begin, datum_end)
  data.table::setnames(lookup, c("datum_begin", "datum_end"), c(begin, end))
  data.table::setkeyv(lookup, c(begin, end))

  new_end <- start_int <- end_int <- end_days <- begin_days <-
    overlap_begin_end <- overlap_period <- overlap_total <- NULL

  data.table::setDT(df)
  df[, id := .I][, new_end := get(end) + 1]

  # Overlap with months
  ans <- data.table::foverlaps(df, lookup, type = "any", which = FALSE)

  if (!is.null(cols) && length(cols) > 0) {
    # start_int: whether period starts after lookup begin
    ans[, start_int := data.table::fifelse(
      get(begin) < get(paste0("i.", begin)), 0, 1
    )]

    # end_int: whether period ends before lookup end
    ans[, end_int := data.table::fifelse(
      get(end) > get(paste0("i.", end)), 0, 1
    )]

    # end_days: fraction for partial last month
    ans[, end_days := data.table::fifelse(
      end_int == 0, elapsed_days(new_end) / 30, 0
    )]

    # begin_days: fraction for partial first month
    ans[, begin_days := data.table::fifelse(
      start_int == 0, (30 - elapsed_days(get(paste0("i.", begin)))) / 30, 0
    )]

    # total fraction for first + last month
    ans[, overlap_begin_end := begin_days + end_days]

    # overlap_period: 1 for full months, fractional otherwise
    ans[, overlap_period := data.table::fifelse(
      overlap_begin_end == 0, 1, overlap_begin_end
    )]

    # total overlap across same id
    ans[, overlap_total := sum(overlap_period, na.rm = TRUE), by = id]

    for (col in cols) {
      ans[, (col) := get(col) * overlap_period / overlap_total]
    }
  }

  out <- ans[, c("id", names(df)), with = FALSE]

  # restore input class
  class(out) <- input_class
  out
}


#' @rdname split_periods_to_months
#' @param ... Columns in `df` to split. Deprecated, use `cols` instead.
#' @export
period_to_months <- function(df, begin, end, ...) {
  lifecycle::deprecate_warn("0.8.0", "period_to_months()",
                            "split_periods_to_months()")

  begin_chr <- deparse(substitute(begin))
  end_chr   <- deparse(substitute(end))
  cols_chr  <- vapply(substitute(list(...))[-1], deparse, FUN.VALUE = character(1))

  split_periods_to_months(df, begin = begin_chr, end = end_chr, cols = cols_chr)
}
