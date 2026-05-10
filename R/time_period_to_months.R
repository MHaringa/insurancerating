#' @noRd
.time_is_flag <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

#' @noRd
.time_is_nonnegative_whole_number <- function(x) {
  is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x) &&
    x >= 0 &&
    x == as.integer(x)
}

#' @noRd
.time_validate_data_frame <- function(x, arg = "`df`") {
  if (!inherits(x, "data.frame")) {
    stop(arg, " must be a data.frame or data.table.", call. = FALSE)
  }
}

#' @noRd
.time_validate_columns <- function(data, cols, arg) {
  if (!is.character(cols) || anyNA(cols) || any(cols == "")) {
    stop(arg, " must be a character vector of column names.", call. = FALSE)
  }
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0L) {
    stop(
      sprintf("%s not found in data: %s.", arg, paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
}

#' @noRd
.time_validate_date_interval <- function(data, period_start, period_end,
                                         data_name = "`df`") {
  .time_validate_columns(data, c(period_start, period_end),
                         "`period_start` and `period_end`")
  if (!lubridate::is.Date(data[[period_start]]) ||
      !lubridate::is.Date(data[[period_end]])) {
    stop("`period_start` and `period_end` must refer to Date columns.",
         call. = FALSE)
  }
  if (anyNA(data[[period_start]]) || anyNA(data[[period_end]])) {
    stop(data_name, " must not contain missing period start or end dates.",
         call. = FALSE)
  }
  if (any(data[[period_start]] > data[[period_end]])) {
    stop("`period_start` must be on or before `period_end` for every row.",
         call. = FALSE)
  }
}

#' @noRd
.time_proration_weight <- function(start_date, end_date) {
  pmin(as.numeric(end_date - start_date) + 1, 30) / 30
}

#' Split policy periods into monthly rows
#'
#' @description
#' Splits policy or exposure periods that cross calendar months into monthly
#' rows. Numeric columns such as exposure or premium can be prorated over the
#' resulting monthly rows.
#'
#' This function uses **standard evaluation (SE)**: column names must be passed
#' as **character strings** (e.g. `period_start = "begin_date"`).
#' The older function [period_to_months()] used non-standard evaluation (NSE) and
#' is deprecated as of version 0.8.0.
#'
#' @param df A `data.frame` or `data.table`.
#' @param period_start Character string. Name of the column with policy period
#' start dates.
#' @param period_end Character string. Name of the column with policy period end
#' dates.
#' @param prorate_cols Character vector with names of numeric columns to prorate
#' over the monthly rows, for example exposure or premium.
#' @param begin,end,cols Deprecated argument names kept for backward
#' compatibility.
#'
#' @return A `data.frame` with the same columns as in `df`, plus an `id` column.
#'
#' @details
#' Rating and monitoring work often needs exposure, premium, claim counts, or
#' policy counts by calendar month. Source portfolios, however, usually contain
#' policy periods that start and end on arbitrary dates. This helper expands
#' those periods into monthly rows before modelling, reporting, or joining to
#' monthly portfolio summaries.
#'
#' Prorated columns are distributed according to the part of the policy period
#' that falls in each monthly row. Full months receive weight 1; partial months
#' use a 30-day month convention. The total value of each prorated column is
#' preserved per original row.
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
#'   period_start = "begin_date",
#'   period_end = "end_date",
#'   prorate_cols = c("premium", "exposure")
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
split_periods_to_months <- function(df,
                                    period_start = NULL,
                                    period_end = NULL,
                                    prorate_cols = NULL,
                                    begin = NULL,
                                    end = NULL,
                                    cols = NULL) {

  if (!is.null(begin)) {
    lifecycle::deprecate_warn("0.9.0", "split_periods_to_months(begin)",
                              "split_periods_to_months(period_start)")
    period_start <- begin
  }
  if (!is.null(end)) {
    lifecycle::deprecate_warn("0.9.0", "split_periods_to_months(end)",
                              "split_periods_to_months(period_end)")
    period_end <- end
  }
  if (!is.null(cols)) {
    lifecycle::deprecate_warn("0.9.0", "split_periods_to_months(cols)",
                              "split_periods_to_months(prorate_cols)")
    prorate_cols <- cols
  }

  .time_validate_data_frame(df)
  .time_validate_columns(df, c(period_start, period_end),
                         "`period_start` and `period_end`")
  .time_validate_date_interval(df, period_start, period_end)
  if ("id" %in% names(df)) {
    stop("`df` must not contain a column named `id`.", call. = FALSE)
  }
  if (is.null(prorate_cols)) {
    prorate_cols <- character(0)
  }
  .time_validate_columns(df, prorate_cols, "`prorate_cols`")
  non_numeric <- prorate_cols[!vapply(df[prorate_cols], is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    stop(
      sprintf("`prorate_cols` must be numeric: %s.", paste(non_numeric, collapse = ", ")),
      call. = FALSE
    )
  }

  input_class <- class(df)
  rows <- vector("list", nrow(df))

  for (i in seq_len(nrow(df))) {
    start_i <- df[[period_start]][i]
    end_i <- df[[period_end]][i]
    month_start <- lubridate::floor_date(start_i, unit = "months")
    month_starts <- seq(month_start, lubridate::floor_date(end_i, unit = "months"),
                        by = "months")
    month_ends <- lubridate::ceiling_date(month_starts, unit = "months") - 1
    split_start <- pmax(start_i, month_starts)
    split_end <- pmin(end_i, month_ends)

    part <- df[rep(i, length(split_start)), , drop = FALSE]
    part[[period_start]] <- as.Date(split_start)
    part[[period_end]] <- as.Date(split_end)
    part$id <- i

    if (length(prorate_cols) > 0L) {
      weights <- .time_proration_weight(part[[period_start]], part[[period_end]])
      weights <- weights / sum(weights)
      for (col in prorate_cols) {
        part[[col]] <- df[[col]][i] * weights
      }
    }

    rows[[i]] <- part
  }

  out <- do.call(rbind, rows)
  out <- out[, c("id", names(df)), drop = FALSE]
  row.names(out) <- NULL
  class(out) <- input_class
  out
}


#' Deprecated alias for `split_periods_to_months()`
#'
#' @description
#' `period_to_months()` is deprecated as of version 0.8.0. Use
#' [split_periods_to_months()] instead.
#'
#' @inheritParams split_periods_to_months
#' @param begin Deprecated NSE argument. Use `period_start` instead.
#' @param end Deprecated NSE argument. Use `period_end` instead.
#' @param ... Deprecated NSE columns to prorate. Use `prorate_cols` instead.
#'
#' @return See [split_periods_to_months()].
#'
#' @export
#' @keywords internal
period_to_months <- function(df, begin, end, ...) {
  lifecycle::deprecate_warn("0.8.0", "period_to_months()",
                            "split_periods_to_months()")

  begin_chr <- deparse(substitute(begin))
  end_chr   <- deparse(substitute(end))
  cols_chr  <- vapply(substitute(list(...))[-1], deparse, FUN.VALUE = character(1))

  split_periods_to_months(
    df,
    period_start = begin_chr,
    period_end = end_chr,
    prorate_cols = cols_chr
  )
}
