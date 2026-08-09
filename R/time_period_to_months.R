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

#' Split portfolio periods into calendar months
#'
#' @description
#' Split policy periods that cross calendar-month boundaries into separate
#' monthly records. Numeric amounts such as earned exposure and earned premium
#' can be allocated over those records while preserving the amount of each
#' original portfolio row.
#'
#' @param data A `data.frame` or `data.table` containing policy or exposure
#'   periods.
#' @param period_start Character string. Name of the column with policy period
#' start dates.
#' @param period_end Character string. Name of the column with policy period end
#' dates.
#' @param prorate_cols Character vector with names of numeric columns to prorate
#' over the monthly rows, for example exposure or premium.
#' @param df,begin,end,cols Deprecated argument names kept for backward
#'   compatibility. Use `data`, `period_start`, `period_end`, and
#'   `prorate_cols`.
#'
#' @return A regular `data.frame` with one row for each calendar month covered
#'   by an original portfolio record. The original columns are retained and an
#'   `id` column identifies the source row. Columns supplied through
#'   `prorate_cols` contain their allocated monthly amounts.
#'
#' @details
#' Pricing, reserving and monitoring analyses often require exposure and premium
#' by calendar month, whereas policy administration data generally contains
#' periods with arbitrary start and end dates. The function converts those
#' periods into a monthly representation before aggregation, modelling or
#' reporting.
#'
#' This is a temporal expansion rather than a portfolio reduction. See
#' [merge_date_ranges()] for consolidating connected periods and
#' [active_rows_by_date()] for matching dated events to active periods.
#'
#' Prorated columns are distributed according to the part of the policy period
#' represented by each monthly row. Full months receive weight 1 and partial
#' months use a 30-day convention. The monthly weights are normalised within
#' each source row. Consequently, monthly exposure and premium sum to their
#' original values, including for periods that contain partial months.
#'
#' Column names are supplied as character strings, for example
#' `period_start = "begin_date"`. The deprecated [period_to_months()] interface
#' used unquoted column names and is retained only for backward compatibility.
#'
#' Expansion and proration are performed internally with
#' [data.table::data.table()] on a local copy. The supplied object is not
#' modified by reference, and the returned object is always a regular
#' `data.frame`.
#'
#' @examples
#' portfolio <- data.frame(
#'   policy_id = c("P001", "P002", "P003"),
#'   sector = c("Industry", "Retail", "Services"),
#'   coverage_start = as.Date(c("2025-01-15", "2025-02-01", "2025-03-20")),
#'   coverage_end = as.Date(c("2025-03-31", "2025-02-28", "2025-05-10")),
#'   earned_exposure = c(0.21, 0.08, 0.14),
#'   earned_premium = c(420, 160, 280)
#' )
#'
#' # Allocate each policy period and its amounts over calendar months.
#' monthly_portfolio <- split_periods_to_months(
#'   portfolio,
#'   period_start = "coverage_start",
#'   period_end = "coverage_end",
#'   prorate_cols = c("earned_exposure", "earned_premium")
#' )
#' monthly_portfolio
#'
#' # The allocated monthly amounts reconcile to the source portfolio.
#' aggregate(
#'   cbind(earned_exposure, earned_premium) ~ id,
#'   data = monthly_portfolio,
#'   FUN = sum
#' )
#'
#' # Deprecated interface with unquoted column names
#' \dontrun{
#' period_to_months(
#'   portfolio,
#'   coverage_start,
#'   coverage_end,
#'   earned_exposure,
#'   earned_premium
#' )
#' }
#'
#' @author Martin Haringa
#' @import data.table
#' @importFrom lubridate is.Date ceiling_date floor_date
#' @seealso [active_rows_by_date()], [merge_date_ranges()], [rating_grid()]
#' @export
split_periods_to_months <- function(data = NULL,
                                    period_start = NULL,
                                    period_end = NULL,
                                    prorate_cols = NULL,
                                    df = NULL,
                                    begin = NULL,
                                    end = NULL,
                                    cols = NULL) {

  if (!is.null(df)) {
    if (!is.null(data)) {
      stop("Use only one of `data` and deprecated `df`.", call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "split_periods_to_months(df)",
      "split_periods_to_months(data)"
    )
    data <- df
  }

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

  .time_validate_data_frame(data, "`data`")
  .time_validate_columns(data, c(period_start, period_end),
                         "`period_start` and `period_end`")
  .time_validate_date_interval(data, period_start, period_end, "`data`")
  if ("id" %in% names(data)) {
    stop("`data` must not contain a column named `id`.", call. = FALSE)
  }
  if (is.null(prorate_cols)) {
    prorate_cols <- character(0)
  }
  .time_validate_columns(data, prorate_cols, "`prorate_cols`")
  non_numeric <- prorate_cols[!vapply(
    prorate_cols,
    function(column) is.numeric(data[[column]]),
    logical(1)
  )]
  if (length(non_numeric) > 0L) {
    stop(
      sprintf("`prorate_cols` must be numeric: %s.", paste(non_numeric, collapse = ", ")),
      call. = FALSE
    )
  }

  dt <- data.table::as.data.table(data.table::copy(data))
  if (nrow(dt) == 0L) {
    out <- data.frame(id = integer(), as.data.frame(dt), check.names = FALSE)
    rownames(out) <- NULL
    return(out)
  }

  start_month <- lubridate::floor_date(dt[[period_start]], unit = "months")
  end_month <- lubridate::floor_date(dt[[period_end]], unit = "months")
  month_count <-
    (lubridate::year(end_month) - lubridate::year(start_month)) * 12L +
    lubridate::month(end_month) - lubridate::month(start_month) + 1L
  source_row <- rep.int(seq_len(nrow(dt)), month_count)
  month_offset <- sequence(month_count) - 1L
  month_index <-
    lubridate::year(start_month[source_row]) * 12L +
    lubridate::month(start_month[source_row]) - 1L + month_offset
  month_start <- as.Date(ISOdate(
    month_index %/% 12L,
    month_index %% 12L + 1L,
    1L
  ))
  month_end <- lubridate::ceiling_date(month_start, unit = "months") - 1

  out <- dt[source_row]
  out[, id := source_row]
  out[, (period_start) := pmax(dt[[period_start]][source_row], month_start)]
  out[, (period_end) := pmin(dt[[period_end]][source_row], month_end)]

  if (length(prorate_cols) > 0L) {
    weights <- .time_proration_weight(out[[period_start]], out[[period_end]])
    weights <- weights / ave(weights, source_row, FUN = sum)
    out[, (prorate_cols) := lapply(.SD, function(value) value * weights),
        .SDcols = prorate_cols]
  }

  data.table::setcolorder(out, c("id", names(data)))
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  rownames(out) <- NULL
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
    data = df,
    period_start = begin_chr,
    period_end = end_chr,
    prorate_cols = cols_chr
  )
}
