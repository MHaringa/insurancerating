#' @noRd
.time_expr_to_name <- function(expr, env) {
  if (identical(expr, quote(NULL))) {
    return(NULL)
  }
  if (is.character(expr)) {
    return(expr)
  }
  if (is.symbol(expr)) {
    return(as.character(expr))
  }
  val <- tryCatch(eval(expr, env), error = function(e) NULL)
  if (is.character(val)) {
    return(val)
  }
  deparse(expr)
}

#' @noRd
.time_dots_to_names <- function(exprs, env) {
  if (length(exprs) == 0L) {
    return(character(0))
  }
  unlist(lapply(exprs, .time_expr_to_name, env = env), use.names = FALSE)
}

#' @noRd
.time_aggregate_function <- function(aggregate_fun) {
  if (is.character(aggregate_fun)) {
    if (length(aggregate_fun) != 1L || is.na(aggregate_fun) ||
        aggregate_fun == "") {
      stop("`aggregate_fun` must be a function or a single function name.",
           call. = FALSE)
    }
    fun <- get(aggregate_fun, mode = "function")
  } else if (is.function(aggregate_fun)) {
    fun <- aggregate_fun
  } else {
    stop("`aggregate_fun` must be a function or a single function name.",
         call. = FALSE)
  }
  fun
}

#' @noRd
.time_apply_aggregate <- function(fun, x, column) {
  fun_args <- names(formals(args(fun)))
  supports_na_rm <- "na.rm" %in% fun_args || "..." %in% fun_args
  value <- if (supports_na_rm) {
    fun(x, na.rm = TRUE)
  } else {
    fun(x[!is.na(x)])
  }

  if (length(value) != 1L) {
    stop(
      "`aggregate_fun` must return one value per merged interval; column `",
      column,
      "` returned ",
      length(value),
      " values.",
      call. = FALSE
    )
  }
  value
}

#' Reduce portfolio periods by merging adjacent date ranges
#'
#' @description
#' Combine overlapping or nearly adjacent coverage periods for the same policy,
#' risk or portfolio segment. The result provides a consolidated time basis for
#' exposure calculations, active-policy counts and period-based reporting.
#'
#' Together with [rating_grid()], this function belongs to the portfolio
#' reduction workflow. Both functions reduce row-level portfolio data while
#' retaining selected totals. `merge_date_ranges()` reduces temporally connected
#' records; [rating_grid()] reduces records with identical risk-factor values.
#'
#' @param data A `data.frame` or `data.table` containing the portfolio periods.
#' @param period_start Character string. Name of the column with period start
#' dates.
#' @param period_end Character string. Name of the column with period end dates.
#' @param group_by Character vector with columns that identify the portfolio
#' entity or rating segment within which date ranges should be merged.
#' @param aggregate_cols Character vector with numeric columns to aggregate over
#' merged ranges, for example premium or exposure.
#' @param aggregate_fun Function or function name used to combine
#' `aggregate_cols` within a merged interval. The default, `"sum"`, is generally
#' appropriate for additive measures such as premium or exposure.
#' @param merge_gap_days Non-negative whole number. Ranges with fewer uncovered
#'   days than this value are treated as continuous. The default, `1`, merges
#'   overlapping periods and periods that start on the day after the preceding
#'   period ends. Use `0` to merge overlapping periods only. A value above `1`
#'   also bridges short uncovered gaps and should represent an explicit
#'   administrative assumption.
#' @param df,begin,end,...,agg_cols,agg,min.gapwidth Deprecated argument names
#'   kept for backward compatibility. Use `data`, `period_start`, `period_end`,
#'   `group_by`, `aggregate_cols`, `aggregate_fun`, and `merge_gap_days`.
#'
#' @importFrom data.table setDT
#' @importFrom data.table setkeyv
#' @importFrom data.table shift
#'
#' @author Martin Haringa
#'
#' @details
#' ## Portfolio reduction
#'
#' `merge_date_ranges()` performs temporal portfolio reduction. It combines
#' connected periods within the same `group_by` values and retains the selected
#' additive amounts. Use [rating_grid()] for the complementary categorical
#' reduction of records with identical risk-factor combinations.
#'
#' ## Connected periods
#'
#' Insurance portfolio extracts often contain multiple rows for the same policy
#' or risk because of renewals, endorsements, product changes, or short
#' administrative gaps. Before calculating portfolio in/outflow, active exposure
#' windows, or policy counts, it can be useful to reduce those rows to stable
#' coverage intervals.
#'
#' `merge_date_ranges()` merges date ranges within each `group_by` combination.
#' Ranges with a gap smaller than `merge_gap_days` are treated as one continuous
#' interval. If `aggregate_cols` is supplied, those columns are aggregated over
#' the merged interval. The grouping columns should identify records for which
#' combining coverage periods is actuarially and operationally meaningful;
#' periods belonging to different risks or contracts should not be pooled.
#' Missing values are not permitted in `group_by`, because such records cannot
#' be assigned reliably to the same policy, risk or segment.
#'
#' Start and end dates are treated as inclusive. Consequently, two periods for
#' which the second starts one day after the first ends have zero uncovered
#' days. They are merged with the default `merge_gap_days = 1`. A value of `5`
#' merges periods with at most four uncovered days between them.
#'
#' ## Aggregation and implementation
#'
#' Aggregated amounts are combined over the source rows, not prorated over
#' calendar days. Summing premium or exposure is appropriate when each source
#' row contains a distinct additive amount. If overlapping rows already contain
#' amounts for the same covered days, users should resolve that overlap before
#' aggregation to avoid double counting.
#'
#' Each output row represents one consolidated period within a `group_by`
#' combination. This is a temporal reduction of the portfolio; records with the
#' same risk-factor values are not combined when their periods remain separate.
#'
#' Internally, interval construction and aggregation use [data.table::data.table()]
#' on a local copy. The supplied object is not modified by reference.
#' For a portfolio that should remain outside R memory, use
#' [merge_date_ranges_db()] to perform the same interval reduction in DuckDB.
#'
#' @return A regular `data.frame` with classes `"merged_date_ranges"` and
#' `"reduce"`, and attributes:
#' \itemize{
#'   \item `begin` — name of the period-start column
#'   \item `end`   — name of the period-end column
#'   \item `cols`  — grouping columns
#' }
#'
#' @examples
#' portfolio <- data.frame(
#'   policy_id = rep(c("P001", "P002"), each = 3),
#'   coverage = rep(c("Fire", "Liability"), each = 3),
#'   period_start = as.Date(c(
#'     "2024-01-01", "2024-02-01", "2024-04-01",
#'     "2024-01-01", "2024-02-03", "2024-03-03"
#'   )),
#'   period_end = as.Date(c(
#'     "2024-01-31", "2024-02-29", "2024-04-30",
#'     "2024-01-31", "2024-03-02", "2024-03-31"
#'   )),
#'   earned_premium = c(100, 110, 120, 80, 90, 95)
#' )
#'
#' # Reduce directly adjacent periods within each policy and coverage.
#' pt1 <- merge_date_ranges(
#'   portfolio,
#'   period_start = "period_start",
#'   period_end = "period_end",
#'   group_by = c("policy_id", "coverage")
#' )
#'
#' summary(pt1, period = "months", policy_id, coverage)
#'
#' # Bridge a short administrative gap and retain additive premium totals.
#' pt2 <- merge_date_ranges(
#'   portfolio,
#'   period_start = "period_start",
#'   period_end = "period_end",
#'   group_by = c("policy_id", "coverage"),
#'   aggregate_cols = "earned_premium",
#'   # Explicitly bridge administrative gaps of up to four uncovered days.
#'   merge_gap_days = 5
#' )
#'
#' summary(pt2, period = "months", policy_id, coverage)
#'
#' @seealso [merge_date_ranges_db()], [rating_grid()], [rating_grid_db()],
#'   [active_rows_by_date()], [split_periods_to_months()]
#'
#' @export
merge_date_ranges <- function(data = NULL,
                              ...,
                              period_start = NULL,
                              period_end = NULL,
                              group_by = NULL,
                              aggregate_cols = NULL,
                              aggregate_fun = "sum",
                              merge_gap_days = 1,
                              df = NULL,
                              begin = NULL,
                              end = NULL,
                              agg_cols = NULL,
                              agg = NULL,
                              min.gapwidth = NULL) {

  env <- parent.frame()
  begin_expr <- substitute(begin)
  end_expr <- substitute(end)
  dots_expr <- as.list(substitute(list(...))[-1])
  agg_cols_expr <- substitute(agg_cols)

  if (!is.null(df)) {
    if (!is.null(data)) {
      stop("Use only one of `data` and deprecated `df`.", call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "merge_date_ranges(df)",
      "merge_date_ranges(data)"
    )
    data <- df
  }

  if (!identical(begin_expr, quote(NULL))) {
    lifecycle::deprecate_warn("0.9.0", "merge_date_ranges(begin)",
                              "merge_date_ranges(period_start)")
    period_start <- .time_expr_to_name(begin_expr, env)
  }
  if (!identical(end_expr, quote(NULL))) {
    lifecycle::deprecate_warn("0.9.0", "merge_date_ranges(end)",
                              "merge_date_ranges(period_end)")
    period_end <- .time_expr_to_name(end_expr, env)
  }
  if (length(dots_expr) > 0L) {
    lifecycle::deprecate_warn("0.9.0", "merge_date_ranges(...)",
                              "merge_date_ranges(group_by)")
    group_by <- .time_dots_to_names(dots_expr, env)
  }
  if (!identical(agg_cols_expr, quote(NULL))) {
    lifecycle::deprecate_warn("0.9.0", "merge_date_ranges(agg_cols)",
                              "merge_date_ranges(aggregate_cols)")
    aggregate_cols <- .time_dots_to_names(as.list(agg_cols_expr)[-1], env)
  }
  if (!is.null(agg)) {
    lifecycle::deprecate_warn("0.9.0", "merge_date_ranges(agg)",
                              "merge_date_ranges(aggregate_fun)")
    aggregate_fun <- agg
  }
  if (!is.null(min.gapwidth)) {
    lifecycle::deprecate_warn("0.9.0", "merge_date_ranges(min.gapwidth)",
                              "merge_date_ranges(merge_gap_days)")
    merge_gap_days <- min.gapwidth
  }

  .time_validate_data_frame(data, "`data`")
  .time_validate_columns(data, c(period_start, period_end),
                         "`period_start` and `period_end`")
  .time_validate_date_interval(data, period_start, period_end, "`data`")
  if (is.null(group_by) || length(group_by) == 0L) {
    stop("`group_by` must contain at least one column name.", call. = FALSE)
  }
  .time_validate_columns(data, group_by, "`group_by`")
  missing_groups <- vapply(
    data[group_by],
    function(column) sum(is.na(column)),
    integer(1)
  )
  missing_groups <- missing_groups[missing_groups > 0L]
  if (length(missing_groups) > 0L) {
    details <- paste0(names(missing_groups), " (", missing_groups, ")")
    stop(
      "`data` contains missing values in `group_by`: ",
      paste(details, collapse = ", "),
      ". Periods with a missing policy or risk identifier cannot be merged reliably.",
      call. = FALSE
    )
  }
  if (is.null(aggregate_cols)) {
    aggregate_cols <- character(0)
  }
  .time_validate_columns(data, aggregate_cols, "`aggregate_cols`")
  output_id_cols <- c(group_by, period_start, period_end)
  overlapping_cols <- intersect(aggregate_cols, output_id_cols)
  if (length(overlapping_cols) > 0L) {
    stop(
      "`aggregate_cols` must not also be used as grouping or period columns: ",
      paste(overlapping_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  non_numeric <- aggregate_cols[!vapply(data[aggregate_cols], is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    stop(
      sprintf("`aggregate_cols` must be numeric: %s.", paste(non_numeric, collapse = ", ")),
      call. = FALSE
    )
  }
  if (!.time_is_nonnegative_whole_number(merge_gap_days)) {
    stop("`merge_gap_days` must be a non-negative whole number.",
         call. = FALSE)
  }
  agg_fun <- .time_aggregate_function(aggregate_fun)

  dt <- data.table::as.data.table(data.table::copy(data))
  data.table::setorderv(dt, c(group_by, period_start, period_end))
  period_start_name <- period_start
  period_end_name <- period_end

  output_cols <- unique(c(group_by, period_start, period_end, aggregate_cols))
  if (nrow(dt) == 0L) {
    dt_reduce <- dt[, output_cols, with = FALSE]
  } else {
    previous_end_col <- ".merge_previous_end"
    interval_col <- ".merge_interval"
    dt[, (previous_end_col) := data.table::shift(
      cummax(as.numeric(.SD[[1L]]))
    ), by = group_by, .SDcols = period_end]
    dt[, (interval_col) := cumsum(
      is.na(.SD[[1L]]) |
        (as.numeric(.SD[[2L]]) - .SD[[1L]] - 1) >= merge_gap_days
    ), by = group_by, .SDcols = c(previous_end_col, period_start)]

    interval_by <- c(group_by, interval_col)
    value_cols <- unique(c(period_start, period_end, aggregate_cols))
    dt_reduce <- dt[, {
      values <- list(
        min(.SD[[period_start_name]]),
        max(.SD[[period_end_name]])
      )
      names(values) <- c(period_start_name, period_end_name)
      for (col in aggregate_cols) {
        values[[col]] <- .time_apply_aggregate(agg_fun, .SD[[col]], col)
      }
      values
    }, by = interval_by, .SDcols = value_cols]
    dt_reduce[, (interval_col) := NULL]
  }

  data.table::setorderv(dt_reduce, c(group_by, period_start, period_end))
  dt_reduce <- as.data.frame(dt_reduce, stringsAsFactors = FALSE)
  rownames(dt_reduce) <- NULL
  attr(dt_reduce, "begin") <- period_start
  attr(dt_reduce, "end") <- period_end
  attr(dt_reduce, "cols") <- group_by
  class(dt_reduce) <- c("merged_date_ranges", "reduce", "data.frame")
  dt_reduce
}

#' Deprecated alias for `merge_date_ranges()`
#'
#' @description
#' `reduce()` is deprecated as of version 0.8.0. Use
#' [merge_date_ranges()] instead.
#'
#' @inheritParams merge_date_ranges
#' @param begin Deprecated NSE argument. Use `period_start` instead.
#' @param end Deprecated NSE argument. Use `period_end` instead.
#' @param ... Deprecated NSE grouping columns. Use `group_by` instead.
#' @param agg_cols Deprecated NSE argument. Use `aggregate_cols` instead.
#' @param agg Deprecated. Use `aggregate_fun` instead.
#' @param min.gapwidth Deprecated. Use `merge_gap_days` instead.
#'
#' @return See [merge_date_ranges()].
#'
#' @export
#' @keywords internal
reduce <- function(df, begin, end, ..., agg_cols = NULL, agg = "sum",
                   min.gapwidth = 5) {

  lifecycle::deprecate_warn("0.8.0", "reduce()", "merge_date_ranges()")

  period_start <- deparse(substitute(begin))
  period_end <- deparse(substitute(end))
  group_by <- vapply(substitute(list(...))[-1], deparse,
                     FUN.VALUE = character(1))
  aggregate_cols <- vapply(substitute(agg_cols)[-1], deparse,
                           FUN.VALUE = character(1))

  merge_date_ranges(
    data = df,
    period_start = period_start,
    period_end = period_end,
    group_by = group_by,
    aggregate_cols = aggregate_cols,
    aggregate_fun = agg,
    merge_gap_days = min.gapwidth
  )
}


#' @export
print.reduce <- function(x, ...) {
  print.data.frame(as.data.frame.reduce(x), ...)
  invisible(x)
}

#' @export
print.merged_date_ranges <- print.reduce

#' @export
as.data.frame.reduce <- function(x, row.names = NULL, optional = FALSE, ...) {
  class(x) <- "data.frame"
  x
}

#' @export
as.data.frame.merged_date_ranges <- as.data.frame.reduce

#' @import data.table
#' @importFrom lubridate days
#' @importFrom lubridate weeks
#' @importFrom lubridate period
#' @importFrom lubridate %m+%
#' @export
summary.reduce <- function(object, ..., period = "days", name = "count") {

  df <- data.table::as.data.table(data.table::copy(as.data.frame(object)))
  begin <- attr(object, "begin")
  end <- attr(object, "end")

  by_begin <- begin
  by_end <- end

  if (!period %in% c("years", "year", "quarters", "quarter", "months", "month",
                     "weeks", "week", "day", "days")) {
    stop("period is not valid: choose 'year', 'quarter', 'month',
         'week', or 'day'", call. = FALSE)
  }

  cols0 <- vapply(substitute(list(...))[-1], deparse, FUN.VALUE = character(1))

  if (length(cols0) > 0) {
    by_begin <- c(by_begin, cols0)
    by_end <- c(by_end, cols0)
  }

  type <- week <- month <- quarter <- NULL # due to NSE notes in R CMD check

  new <- data.table::data.table(df)[, list(count = .N),
                                    by = c(by_begin)][, type := "in"]
  lost <- data.table::data.table(df)[, list(count = .N),
                                     by = c(by_end)][, type := "out"]

  if (period %in% c("days", "day")) {
    new[, date := .SD[[1]], .SDcols = begin]
    lost[, date := .SD[[1]] %m+% lubridate::days(1), .SDcols = end]
  }

  if (period %in% c("weeks", "week")) {
    new[, week := .SD[[1]], .SDcols = begin]
    new[, week := paste0(data.table::year(week), "W",
                         ifelse(nchar(data.table::week(week)) == 1,
                                paste0("0", data.table::week(week)),
                                data.table::week(week)))]
    lost[, week := .SD[[1]] %m+% lubridate::weeks(1), .SDcols = end]
    lost[, week := paste0(data.table::year(week), "W",
                          ifelse(nchar(data.table::week(week)) == 1,
                                 paste0("0", data.table::week(week)),
                                 data.table::week(week)))]
  }

  if (period %in% c("months", "month")) {
    new[, month := .SD[[1]], .SDcols = begin]
    new[, month := paste0(data.table::year(month), "M",
                          ifelse(nchar(data.table::month(month)) == 1,
                                 paste0("0", data.table::month(month)),
                                 data.table::month(month)))]
    lost[, month := .SD[[1]] %m+% lubridate::period(month = 1), .SDcols = end]
    lost[, month := paste0(data.table::year(month), "M",
                           ifelse(nchar(data.table::month(month)) == 1,
                                  paste0("0", data.table::month(month)),
                                  data.table::month(month)))]
  }

  if (period %in% c("quarters", "quarter")) {
    new[, quarter := paste0(data.table::year(.SD[[1]]), "Q",
                            data.table::quarter(.SD[[1]])), .SDcols = begin]
    lost[, quarter := paste0(data.table::year(.SD[[1]] %m+% lubridate::period(month = 3)), "Q",
                             data.table::quarter(.SD[[1]] %m+% lubridate::period(month = 3))),
         .SDcols = end]
  }

  if (period %in% c("years", "year")) {
    new[, year := data.table::year(.SD[[1]]), .SDcols = begin]
    lost[, year := data.table::year(.SD[[1]]), .SDcols = end]
  }

  new[, c(begin) := NULL]
  lost[, c(end) := NULL]

  dt <- data.table::rbindlist(list(new, lost))

  if (length(cols0) == 0) {
    dt <- dt[, .(count = sum(count)), by = c(names(dt)[ncol(dt)], "type")]
    data.table::setorderv(dt, c(names(dt)[ncol(dt)], "type", "count"),
                          c(-1, 1, 1))
    df <- as.data.frame(dt)
  }

  if (length(cols0) > 0) {
    dt <- dt[, .(count = sum(count)),
             by = c(names(dt)[ncol(dt)], "type", cols0)]
    data.table::setcolorder(dt, c(names(dt)[1], "type", "count", cols0))
    data.table::setorderv(dt, c(names(dt)[1], cols0, "type"),
                          c(-1, rep(1, length(cols0)), 1))
    df <- as.data.frame(dt)
  }

  if (name != "count") {
    if (!is.character(name)) stop("Column name should be a character",
                                  call. = FALSE)
    names(df)[names(df) == "count"] <- name
  }

  df
}

#' @export
summary.merged_date_ranges <- summary.reduce
