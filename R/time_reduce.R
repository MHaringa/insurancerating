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
.time_apply_aggregate <- function(fun, x, column, supports_na_rm = NULL) {
  if (is.null(supports_na_rm)) {
    fun_args <- names(formals(args(fun)))
    supports_na_rm <- "na.rm" %in% fun_args || "..." %in% fun_args
  }
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

#' @noRd
.time_summary_date_column <- function(object, attribute, legacy_attribute) {
  column <- attr(object, attribute, exact = TRUE)
  if (is.null(column)) column <- attr(object, legacy_attribute, exact = TRUE)
  if (is.null(column)) {
    stop("Missing `", attribute, "` metadata. Recreate the result with ",
         "`merge_date_ranges()` or set `attr(object, \"", attribute,
         "\")` to the corresponding date-column name.", call. = FALSE)
  }
  if (!is.character(column) || length(column) != 1L || !is.null(dim(column)) ||
      is.na(column) || !nzchar(column)) {
    stop("The `", attribute, "` attribute must name one date column ",
         "using a non-missing, non-empty character string.", call. = FALSE)
  }
  if (!column %in% names(object)) {
    stop("The `", attribute, "` attribute refers to column `", column,
         "`, which is missing. Restore the column, or update `attr(object, \"",
         attribute, "\")` if it was renamed.", call. = FALSE)
  }
  if (!inherits(object[[column]], "Date")) {
    stop("Column `", column, "` named by `", attribute,
         "` must be a Date column.", call. = FALSE)
  }
  column
}

#' Reduce portfolio periods by merging adjacent date ranges
#'
#' @description
#' Combine overlapping or nearly adjacent coverage periods for the same policy,
#' risk or portfolio segment, optionally aggregating amounts such as premium
#' or exposure. Separate periods remain separate rows.
#'
#' @param data A `data.frame` or `data.table` containing the portfolio periods.
#' @param period_start Character string. Name of the column with period start
#' dates (class `Date`).
#' @param period_end Character string. Name of the period-end column (class `Date`).
#' @param group_by Character vector with columns that identify the portfolio
#' entity or rating segment within which date ranges should be merged.
#' @param allow_na_in Optional character vector naming columns in `group_by`
#'   where missing values may form a distinct grouping value. Missing values
#'   in any other grouping column cause an error. The default, `NULL`, allows
#'   none. Unlike `drop_na` in [rating_grid()], this argument does not filter
#'   rows out.
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
#' ## Connected periods
#'
#' Periods merge only when their `group_by` values match. Include the policy
#' or risk identifier when unrelated contracts must remain separate.
#'
#' Dates are inclusive: a period ending on 31 January and one starting on
#' 1 February have no uncovered days and merge by default. With
#' `merge_gap_days = 5`, gaps of up to four uncovered days are also bridged.
#'
#' Use e.g. `allow_na_in = "coverage"` to treat missing coverage as a distinct
#' group value. Missing values match each other, not known values; other
#' grouping values and the date-gap rule still apply. Avoid allowing missing
#' identifiers if this could combine unrelated risks.
#'
#' ## Aggregation
#'
#' `aggregate_cols` are combined over source rows, not prorated over calendar
#' days. Sum only distinct additive amounts: merging overlapping dates does
#' not remove duplicated premium or exposure. Resolve such duplicates first.
#'
#' The input is not modified. For database-backed processing, use
#' [merge_date_ranges_db()]. To aggregate identical rating-factor combinations
#' rather than connected periods, use [rating_grid()].
#'
#' ## Summaries
#'
#' `summary()` counts consolidated periods entering and leaving the portfolio.
#' Choose `period = "days"` (default), `"weeks"`, `"months"`, `"quarters"` or
#' `"years"`, and optionally rename the count column with `name`.
#' Supply grouping columns explicitly, e.g. `summary(x, coverage, period = "months")`;
#' the original `group_by` is not applied automatically. These are period
#' movement counts, not counts of unique policies.
#'
#' If you rename a date column, update its `period_start` or `period_end`
#' attribute so `summary()` can find it. Older saved objects with `begin` and
#' `end` attributes remain supported when the new attributes are absent.
#'
#' @return A `data.frame` with one row per consolidated period, containing the
#' grouping columns, dates and requested aggregates. It has classes
#' `"merged_date_ranges"` and `"reduce"`, and attributes:
#' \itemize{
#'   \item `period_start`: name of the period-start column
#'   \item `period_end`: name of the period-end column
#'   \item `group_by`: original grouping columns
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
#' # Count movements by month, policy and coverage.
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
#' pt2
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
                              allow_na_in = NULL,
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
  if (!is.null(allow_na_in)) {
    if (!is.character(allow_na_in) || !is.null(dim(allow_na_in)) ||
        anyNA(allow_na_in) || any(allow_na_in == "")) {
      stop("`allow_na_in` must be NULL or a character vector of column names.",
           call. = FALSE)
    }
    unknown <- setdiff(allow_na_in, group_by)
    if (length(unknown)) {
      stop("Columns in `allow_na_in` must also be in `group_by`: ",
           paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
  }
  missing_groups <- vapply(
    setdiff(group_by, allow_na_in),
    function(column) sum(is.na(data[[column]])),
    integer(1)
  )
  missing_groups <- missing_groups[missing_groups > 0L]
  if (length(missing_groups) > 0L) {
    details <- paste0(names(missing_groups), " (", missing_groups, ")")
    stop(
      "NA values in `group_by` columns: ",
      paste(details, collapse = ", "),
      ". If NA is an intended grouping value, use `allow_na_in = ",
      deparse1(union(allow_na_in, names(missing_groups))),
      "`. Otherwise, resolve the missing values before merging.",
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
  non_numeric <- aggregate_cols[!vapply(
    aggregate_cols, function(column) is.numeric(data[[column]]), logical(1)
  )]
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

  output_cols <- unique(c(group_by, period_start, period_end, aggregate_cols))
  if (nrow(data) == 0L) {
    dt_reduce <- stats::setNames(lapply(output_cols, function(col) data[[col]][integer()]),
                                 output_cols)
  } else {
    # Dense ranks use data.table's key equality/order, including NA and factors,
    # without sorting or copying the wide grouping columns themselves.
    work <- data.table::data.table(
      group_id = data.table::frankv(data, cols = group_by, ties.method = "dense",
                                    na.last = FALSE),
      row_id = seq_len(nrow(data)),
      start = data[[period_start]], end = data[[period_end]]
    )
    data.table::setorderv(work, c("group_id", "start", "end"))
    running_end <- NULL # data.table NSE
    data.table::set(work, j = "running_end", value = as.numeric(work[["end"]]))
    work[, running_end := cummax(running_end), by = "group_id"]
    previous_end <- data.table::shift(work[["running_end"]])
    new_group <- work[["group_id"]] != data.table::shift(work[["group_id"]], fill = 0L)
    new_interval <- new_group | is.na(previous_end) |
      (as.numeric(work[["start"]]) - previous_end - 1) >= merge_gap_days
    data.table::set(work, j = "interval", value = cumsum(new_interval))
    data.table::set(work, j = c("group_id", "running_end"), value = NULL)
    rm(previous_end, new_group, new_interval)

    aggregate_cols <- unique(aggregate_cols)
    value_names <- sprintf("value_%d", seq_along(aggregate_cols))
    fast_sum <- identical(agg_fun, base::sum) && all(vapply(
      aggregate_cols, function(col) !is.object(data[[col]]), logical(1)
    ))
    integer_cols <- vapply(aggregate_cols, function(col) is.integer(data[[col]]), logical(1))
    for (i in seq_along(aggregate_cols)) {
      value <- data[[aggregate_cols[i]]][work[["row_id"]]]
      # Avoid integer-overflow warnings in gsum; restore integer totals below
      # when representable, just as base sum does for ordinary integer input.
      if (fast_sum && integer_cols[i]) value <- as.double(value)
      data.table::set(work, j = value_names[i], value = value)
    }
    if (!length(aggregate_cols) || fast_sum) {
      sums <- lapply(value_names, function(col) {
        substitute(sum(value, na.rm = TRUE), list(value = as.name(col)))
      })
      query <- as.call(c(list(quote(list)),
        list(row_id = quote(row_id[1L]), start = quote(min(start)), end = quote(max(end))),
        stats::setNames(sums, value_names)))
      reduced <- work[, eval(query), by = "interval"]
      for (i in which(integer_cols)) {
        total <- reduced[[value_names[i]]]
        if (all(is.finite(total) & abs(total) <= .Machine$integer.max)) {
          data.table::set(reduced, j = value_names[i], value = as.integer(total))
        }
      }
    } else {
      fun_args <- names(formals(args(agg_fun)))
      supports_na_rm <- "na.rm" %in% fun_args || "..." %in% fun_args
      reduced <- work[, {
        values <- list(row_id = .SD[["row_id"]][1L],
                       start = min(.SD[["start"]]), end = max(.SD[["end"]]))
        for (i in seq_along(aggregate_cols)) {
          values[[value_names[i]]] <- .time_apply_aggregate(
            agg_fun, .SD[[value_names[i]]], aggregate_cols[i], supports_na_rm
          )
        }
        values
      }, by = "interval", .SDcols = c("row_id", "start", "end", value_names)]
    }
    rm(work)

    # Intervals are already in key/date order. Gather the wide keys only for
    # output rows, preserving their classes and missing values.
    dt_reduce <- c(
      lapply(group_by, function(col) data[[col]][reduced[["row_id"]]]),
      lapply(c("start", "end", value_names), function(col) reduced[[col]])
    )
    names(dt_reduce) <- c(group_by, period_start, period_end, aggregate_cols)
  }
  data.table::setDF(dt_reduce)
  rownames(dt_reduce) <- NULL
  attr(dt_reduce, "period_start") <- period_start
  attr(dt_reduce, "period_end") <- period_end
  attr(dt_reduce, "group_by") <- group_by
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

  begin <- .time_summary_date_column(object, "period_start", "begin")
  end <- .time_summary_date_column(object, "period_end", "end")

  by_begin <- begin
  by_end <- end

  if (!period %in% c("years", "year", "quarters", "quarter", "months", "month",
                     "weeks", "week", "day", "days")) {
    stop("period is not valid: choose 'year', 'quarter', 'month',
         'week', or 'day'", call. = FALSE)
  }

  cols0 <- vapply(substitute(list(...))[-1], deparse, FUN.VALUE = character(1))
  selected_columns <- unique(c(begin, end, cols0))
  # Project before conversion so wide portfolio columns are not copied.
  df <- data.table::as.data.table(
    as.data.frame(object)[, selected_columns, drop = FALSE]
  )

  if (length(cols0) > 0) {
    by_begin <- c(by_begin, cols0)
    by_end <- c(by_end, cols0)
  }

  type <- week <- month <- quarter <- NULL # due to NSE notes in R CMD check

  new <- df[, list(count = .N), by = c(by_begin)][, type := "in"]
  lost <- df[, list(count = .N), by = c(by_end)][, type := "out"]

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
