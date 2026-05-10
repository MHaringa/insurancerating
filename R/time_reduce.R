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

#' Reduce portfolio periods by merging adjacent date ranges
#'
#' @description
#' Merges overlapping or nearly adjacent policy periods within portfolio groups.
#'
#' @param df A `data.frame` or `data.table`.
#' @param period_start Character string. Name of the column with period start
#' dates.
#' @param period_end Character string. Name of the column with period end dates.
#' @param group_by Character vector with columns that identify the portfolio
#' entity or rating segment within which date ranges should be merged.
#' @param aggregate_cols Character vector with numeric columns to aggregate over
#' merged ranges, for example premium or exposure.
#' @param aggregate_fun Aggregation function or function name. Defaults to
#' `"sum"`.
#' @param merge_gap_days Non-negative whole number. Ranges with a gap smaller
#' than this number of days are merged. Defaults to 5.
#' @param begin,end,...,agg_cols,agg,min.gapwidth Deprecated NSE argument names
#' kept for backward compatibility.
#'
#' @importFrom data.table setDT
#' @importFrom data.table setkeyv
#' @importFrom data.table shift
#'
#' @author Martin Haringa
#'
#' @details
#' Insurance portfolio extracts often contain multiple rows for the same policy
#' or risk because of renewals, endorsements, product changes, or short
#' administrative gaps. Before calculating portfolio in/outflow, active exposure
#' windows, or policy counts, it can be useful to reduce those rows to stable
#' coverage intervals.
#'
#' `merge_date_ranges()` merges date ranges within each `group_by` combination.
#' Ranges with a gap smaller than `merge_gap_days` are treated as one continuous
#' interval. If `aggregate_cols` is supplied, those columns are aggregated over
#' the merged interval.
#'
#' @return A `data.table` of class `"reduce"`, with attributes:
#' \itemize{
#'   \item `begin` — name of the period-start column
#'   \item `end`   — name of the period-end column
#'   \item `cols`  — grouping columns
#' }
#'
#' @examples
#' portfolio <- data.frame(
#'   policy_nr   = rep("12345", 11),
#'   productgroup= rep("fire", 11),
#'   product     = rep("contents", 11),
#'   begin_dat   = as.Date(c(16709,16740,16801,17410,17440,17805,17897,
#'                           17956,17987,18017,18262), origin="1970-01-01"),
#'   end_dat     = as.Date(c(16739,16800,16831,17439,17531,17896,17955,
#'                           17986,18016,18261,18292), origin="1970-01-01"),
#'   premium     = c(89,58,83,73,69,94,91,97,57,65,55)
#' )
#'
#' # Merge periods
#' pt1 <- merge_date_ranges(
#'   portfolio,
#'   period_start = "begin_dat",
#'   period_end = "end_dat",
#'   group_by = c("policy_nr", "productgroup", "product"),
#'   merge_gap_days = 5
#' )
#'
#' # Aggregate per period
#' summary(pt1, period = "days", policy_nr, productgroup, product)
#'
#' # Merge periods and sum premium per period
#' pt2 <- merge_date_ranges(
#'   portfolio,
#'   period_start = "begin_dat",
#'   period_end = "end_dat",
#'   group_by = c("policy_nr", "productgroup", "product"),
#'   aggregate_cols = "premium",
#'   merge_gap_days = 5
#' )
#'
#' # Create summary with aggregation per week
#' summary(pt2, period = "weeks", policy_nr, productgroup, product)
#'
#' @export
merge_date_ranges <- function(df,
                              ...,
                              period_start = NULL,
                              period_end = NULL,
                              group_by = NULL,
                              aggregate_cols = NULL,
                              aggregate_fun = "sum",
                              merge_gap_days = 5,
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

  .time_validate_data_frame(df)
  .time_validate_columns(df, c(period_start, period_end),
                         "`period_start` and `period_end`")
  .time_validate_date_interval(df, period_start, period_end)
  if (is.null(group_by) || length(group_by) == 0L) {
    stop("`group_by` must contain at least one column name.", call. = FALSE)
  }
  .time_validate_columns(df, group_by, "`group_by`")
  if (is.null(aggregate_cols)) {
    aggregate_cols <- character(0)
  }
  .time_validate_columns(df, aggregate_cols, "`aggregate_cols`")
  non_numeric <- aggregate_cols[!vapply(df[aggregate_cols], is.numeric, logical(1))]
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

  dt <- data.table::as.data.table(data.table::copy(df))
  data.table::setorderv(dt, c(group_by, period_start, period_end))
  split_dt <- split(dt, dt[, group_by, with = FALSE], drop = TRUE)

  reduced <- lapply(split_dt, function(part) {
    part <- part[order(part[[period_start]], part[[period_end]])]
    out <- vector("list", nrow(part))
    out_n <- 0L
    current <- part[1, ]
    current_start <- current[[period_start]]
    current_end <- current[[period_end]]
    current_values <- if (length(aggregate_cols) > 0L) {
      as.list(current[, aggregate_cols, with = FALSE])
    } else {
      list()
    }

    flush_current <- function() {
      row <- current[, c(group_by, period_start, period_end), with = FALSE]
      row[[period_start]] <- current_start
      row[[period_end]] <- current_end
      for (col in aggregate_cols) {
        row[[col]] <- agg_fun(current_values[[col]], na.rm = TRUE)
      }
      row
    }

    for (i in seq_len(nrow(part))) {
      if (i == 1L) next
      next_start <- part[[period_start]][i]
      next_end <- part[[period_end]][i]
      gap_days <- as.numeric(next_start - current_end) - 1

      if (gap_days < merge_gap_days) {
        current_end <- max(current_end, next_end)
        for (col in aggregate_cols) {
          current_values[[col]] <- c(current_values[[col]], part[[col]][i])
        }
      } else {
        out_n <- out_n + 1L
        out[[out_n]] <- flush_current()
        current <- part[i, ]
        current_start <- current[[period_start]]
        current_end <- current[[period_end]]
        current_values <- if (length(aggregate_cols) > 0L) {
          as.list(current[, aggregate_cols, with = FALSE])
        } else {
          list()
        }
      }
    }

    out_n <- out_n + 1L
    out[[out_n]] <- flush_current()
    data.table::rbindlist(out[seq_len(out_n)])
  })

  dt_reduce <- data.table::rbindlist(reduced)
  data.table::setorderv(dt_reduce, c(group_by, period_start, period_end))
  attr(dt_reduce, "begin") <- period_start
  attr(dt_reduce, "end") <- period_end
  attr(dt_reduce, "cols") <- group_by
  class(dt_reduce) <- c("merged_date_ranges", "reduce", class(dt_reduce))
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
    df = df,
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
  class(x) <- c("data.frame", "data.table")
  print(x)
}

#' @export
print.merged_date_ranges <- print.reduce

#' @export
as.data.frame.reduce <- function(x, ...) {
  class(x) <- c("data.frame", "data.table")
  return(as.data.frame(x))
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

  df <- object
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
