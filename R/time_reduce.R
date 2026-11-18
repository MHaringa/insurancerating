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
.time_can_hash_groups <- function(data, group_by) {
  if (data.table::getNumericRounding() != 0L || anyDuplicated(group_by)) {
    return(FALSE)
  }
  # Unknown classes may define equality differently in vctrs and data.table.
  supported_classes <- list("factor", c("ordered", "factor"), "Date",
                            c("IDate", "Date"), c("POSIXct", "POSIXt"))
  all(vapply(group_by, function(col) {
    x <- data[[col]]
    if (!is.null(dim(x))) return(FALSE)
    if (!is.object(x)) {
      return(typeof(x) %in% c("logical", "integer", "double", "character"))
    }
    any(vapply(supported_classes, identical, logical(1), y = class(x)))
  }, logical(1)))
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
#' @param allow_na_in Optional character vector naming columns in `group_by`
#'   where missing values may form a group. The default, `NULL`, rejects
#'   missing values in every grouping column.
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
#' For an optional grouping attribute, use e.g. `allow_na_in = "coverage"`
#' to retain `NA` as a distinct value. Two missing values then match if the
#' other grouping keys match; a missing value never matches a known coverage.
#' The usual date-gap rule still applies. Keep policy or risk identifiers
#' out of `allow_na_in` when missing identifiers could pool unrelated risks.
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
#' Internally, interval construction uses a
#' compact [data.table::data.table()] with group identifiers, dates and row
#' positions. Only requested aggregation columns are added to that working
#' table; grouping attributes are retrieved for the output rows afterwards.
#' For ordinary grouping columns and standard numeric sums (or no aggregation),
#' groups are identified by hashing and only the output is sorted by grouping
#' columns and dates. Other column classes, custom aggregation functions and
#' nonzero [data.table::setNumericRounding()] settings retain the ranked-group
#' route, preserving their existing grouping and processing behaviour.
#' The wide input portfolio is not copied or sorted in place, and the supplied
#' object is not modified by reference. Standard `sum` aggregation of ordinary
#' numeric columns uses an optimized grouped calculation. Other aggregation
#' functions and numeric classes retain the general aggregation route.
#' Large inputs and outputs still require sufficient memory.
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
  fast_sum <- identical(agg_fun, base::sum) && all(vapply(
    aggregate_cols, function(col) !is.object(data[[col]]), logical(1)
  ))

  output_cols <- unique(c(group_by, period_start, period_end, aggregate_cols))
  if (nrow(data) == 0L) {
    dt_reduce <- stats::setNames(lapply(output_cols, function(col) data[[col]][integer()]),
                                 output_cols)
  } else {
    hash_groups <- (!length(aggregate_cols) || fast_sum) &&
      .time_can_hash_groups(data, group_by)
    if (hash_groups) {
      # A shallow key frame avoids sorting/copying the wide input columns.
      keys <- structure(lapply(group_by, function(col) data[[col]]),
                        names = group_by, class = "data.frame",
                        row.names = c(NA_integer_, -nrow(data)))
      group_id <- as.integer(vctrs::vec_group_id(keys))
      rm(keys)
    } else {
      group_id <- data.table::frankv(data, cols = group_by, ties.method = "dense",
                                    na.last = FALSE)
    }
    work <- data.table::data.table(
      group_id = group_id,
      row_id = seq_len(nrow(data)),
      start = data[[period_start]], end = data[[period_end]]
    )
    rm(group_id)
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

    # Gather keys only for output rows. Hash IDs follow first appearance;
    # sorting the reduced result restores the same key/date order as ranks.
    dt_reduce <- c(
      lapply(group_by, function(col) data[[col]][reduced[["row_id"]]]),
      lapply(c("start", "end", value_names), function(col) reduced[[col]])
    )
    names(dt_reduce) <- c(group_by, period_start, period_end, aggregate_cols)
    if (hash_groups) {
      data.table::setDT(dt_reduce)
      data.table::setorderv(dt_reduce, unique(c(group_by, period_start, period_end)))
    }
  }
  data.table::setDF(dt_reduce)
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
