#' Reduce portfolio by merging redundant date ranges
#'
#' @description Transform all the date ranges together as a set to produce a
#' new set of date ranges. Ranges separated by a gap of at least `min.gapwidth`
#' days are not merged.
#'
#' @param df A `data.frame` or `data.table`.
#' @param begin Name of column in `df` with begin dates.
#' @param end Name of column in `df` with end dates.
#' @param ... Names of columns in `df` used to group date ranges by.
#' @param agg_cols List of columns in `df` to aggregate (default = NULL).
#' @param agg Aggregation function as character string (default = `"sum"`).
#' @param min.gapwidth Ranges separated by at least `min.gapwidth` days are not
#' merged (default = 5).
#'
#' @importFrom data.table setDT
#' @importFrom data.table setkeyv
#' @importFrom data.table shift
#'
#' @author Martin Haringa
#'
#' @details This function is inspired by `IRanges::reduce()`, adapted for
#' insurance portfolios.
#'
#' @return A `data.table` of class `"reduce"`, with attributes:
#' \itemize{
#'   \item `begin` — name of the begin-date column
#'   \item `end`   — name of the end-date column
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
#' pt1 <- merge_date_ranges(portfolio, begin = begin_dat, end = end_dat,
#'     policy_nr, productgroup, product, min.gapwidth = 5)
#'
#' # Aggregate per period
#' summary(pt1, period = "days", policy_nr, productgroup, product)
#'
#' # Merge periods and sum premium per period
#' pt2 <- merge_date_ranges(portfolio, begin = begin_dat, end = end_dat,
#'     policy_nr, productgroup, product, agg_cols = list(premium),
#'     min.gapwidth = 5)
#'
#' # Create summary with aggregation per week
#' summary(pt2, period = "weeks", policy_nr, productgroup, product)
#'
#' @export
merge_date_ranges <- function(df, begin, end, ..., agg_cols = NULL,
                              agg = "sum", min.gapwidth = 5) {

  .begin <- deparse(substitute(begin))
  .end <- deparse(substitute(end))
  start_dt <- end_dt <- aggcols0 <- NULL
  if (!inherits(df[[.begin]], c("Date", "POSIXt")) ||
      !inherits(df[[.end]], c("Date", "POSIXt"))) {
    stop("Columns begin and end should be Date objects. Use e.g.
         lubridate::ymd() to create Date object.",
         call. = FALSE)
  }
  if (anyNA(df[[.begin]])) {
    stop("NA values in data.table 'begin' column: '",
         .begin, "'. All rows with NA values in the range columns must be
         removed for reduce() to work.",
         call. = FALSE)
  } else if (anyNA(df[[.end]])) {
    stop("NA values in data.table 'end' column: '",
         .end, "'. All rows with NA values in the range columns must be removed
         for reduce() to work.",
         call. = FALSE)
  }
  cols0 <- vapply(substitute(list(...))[-1], deparse, FUN.VALUE = character(1))
  aggcols0 <- vapply(substitute(agg_cols)[-1], deparse,
                     FUN.VALUE = character(1))
  if (length(cols0) == 0) {
    stop("define columns to group date ranges by", call. = FALSE)
  }
  cols <- c(cols0, .begin, .end)

  dt <- data.table::setDT(df)
  data.table::setkeyv(dt, cols)
  dt[, `:=`(c(.begin), get(.begin) - min.gapwidth)]
  dt[, `:=`(c(.end), get(.end) + min.gapwidth)]
  if (length(aggcols0) == 0) {
    dt <- unique(dt, by = cols)
    dt_reduce <- dt[, .(start_dt = get(.begin), end_dt = get(.end),
                        index = c(0, cumsum(as.numeric(
                          data.table::shift(get(.begin), 1, type = "lead")) >
                            cummax(as.numeric(get(.end))))[-.N])),
                    keyby = c(cols0)]
    dt_reduce <- dt_reduce[, .(start_dt = min(start_dt),
                               end_dt = max(end_dt)), by = c(cols0, "index")]
  }
  if (length(aggcols0) > 0) {
    dt <- dt[, lapply(.SD, get(agg), na.rm = TRUE), by = cols, .SDcols =
               aggcols0]
    dt_reduce <- dt[, .(start_dt = get(.begin), end_dt = get(.end),
                        index = c(0, cumsum(as.numeric(
                          data.table::shift(get(.begin), 1, type = "lead")) >
                            cummax(as.numeric(get(.end))))[-.N])),
                    keyby = c(cols0)]
    dt_reduce <- cbind(dt_reduce, dt[, aggcols0, with = FALSE])
    dt_reduce <- dt_reduce[, c(end_dt = max(end_dt), start_dt = min(start_dt),
                               lapply(.SD, get(agg), na.rm = TRUE)),
                           by = c(cols0, "index"),
                           .SDcols = aggcols0]
  }
  data.table::setnames(dt_reduce,
                       old = c("start_dt", "end_dt"),
                       new = c(.begin, .end))
  dt_reduce[, `:=`(c(.begin), get(.begin) + min.gapwidth)]
  dt_reduce <- dt_reduce[, `:=`(c(.end), get(.end) -
                                  min.gapwidth)]
  attr(dt_reduce, "begin") <- .begin
  attr(dt_reduce, "end") <- .end
  attr(dt_reduce, "cols") <- cols0
  class(dt_reduce) <- append("reduce", class(dt_reduce))
  dt_reduce
}

#' @importFrom lifecycle deprecate_warn
#' @rdname merge_date_ranges
#' @export
reduce <- function(df, begin, end, ..., agg_cols = NULL, agg = "sum",
                   min.gapwidth = 5) {

  lifecycle::deprecate_warn("0.8.0", "reduce()", "merge_date_ranges()")

  # capture column names as character
  begin_sym <- substitute(begin)
  end_sym   <- substitute(end)
  dots_syms <- as.list(substitute(list(...))[-1])
  agg_syms  <- as.list(substitute(agg_cols)[-1])

  do.call(
    merge_date_ranges,
    c(list(df = df,
           begin = begin_sym,
           end = end_sym,
           agg_cols = agg_syms,
           agg = agg,
           min.gapwidth = min.gapwidth),
      dots_syms)
  )
}


#' @export
print.reduce <- function(x, ...) {
  class(x) <- c("data.frame", "data.table")
  print(x)
}

#' @export
as.data.frame.reduce <- function(x, ...) {
  class(x) <- c("data.frame", "data.table")
  return(as.data.frame(x))
}

##' Summarize reduce objects
#'
#' @description
#' Method for `summary()` applied to objects of class `"reduce"`, as produced
#' by [merge_date_ranges()]. It counts how many customers (or policies) are new
#' or lost within a given period, optionally grouped by other columns.
#'
#' @param object An object of class `"reduce"`, created by [merge_date_ranges()].
#' @param period Character string indicating the aggregation period. Options are
#' `"quarters"`, `"months"`, `"weeks"`, or `"days"` (default = `"days"`).
#' @param ... Names of columns in `object` to aggregate counts by.
#' @param name Character string: name of the new count column in the output.
#' Defaults to `"count"`.
#'
#' @import data.table
#' @importFrom lubridate days
#' @importFrom lubridate weeks
#' @importFrom lubridate %m+%
#'
#' @return A `data.frame` containing aggregated counts of new (`"in"`) and lost
#' (`"out"`) records, per chosen period (and per grouping variables if supplied).
#'
#' @seealso [merge_date_ranges()]
#'
#' @examples
#' \dontrun{
#' pt <- merge_date_ranges(portfolio, begin = begin_dat, end = end_dat,
#'                         policy_nr, productgroup, product)
#' summary(pt, period = "months", policy_nr, productgroup)
#' }
#'
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
    new[, date := get(begin)]
    lost[, date := get(end) %m+% lubridate::days(1)]
  }

  if (period %in% c("weeks", "week")) {
    new[, week := get(begin)]
    new[, week := paste0(data.table::year(week), "W",
                         ifelse(nchar(data.table::week(week)) == 1,
                                paste0("0", data.table::week(week)),
                                data.table::week(week)))]
    lost[, week := get(end) %m+% lubridate::weeks(1)]
    lost[, week := paste0(data.table::year(week), "W",
                          ifelse(nchar(data.table::week(week)) == 1,
                                 paste0("0", data.table::week(week)),
                                 data.table::week(week)))]
  }

  if (period %in% c("months", "month")) {
    new[, month := get(begin)]
    new[, month := paste0(data.table::year(month), "M",
                          ifelse(nchar(data.table::month(month)) == 1,
                                 paste0("0", data.table::month(month)),
                                 data.table::month(month)))]
    lost[, month := get(end) %m+% months(1)]
    lost[, month := paste0(data.table::year(month), "M",
                           ifelse(nchar(data.table::month(month)) == 1,
                                  paste0("0", data.table::month(month)),
                                  data.table::month(month)))]
  }

  if (period %in% c("quarters", "quarter")) {
    new[, quarter := paste0(data.table::year(get(begin)), "Q",
                            data.table::quarter(get(begin)))]
    lost[, quarter := paste0(data.table::year(get(end) %m+% months(3)), "Q",
                             data.table::quarter(get(end) %m+% months(3)))]
  }

  if (period %in% c("years", "year")) {
    new[, year := data.table::year(get(begin))]
    lost[, year := data.table::year(get(end))]
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
