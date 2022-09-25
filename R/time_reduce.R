#' Reduce portfolio by merging redundant date ranges
#'
#' @description Transform all the date ranges together as a set to produce a
#' new set of date ranges. Ranges separated by a gap of at least `min.gapwidth`
#' days are not merged.
#'
#' @param df data.frame
#' @param begin name of column `df` with begin dates
#' @param end name of column in `df` with end dates
#' @param ... names of columns in `df` used to group date ranges by
#' @param agg_cols list with columns in `df` to aggregate by (defaults to NULL)
#' @param agg aggregation type (defaults to "sum")
#' @param min.gapwidth ranges separated by a gap of at least `min.gapwidth`
#' days are not merged. Defaults to 5.
#'
#' @import data.table
#' @importFrom lubridate %m+%
#'
#' @author Martin Haringa
#'
#' @details This function is adopted from `IRanges::reduce()`.
#'
#' @return An object of class `"reduce"`.
#' The function `summary` is used to obtain and print a summary of the results.
#' An object of class `"reduce"` is a list usually containing at least the
#' following elements:
#' \item{df}{data frame with reduced time periods}
#' \item{begin}{name of column in `df` with begin dates}
#' \item{end}{name of column in `df` with end dates}
#' \item{cols}{names of columns in `df` used to group date ranges by}
#'
#' @examples
#' portfolio <- structure(list(policy_nr = c("12345", "12345", "12345", "12345",
#' "12345", "12345", "12345", "12345", "12345", "12345", "12345"),
#' productgroup = c("fire", "fire", "fire", "fire", "fire", "fire",
#' "fire", "fire", "fire", "fire", "fire"), product = c("contents",
#' "contents", "contents", "contents", "contents", "contents", "contents",
#' "contents", "contents", "contents", "contents"),
#' begin_dat = structure(c(16709,16740, 16801, 17410, 17440, 17805, 17897,
#' 17956, 17987, 18017, 18262), class = "Date"),
#' end_dat = structure(c(16739, 16800, 16831, 17439, 17531, 17896, 17955,
#' 17986, 18016, 18261, 18292), class = "Date"),
#' premium = c(89L, 58L, 83L, 73L, 69L, 94L, 91L, 97L, 57L, 65L, 55L)),
#' row.names = c(NA, -11L), class = "data.frame")
#'
#' # Merge periods
#' pt1 <- reduce(portfolio, begin = begin_dat, end = end_dat, policy_nr,
#'     productgroup, product, min.gapwidth = 5)
#'
#' # Aggregate per period
#' summary(pt1, period = "days", policy_nr, productgroup, product)
#'
#' # Merge periods and sum premium per period
#' pt2 <- reduce(portfolio, begin = begin_dat, end = end_dat, policy_nr,
#'     productgroup, product, agg_cols = list(premium), min.gapwidth = 5)
#'
#' # Create summary with aggregation per week
#' summary(pt2, period = "weeks", policy_nr, productgroup, product)
#'
#'
#' @export
reduce <- function (df, begin, end, ..., agg_cols = NULL, agg = "sum",
                    min.gapwidth = 5){

  .begin <- deparse(substitute(begin))
  .end <- deparse(substitute(end))
  start_dt = end_dt = aggcols0 = NULL
  if (!inherits(df[[.begin]], c("Date", "POSIXt")) |
      !inherits(df[[.end]], c("Date", "POSIXt"))) {
    stop("Columns begin and end should be Date objects.\n         Use e.g. lubridate::ymd() to create Date object.",
         call. = FALSE)
  }
  if (anyNA(df[[.begin]])) {
    stop("NA values in data.table 'begin' column: '",
         .begin, "'. All rows with\n         NA values in the range columns must be removed for reduce() to work.",
         call. = FALSE)
  }
  else if (anyNA(df[[.end]])) {
    stop("NA values in data.table 'end' column: '",
         .end, "'. All rows with NA\n         values in the range columns must be removed for reduce() to work.",
         call. = FALSE)
  }
  cols0 <- vapply(substitute(list(...))[-1], deparse, FUN.VALUE = character(1))
  aggcols0 <- vapply(substitute(agg_cols)[-1], deparse, FUN.VALUE = character(1))
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
    dt <- dt[, lapply(.SD, get(agg), na.rm = TRUE), by = cols, .SDcols = aggcols0]
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
  return(dt_reduce)
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

#' Automatically create a summary for objects obtained from reduce()
#'
#' @description Takes an object produced by `reduce()`, and counts new and lost
#' customers.
#'
#' @param object reduce object produced by `reduce()`
#' @param period a character string indicating the period to aggregate on.
#' Four options are available: "quarters", "months", "weeks", and "days"
#' (the default option)
#' @param ... names of columns to aggregate counts by
#' @param name The name of the new column in the output. If omitted, it will
#' default to count.
#'
#' @import data.table
#' @importFrom lubridate days
#' @importFrom lubridate weeks
#' @importFrom lubridate %m+%
#'
#' @return data.frame
#'
#' @export
summary.reduce <- function(object, ..., period = "days", name = "count"){

  df <- object
  begin <- attr(object, "begin")
  end <- attr(object, "end")
  cols <- attr(object, "cols")

  by_begin <- begin
  by_end <- end

  if (!period %in% c("years", "year", "quarters", "quarter", "months", "month",
                     "weeks", "week", "day", "days")){
    stop("period is not valid: choose 'year', 'quarter', 'month',
         'week', or 'day'", call. = FALSE)
  }

  cols0 <- vapply(substitute(list(...))[-1], deparse, FUN.VALUE = character(1))

  if( length(cols0) > 0){
    by_begin <- c(by_begin, cols0)
    by_end <- c(by_end, cols0)
  }

  type = week = month = quarter = NULL # due to NSE notes in R CMD check

  new <- data.table::data.table(df)[, list(count = .N),
                                    by = c(by_begin)][, type := "in"]
  lost <- data.table::data.table(df)[, list(count = .N),
                                     by = c(by_end)][, type := "out"]

  if (period %in% c("days", "day")){
    new[, date := get(begin)]
    lost[, date := get(end) %m+% lubridate::days(1)]
  }

  if (period %in% c("weeks", "week")){
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

  if ( period %in% c("months", "month")){
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

  if ( period %in% c("quarters", "quarter")){
    new[, quarter := paste0(data.table::year(get(begin)), "Q",
                            data.table::quarter(get(begin)))]
    lost[, quarter := paste0(data.table::year(get(end) %m+% months(3)), "Q",
                             data.table::quarter(get(end) %m+% months(3)))]
  }

  if ( period %in% c("years", "year")){
    new[, year := data.table::year(get(begin))]
    lost[, year := data.table::year(get(end))]
  }

  new[, c(begin) := NULL]
  lost[, c(end) := NULL]

  dt <- data.table::rbindlist(list(new, lost))

  if ( length(cols0) == 0){
    dt <- dt[, .(count = sum(count)), by = c(names(dt)[ncol(dt)], "type")]
    data.table::setorderv(dt, c(names(dt)[ncol(dt)], "type", "count"),
                          c(-1,1,1))
    df <- as.data.frame(dt)
  }

  if( length(cols0) > 0){
    dt <- dt[, .(count = sum(count)),
             by = c(names(dt)[ncol(dt)], "type", cols0)]
    data.table::setcolorder(dt, c(names(dt)[1], "type", "count", cols0))
    data.table::setorderv(dt, c(names(dt)[1], cols0, "type"),
                          c(-1, rep(1, length(cols0)), 1))
    df <- as.data.frame(dt)
  }

  if( name != "count" ){
    if ( !is.character(name) ) stop ( "Column name should be a character",
                                      call. = FALSE )
    names(df)[names(df) == 'count'] <- name
  }

  return(df)
}


