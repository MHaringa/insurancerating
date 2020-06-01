#' Reduce portfolio by merging redundant date ranges
#'
#' @description Transform all the date ranges together as a set to produce a new set of date ranges. Ranges separated by a gap of at least \code{min.gapwidth} days are not merged.
#'
#' @param df data.frame
#' @param begin name of column \code{df} with begin dates
#' @param end name of column in \code{df} with end dates
#' @param ... names of columns in \code{df} used to group date ranges by
#' @param agg_cols list with columns in \code{df} to aggregate by (defaults to NULL)
#' @param agg aggregation type (defaults to "sum")
#' @param min.gapwidth ranges separated by a gap of at least \code{min.gapwidth} days are not merged. Defaults to 5.
#'
#' @import data.table
#' @importFrom dplyr lead
#' @importFrom lubridate %m+%
#' @importFrom lubridate is.Date
#'
#' @details This function is adopted from \code{IRanges::reduce()}.
#'
#' @return An object of class \code{"reduce"}.
#' The function \code{summary} is used to obtain and print a summary of the results.
#' An object of class \code{"reduce"} is a list usually containing at least the following elements:
#' \item{df}{data frame with reduced time periods}
#' \item{begin}{name of column in \code{df} with begin dates}
#' \item{end}{name of column in \code{df} with end dates}
#' \item{cols}{names of columns in \code{df} used to group date ranges by}
#'
#' @examples
#' portfolio <- structure(list(policy_nr = c("12345", "12345", "12345", "12345",
#' "12345", "12345", "12345", "12345", "12345", "12345", "12345"),
#' productgroup = c("fire", "fire", "fire", "fire", "fire", "fire",
#' "fire", "fire", "fire", "fire", "fire"), product = c("contents",
#' "contents", "contents", "contents", "contents", "contents", "contents",
#' "contents", "contents", "contents", "contents"), begin_dat = structure(c(16709,
#' 16740, 16801, 17410, 17440, 17805, 17897, 17956, 17987, 18017,
#' 18262), class = "Date"), end_dat = structure(c(16739, 16800,
#' 16831, 17439, 17531, 17896, 17955, 17986, 18016, 18261, 18292),
#' class = "Date"), premium = c(89L, 58L, 83L, 73L, 69L, 94L,
#' 91L, 97L, 57L, 65L, 55L)), row.names = c(NA, -11L), class = "data.frame")
#'
#' # Merge periods
#' reduce(portfolio, begin = begin_dat, end = end_dat, policy_nr,
#'     productgroup, product, min.gapwidth = 5)
#'
#' # Merge periods and sum premium per period
#' reduce(portfolio, begin = begin_dat, end = end_dat, policy_nr,
#'     productgroup, product, agg_cols = list(premium), min.gapwidth = 5)
#'
#' @export
reduce <- function(df, begin, end, ..., agg_cols = NULL, agg = "sum", min.gapwidth = 5) {

  .begin <- deparse(substitute(begin))
  .end <- deparse(substitute(end))

  start_dt = end_dt = aggcols0 = NULL # due to NSE notes in R CMD check

  if (!lubridate::is.Date(df[[.begin]]) | !lubridate::is.Date(df[[.end]])) {
    stop("Columns begin and end should be Date objects. Use e.g. lubridate::ymd() to create Date object.")
  }

  if (anyNA(df[[.begin]])) {
    stop("NA values in data.table 'begin' column: '", .begin, "'. All rows with NA values in the range columns must be removed for reduce() to work.")
  }
  else if (anyNA(df[[.end]])) {
    stop("NA values in data.table 'end' column: '", .end, "'. All rows with NA values in the range columns must be removed for reduce() to work.")
  }

  splitvars <- substitute(list(...))[-1]
  cols0 <- sapply(splitvars, deparse)

  aggvars <- substitute(agg_cols)[-1]
  aggcols0 <- sapply(aggvars, deparse)

  if ( length(cols0) == 0 ){
    stop("define columns to group date ranges by")
  }

  cols <- c(cols0, .begin, .end)

  # Set keys (also orders)
  dt <- data.table::setDT(df)
  data.table::setkeyv(dt, cols)

  # Add gapwidth
  dt[, c(.begin) := get(.begin) - min.gapwidth]
  dt[, c(.end) := get(.end) + min.gapwidth]

  # Get rid of overlapping transactions
  if (length(aggcols0) == 0){
    dt_reduce <- dt[,.(start_dt = get(.begin),
                       end_dt = get(.end),
                       index = c(0, cumsum(as.numeric(dplyr::lead(get(.begin))) > cummax(as.numeric(get(.end))))[-.N])),
                    keyby = c(cols0)]
    dt_reduce <- dt_reduce[,.(start_dt = min(start_dt), end_dt = max(end_dt)), by = c(cols0, "index")]
  }

  if ( length(aggcols0) > 0 ){
    dt_reduce <- dt[, .(start_dt = get(.begin),
                        end_dt = get(.end),
                        index = c(0, cumsum(as.numeric(dplyr::lead(get(.begin))) > cummax(as.numeric(get(.end))))[-.N])),
                    keyby = c(cols0)]
    dt_reduce <- cbind(dt_reduce, dt[, aggcols0, with = FALSE]) # ..aggcols0
    dt_reduce <- dt_reduce[, c(end_dt = max(end_dt), start_dt = min(start_dt), lapply(.SD, get(agg))), by = c(cols0, "index"), .SDcols = aggcols0]
  }

  data.table::setnames(dt_reduce, old = c("start_dt", "end_dt"), new = c(.begin, .end))

  # Reverse gapwidth
  dt_reduce[, c(.begin) := get(.begin) + min.gapwidth]
  dt_reduce <- dt_reduce[, c(.end) := get(.end) - min.gapwidth]

  return(structure(list(df = as.data.frame(dt_reduce),
                        begin = .begin,
                        end = .end,
                        cols = cols0),
                   class = "reduce"))
}

#' @export
print.reduce <- function(x, ...) {
  print(x$df)
}

#' @export
as.data.frame.reduce <- function(x, ...) {
  df <- x$df
  return(as.data.frame(df))
}

#' Automatically create a summary for objects obtained from reduce()
#'
#' @description Takes an object produced by \code{reduce()}, and counts new and lost customers.
#'
#' @param object reduce object produced by \code{reduce()}
#' @param period a character string indicating the period to aggregate on. Four options are available: "quarters", "months", "weeks", and "days" (the default option)
#' @param ... names of columns to aggregate counts by
#'
#' @import data.table
#' @importFrom lubridate days
#' @importFrom lubridate weeks
#' @importFrom lubridate %m+%
#'
#' @return data.frame
#'
#' @examples
#' portfolio <- structure(list(policy_nr = c("12345", "12345", "12345", "12345",
#' "12345", "12345", "12345", "12345", "12345", "12345", "12345"),
#' productgroup = c("fire", "fire", "fire", "fire", "fire", "fire",
#' "fire", "fire", "fire", "fire", "fire"), product = c("contents",
#' "contents", "contents", "contents", "contents", "contents", "contents",
#' "contents", "contents", "contents", "contents"), begin_dat = structure(c(16709,
#' 16740, 16801, 17410, 17440, 17805, 17897, 17956, 17987, 18017,
#' 18262), class = "Date"), end_dat = structure(c(16739, 16800,
#' 16831, 17439, 17531, 17896, 17955, 17986, 18016, 18261, 18292),
#' class = "Date"), premium = c(89L, 58L, 83L, 73L, 69L, 94L,
#' 91L, 97L, 57L, 65L, 55L)), row.names = c(NA, -11L), class = "data.frame")
#'
#' pt1 <- reduce(portfolio, begin = begin_dat, end = end_dat, policy_nr,
#'     productgroup, product, min.gapwidth = 5)
#' summary(pt1, period = "days", policy_nr, productgroup, product)
#'
#' pt2 <- reduce(portfolio, begin = begin_dat, end = end_dat, policy_nr,
#'     productgroup, product, agg_cols = list(premium), min.gapwidth = 5)
#' summary(pt2, period = "weeks", policy_nr, productgroup, product)
#'
#' @export
summary.reduce <- function(object, period = "days", ...){

  if (!inherits(object, "reduce")) {
    stop("summary.reduce requires a reduce object, use object = object")
  }

  df <- object$df
  begin <- object$begin
  end <- object$end
  cols <- object$cols

  by_begin <- begin
  by_end <- end

  if (!period %in% c("quarters", "quarter", "months", "month", "weeks", "week", "day", "days")){
    stop("period is not valid: choose 'quarter', 'month', 'week', or 'day'")
  }

  splitvars <- substitute(list(...))[-1]
  cols0 <- sapply(splitvars, deparse)

  if( length(cols0) > 0){
    by_begin <- c(by_begin, cols0)
    by_end <- c(by_end, cols0)
  }

  type = week = month = quarter = NULL # due to NSE notes in R CMD check

  new <- data.table::data.table(df)[, list(count = .N), by = c(by_begin)][, type := "in"]
  lost <- data.table::data.table(df)[, list(count = .N), by = c(by_end)][, type := "out"]

  if (period %in% c("days", "day")){
    new[, date := get(begin)]
    lost[, date := get(end) %m+% lubridate::days(1)]
  }

  if (period %in% c("weeks", "week")){
    new[, week := get(begin)]
    new[, week := paste0(data.table::year(week), "W",
                         ifelse(nchar(data.table::week(week)) == 1, paste0("0", data.table::week(week)), data.table::week(week)))]
    lost[, week := get(end) %m+% lubridate::weeks(1)]
    lost[, week := paste0(data.table::year(week), "W",
                          ifelse(nchar(data.table::week(week)) == 1, paste0("0", data.table::week(week)), data.table::week(week)))]
  }

  if ( period %in% c("months", "month")){
    new[, month := get(begin)]
    new[, month := paste0(data.table::year(month), "M",
                          ifelse(nchar(data.table::month(month)) == 1, paste0("0", data.table::month(month)), data.table::month(month)))]
    lost[, month := get(end) %m+% months(1)]
    lost[, month := paste0(data.table::year(month), "M",
                           ifelse(nchar(data.table::month(month)) == 1, paste0("0", data.table::month(month)), data.table::month(month)))]
  }

  if ( period %in% c("quarters", "quarter")){
    new[, quarter := paste0(data.table::year(get(begin)), "Q", data.table::quarter(get(begin)))]
    lost[, quarter := paste0(data.table::year(get(end) %m+% months(3)), "Q", data.table::quarter(get(end) %m+% months(3)))]
  }

  new[, c(begin) := NULL]
  lost[, c(end) := NULL]

  dt <- data.table::rbindlist(list(new, lost))

  if ( length(cols0) == 0){
    data.table::setorderv(dt, c(names(dt)[ncol(dt)], "type", "count"), c(-1,1,1))
    df <- as.data.frame(dt)[, 3:1]
  }

  if( length(cols0) > 0){
    data.table::setcolorder(dt, c(names(dt)[ncol(dt)], "type", "count", cols0))
    data.table::setorderv(dt, c(names(dt)[1], cols0, "type"), c(-1, rep(1, length(cols0)), 1))
    df <- as.data.frame(dt)
  }

  return(df)
}






