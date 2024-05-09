#' Find active rows per date
#'
#' @description Fast overlap joins. Usually, `df` is a very large data.table
#' (e.g. insurance portfolio) with small interval ranges, and `dates` is much
#' smaller with (e.g.) claim dates.
#'
#' @param df data.frame with portfolio (df should include time period)
#' @param dates data.frame with dates to join
#' @param df_begin column name with begin dates of time period in `df`
#' @param df_end column name with end dates of time period in `df`
#' @param dates_date column name with dates in `dates`
#' @param ... additional column names in `dates` to join by
#' @param nomatch When a row (with interval say, `[a,b]`) in x has no match in
#' y, nomatch=NA means NA is returned for y's non-by.y columns for that row of
#' x. nomatch=NULL (default) means no rows will be returned for that row of x.
#' @param mult When multiple rows in y match to the row in x, `mult` controls
#' which values are returned - "all" (default), "first" or "last".
#'
#' @author Martin Haringa
#'
#' @import data.table
#' @importFrom lubridate is.Date
#'
#' @return returned class is equal to class of `df`
#' @examples
#' library(lubridate)
#' portfolio <- data.frame(
#' begin1 = ymd(c("2014-01-01", "2014-01-01")),
#' end = ymd(c("2014-03-14", "2014-05-10")),
#' termination = ymd(c("2014-03-14", "2014-05-10")),
#' exposure = c(0.2025, 0.3583),
#' premium =  c(125, 150),
#' car_type = c("BMW", "TESLA"))
#'
#' ## Find active rows on different dates
#' dates0 <- data.frame(active_date = seq(ymd("2014-01-01"), ymd("2014-05-01"),
#' by = "months"))
#' rows_per_date(portfolio, dates0, df_begin = begin1, df_end = end,
#' dates_date = active_date)
#'
#' ## With extra identifiers (merge claim date with time interval in portfolio)
#' claim_dates <- data.frame(claim_date = ymd("2014-01-01"),
#' car_type = c("BMW", "VOLVO"))
#'
#' ### Only rows are returned that can be matched
#' rows_per_date(portfolio, claim_dates, df_begin = begin1,
#'    df_end = end, dates_date = claim_date, car_type)
#'
#' ### When row cannot be matched, NA is returned for that row
#' rows_per_date(portfolio, claim_dates, df_begin = begin1,
#'    df_end = end, dates_date = claim_date, car_type, nomatch = NA)
#'
#' @export
rows_per_date <- function(df, dates, df_begin, df_end, dates_date, ...,
                          nomatch = NULL, mult = "all") {

  cols0 <- vapply(substitute(list(...))[-1], deparse, FUN.VALUE = character(1))

  begin00 <- deparse(substitute(df_begin))
  end00 <-  deparse(substitute(df_end))
  reeks00 <- deparse(substitute(dates_date))
  class00 <- class(df)

  if (!lubridate::is.Date(df[[begin00]]) || !lubridate::is.Date(df[[end00]])) {
    stop("Columns df_begin and df_end should be Date objects.
         Use e.g. lubridate::ymd() to create Date object.",
         call. = FALSE)
  }

  if (!lubridate::is.Date(dates[[reeks00]])) {
    stop("Column dates_date must be a Date object.
         Use e.g. lubridate::ymd() to create Date object.",
         call. = FALSE)
  }

  cols1 <- as.character(c(reeks00, cols0))
  dates_nm <- setdiff(names(dates), cols1)
  lookup <- data.table::data.table(dates)[, c(cols1, dates_nm), with = FALSE]
  data.table::setnames(lookup, old = c(reeks00), new = c(begin00))
  lookup2 <- lookup[, c(end00) := get(begin00)][, index_dates := .I]
  data.table::setkeyv(lookup2, as.character(c(cols0, begin00, end00)))
  data.table::setDT(df)
  df1 <- df[, index_df := .I]
  ans <- data.table::foverlaps(df1, lookup2, type = "any", which = FALSE,
                               nomatch = nomatch, mult = mult)
  ans[, c(begin00) := NULL]
  data.table::setnames(ans,
                       old = c(end00, paste0("i.", begin00),
                               paste0("i.", end00)),
                       new = c(reeks00, begin00, end00))
  ans <- ans[order(get(reeks00))]
  data.table::setcolorder(ans, c(names(df1), setdiff(names(df1), names(ans))))
  class(ans) <- class00
  return(ans)
}
