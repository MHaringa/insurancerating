#' Find active rows per date
#'
#' @description Find active rows per date.
#'
#' @param df data.frame
#' @param dates vector of dates
#' @param begin column name in `df` with begin dates
#' @param end column name in `df` with end dates
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
#' premium =  c(125, 150))
#'
#' active_date <- seq(ymd("2014-01-01"), ymd("2014-05-01"), by = "months")
#' rows_per_date(portfolio, active_date, begin = begin1, end = end)
#'
#' @export
rows_per_date <- function(df, dates, begin, end){

  begin00 <- deparse(substitute(begin))
  end00 <-  deparse(substitute(end))
  reeks00 <- deparse(substitute(dates))
  class00 <- class(df)

  if (!lubridate::is.Date(df[[begin00]]) | !lubridate::is.Date(df[[end00]])) {
    stop("Columns begin and end should be Date objects. Use e.g. lubridate::ymd() to create Date object.",
         call. = FALSE)
  }

  if (!lubridate::is.Date(dates)){
    stop("dates must be a vector of dates. Use e.g. lubridate::ymd() to create Date object.",
         call. = FALSE)
  }

  lookup <- data.table::data.table(datum_begin = dates, datum_end = dates)
  data.table::setnames(lookup, old = c("datum_begin", "datum_end"), new = c(begin00, end00))
  data.table::setkeyv(lookup, c(begin00, end00))

  data.table::setDT(df)
  ans <- data.table::foverlaps(df, lookup, type = "any", which = FALSE, nomatch=NULL)
  ans[, c(begin00) := NULL]
  data.table::setnames(ans, c(end00, paste0("i.", begin00), paste0("i.", end00)), new = c(reeks00, begin00, end00))
  ans <- ans[order(get(reeks00))]
  class(ans) <- class00
  return(ans)
}
