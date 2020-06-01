#' Split period to months
#'
#' @description The function splits rows with a time period longer than one month to multiple rows with a time period of exactly one month each.
#' Values in numeric columns (e.g. exposure or premium) are divided over the months proportionately.
#'
#' @param df data.frame
#' @param begin column in \code{df} with begin dates
#' @param end column in \code{df} with end dates
#' @param ... numeric columns in \code{df} to split
#'
#' @return data.frame with same columns as in \code{df}, and one extra column called \code{id}
#'
#' @author Martin Haringa
#'
#' @import data.table
#' @importFrom lubridate is.Date
#' @importFrom lubridate ceiling_date
#'
#' @details In insurance portfolios it is common that rows relate to periods longer than one month.
#' This is for example problematic in case exposures per month are desired.
#'
#' Since insurance premiums are constant over the months, and do not depend on the number of days per month, the function assumes that each month
#' has the same number of days (i.e. 30).
#'
#' @examples
#' library(lubridate)
#' portfolio <- data.frame(
#' begin1 = ymd(c("2014-01-01", "2014-01-01")),
#' end = ymd(c("2014-03-14", "2014-05-10")),
#' termination = ymd(c("2014-03-14", "2014-05-10")),
#' exposure = c(0.2025, 0.3583),
#' premium =  c(125, 150))
#' period_to_months(portfolio, begin1, end, premium, exposure)
#'
#' @export
period_to_months <- function (df, begin, end, ...) {
  begin00 <- deparse(substitute(begin))
  end00 <- deparse(substitute(end))
  df00 <- deparse(substitute(df))
  splitvars <- substitute(list(...))[-1]
  cols <- sapply(splitvars, deparse)
  column_names <- names(df)

  if (!lubridate::is.Date(df[[begin00]]) | !lubridate::is.Date(df[[end00]])) {
    stop("Columns begin and end should be Date objects. Use e.g. lubridate::ymd() to create Date object.")
  }

  if (length(cols) > 0 & !all(cols %in% column_names)){
    stop("Numeric column names to split not found in ", df00)
  }

  # Create look up table
  datum_begin <- seq(min(df[[begin00]]), max(df[[end00]]), by = "months")
  datum_end <- lubridate::ceiling_date(datum_begin, unit = "months") - 1
  datum_begin <- lubridate::floor_date(datum_begin, unit = "months")
  lookup <- data.table::data.table(datum_begin, datum_end)
  data.table::setnames(lookup, old = c("datum_begin", "datum_end"), new = c(begin00, end00))
  data.table::setkeyv(lookup, c(begin00, end00))

  # due to NSE notes in R CMD check
  new_end00 = start_int = end_int = end_days = begin_days = overlap_begin_end = overlap_period = overlap_total = NULL

  data.table::setDT(df)
  df[, id := .I][ # Add row id
    , new_end00 := get(end00) + 1] # Add one day such that day 31 equals 0 days in new month

  # Overlap periods
  ans <- data.table::foverlaps(df, lookup, type = "any", which = FALSE)

  # Divide periods
  if (length(cols) > 0){
    ans[, start_int := data.table::fifelse(get(begin00) < get(paste0("i.", begin00)), 0, 1)][
      , end_int := data.table::fifelse(get(end00) > get(paste0("i.", end00)), 0, 1)][
        , end_days := data.table::fifelse(end_int == 0, elapsed_days(new_end00) / 30, 0)][
          , begin_days := data.table::fifelse(start_int == 0, (30 - elapsed_days(get(paste0("i.", begin00)))) / 30, 0)][
            , overlap_begin_end := begin_days + end_days][
              , overlap_period := data.table::fifelse(overlap_begin_end == 0, 1, overlap_begin_end)][
                , overlap_total := sum(overlap_period, na.rm = TRUE), by = id]

    # Divide split columns
    for (i in 1:length(cols)) {
      ans[, `:=`(cols[i], get(cols[i]) * overlap_period / overlap_total)]
    }
  }

  ans[, c("id", column_names), with = FALSE]
}

