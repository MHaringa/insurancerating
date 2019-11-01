#' Split period to months
#'
#' @description The function splits rows with a time period longer than one month to multiple rows with a time period of exactly one month each.
#' Values in numeric columns (e.g. exposure or premium) are divided over the months proportionately.
#'
#' @param df data.frame
#' @param begin column in \code{df} with begin dates
#' @param end column in \code{df} with end dates
#' @param ... numeric columns in df to split
#'
#' @return data.frame with same columns as in df, and one extra column called id
#'
#' @author Martin Haringa
#'
#' @importFrom data.table data.table
#' @importFrom data.table setnames
#' @importFrom data.table setkeyv
#' @import lubridate
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
#'
#' @export
period_to_months <- function(df, begin, end, ...){

  # Columns in df to split
  begin00 <- deparse(substitute(begin))
  end00 <- deparse(substitute(end))

  splitvars <- substitute(list(...))[-1]
  cols <- sapply(splitvars, deparse)

  # Vector column names
  column_names <- names(df)

  if (!is.Date(df[[begin00]]) | !is.Date(df[[end00]])) {
    stop("Columns begin and end should be Date objects. Use e.g. lubridate::ymd() to create Date object.")
  }

  # Create lookup table
  datum_begin <- seq(min(df[[begin00]]), max(df[[end00]]), by = "months")
  datum_end <- lubridate::ceiling_date(datum_begin, unit = "months") - 1
  lookup <- data.table::data.table(datum_begin, datum_end)
  data.table::setnames(lookup, old = c("datum_begin", "datum_end"), new = c(begin00, end00))
  data.table::setkeyv(lookup, c(begin00, end00))

  # Calculate number of months and days between begin and end date
  df$id <- 1:nrow(df)
  df$maanden <- lubridate::interval(df[[begin00]], df[[end00]] + 1) %/% months(1)
  df$dagen <- lubridate::interval(lubridate::floor_date(df[[end00]] + 1, unit = "month"), df[[end00]] + 1) %/% lubridate::days(1)
  df$maanden_tot <- df$maanden + df$dagen / 30

  # Join lookup table with df
  data.table::setDT(df)
  ans <- data.table::foverlaps(df, lookup, type = "any", which = FALSE)
  ans$maanden <- lubridate::interval(ans[[begin00]], ans[[end00]] + 1) %/% months(1)
  ans[, difference := ifelse(get(end00) == max(get(end00)), maanden - (sum(maanden) - maanden_tot), maanden), by = id]

  for (i in 1:length(cols)){
    ans[, cols[i] := get(cols[i]) / sum(difference) * difference, by = id]
  }

  output <- as.data.frame(ans)[, c("id", column_names)]

  return(output)
}


