#' Univariate average claim severity (deprecated function; use 'univariate()' instead)
#' @noRd
#'
#' @description Average claim frequency for discrete risk factors in an insurance portfolio. For each level of the risk factor the average claim severity is equal to the ratio between the severity and the number of claims.
#'
#' @param df data.frame with insurance portfolio
#' @param x column in \code{df} with risk factor
#' @param severity column in \code{df} with severity (default is NULL)
#' @param nclaims column in \code{df} with number of claims
#' @param exposure column in \code{df} with exposure
#' @param premium column in \code{df} with premium (default is NULL)
#'
#' @return An list of class \code{univ_freq} with components
#' \item{df}{data frame with claim frequency}
#' \item{xvar}{name of column in df with risk factor}
#' \item{severity}{name of column in df with severity}
#' \item{nclaims}{name of column in df with number of claims}
#' \item{exposure}{name of column in df with exposure}
#' \item{premium}{name of column in df with premium}
#'
#' @importFrom data.table data.table
#'
#' @examples univariate_average_severity(MTPL2, x = area, severity = amount,
#'                                          nclaims = nclaims, premium = premium)
#' @exportClass univ_avgsev
#'
#' @author Martin Haringa
#'
univariate_average_severity <- function(df, x, severity, nclaims, exposure = NULL, premium = NULL){

  x00 <- deparse(substitute(x))
  severity00 <- deparse(substitute(severity))
  nclaims00 <- deparse(substitute(nclaims))
  exposure00 <- deparse(substitute(exposure))
  premium00 <- deparse(substitute(premium))

  cols <- c(severity00, nclaims00, exposure00, premium00)
  cols <- cols[cols != "NULL"]

  average_severity = NULL # due to NSE notes in R CMD check

  dt <- data.table::data.table(df)[, lapply(.SD, sum, na.rm = TRUE),
                       by = x00, .SDcols = cols][, average_severity := get(severity00) / get(nclaims00)]

  return(structure(list(df = as.data.frame(dt),
                        xvar = x00,
                        severity = severity00,
                        nclaims = nclaims00,
                        exposure = exposure00,
                        premium = premium00),
                   class = "univ_avgsev"))
}

#' @export
print.univ_avgsev <- function(x, ...) {
  print(x$df)
}

#' @export
as.data.frame.univ_avgsev <- function(x, ...) {
  df <- x$df
  return(as.data.frame(df))
}





