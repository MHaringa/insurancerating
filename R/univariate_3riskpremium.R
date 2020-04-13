#' Univariate risk premium (deprecated function; use 'univariate()' instead)
#' @noRd
#'
#' @description Risk premium for discrete risk factors in an insurance portfolio. For each level of the risk factor the risk premium is equal to the ratio between the severity and the exposure.
#'
#' @param df data.frame with insurance portfolio
#' @param x column in \code{df} with risk factor
#' @param severity column in \code{df} with severity (default is NULL)
#' @param nclaims column in \code{df} with number of claims
#' @param exposure column in \code{df} with exposure
#' @param premium column in \code{df} with premium (default is NULL)
#'
#' @return An list of class \code{univ_freq} with components
#' \item{df}{data frame with risk premium}
#' \item{xvar}{name of column in df with risk factor}
#' \item{severity}{name of column in df with severity}
#' \item{nclaims}{name of column in df with number of claims}
#' \item{exposure}{name of column in df with exposure}
#' \item{premium}{name of column in df with premium}
#'
#' @importFrom data.table data.table
#'
#' @examples univariate_risk_premium(MTPL2, x = area, severity = amount, exposure = exposure)
#' @exportClass univ_premium
#'
#' @author Martin Haringa
#'
univariate_risk_premium <- function(df, x, severity, exposure, nclaims = NULL, premium = NULL){

  x00 <- deparse(substitute(x))
  severity00 <- deparse(substitute(severity))
  exposure00 <- deparse(substitute(exposure))
  nclaims00 <- deparse(substitute(nclaims))
  premium00 <- deparse(substitute(premium))

  cols <- c(severity00, exposure00, nclaims00, premium00)
  cols <- cols[cols != "NULL"]

  risk_premium = NULL # due to NSE notes in R CMD check

  dt <- data.table::data.table(df)[, lapply(.SD, sum, na.rm = TRUE),
                       by = x00, .SDcols = cols][, risk_premium := get(severity00) / get(exposure00)]

  return(structure(list(df = as.data.frame(dt),
                        xvar = x00,
                        severity = severity00,
                        exposure = exposure00,
                        nclaims = nclaims00,
                        premium = premium00),
                   class = "univ_premium"))
}

#' @export
print.univ_premium <- function(x, ...) {
  print(x$df)
}

#' @export
as.data.frame.univ_premium <- function(x, ...) {
  df <- x$df
  return(as.data.frame(df))
}














