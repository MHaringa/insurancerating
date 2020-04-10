#' Univariate average premium
#' @noRd
#'
#' @description Average premium for discrete risk factors in an insurance portfolio. For each level of the risk factor the average premium is equal to the ratio between the premium and the exposure.
#'
#' @param df data.frame with insurance portfolio
#' @param x column in \code{df} with risk factor
#' @param severity column in \code{df} with severity (default is NULL)
#' @param premium column in \code{df} with premium (default is NULL)
#' @param exposure column in \code{df} with exposure
#' @param nclaims column in \code{df} with number of claims
#'
#' @return An list of class \code{univ_lossratio} with components
#' \item{df}{data frame with average premium}
#' \item{xvar}{name of column in df with risk factor}
#' \item{severity}{name of column in df with severity}
#' \item{nclaims}{name of column in df with number of claims}
#' \item{exposure}{name of column in df with exposure}
#' \item{premium}{name of column in df with premium}
#'
#' @importFrom data.table data.table
#'
#' @examples univariate_average_premium(MTPL2, x = area, premium = premium, exposure = exposure)
#' @exportClass univ_avgpremium
#'
#' @author Martin Haringa
#'
univariate_average_premium <- function(df, x, premium, exposure, nclaims = NULL, severity = NULL){

  x00 <- deparse(substitute(x))
  severity00 <- deparse(substitute(severity))
  premium00 <- deparse(substitute(premium))
  exposure00 <- deparse(substitute(exposure))
  nclaims00 <- deparse(substitute(nclaims))

  cols <- c(severity00,  premium00, exposure00, nclaims00)
  cols <- cols[cols != "NULL"]

  average_premium = NULL # due to NSE notes in R CMD check

  dt <- data.table::data.table(df)[, lapply(.SD, sum, na.rm = TRUE),
                                   by = x00, .SDcols = cols][, average_premium := get(premium00) / get(exposure00)]

  return(structure(list(df = as.data.frame(dt),
                        xvar = x00,
                        severity = severity00,
                        nclaims = nclaims00,
                        exposure = exposure00,
                        premium = premium00),
                   class = "univ_avgpremium"))
}

#' @export
print.univ_avgpremium <- function(x, ...) {
  print(x$df)
}

#' @export
as.data.frame.univ_avgpremium <- function(x, ...) {
  df <- x$df
  return(as.data.frame(df))
}









