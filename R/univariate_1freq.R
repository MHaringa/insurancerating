#' Univariate claim frequency
#' @noRd
#'
#' @description Claim frequency for discrete risk factors in an insurance portfolio. For each level of the risk factor the claim frequency is equal to the ratio between the number of claims and the exposure.
#'
#' @param df data.frame with insurance portfolio
#' @param x column in \code{df} with risk factor
#' @param nclaims column in \code{df} with number of claims
#' @param exposure column in \code{df} with exposure
#' @param severity column in \code{df} with severity (default is NULL)
#' @param premium column in \code{df} with premium (default is NULL)
#'
#' @return An list of class \code{univ_freq} with components
#' \item{df}{data frame with claim frequency}
#' \item{xvar}{name of column in df with risk factor}
#' \item{nclaims}{name of column in df with number of claims}
#' \item{exposure}{name of column in df with exposure}
#' \item{severity}{name of column in df with severity}
#' \item{premium}{name of column in df with premium}
#'
#' @importFrom data.table data.table
#'
#' @examples univariate_frequency(MTPL2, x = area, nclaims = nclaims, exposure = exposure)
#' @exportClass univ_freq
#'
#' @author Martin Haringa
#'
univariate_frequency <- function(df, x, nclaims, exposure, severity = NULL, premium = NULL){

  x00 <- deparse(substitute(x))
  nclaims00 <- deparse(substitute(nclaims))
  exposure00 <- deparse(substitute(exposure))
  severity00 <- deparse(substitute(severity))
  premium00 <- deparse(substitute(premium))

  cols <- c(nclaims00, exposure00, severity00, premium00)
  cols <- cols[cols != "NULL"]

  frequency = NULL # due to NSE notes in R CMD check

  dt <- data.table::data.table(df)[, lapply(.SD, sum, na.rm = TRUE),
                       by = x00, .SDcols = cols][, frequency := get(nclaims00) / get(exposure00)]

  return(structure(list(df = as.data.frame(dt),
                        xvar = x00,
                        nclaims = nclaims00,
                        exposure = exposure00,
                        severity = severity00,
                        premium = premium00),
                   class = "univ_freq"))
}

#' @export
print.univ_freq <- function(x, ...) {
  print(x$df)
}

#' @export
as.data.frame.univ_freq <- function(x, ...) {
  df <- x$df
  return(as.data.frame(df))
}

