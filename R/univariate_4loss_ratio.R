#' Univariate loss ratio (deprecated function; use 'univariate()' instead)
#' @noRd
#'
#' @description Average loss ratio for discrete risk factors in an insurance portfolio. For each level of the risk factor the loss ratio is equal to the ratio between the severity and the earned premium.
#'
#' @param df data.frame with insurance portfolio
#' @param x column in \code{df} with risk factor
#' @param severity column in \code{df} with severity (default is NULL)
#' @param premium column in \code{df} with premium (default is NULL)
#' @param exposure column in \code{df} with exposure
#' @param nclaims column in \code{df} with number of claims
#'
#' @return An list of class \code{univ_lossratio} with components
#' \item{df}{data frame with average loss ratio}
#' \item{xvar}{name of column in df with risk factor}
#' \item{severity}{name of column in df with severity}
#' \item{nclaims}{name of column in df with number of claims}
#' \item{exposure}{name of column in df with exposure}
#' \item{premium}{name of column in df with premium}
#'
#' @importFrom data.table data.table
#'
#' @examples univariate_loss_ratio(MTPL2, x = area, severity = amount, premium = premium)
#' @exportClass univ_lossratio
#'
#' @author Martin Haringa
#'
univariate_loss_ratio <- function(df, x, severity, premium, exposure = NULL, nclaims = NULL){

  x00 <- deparse(substitute(x))
  severity00 <- deparse(substitute(severity))
  premium00 <- deparse(substitute(premium))
  exposure00 <- deparse(substitute(exposure))
  nclaims00 <- deparse(substitute(nclaims))

  cols <- c(severity00,  premium00, exposure00, nclaims00)
  cols <- cols[cols != "NULL"]

  loss_ratio = NULL # due to NSE notes in R CMD check

  dt <- data.table::data.table(df)[, lapply(.SD, sum, na.rm = TRUE),
                       by = x00, .SDcols = cols][, loss_ratio := get(severity00) / get(premium00)]

  return(structure(list(df = as.data.frame(dt),
                        xvar = x00,
                        severity = severity00,
                        nclaims = nclaims00,
                        exposure = exposure00,
                        premium = premium00),
                   class = "univ_lossratio"))
}

#' @export
print.univ_lossratio <- function(x, ...) {
  print(x$df)
}

#' @export
as.data.frame.univ_lossratio <- function(x, ...) {
  df <- x$df
  return(as.data.frame(df))
}















