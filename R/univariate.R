#' Univariate analysis for discrete risk factors
#'
#' @description Univariate analysis for discrete risk factors in an insurance portfolio. The following summary statistics are calculated:
#' \itemize{
#'  \item{frequency (i.e. number of claims / exposure)}
#'  \item{average severity (i.e. severity / number of claims)}
#'  \item{risk premium (i.e. severity / exposure)}
#'  \item{loss ratio (i.e. severity / premium)}
#'  \item{average premium (i.e. premium / exposure)}
#' }
#' If input arguments are not specified, the summary statistics related to these arguments are ignored.
#'
#' @param df data.frame with insurance portfolio
#' @param x column in \code{df} with risk factor
#' @param severity column in \code{df} with severity (default is NULL)
#' @param premium column in \code{df} with premium (default is NULL)
#' @param exposure column in \code{df} with exposure (default is NULL)
#' @param nclaims column in \code{df} with number of claims (default is NULL)
#'
#' @return A list of class \code{univ_all} with components
#' \item{df}{data frame}
#' \item{xvar}{name of column in df with risk factor}
#' \item{severity}{name of column in df with severity}
#' \item{nclaims}{name of column in df with number of claims}
#' \item{exposure}{name of column in df with exposure}
#' \item{premium}{name of column in df with premium}
#'
#' @exportClass univariate
#'
#' @importFrom data.table data.table
#'
#' @examples
#' univariate(MTPL2, x = area, severity = amount, nclaims = nclaims,
#'            exposure = exposure, premium = premium)
#'
#' # The summary statistics related to premium are not calculated
#' univariate(MTPL2, x = area, severity = amount, nclaims = nclaims, exposure = exposure)
#'
#' @export
univariate <- function(df, x, severity = NULL, nclaims = NULL, exposure = NULL, premium = NULL){

  x00 <- deparse(substitute(x))
  severity00 <- deparse(substitute(severity))
  nclaims00 <- deparse(substitute(nclaims))
  exposure00 <- deparse(substitute(exposure))
  premium00 <- deparse(substitute(premium))

  cols <- c(severity00, nclaims00, exposure00, premium00)
  cols <- cols[cols != "NULL"]

  if ( length( cols ) == 0 ) {
    stop("Define column names.")
  }

  dt <- data.table::data.table(df)[, lapply(.SD, sum, na.rm = TRUE), by = x00, .SDcols = cols]

  frequency = average_severity = risk_premium = loss_ratio = average_premium = NULL # due to NSE notes in R CMD check

  # Frequency
  if ( all(c(nclaims00, exposure00) %in% cols)  ){
    dt <- dt[, frequency := get(nclaims00) / get(exposure00)]
  }

  # Average severity
  if ( all(c(severity00, nclaims00) %in% cols) ){
    dt <- dt[, average_severity := get(severity00) / get(nclaims00)]
  }

  # Risk premium
  if ( all(c(severity00, exposure00) %in% cols) ){
    dt <- dt[, risk_premium := get(severity00) / get(exposure00)]
  }

  # Loss ratio
  if ( all(c(severity00, premium00) %in% cols) ){
    dt <- dt[, loss_ratio := get(severity00) / get(premium00)]
  }

  # Average premium
  if ( all(c(premium00, exposure00) %in% cols) ){
    dt <- dt[, average_premium := get(premium00) / get(exposure00)]
  }

  return(structure(list(df = as.data.frame(dt),
                        xvar = x00,
                        severity = severity00,
                        nclaims = nclaims00,
                        exposure = exposure00,
                        premium = premium00),
                   class = "univariate"))
}

#' @export
print.univariate <- function(x, ...) {
  print(x$df)
}

#' @export
as.data.frame.univariate <- function(x, ...) {
  df <- x$df
  return(as.data.frame(df))
}






