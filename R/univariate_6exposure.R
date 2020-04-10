#' Univariate exposure
#' @noRd
#'
#' @description Exposure for discrete risk factors in an insurance portfolio.
#'
#' @param df data.frame with insurance portfolio
#' @param x column in \code{df} with risk factor
#' @param exposure column in \code{df} with exposure
#'
#' @return An list of class \code{univ_lossratio} with components
#' \item{df}{data frame with average premium}
#' \item{xvar}{name of column in df with risk factor}
#' \item{exposure}{name of column in df with exposure}
#'
#' @importFrom data.table data.table
#'
#' @examples univariate_exposure(MTPL2, area, exposure)
#' @exportClass univ_exposure
#'
#' @author Martin Haringa
#'
univariate_exposure <- function(df, x, exposure = exposure){

  x00 <- deparse(substitute(x))
  exposure00 <- deparse(substitute(exposure))

  dt <- data.table::data.table(df)[, lapply(.SD, sum, na.rm = TRUE),
                       by = x00, .SDcols = exposure00]

  return(structure(list(df = as.data.frame(dt),
                        xvar = x00,
                        exposure = exposure00),
                   class = "univ_exposure"))
}

#' @export
print.univ_exposure <- function(x, ...) {
  print(x$df)
}

#' @export
as.data.frame.univ_exposure <- function(x, ...) {
  df <- x$df
  return(as.data.frame(df))
}









