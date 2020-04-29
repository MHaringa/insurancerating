#' Univariate analysis for discrete risk factors (deprecated function; use 'univariate()' instead)
#'
#'
#' @description Univariate analysis for discrete risk factors in an insurance portfolio. The following summary statistics are calculated:
#' \itemize{
#'  \item{frequency (i.e. number of claims / exposure)}
#'  \item{average severity (i.e. severity / number of claims)}
#'  \item{risk premium (i.e. severity / exposure)}
#'  \item{loss ratio (i.e. severity / premium)}
#'  \item{average premium (i.e. premium / exposure)}
#' }
#'
#' @param df data.frame with insurance portfolio
#' @param x column in \code{df} with risk factor
#' @param severity column in \code{df} with severity (default is NULL)
#' @param premium column in \code{df} with premium (default is NULL)
#' @param exposure column in \code{df} with exposure (default is NULL)
#' @param nclaims column in \code{df} with number of claims (default is NULL)
#'
#' @return An list of class \code{univ_all} with components
#' \item{df}{data frame}
#' \item{xvar}{name of column in df with risk factor}
#' \item{severity}{name of column in df with severity}
#' \item{nclaims}{name of column in df with number of claims}
#' \item{exposure}{name of column in df with exposure}
#' \item{premium}{name of column in df with premium}
#'
#' @importFrom data.table data.table
#'
#'
#' @export
univariate_all <- function(df, x, severity = NULL, nclaims = NULL, exposure = NULL, premium = NULL){

  .Defunct("univariate")
  univariate(df = df, x = x, severity = severity, nclaims = nclaims, exposure = exposure, premium = premium)
}








