#' Fitting GAM for continuous variables in insurance rating (frequency)
#'
#' @description The function provides an interface to finding class intervals for continuous numerical variables, for example to bin the continuous factors
#' such that categorical risk factors result which capture the effect of the covariate on the response in an accurate way,
#' while being easy to use in a generalized linear model (GLM).
#'
#' @param data data.frame of an insurance portfolio.
#' @param nclaims column in \code{data} with number of claims.
#' @param x column in \code{data} with continuous risk factor.
#' @param exposure column in \code{data} with exposure.
#' @param accuracy_x round elements in column \code{x} to multiple of number \code{accuracy_x} (default value is 1). GAMs can be slow for fitting models with large
#' datasets. Therefore, an argument is added such that elements in the continuous risk factor are rounded and aggregated to a multiple of number \code{accuracy_x}.
#' Setting this argument equal to 1 results in a large speed enhancement. Setting this argument equal to 0 results in using the original values for the
#' continuous risk factor.
#'
#' @details A Poisson distribution is assumed for the number of claims. The logarithm of the exposure is
#' included in the model as an offset, such that the expected number of claims is proportional to exposure.
#'
#' @references Antonio, K. and Valdez, E. A. (2012). Statistical concepts of a priori and a posteriori risk classification in insurance. Advances in Statistical Analysis, 96(2):187–224.
#'
#' @return The function returns an object of class "gam".
#'
#' @export gam_frequency
#'
#' @author Martin Haringa
#'
#' @import mgcv
#' @importFrom stats poisson
#' @importFrom stats aggregate
#'
#' @examples gam_frequency(MTPL, nclaims, age_policyholder, exposure)
gam_frequency <- function(data, nclaims, x, exposure, accuracy_x = 1){

  # Turn into character vector
  nclaims <- deparse(substitute(nclaims))
  x <- deparse(substitute(x))
  exposure <- deparse(substitute(exposure))

  df <- data.frame("nclaims" = data[[nclaims]], "x" = data[[x]], "exposure" = data[[exposure]])

  if( accuracy_x < 0 ) stop('The argument "accuracy_x" should be non-negative.')
  if( accuracy_x > 0 ) {
    df$x <- round(df$x / accuracy_x) * accuracy_x
    df <- aggregate(list(nclaims = df$nclaims, exposure = df$exposure), by = list(x = df$x), FUN = sum)
  }

  if( sum(df$exposure == 0) > 0 ) stop('Exposures should be greater than zero.')

  # Fit GAM
  gam_x <- mgcv::gam(nclaims ~ s(x), data = df, family = poisson(), offset = log(exposure))

  return(gam_x)
}
