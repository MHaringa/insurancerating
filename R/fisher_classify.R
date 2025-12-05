#' Fisher's natural breaks classification
#'
#' @description
#' Classifies a continuous numeric vector into intervals using
#' Fisher-Jenks natural breaks. Useful for choropleth mapping or
#' other applications where grouped ranges are required.
#'
#' @param vec A numeric vector to be classified.
#' @param n Integer. Number of classes to generate (default = 7).
#' @param diglab Integer. Number of significant digits to use for labels
#'   (default = 2).
#'
#' @return A factor with class intervals as levels.
#'
#' @details
#' The "fisher" style uses the algorithm proposed by W. D. Fisher (1958)
#' and discussed by Slocum et al. (2005) as the Fisher-Jenks algorithm.
#' This function is a wrapper around the \pkg{classInt} package.
#'
#' @references
#' Bivand, R. (2018). *classInt: Choose Univariate Class Intervals*.
#' R package version 0.2-3. <https://CRAN.R-project.org/package=classInt>
#'
#' Fisher, W. D. (1958). *On grouping for maximum homogeneity*.
#' Journal of the American Statistical Association, 53, pp. 789–798.
#' doi:10.1080/01621459.1958.10501479
#'
#' @author Martin Haringa
#'
#' @importFrom classInt classIntervals
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#' fisher_classify(x, n = 5)
#'
#' @export
fisher_classify <- function(vec, n = 7, diglab = 2) {
  if (!is.numeric(vec)) stop("`vec` must be numeric")
  if (length(vec) < n) stop("`vec` must be longer than number of classes `n`")

  breaks <- classInt::classIntervals(vec, n = n, style = "fisher",
                                     intervalClosure = "right")$brks
  cut(vec, breaks = breaks, include.lowest = TRUE, dig.lab = diglab)
}


#' @rdname fisher_classify
#' @export
fisher <- function(vec, n = 7, diglab = 2) {
  lifecycle::deprecate_warn("0.7.6", "fisher()", "fisher_classify()")
  fisher_classify(vec, n = n, diglab = diglab)
}
