#' Fisher's natural breaks classification
#'
#' @description
#' Classifies a continuous numeric vector into intervals using
#' Fisher-Jenks natural breaks. Useful for choropleth mapping or
#' other applications where grouped ranges are required.
#'
#' @param x A numeric vector to be classified.
#' @param n Integer. Number of classes to generate (default = 7).
#' @param dig.lab Integer. Number of significant digits to use for interval
#'   labels (default = 2).
#' @param diglab Deprecated. Use `dig.lab` instead.
#'
#' @return A factor indicating the interval to which each element of `x`
#'   belongs.
#'
#' @details
#' The `"fisher"` style uses the algorithm proposed by Fisher (1958), commonly
#' referred to as the Fisher-Jenks algorithm. This function is a thin wrapper
#' around [classInt::classIntervals()].
#'
#' The argument `diglab` is deprecated and will be removed in a future version.
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
fisher_classify <- function(x, n = 7, dig.lab = NULL, diglab = NULL) {

  if (!is.null(diglab) && !is.null(dig.lab)) {
    stop("Use either `dig.lab` or `diglab`, not both.", call. = FALSE)
  }

  if (!is.null(diglab)) {
    lifecycle::deprecate_warn(
      when = "0.9.0",
      what = "fisher_classify(diglab = )",
      with = "fisher_classify(dig.lab = )"
    )
    dig.lab <- diglab
  }

  if (is.null(dig.lab)) {
    dig.lab <- 2
  }

  if (!is.numeric(x)) stop("`x` must be numeric")
  if (length(x) < n) stop("`x` must be longer than number of classes `n`")

  breaks <- classInt::classIntervals(
    x,
    n = n,
    style = "fisher",
    intervalClosure = "right"
  )$brks

  cut(x, breaks = breaks, include.lowest = TRUE, dig.lab = dig.lab)
}


#' @rdname fisher_classify
#' @description
#' `fisher()` is deprecated as of version 0.8.0.
#' Please use [fisher_classify()] instead.
#'
#' @export
fisher <- function(x, n = 7, diglab = 2) {
  lifecycle::deprecate_warn("0.8.0", "fisher()", "fisher_classify()")
  fisher_classify(x, n = n, diglab = diglab)
}
