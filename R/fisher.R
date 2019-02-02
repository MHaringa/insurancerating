#' Fisher's natural breaks classification
#'
#' @description The function provides an interface to finding class intervals for continuous numerical variables, for example for choosing colours for plotting maps.
#'
#' @param vec a continuous numerical variable
#' @param n number of classes required (n = 7 is default)
#'
#' @return Vector with clustering
#'
#' @import classInt
#'
#' @author Martin Haringa
#'
#' @details The "fisher" style uses the algorithm proposed by W. D. Fisher (1958) and discussed by Slocum et al. (2005) as the Fisher-Jenks algorithm.
#' This function is adopted from the classInt package.
#'
#' @references Bivand, R. (2018). classInt: Choose Univariate Class Intervals. R package version 0.2-3. \url{https://CRAN.R-project.org/package=classInt}
#' @references Fisher, W. D. 1958 "On grouping for maximum homogeneity", Journal of the American Statistical Association, 53, pp. 789–798.
#' doi: 10.1080/01621459.1958.10501479.
#'
#' @export fisher
fisher <- function(vec, n = 7){
  cluster <- classIntervals(vec, n = n, style = 'fisher', intervalClosure = 'right')[[2]]
  cut(vec, breaks = cluster, include.lowest = TRUE, dig.lab = 2)
}



