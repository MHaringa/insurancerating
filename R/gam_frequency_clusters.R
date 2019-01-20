#' Breaks for continuous variables in insurance rating
#'
#' @description The function provides an interface to finding class intervals for continuous numerical variables, for example to bin the continuous factors
#' such that categorical risk factors result which capture the effect of the covariate on the response in an accurate way,
#' while being easy to use in a generalized linear model (GLM).
#'
#' @param gam_x A fitted gam object as produced by \code{gam_frequency()}.
#' @param cp 	complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted.
#' For instance, with anova splitting, this means that the overall R-squared must increase by \code{cp} at each step.
#' The main role of this parameter is to save computing time by pruning off splits that are obviously not worthwhile.
#' Essentially,the user informs the program that any split which does not improve the fit by \code{cp} will likely be pruned off
#' by cross-validation, and that hence the program need not pursue it (\code{cp} = 0 is default).
#'
#' @references Antonio, K. and Valdez, E. A. (2012). Statistical concepts of a priori and a posteriori risk classification in insurance. Advances in Statistical Analysis, 96(2):187–224.
#'
#' @details Bins are created where consecutive values of a continuous risk factor are grouped together. Regression trees
#' are used as a technique to perform the binning. Here regression trees from the \code{rpart} package are used.
#'
#' @return Numerical vector with splits.
#'
#' @export gam_frequency_clusters
#'
#' @author Martin Haringa
#'
#' @import mgcv
#' @import rpart
#' @import ggplot2
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom graphics plot
#' @importFrom stats poisson
#' @importFrom stats predict
#' @importFrom stats setNames
#'
#' @examples gam_fit <- gam_frequency(MTPL, nclaims, age_policyholder, exposure)
#' @examples gam_frequency_clusters(gam_fit)
gam_frequency_clusters <- function(gam_x, cp = 0){

  tryCatch(predict(gam_x), error = function(e) {
    message('First argument should be output obtained from "gam_frequency" function')
  })

  # Invisible plot; interest in return value from plot function
  png("temp.xyz")
  gam0 <- plot(gam_x, se = FALSE, rug = FALSE, shift = mean(predict(gam_x)), trans = exp)
  dev.off()
  file.remove("temp.xyz")
  invisible(gam0)

  if( !is.numeric(gam0[[1]]$x) ) stop('first argument should be output obtained from gam_frequency function')

  name <- gam0[[1]]$xlab
  elements <- gam0[[1]]$raw
  counting <- rle(sort(elements))
  counting2 <- setNames(data.frame(counting[[2]]), name)
  pred <- as.numeric(mgcv::predict.gam(gam_x, counting2, type = 'response'))
  new <- data.frame(counting2, pred, n = counting[[1]])

  tree_x <- rpart::rpart(pred ~ x, data = new, weights = n, control = rpart::rpart.control(cp = cp))
  split_x <- sort(as.numeric(tree_x$splits[,4]))
  clusters <- c(min(counting[[2]]), unique(floor(split_x)), max(counting[[2]]))
  return(clusters)
}
