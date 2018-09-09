#' Breaks for continuous variables in insurance rating (severity)
#'
#' @description The function provides an interface to finding class intervals for continuous numerical variables, for example to bin the continuous factors
#' such that categorical risk factors result which capture the effect of the covariate on the response in an accurate way,
#' while being easy to use in a generalized linear model (GLM).
#'
#' @param data data.frame of an insurance portfolio.
#' @param amount column in \code{data} with claim amount.
#' @param x column in \code{data} with continuous risk factor.
#' @param nclaims column in \code{data} with number of claims.
#' @param cp 	complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted.
#' For instance, with anova splitting, this means that the overall R-squared must increase by cp at each step.
#' The main role of this parameter is to save computing time by pruning off splits that are obviously not worthwhile.
#' Essentially,the user informs the program that any split which does not improve the fit by cp will likely be pruned off
#' by cross-validation, and that hence the program need not pursue it (cp = 0 is default).
#' @param color_splits change the color of the splits in the graph (\code{grey} is default).
#' @param color_gam change the color of the line in the graph (\code{steelblue} is default).
#' @param show_splits show splits (TRUE/FALSE).
#'
#' @details A Gamma distribution is assumed for the claim severity. The claim severity is the amount of loss associated with an average
#' insurance claim. The number of claims is included as prior weights on the contribution of the data to the log likelihood.
#' Note that a weight of 2, for example, is equivalent to having made exactly the same observation twice. Bins
#' are created where consecutive values of a continuous risk factor are grouped together. Regression trees
#' are used as a technique to perform the binning. Here regression trees from the \code{rpart} package are used.
#'
#' @return A list with components:
#' @return \item{x_clusters}{Vector with corresponding cluster for each element in x.}
#' @return \item{gam_plot}{Plot with predicted number of claims from the frequency model, binned by decision trees.}
#' @return \item{clusters}{Numerical vector with splits.}
#'
#' @export clustering_severity
#'
#' @author Martin Haringa
#'
#' @import mgcv
#' @import rpart
#' @import ggplot2
#'
#' @examples clustering_severity(MTPL, amount, age_policyholder, nclaims)
clustering_severity <- function(data, amount, x, nclaims, cp = 0, color_splits = "grey", color_gam = "steelblue", show_splits = TRUE){

  # Turn into character vector
  amount <- deparse(substitute(amount))
  x <- deparse(substitute(x))
  nclaims <- deparse(substitute(nclaims))


  df0 <- data.frame("amount" = data[[amount]], "x" = data[[x]], "nclaims" = data[[nclaims]])

  df <- df0[df0$amount > 0, ]

  # Fit GAM
  gam_x <- mgcv::gam(amount ~ s(x), data = df, family = Gamma("log"), weights = nclaims)

  # Invisible plot; interest in return value from plot function
  png("temp.xyz")
  gam0 <- plot(gam_x, se = FALSE, rug = FALSE, shift = mean(predict(gam_x)), trans = exp)
  dev.off()
  file.remove("temp.xyz")
  invisible(gam0)

  df1 <- data.frame(x = gam0[[1]]$x, y = exp(gam0[[1]]$fit + mean(predict(gam_x))))

  name <- gam0[[1]]$xlab
  elements <- gam0[[1]]$raw
  counting <- rle(sort(elements))
  counting2 <- setNames(data.frame(counting[[2]]), name)
  pred <- as.numeric(mgcv::predict.gam(gam_x, counting2, type = 'response'))
  new <- data.frame(counting2, pred, n = counting[[1]])

  tree_x <- rpart::rpart(pred ~ x, data = new, weights = n, control = rpart::rpart.control(cp = cp))
  split_x <- sort(as.numeric(tree_x$splits[,4]))
  gam_plot <- ggplot(data = df1, aes(x = x, y = y)) +
    geom_line(color = color_gam) +
    theme_bw(base_size = 12) +
    {if(show_splits) geom_vline(xintercept = split_x, color = color_splits, linetype = 2)} +
    labs(y = "Average cost of a claim", x = x)

  clusters <- c(min(counting[[2]]), unique(floor(split_x)), max(counting[[2]]))
  x_clusters <- cut(df$x, breaks = clusters, include.lowest = TRUE)

  return(list(x_clusters,
              gam_plot,
              clusters))
}
