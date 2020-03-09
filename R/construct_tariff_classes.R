#' Get splits from partykit object
#' @param x A party object.
#'
get_splits <- function(x) {

  lrp <- utils::getFromNamespace(".list.rules.party", "partykit")
  splits_list <- lrp(x)
  last_line <- unname(splits_list[length(splits_list)])

  # Remove punctuation marks
  splits_vector <- regmatches(last_line, gregexpr("[[:digit:]]+", last_line))

  splits <- as.numeric(unlist(splits_vector))
  return(splits)
}

#' Construct insurance tariff classes
#'
#' @description The function provides an interface to finding class intervals for continuous numerical variables. The goal is to bin the continuous factors
#' such that categorical risk factors result which capture the effect of the covariate on the response in an accurate way,
#' while being easy to use in a generalized linear model (GLM).
#'
#' @param object fitgam object produced by fit_gam
#' @param alpha complexity parameter. The complexity parameter (alpha) is used to control the number of tariff classes. Higher values for \code{alpha}
#' render less tariff classes. (\code{alpha} = 0 is default).
#' @param niterations in case the run does not converge, it terminates after a specified number of iterations defined by niterations.
#' @param ntrees the number of trees in the population.
#' @param seed an numeric seed to initialize the random number generator (for reproducibility).
#'
#' @details The function provides an interface to finding class intervals for continuous numerical variables in the following three types of models: claim frequency,
#' claim severity or burning cost model. Evolutionary trees are used as a technique to bin the resulting GAM estimates into risk homogeneous categories. This method is based on the work
#' by Henckaerts et al. (2018). See Grubinger et al. (2014) for more details on the various parameters that control aspects of the evtree fit.
#'
#' @import evtree
#'
#' @references Henckaerts, R., Antonio, K., Clijsters, M. and Verbelen, R. (2018). A data driven binning strategy for the construction of insurance tariff classes.
#' Scandinavian Actuarial Journal, 2018:8, 681-705. doi:10.1080/03461238.2018.1429300.
#' @references Antonio, K. and Valdez, E. A. (2012). Statistical concepts of a priori and a posteriori risk classification in insurance.
#' Advances in Statistical Analysis, 96(2):187–224. doi:10.1007/s10182-011-0152-7.
#' @references Grubinger, T., Zeileis, A., and Pfeiffer, K.-P. (2014). evtree: Evolutionary learning of globally
#' optimal classification and regression trees in R. Journal of Statistical Software, 61(1):1–29. doi:10.18637/jss.v061.i01.
#' @references Wood, S.N. (2011). Fast stable restricted maximum likelihood and marginal likelihood estimation of semiparametric
#' generalized linear models. Journal of the Royal Statistical Society (B) 73(1):3-36. doi:10.1111/j.1467-9868.2010.00749.x.
#'
#' @return A list with components
#' \item{prediction}{data frame with the predicted claim frequency for each element of vector \code{x}}
#' \item{x}{name of variable for which tariff classes are constructed}
#' \item{model}{either 'frequency' or 'severity'}
#' \item{data}{data frame with original data aggregated on the level of the variable for which tariff classes are constructed}
#' \item{splits}{vector with boundaries of the constructed tariff classes}
#' \item{tariff_classes}{values in vector \code{x} coded according to which constructed tariff class they fall}
#'
#' @export construct_tariff_classes
#' @exportClass constructtariffclasses
#'
#' @author Martin Haringa
#'
#' \dontrun{
#' library(dplyr)
#' fit_gam(MTPL, nclaims = nclaims, x = age_policyholder, exposure = exposure) %>%
#'    construct_tariff_classes(.)
#' }
construct_tariff_classes <- function (object, alpha = 0, niterations = 10000, ntrees = 200, seed = 1) {

  new <- object[[4]]
  counting <- new$x

  split_x <- tryCatch(
    {
      tree_x <- evtree::evtree(pred ~ x,
                               data = new,
                               control = evtree::evtree.control(alpha = alpha,
                                                                ntrees = ntrees,
                                                                niterations = niterations,
                                                                seed = seed))

      split_obtained <- get_splits(tree_x)
      unique(floor(split_obtained))

    },
    error = function(e) {
      NULL
    })

  # Add min and max to binning
  splits <- c(min(counting), split_x, max(counting))
  cuts <- cut(counting, breaks = splits, include.lowest = TRUE)

  return(structure(list(prediction = object[[1]],
                        x = object[[2]],
                        model = object[[3]],
                        data = object[[4]],
                        splits = splits,
                        tariff_classes = cuts),
                   class = "constructtariffclasses"))
}
