#' Construct insurance tariff classes
#'
#' @description The function provides an interface to finding class intervals for continuous numerical variables. The goal is to bin the continuous factors
#' such that categorical risk factors result which capture the effect of the covariate on the response in an accurate way,
#' while being easy to use in a generalized linear model (GLM).
#'
#' @param data data.frame of an insurance portfolio
#' @param nclaims column in \code{data} with number of claims
#' @param x column in \code{data} with continuous risk factor
#' @param exposure column in \code{data} with exposure
#' @param amount column in \code{data} with claim amount
#' @param model choose either 'frequency' (using Poisson GAM) or 'severity' (using lognormal GAM). (model = 'frequency' is default).
#' @param approximation if TRUE, elements in \code{nclaims} and \code{exposure} are aggregated to the level of unique elements in \code{x}.
#' Approximation should be used for large insurance portfolios to avoid excessive computation times (default is TRUE).
#' @param alpha complexity parameter. The complexity parameter (alpha) is used to control the number of tariff classes. Higher values for \code{alpha}
#' render less tariff classes. (\code{alpha} = 0 is default).
#' @param niterations in case the run does not converge, it terminates after a specified number of iterations defined by niterations.
#' @param ntrees the number of trees in the population.
#' @param seed an numeric seed to initialize the random number generator (for reproducibility).
#'
#' @details In the frequency model a Poisson GAM is used for fitting the number of claims. The logarithm of the exposure is included as an offset,
#' such that the expected number of claims is proportional to the exposure. In the severity model a lognormal GAM is used for fitting the average cost of a claim.
#' The average cost of a claim is defined as the ratio of the claim amount and the number of claims.  Subsequently, evolutionary trees are used as a
#' technique to bin the resulting GAM estimates into risk homogeneous categories. This method is based on the work by Antonio and Valdez (2012).
#'
#' @import mgcv
#' @import evtree
#' @import ggplot2
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom graphics plot
#' @importFrom stats poisson
#' @importFrom stats gaussian
#' @importFrom stats aggregate
#' @importFrom stats predict
#' @importFrom stats setNames
#'
#' @references Henckaerts, R., Antonio, K., Clijsters, M. and Verbelen, R. (2018). A data driven binning strategy for the construction of insurance tariff classes.
#' Scandinavian Actuarial Journal, 2018:8, 681-705. doi:10.1080/03461238.2018.1429300.
#' @references Antonio, K. and Valdez, E. A. (2012). Statistical concepts of a priori and a posteriori risk classification in insurance.
#' Advances in Statistical Analysis, 96(2):187–224. doi:10.1007/s10182-011-0152-7.
#' @references Therneau, T. and Atkinson, B. (2018). rpart: Recursive Partitioning and Regression Trees.
#' R package version 4.1-13. \url{https://CRAN.R-project.org/package=rpart}
#' @references Wood, S.N. (2011). Fast stable restricted maximum likelihood and marginal likelihood estimation of semiparametric
#' generalized linear models. Journal of the Royal Statistical Society (B) 73(1):3-36. doi:10.1111/j.1467-9868.2010.00749.x.
#'
#' @return A list with components
#' \item{splits}{vector with boundaries of the constructed tariff classes}
#' \item{prediction}{data frame with the predicted claim frequency for each element of vector \code{x}}
#' \item{x}{name of variable for which tariff classes are constructed}
#' \item{tariff_classes}{values in vector \code{x} coded according to which constructed tariff class they fall}
#' \item{model}{either 'frequency' or 'severity'}
#'
#' @export construct_tariff_classes
#' @exportClass insurancerating
#'
#' @author Martin Haringa
#'
#' @examples construct_tariff_classes(MTPL, nclaims, age_policyholder, exposure)
construct_tariff_classes <- function (data, nclaims, x, exposure, amount = NULL, model = "frequency", approximation = TRUE,
                                                  alpha = 0, niterations = 10000, ntrees = 200, seed = 1) {
  if (nrow(data) < 10)
    stop("The spline smoothers assume a default of 10 degrees of freedom. So at least 10 datapoints are required.")

  if (!model %in% c("frequency", "severity"))
    stop("Choose correct model specification: 'frequency' or 'severity'")

  nclaims <- deparse(substitute(nclaims))
  x <- deparse(substitute(x))
  exposure <- deparse(substitute(exposure))

  if( model == "frequency" ){
    df <- data.frame(nclaims = data[[nclaims]],
                     x = data[[x]],
                     exposure = data[[exposure]])

    if( isTRUE(approximation) ) {
      df <- aggregate(list(nclaims = df$nclaims,
                           exposure = df$exposure),
                      by = list(x = df$x),
                      FUN = sum,
                      na.rm = TRUE,
                      na.action = NULL)
    }
    if( sum(df$exposure == 0) > 0 )
      stop("Exposures should be greater than zero.")
    gam_x <- mgcv::gam(nclaims ~ s(x), data = df, family = poisson(),
                       offset = log(exposure))
  }

  if(model == "severity"){
    amount = deparse(substitute(amount))
    df <- data.frame(nclaims = data[[nclaims]],
                     x = data[[x]],
                     exposure = data[[exposure]],
                     amount = data[[amount]])
    if (isTRUE(approximation)) {
      df <- aggregate(list(nclaims = df$nclaims,
                           exposure = df$exposure,
                           amount = df$amount),
                      by = list(x = df$x),
                      FUN = sum,
                      na.rm = TRUE,
                      na.action = NULL)
      df$avg_claimsize <- df$amount / df$nclaims
    }
    gam_x <- mgcv::gam(log(avg_claimsize) ~ s(x), data = df, family = gaussian,
                       weights = nclaims)
  }

  png("temp.xyz")
  gam0 <- plot(gam_x,
               se = TRUE,
               rug = FALSE,
               shift = mean(predict(gam_x)),
               trans = exp)
  dev.off()
  file.remove("temp.xyz")
  invisible(gam0)

  out <- data.frame(x = gam0[[1]]$x,
                    predicted = exp(gam0[[1]]$fit + mean(predict(gam_x))),
                    lwr_95 = exp(gam0[[1]]$fit - 1.96 * gam0[[1]]$se + mean(predict(gam_x))),
                    upr_95 = exp(gam0[[1]]$fit + 1.96 * gam0[[1]]$se + mean(predict(gam_x))))

  name <- gam0[[1]]$xlab

  counting <- rle(sort(gam0[[1]]$raw))

  counting_name <- setNames(data.frame(counting[[2]]), name)

  pred <- as.numeric(mgcv::predict.gam(gam_x,
                                       counting_name,
                                       type = "response"))

  new <- data.frame(counting_name, pred, n = counting[[1]])

  lrp <- utils::getFromNamespace(".list.rules.party", "partykit")

  split_x <- tryCatch(
    {
      tree_x <- evtree::evtree(pred ~ x,
                               data = new,
                               weights = n,
                               control = evtree::evtree.control(alpha = alpha,
                                                                ntrees = ntrees,
                                                                niterations = niterations,
                                                                seed = seed))
      list_splits <- lrp(tree_x)
      all_splits <- unname(list_splits[length(list_splits)])
      splits_vector <- regmatches(all_splits, gregexpr("[[:digit:]]+", all_splits))
      as.numeric(unlist(splits_vector))
    },
    error = function(e) {
      NULL
    })

  splits <- c(min(counting[[2]]), unique(floor(split_x)), max(counting[[2]]))
  cuts <- cut(data[[x]], breaks = splits, include.lowest = TRUE)
  return(structure(list(splits = splits,
                        prediction = out,
                        x = x,
                        tariff_classes = cuts,
                        model = model),
                   class = "insurancerating"))
}
