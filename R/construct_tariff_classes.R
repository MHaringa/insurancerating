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
#' @param data data.frame of an insurance portfolio
#' @param nclaims column in \code{data} with number of claims
#' @param x column in \code{data} with continuous risk factor
#' @param exposure column in \code{data} with exposure
#' @param amount column in \code{data} with claim amount
#' @param pure_premium column in \code{data} with pure premium
#' @param model choose either 'frequency', 'severity' or 'burning' (model = 'frequency' is default). See details section.
#' @param alpha complexity parameter. The complexity parameter (alpha) is used to control the number of tariff classes. Higher values for \code{alpha}
#' render less tariff classes. (\code{alpha} = 0 is default).
#' @param niterations in case the run does not converge, it terminates after a specified number of iterations defined by niterations.
#' @param ntrees the number of trees in the population.
#' @param seed an numeric seed to initialize the random number generator (for reproducibility).
#'
#' @details The function provides an interface to finding class intervals for continuous numerical variables in the following three types of models: claim frequency,
#' claim severity or burning cost model. The 'frequency' specification uses a Poisson GAM for fitting the number of claims. The logarithm of the exposure is included
#' as an offset, sucht that the expected number of claims is proportional to the exposure. The 'severity' specification uses a lognormal GAM for fitting the average
#' cost of a claim. The average cost of a claim is defined as the ratio of the claim amount and the number of claims. The number of claims is included as a weight.
#' The 'burning' specification uses a lognormal GAM for fitting the pure premium of a claim. The pure premium is obtained by multiplying the estimated frequency and
#' the estimated severity of claims. The word burning cost is used here as equivalent of risk premium and pure premium.
#'
#' Subsequently, evolutionary trees are used as a technique to bin the resulting GAM estimates into risk homogeneous categories. This method is based on the work
#' by Henckaerts et al. (2018). See Grubinger et al. (2014) for more details on the various parameters that control aspects of the evtree fit.
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
#' @references Grubinger, T., Zeileis, A., and Pfeiffer, K.-P. (2014). evtree: Evolutionary learning of globally
#' optimal classification and regression trees in R. Journal of Statistical Software, 61(1):1–29. doi:10.18637/jss.v061.i01.
#' @references Wood, S.N. (2011). Fast stable restricted maximum likelihood and marginal likelihood estimation of semiparametric
#' generalized linear models. Journal of the Royal Statistical Society (B) 73(1):3-36. doi:10.1111/j.1467-9868.2010.00749.x.
#'
#' @return A list with components
#' \item{splits}{vector with boundaries of the constructed tariff classes}
#' \item{prediction}{data frame with the predicted claim frequency for each element of vector \code{x}}
#' \item{x}{name of variable for which tariff classes are constructed}
#' \item{tariff_classes}{values in vector \code{x} coded according to which constructed tariff class they fall}
#' \item{model}{either 'frequency' or 'severity'}
#' \item{data}{data frame with original data aggregated on the level of the variable for which tariff classes are constructed}
#'
#' @export construct_tariff_classes
#' @exportClass insurancerating
#'
#' @author Martin Haringa
#'
#' @examples construct_tariff_classes(MTPL, nclaims, age_policyholder, exposure)
construct_tariff_classes <- function (data, nclaims, x, exposure, amount = NULL, pure_premium = NULL, model = "frequency",
                                                  alpha = 0, niterations = 10000, ntrees = 200, seed = 1) {
  if (nrow(data) < 10)
    stop("At least 10 datapoints are required. The spline smoothers assume a default of 10 degrees of freedom.")

  if (!model %in% c("frequency", "severity", "burning"))
    stop("Choose correct model specification: 'frequency', 'severity' or 'burning'.")

  nclaims <- deparse(substitute(nclaims))
  x <- deparse(substitute(x))
  exposure <- deparse(substitute(exposure))
  amount <- deparse(substitute(amount))
  pure_premium <- deparse(substitute(pure_premium))

  if ( is.character(data[[nclaims]]) ) {
    stop( "nclaims should be numeric" )
  }

  if ( is.character(data[[x]]) ) {
    stop( "x should be numeric" )
  }

  if ( is.character(data[[exposure]]) ) {
    stop( "exposure should be numeric" )
  }


  if( model == "frequency" ){

    df <- tryCatch(
      {
        df <- data.frame(nclaims = data[[nclaims]],
                         x = data[[x]],
                         exposure = data[[exposure]])

        df <- aggregate(list(nclaims = df$nclaims,
                             exposure = df$exposure),
                        by = list(x = df$x),
                        FUN = sum,
                        na.rm = TRUE,
                        na.action = NULL)

        df$frequency <- df$nclaims / df$exposure

        df
      },
      error = function(e) {
        e$message <- "nclaims, x, and exposure should be specified for the frequency model."
        stop(e)
      })


    if( sum(df$exposure == 0) > 0 )
      stop("Exposures should be greater than zero.")

    # Poisson GAM
    gam_x <- mgcv::gam(nclaims ~ s(x),
                       data = df,
                       family = poisson(),
                       offset = log(exposure))


  }

  if( model == "severity" ){

    df <- tryCatch(
      {
        df <- data.frame(nclaims = data[[nclaims]],
                         x = data[[x]],
                         exposure = data[[exposure]],
                         amount = data[[amount]])

        df <- aggregate(list(nclaims = df$nclaims,
                             exposure = df$exposure,
                             amount = df$amount),
                        by = list(x = df$x),
                        FUN = sum,
                        na.rm = TRUE,
                        na.action = NULL)

        df <- subset(df, nclaims > 0 & amount > 0)

        df$avg_claimsize <- df$amount / df$nclaims

        df
      },
      error = function(e) {
        e$message <- "nclaims, x, exposure, and amount should be specified for the severity model."
        stop(e)
      })


    # lognormal
    gam_x <- mgcv::gam(log(avg_claimsize) ~ s(x),
                       data = df,
                       family = gaussian,
                       weights = nclaims)


  }

  if( model == "burning" ){

    df <- tryCatch(
      {
        df <- data.frame(x = data[[x]],
                         exposure = data[[exposure]],
                         pure_premium = data[[pure_premium]])

        df <- aggregate(list(exposure = df$exposure,
                             pure_premium = df$pure_premium),
                        by = list(x = df$x),
                        FUN = sum,
                        na.rm = TRUE,
                        na.action = NULL)

        df <- subset(df, pure_premium > 0 & exposure > 0)

        df$avg_premium <- df$pure_premium / df$exposure

        df
      },
      error = function(e) {
        e$message <- "x, exposure, and pure_premium should be specified for the burning cost model."
        stop(e)
      })


    # lognormal
    gam_x <- mgcv::gam(log(avg_premium) ~ s(x),
                       data = df,
                       family = gaussian,
                       weights = exposure)
  }

  # Create invisible plot; only interested in output
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
  counting <- sort(gam0[[1]]$raw)
  counting_name <- setNames(data.frame(counting), name)
  pred <- as.numeric(mgcv::predict.gam(gam_x,
                                       counting_name,
                                       type = "response"))
  df_new <- data.frame(counting_name, pred)
  new <- merge(df_new, df, by = "x")

  split_x <- tryCatch(
    {
      tree_x <- evtree::evtree(pred ~ x,
                               data = new,
                               # weights = weights,
                               control = evtree::evtree.control(alpha = alpha,
                                                                ntrees = ntrees,
                                                                niterations = niterations,
                                                                seed = seed))

      get_splits(tree_x)

    },
    error = function(e) {
      NULL
    })

  # Add min and max to binning
  splits <- c(min(counting), unique(floor(split_x)), max(counting))
  cuts <- cut(data[[x]], breaks = splits, include.lowest = TRUE)
  return(structure(list(splits = splits,
                        prediction = out,
                        x = x,
                        tariff_classes = cuts,
                        model = model,
                        data = df),
                   class = "insurancerating"))
}
