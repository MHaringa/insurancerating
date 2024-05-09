#' Generalized additive model
#'
#' @description Fits a generalized additive model (GAM) to continuous risk
#' factors in one of the following three types of models: the number of reported
#' claims (claim frequency), the severity of reported claims (claim severity)
#' or the burning cost (i.e. risk premium or pure premium).
#'
#' @param data data.frame of an insurance portfolio
#' @param nclaims column in `data` with number of claims
#' @param x column in `data` with continuous risk factor
#' @param exposure column in `data` with exposure
#' @param amount column in `data` with claim amount
#' @param pure_premium column in `data` with pure premium
#' @param model choose either 'frequency', 'severity' or 'burning'
#' (model = 'frequency' is default). See details section.
#' @param round_x round elements in column `x` to multiple of `round_x`. This
#' gives a speed enhancement for data containing many levels for `x`.
#'
#' @details The 'frequency' specification uses a Poisson GAM for fitting the
#' number of claims. The logarithm of the exposure is included as an offset,
#' such that the expected number of claims is proportional to the exposure.
#'
#' The 'severity' specification uses a lognormal GAM for fitting the average
#' cost of a claim. The average cost of a claim is defined as the ratio of the
#' claim amount and the number of claims. The number of claims is included as a
#' weight.
#'
#' The 'burning' specification uses a lognormal GAM for fitting the pure
#' premium of a claim. The pure premium is obtained by multiplying the estimated
#' frequency and the estimated severity of claims. The word burning cost is
#' used here as equivalent of risk premium and pure premium. Note that the
#' functionality for fitting a GAM for pure premium is still experimental (in
#' the early stages of development).
#'
#' @importFrom mgcv gam
#' @importFrom mgcv predict.gam
#' @import ggplot2
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom graphics plot
#' @importFrom stats poisson
#' @importFrom stats gaussian
#' @importFrom stats aggregate
#' @importFrom stats predict
#' @importFrom stats setNames
#' @importFrom stats qnorm
#'
#' @references Antonio, K. and Valdez, E. A. (2012). Statistical concepts of a
#' priori and a posteriori risk classification in insurance. Advances in
#' Statistical Analysis, 96(2):187–224. doi:10.1007/s10182-011-0152-7.
#' @references Grubinger, T., Zeileis, A., and Pfeiffer, K.-P. (2014). evtree:
#' Evolutionary learning of globally optimal classification and regression trees
#' in R. Journal of Statistical Software, 61(1):1–29. doi:10.18637/jss.v061.i01.
#' @references Henckaerts, R., Antonio, K., Clijsters, M. and Verbelen, R.
#' (2018). A data driven binning strategy for the construction of insurance
#' tariff classes. Scandinavian Actuarial Journal, 2018:8, 681-705.
#' doi:10.1080/03461238.2018.1429300.
#' @references Wood, S.N. (2011). Fast stable restricted maximum likelihood and
#' marginal likelihood estimation of semiparametric generalized linear models.
#' Journal of the Royal Statistical Society (B) 73(1):3-36.
#' doi:10.1111/j.1467-9868.2010.00749.x.
#'
#' @return A list with components
#' \item{prediction}{data frame with predicted values}
#' \item{x}{name of continuous risk factor}
#' \item{model}{either 'frequency', 'severity' or 'burning'}
#' \item{data}{data frame with predicted values and observed values}
#' \item{x_obs}{observations for continuous risk factor}
#'
#' @author Martin Haringa
#'
#' @examples fit_gam(MTPL, nclaims = nclaims, x = age_policyholder,
#' exposure = exposure)
#'
#' @export
fit_gam <- function(data, nclaims, x, exposure, amount = NULL,
                    pure_premium = NULL, model = "frequency", round_x = NULL) {

  if (nrow(data) < 10)
    stop("At least 10 datapoints are required. The spline smoothers assume a
         default of 10 degrees of freedom.", call. = FALSE)

  if (!model %in% c("frequency", "severity", "burning"))
    stop("Choose correct model specification: 'frequency', 'severity' or
         'burning'.", call. = FALSE)

  nclaims <- deparse(substitute(nclaims))
  x <- deparse(substitute(x))
  exposure <- deparse(substitute(exposure))
  amount <- deparse(substitute(amount))
  pure_premium <- deparse(substitute(pure_premium))

  if (!is.numeric(data[[x]])) {
    stop("x should be numeric", call. = FALSE)
  }

  if (!is.numeric(data[[exposure]])) {
    stop("exposure should be numeric", call. = FALSE)
  }


  if (model == "frequency") {

    df <- tryCatch({
      df <- data.frame(nclaims = data[[nclaims]],
                       x = data[[x]],
                       exposure = data[[exposure]])

      if (is.numeric(round_x)) {
        df$x <- round(df$x / round_x) * round_x
      }

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
      e$message <- cat("nclaims, x, and exposure must be specified
        for the frequency model.")
      stop(e)
    })

    if (sum(df$exposure == 0) > 0) {
      stop("Exposures should be greater than zero.", call. = FALSE)
    }

    # Poisson GAM
    gam_x <- mgcv::gam(nclaims ~ s(x),
                       data = df,
                       family = poisson(),
                       offset = log(exposure))


  }

  if (model == "severity") {

    df <- tryCatch({
      df <- data.frame(nclaims = data[[nclaims]],
                       x = data[[x]],
                       exposure = data[[exposure]],
                       amount = data[[amount]])

      if (is.numeric(round_x)) df$x <- round(df$x / round_x) * round_x

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
      e$message <- cat("nclaims, x, exposure, and amount should be specified
        for the severity model.")
      stop(e)
    })

    # lognormal
    gam_x <- mgcv::gam(log(avg_claimsize) ~ s(x),
                       data = df,
                       family = gaussian,
                       weights = nclaims)
  }

  if (model == "burning") {

    df <- tryCatch({
        df <- data.frame(x = data[[x]],
                         exposure = data[[exposure]],
                         pure_premium = data[[pure_premium]])

        df <- aggregate(list(exposure = df$exposure,
                             pure_premium = df$pure_premium,
                             weighted_premium = df$exposure * df$pure_premium),
                        by = list(x = df$x),
                        FUN = sum,
                        na.rm = TRUE,
                        na.action = NULL)

        df <- subset(df, pure_premium > 0 & exposure > 0)
        # Solve issue 2: df$avg_premium <- df$pure_premium / df$exposure
        df$avg_premium <- df$weighted_premium / df$exposure
        df
      },
      error = function(e) {
        e$message <- cat("x, exposure, and pure_premium should be specified for
        the burning cost model.")
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
                    lwr_95 = exp(gam0[[1]]$fit - qnorm(.975) * gam0[[1]]$se +
                                   mean(predict(gam_x))),
                    upr_95 = exp(gam0[[1]]$fit + qnorm(.975) * gam0[[1]]$se +
                                   mean(predict(gam_x))))

  name <- gam0[[1]]$xlab
  counting <- sort(gam0[[1]]$raw)
  counting_name <- setNames(data.frame(counting), name)
  pred <- as.numeric(mgcv::predict.gam(gam_x,
                                       counting_name,
                                       type = "response"))
  df_new <- data.frame(counting_name, pred)
  new <- merge(df_new, df, by = "x")

  return(structure(list(prediction = out,
                        x = x,
                        model = model,
                        data = new,
                        x_obs = data[[x]]),
                   class = "fitgam"))
}

#' @export
print.fitgam <- function(x, ...) {
  print(x$prediction)
}

#' @export
as.data.frame.fitgam <- function(x, ...) {
  prediction <- x$prediction
  as.data.frame(prediction)
}

#' Automatically create a ggplot for objects obtained from fit_gam()
#'
#' @description Takes an object produced by `fit_gam()`, and plots the fitted
#' GAM.
#'
#' @param object fitgam object produced by `fit_gam()`
#' @param conf_int determines whether 95 percent confidence intervals will be
#' plotted. The default is `conf_int = FALSE`.
#' @param color_gam a color can be specified either by name (e.g.: "red") or by
#' hexadecimal code (e.g. : "#FF1234") (default is "steelblue")
#' @param x_stepsize set step size for labels horizontal axis
#' @param show_observations add observed frequency/severity points for each
#' level of the variable for which tariff classes are constructed
#' @param size_points size for points (1 is default)
#' @param color_points change the color of the points in the graph ("black" is
#' default)
#' @param rotate_labels rotate x-labels 45 degrees (this might be helpful for
#' overlapping x-labels)
#' @param remove_outliers do not show observations above this number in the
#' plot. This might be helpful for outliers.
#' @param ... other plotting parameters to affect the plot
#'
#' @return a ggplot object
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#' fit_gam(MTPL, nclaims = nclaims, x = age_policyholder,
#'         exposure = exposure) %>%
#'    autoplot(., show_observations = TRUE)
#' }
#'
#' @author Martin Haringa
#'
#' @export
autoplot.fitgam <- function(object, conf_int = FALSE, color_gam = "steelblue",
                            show_observations = FALSE, x_stepsize = NULL,
                            size_points = 1, color_points = "black",
                            rotate_labels = FALSE,
                            remove_outliers = NULL, ...) {

  prediction <- object[[1]]
  xlab <- object[[2]]
  ylab <- object[[3]]
  points <- object[[4]]

  if (isTRUE(conf_int) && sum(prediction$upr_95 > 1e9) > 0) {
    message("The confidence bounds are too large to show.")
  }

  if (is.numeric(remove_outliers) && isTRUE(show_observations)) {
    if (ylab == "frequency") {
      points <- points[points$frequency < remove_outliers, ]
    }

    if (ylab == "severity") {
      points <- points[points$avg_claimsize < remove_outliers, ]
    }

    if (ylab == "burning") {
      points <- points[points$avg_premium < remove_outliers, ]
    }
  }

  ggplot(data = prediction, aes(x = x, y = predicted)) +
    geom_line(color = color_gam) +
    theme_bw(base_size = 12) + {
      if (isTRUE(conf_int) && sum(prediction$upr_95 > 1e9) == 0) {
        geom_ribbon(aes(ymin = lwr_95, ymax = upr_95), alpha = 0.12)
      }
    } + {
      if (is.numeric(x_stepsize)) {
        scale_x_continuous(breaks = seq(floor(min(prediction$x)),
                                        ceiling(max(prediction$x)),
                                        by = x_stepsize))
      }
    } + {
      if (isTRUE(show_observations) && ylab == "frequency") {
        geom_point(data = points, aes(x = x, y = frequency), size = size_points,
                   color = color_points)
      }
    } + {
      if (isTRUE(show_observations) && ylab == "severity") {
        geom_point(data = points, aes(x = x, y = avg_claimsize),
                   size = size_points, color = color_points)
      }
    } + {
      if (isTRUE(show_observations) && ylab == "burning") {
        geom_point(data = points, aes(x = x, y = avg_premium),
                   size = size_points,
                   color = color_points)
      }
    } + {
      if (ylab == "severity") scale_y_continuous(labels = scales::comma)
    } + {
      if (isTRUE(rotate_labels)) {
        theme(axis.text.x = element_text(angle = 45,
                                         vjust = 1,
                                         hjust = 1))
      }
    } +
    labs(y = paste0("Predicted ", ylab), x = xlab)
}
