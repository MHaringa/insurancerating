#' Construct insurance tariff classes
#'
#' @description
#' Constructs insurance tariff classes for objects of class `"fitgam"` produced
#' by [riskfactor_gam()] (formerly [fit_gam()]). The goal is to bin continuous
#' risk factors into categorical tariff classes that capture the effect of the
#' covariate on the response in an accurate way, while remaining easy to use in
#' a generalized linear model (GLM).
#'
#' @param object An object of class `"fitgam"`, produced by [riskfactor_gam()].
#' @param alpha Complexity parameter passed to [evtree::evtree.control()]. Higher
#'   values yield fewer tariff classes. Default = 0.
#' @param niterations Maximum number of iterations before termination. Passed to
#'   [evtree::evtree.control()]. Default = 10000.
#' @param ntrees Number of trees in the population. Passed to
#'   [evtree::evtree.control()]. Default = 200.
#' @param seed Integer, seed for the random number generator (for reproducibility).
#'
#' @details
#' Evolutionary trees (via [evtree::evtree()]) are used as a technique to bin the
#' fitted GAM object into risk-homogeneous categories.
#' This method is based on the work by Henckaerts et al. (2018).
#' See Grubinger et al. (2014) for details on the parameters controlling the
#' evtree fit.
#'
#' @return A `list` of class `"constructtariffclasses"` with components:
#' \describe{
#'   \item{prediction}{Data frame with predicted values.}
#'   \item{x}{Name of the continuous risk factor for which tariff classes are constructed.}
#'   \item{model}{Model type: `"frequency"`, `"severity"`, or `"burning"`.}
#'   \item{data}{Data frame with predicted and observed values.}
#'   \item{x_obs}{Observed values of the continuous risk factor.}
#'   \item{splits}{Numeric vector with boundaries of the constructed tariff classes.}
#'   \item{tariff_classes}{Factor with the tariff class each observation falls into.}
#' }
#'
#' @author Martin Haringa
#'
#' @references Antonio, K. and Valdez, E. A. (2012). Statistical concepts of a
#' priori and a posteriori risk classification in insurance. *Advances in
#' Statistical Analysis*, 96(2), 187–224. \doi{doi:10.1007/s10182-011-0152-7}
#'
#' @references Grubinger, T., Zeileis, A., and Pfeiffer, K.-P. (2014). *evtree:
#' Evolutionary learning of globally optimal classification and regression trees
#' in R*. Journal of Statistical Software, 61(1), 1–29.
#' \doi{doi:10.18637/jss.v061.i01}
#'
#' @references Henckaerts, R., Antonio, K., Clijsters, M., & Verbelen, R.
#' (2018). A data driven binning strategy for the construction of insurance
#' tariff classes. *Scandinavian Actuarial Journal*, 2018(8), 681–705.
#' \doi{doi:10.1080/03461238.2018.1429300}
#'
#' @references Wood, S.N. (2011). Fast stable restricted maximum likelihood and
#' marginal likelihood estimation of semiparametric generalized linear models.
#' *JRSS B*, 73(1), 3–36. \doi{doi:10.1111/j.1467-9868.2010.00749.x}
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Recommended new usage (SE)
#' riskfactor_gam(MTPL,
#'                nclaims = "nclaims",
#'                x = "age_policyholder",
#'                exposure = "exposure") |>
#'   construct_tariff_classes()
#'
#' # Deprecated usage (NSE, still works with warning)
#' fit_gam(MTPL, nclaims = nclaims, x = age_policyholder, exposure = exposure) |>
#'   construct_tariff_classes()
#' }
#'
#' @importFrom evtree evtree evtree.control
#'
#' @export
construct_tariff_classes <- function(object, alpha = 0, niterations = 10000,
                                     ntrees = 200, seed = 1) {

  if (!inherits(object, "fitgam")) {
    stop("Input must be of class 'fitgam' as returned by riskfactor_gam().",
         call. = FALSE)
  }

  data_used <- object$data
  x_obs <- object$x_obs

  split_x <- tryCatch({
    tree_x <- evtree::evtree(
      pred ~ x,
      data = data_used,
      control = evtree::evtree.control(
        alpha = alpha,
        ntrees = ntrees,
        niterations = niterations,
        seed = seed
      )
    )
    split_obtained <- get_splits(tree_x)
    unique(floor(split_obtained))
  }, error = function(e) {
    NULL
  })

  # Add min and max to binning
  splits <- c(min(x_obs, na.rm = TRUE), split_x, max(x_obs, na.rm = TRUE))
  cuts <- cut(x_obs, breaks = splits, include.lowest = TRUE)

  structure(
    list(
      prediction = object$prediction,
      x = object$x,
      model = object$model,
      data = data_used,
      x_obs = x_obs,
      splits = splits,
      tariff_classes = cuts
    ),
    class = "constructtariffclasses"
  )
}

#' Print method for constructtariffclasses objects
#'
#' @description
#' Displays the tariff class splits of an object created by
#' [construct_tariff_classes()].
#'
#' @param x An object of class `"constructtariffclasses"`.
#' @param ... Further arguments passed to or from other methods (ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.constructtariffclasses <- function(x, ...) {
  cat("Tariff class splits:\n")
  print(x$splits)
  invisible(x)
}

#' Coerce constructtariffclasses to a vector
#'
#' @description
#' Extracts the tariff class splits as a numeric vector.
#'
#' @param x An object of class `"constructtariffclasses"`.
#' @param ... Further arguments passed to or from other methods (ignored).
#'
#' @return A numeric vector with the split points of the tariff classes.
#'
#' @export
as.vector.constructtariffclasses <- function(x, ...) {
  as.vector(x$splits)
}

#' Autoplot for tariff class objects
#'
#' @description
#' `autoplot()` method for objects created by [construct_tariff_classes()].
#' Produces a [ggplot2::ggplot()] of the fitted GAM together with the constructed
#' tariff class splits. Optionally, confidence intervals and observed data points
#' can be added.
#'
#' @param object An object of class `"constructtariffclasses"`, produced by
#'   [construct_tariff_classes()].
#' @param conf_int Logical, whether to plot 95% confidence intervals.
#'   Default = `FALSE`.
#' @param color_gam Color of the fitted GAM line. Default = `"steelblue"`.
#' @param color_splits Color of the vertical split lines. Default = `"grey50"`.
#' @param show_observations Logical, whether to add observed data points for each
#'   level of the risk factor. Default = `FALSE`.
#' @param size_points Numeric, size of points if `show_observations = TRUE`.
#'   Default = 1.
#' @param color_points Color of observed points. Default = `"black"`.
#' @param rotate_labels Logical, whether to rotate x-axis labels by 45 degrees.
#'   Default = `FALSE`.
#' @param remove_outliers Numeric, exclude observations above this value from
#'   the plot (helps with extreme outliers). Default = `NULL`.
#' @param ... Additional arguments passed to [ggplot2::autoplot()].
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @author Martin Haringa
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' riskfactor_gam(MTPL,
#'                nclaims = "nclaims",
#'                x = "age_policyholder",
#'                exposure = "exposure") |>
#'   construct_tariff_classes() |>
#'   autoplot(show_observations = TRUE)
#' }
#'
#' @import ggplot2
#'
#' @export
autoplot.constructtariffclasses <- function(object,
                                            conf_int = FALSE,
                                            color_gam = "steelblue",
                                            show_observations = FALSE,
                                            color_splits = "grey50",
                                            size_points = 1,
                                            color_points = "black",
                                            rotate_labels = FALSE,
                                            remove_outliers = NULL,
                                            ...) {
  if (!inherits(object, "constructtariffclasses")) {
    stop("Input must be of class 'constructtariffclasses'.", call. = FALSE)
  }

  prediction <- object$prediction
  xlab <- object$x
  ylab <- object$model
  points <- object$data
  splits <- object$splits

  # Filter out outliers if requested
  if (is.numeric(remove_outliers) && isTRUE(show_observations)) {
    if (ylab == "frequency") {
      points <- points[points$frequency < remove_outliers, ]
    } else if (ylab == "severity") {
      points <- points[points$avg_claimsize < remove_outliers, ]
    } else if (ylab == "burning") {
      points <- points[points$avg_premium < remove_outliers, ]
    }
  }

  p <- ggplot(prediction, aes(x = x, y = predicted)) +
    geom_line(color = color_gam) +
    theme_bw(base_size = 12) +
    geom_vline(xintercept = splits, color = color_splits, linetype = 2) +
    labs(y = paste0("Predicted ", ylab), x = xlab)

  if (isTRUE(conf_int) && all(prediction$upr_95 < 1e9)) {
    p <- p + geom_ribbon(aes(ymin = lwr_95, ymax = upr_95), alpha = 0.12)
  }

  if (isTRUE(show_observations)) {
    if (ylab == "frequency") {
      p <- p + geom_point(data = points, aes(x = x, y = frequency),
                          size = size_points, color = color_points)
    } else if (ylab == "severity") {
      p <- p + geom_point(data = points, aes(x = x, y = avg_claimsize),
                          size = size_points, color = color_points) +
        scale_y_continuous(labels = scales::comma)
    } else if (ylab == "burning") {
      p <- p + geom_point(data = points, aes(x = x, y = avg_premium),
                          size = size_points, color = color_points)
    }
  }

  if (isTRUE(rotate_labels)) {
    p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  }

  p
}
