#' Construct insurance tariff classes
#'
#' @description Constructs insurance tariff classes to `fitgam` objects produced by `fit_gam`. The goal is to bin the continuous risk factors
#' such that categorical risk factors result which capture the effect of the covariate on the response in an accurate way,
#' while being easy to use in a generalized linear model (GLM).
#'
#' @param object fitgam object produced by `fit_gam`
#' @param alpha complexity parameter. The complexity parameter (alpha) is used to control the number of tariff classes. Higher values for `alpha`
#' render less tariff classes. (`alpha` = 0 is default).
#' @param niterations in case the run does not converge, it terminates after a specified number of iterations defined by niterations.
#' @param ntrees the number of trees in the population.
#' @param seed an numeric seed to initialize the random number generator (for reproducibility).
#'
#' @details Evolutionary trees are used as a technique to bin the `fitgam` object produced by `fit_gam` into risk homogeneous categories.
#' This method is based on the work by Henckaerts et al. (2018). See Grubinger et al. (2014) for more details on the various parameters that
#' control aspects of the evtree fit.
#'
#' @import evtree
#'
#' @references Antonio, K. and Valdez, E. A. (2012). Statistical concepts of a priori and a posteriori risk classification in insurance.
#' Advances in Statistical Analysis, 96(2):187–224. doi:10.1007/s10182-011-0152-7.
#' @references Grubinger, T., Zeileis, A., and Pfeiffer, K.-P. (2014). evtree: Evolutionary learning of globally
#' optimal classification and regression trees in R. Journal of Statistical Software, 61(1):1–29. doi:10.18637/jss.v061.i01.
#' @references Henckaerts, R., Antonio, K., Clijsters, M. and Verbelen, R. (2018). A data driven binning strategy for the construction of insurance tariff classes.
#' Scandinavian Actuarial Journal, 2018:8, 681-705. doi:10.1080/03461238.2018.1429300.
#' @references Wood, S.N. (2011). Fast stable restricted maximum likelihood and marginal likelihood estimation of semiparametric
#' generalized linear models. Journal of the Royal Statistical Society (B) 73(1):3-36. doi:10.1111/j.1467-9868.2010.00749.x.
#'
#' @return A list of class `constructtariffclasses` with components
#' \item{prediction}{data frame with predicted values}
#' \item{x}{name of continuous risk factor for which tariff classes are constructed}
#' \item{model}{either 'frequency', 'severity' or 'burning'}
#' \item{data}{data frame with predicted values and observed values}
#' \item{x_obs}{observations for continuous risk factor}
#' \item{splits}{vector with boundaries of the constructed tariff classes}
#' \item{tariff_classes}{values in vector `x` coded according to which constructed tariff class they fall}
#'
#' @author Martin Haringa
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' fit_gam(MTPL, nclaims = nclaims, x = age_policyholder, exposure = exposure) %>%
#'    construct_tariff_classes(.)
#' }
#'
#' @export
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
  cuts <- cut(object[[5]], breaks = splits, include.lowest = TRUE)

  return(structure(list(prediction = object[[1]],
                        x = object[[2]],
                        model = object[[3]],
                        data = object[[4]],
                        x_obs = object[[5]],
                        splits = splits,
                        tariff_classes = cuts),
                   class = "constructtariffclasses"))
}

#' @export
print.constructtariffclasses <- function(x, ...) {
  print(x$splits)
}

#' @export
as.vector.constructtariffclasses <- function(x, ...) {
  splits <- x$splits
  return(as.vector(splits))
}

#' Automatically create a ggplot for objects obtained from construct_tariff_classes()
#'
#' @description Takes an object produced by `construct_tariff_classes()`, and plots the fitted GAM.
#' In addition the constructed tariff classes are shown.
#'
#' @param object constructtariffclasses object produced by `construct_tariff_classes`
#' @param conf_int determines whether 95 percent confidence intervals will be plotted. The default is `conf_int = FALSE`
#' @param color_gam a color can be specified either by name (e.g.: "red") or by hexadecimal code (e.g. : "#FF1234") (default is "steelblue")
#' @param color_splits change the color of the splits in the graph ("grey50" is default)
#' @param show_observations add observed frequency/severity points for each level of the variable for which tariff classes are constructed
#' @param size_points size for points (1 is default)
#' @param color_points change the color of the points in the graph ("black" is default)
#' @param rotate_labels rotate x-labels 45 degrees (this might be helpful for overlapping x-labels)
#' @param remove_outliers do not show observations above this number in the plot. This might be helpful for outliers.
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
#' fit_gam(MTPL, nclaims = nclaims, x = age_policyholder, exposure = exposure) %>%
#'    construct_tariff_classes(.) %>%
#'    autoplot(., show_observations = TRUE)
#' }
#'
#' @author Martin Haringa
#'
#' @export
autoplot.constructtariffclasses <- function(object, conf_int = FALSE, color_gam = "steelblue", show_observations = FALSE, color_splits = "grey50",
                                            size_points = 1, color_points = "black", rotate_labels = FALSE,
                                            remove_outliers = NULL, ...){

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }

  if (!inherits(object, "constructtariffclasses")) {
    stop("autoplot.constructtariffclasses requires a constructtariffclasses object, use object = object", call. = FALSE)
  }

  prediction <- object[[1]]
  xlab <- object[[2]]
  ylab <- object[[3]]
  points <- object[[4]]
  gamcluster <- object[[6]]

  if(isTRUE(conf_int) & sum(prediction$upr_95 > 1e9) > 0){
    message("The confidence bounds are too large to show.")
  }

  if(is.numeric(remove_outliers) & isTRUE(show_observations)) {
    if (ylab == "frequency") points <- points[points$frequency < remove_outliers, ]
    if (ylab == "severity") points <- points[points$avg_claimsize < remove_outliers, ]
    if (ylab == "burning") points <- points[points$avg_premium < remove_outliers, ]
  }

  gam_plot <- ggplot(data = prediction, aes(x = x, y = predicted)) +
    geom_line(color = color_gam) +
    theme_bw(base_size = 12) +
    geom_vline(xintercept = gamcluster, color = color_splits, linetype = 2) +
    {if(isTRUE(conf_int) & sum(prediction$upr_95 > 1e9) == 0) geom_ribbon(aes(ymin = lwr_95, ymax = upr_95), alpha = 0.12)} +
    scale_x_continuous(breaks = gamcluster) +
    {if(isTRUE(show_observations) & ylab == "frequency") geom_point(data = points, aes(x = x, y = frequency), size = size_points, color = color_points)} +
    {if(isTRUE(show_observations) & ylab == "severity") geom_point(data = points, aes(x = x, y = avg_claimsize), size = size_points, color = color_points)} +
    {if(isTRUE(show_observations) & ylab == "burning") geom_point(data = points, aes(x = x, y = avg_premium), size = size_points, color = color_points)} +
    {if(ylab == "severity") scale_y_continuous(labels = scales::comma)} +
    {if(isTRUE(rotate_labels)) theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) } +
    labs(y = paste0("Predicted ", ylab), x = xlab)

  return(gam_plot)
}


