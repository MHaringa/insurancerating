#' Construct insurance tariff classes
#'
#' @description
#' Constructs insurance tariff classes for objects of class `"riskfactor_gam"`
#' produced by [risk_factor_gam()] (formerly [riskfactor_gam()] and
#' [fit_gam()]). The function derives
#' data-driven candidate tariff classes from the fitted GAM response pattern.
#' These classes are intended to help translate continuous risk factors into
#' categorical rating factors that remain interpretable and practical for use in
#' a generalized linear model (GLM).
#'
#' @param object An object of class `"riskfactor_gam"`, produced by
#'   [risk_factor_gam()]. Objects with the old `"fitgam"` class are still
#'   supported for backward compatibility.
#' @param complexity Numeric. Controls the complexity penalty used when deriving
#'   classes. Higher values generally yield fewer tariff classes. Default = 0.
#' @param max_iterations Integer. Maximum number of search iterations used by
#'   the underlying class construction algorithm. Default = 10000.
#' @param population_size Integer. Number of candidate trees used by the
#'   underlying class construction algorithm. Default = 200.
#' @param seed Integer, seed for the random number generator (for reproducibility).
#' @param alpha Deprecated. Use `complexity` instead.
#' @param niterations Deprecated. Use `max_iterations` instead.
#' @param ntrees Deprecated. Use `population_size` instead.
#'
#' @details
#' Evolutionary trees (via [evtree::evtree()]) are used as a technique to bin the
#' fitted GAM object into candidate tariff classes.
#' This method is based on the work by Henckaerts et al. (2018).
#' See Grubinger et al. (2014) for details on the parameters controlling the
#' evtree fit.
#'
#' @return A `list` of class `"tariff_classes"` with components:
#' \describe{
#'   \item{prediction}{Data frame with predicted values.}
#'   \item{x}{Name of the continuous risk factor for which tariff classes are constructed.}
#'   \item{model}{Model type: `"frequency"`, `"severity"`, or `"pure_premium"`.}
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
#' risk_factor_gam(MTPL,
#'                 risk_factor = "age_policyholder",
#'                 claim_count = "nclaims",
#'                 exposure = "exposure") |>
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
construct_tariff_classes <- function(object, complexity = 0,
                                     max_iterations = 10000,
                                     population_size = 200, seed = 1,
                                     alpha = NULL, niterations = NULL,
                                     ntrees = NULL) {

  if (!inherits(object, "riskfactor_gam") && !inherits(object, "fitgam")) {
    stop("Input must be of class 'riskfactor_gam' as returned by risk_factor_gam().",
         call. = FALSE)
  }

  if (!is.null(alpha)) {
    lifecycle::deprecate_warn(
      "0.8.0",
      "construct_tariff_classes(alpha = )",
      "construct_tariff_classes(complexity = )"
    )
    complexity <- alpha
  }
  if (!is.null(niterations)) {
    lifecycle::deprecate_warn(
      "0.8.0",
      "construct_tariff_classes(niterations = )",
      "construct_tariff_classes(max_iterations = )"
    )
    max_iterations <- niterations
  }
  if (!is.null(ntrees)) {
    lifecycle::deprecate_warn(
      "0.8.0",
      "construct_tariff_classes(ntrees = )",
      "construct_tariff_classes(population_size = )"
    )
    population_size <- ntrees
  }

  validate_tariff_class_control(
    complexity = complexity,
    max_iterations = max_iterations,
    population_size = population_size,
    seed = seed
  )

  data_used <- object$data
  x_obs <- object$x_obs

  if (!is.numeric(x_obs)) {
    stop("`object$x_obs` must be numeric.", call. = FALSE)
  }

  x_range <- range(x_obs, na.rm = TRUE)
  if (!all(is.finite(x_range)) || x_range[1] == x_range[2]) {
    stop(
      "Cannot construct tariff classes because the risk factor has fewer than ",
      "two distinct finite values.",
      call. = FALSE
    )
  }

  split_x <- tryCatch(
    {
    tree_x <- evtree::evtree(
      pred ~ x,
      data = data_used,
      control = evtree::evtree.control(
        alpha = complexity,
        ntrees = population_size,
        niterations = max_iterations,
        seed = seed
      )
    )
    split_obtained <- get_splits(tree_x)
    split_obtained[split_obtained > x_range[1] & split_obtained < x_range[2]]
    },
    error = function(e) {
      stop(
        "Could not construct tariff classes with evtree: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (length(split_x) == 0) {
    warning(
      "No internal tariff class split was found; returning one interval.",
      call. = FALSE
    )
  }

  # Add min and max to binning
  splits <- sort(unique(c(x_range[1], split_x, x_range[2])))
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
    class = c("tariff_classes", "constructtariffclasses")
  )
}

validate_tariff_class_control <- function(complexity, max_iterations,
                                          population_size, seed) {
  if (!is.numeric(complexity) || length(complexity) != 1L ||
      !is.finite(complexity) || complexity < 0) {
    stop("`complexity` must be a single non-negative number.", call. = FALSE)
  }
  if (!is.numeric(max_iterations) || length(max_iterations) != 1L ||
      !is.finite(max_iterations) || max_iterations <= 0) {
    stop("`max_iterations` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(population_size) || length(population_size) != 1L ||
      !is.finite(population_size) || population_size <= 0) {
    stop("`population_size` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
    stop("`seed` must be a single finite number.", call. = FALSE)
  }
  invisible(NULL)
}

#' @export
print.tariff_classes <- function(x, ...) {
  cat("Tariff class splits:\n")
  print(x$splits)
  invisible(x)
}

#' @export
print.constructtariffclasses <- print.tariff_classes

#' @export
as.vector.tariff_classes <- function(x, ...) {
  as.vector(x$splits)
}

#' @export
as.vector.constructtariffclasses <- as.vector.tariff_classes


#' Autoplot for tariff class objects
#'
#' @description
#' `autoplot()` method for objects created by [construct_tariff_classes()].
#' Produces a [ggplot2::ggplot()] of the fitted GAM together with the constructed
#' tariff class splits. Optionally, confidence intervals and observed data points
#' can be added.
#'
#' @param object An object of class `"tariff_classes"`, produced by
#'   [construct_tariff_classes()].
#' @param confidence Logical, whether to plot 95% confidence intervals.
#'   Default = `FALSE`.
#' @param conf_int Deprecated. Use `confidence` instead.
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
#' @import ggplot2
#'
#' @export
autoplot.tariff_classes <- function(object,
                                    confidence = FALSE,
                                    color_gam = "steelblue",
                                    show_observations = FALSE,
                                    color_splits = "grey50",
                                    size_points = 1,
                                    color_points = "black",
                                    rotate_labels = FALSE,
                                    remove_outliers = NULL,
                                    conf_int = NULL,
                                    ...) {
  if (!is.null(conf_int)) {
    lifecycle::deprecate_warn("0.9.0", "autoplot(conf_int)",
                              "autoplot(confidence)")
    confidence <- conf_int
  }

  if (!inherits(object, "tariff_classes") &&
      !inherits(object, "constructtariffclasses")) {
    stop("Input must be of class 'tariff_classes'.", call. = FALSE)
  }

  prediction <- object$prediction
  xlab <- object$x
  ylab <- object$model
  points <- object$data
  splits <- object$splits

  # determine fitted-value column robustly
  y_pred_col <- "fitted"
  y_pred_col <- y_pred_col[y_pred_col %in% names(prediction)]

  if (length(y_pred_col) == 0) {
    stop(
      "Could not find a fitted-value column in `object$prediction`. ",
      "Expected one of: predicted, pred, fit, yhat.",
      call. = FALSE
    )
  }

  y_pred_col <- y_pred_col[1]

  # confidence interval column names
  lwr_col <- c("conf_low", "lwr_95", "lower_95", "lwr", "lower")
  upr_col <- c("conf_high", "upr_95", "upper_95", "upr", "upper")

  lwr_col <- lwr_col[lwr_col %in% names(prediction)]
  upr_col <- upr_col[upr_col %in% names(prediction)]

  has_conf <- length(lwr_col) > 0 && length(upr_col) > 0
  if (has_conf) {
    lwr_col <- lwr_col[1]
    upr_col <- upr_col[1]
  }

  # Filter out outliers if requested
  if (is.numeric(remove_outliers) && isTRUE(show_observations)) {
    if (ylab == "frequency" && "frequency" %in% names(points)) {
      points <- points[points$frequency < remove_outliers, , drop = FALSE]
    } else if (ylab == "severity" && "avg_claimsize" %in% names(points)) {
      points <- points[points$avg_claimsize < remove_outliers, , drop = FALSE]
    } else if (ylab %in% c("pure_premium", "burning") &&
               "avg_premium" %in% names(points)) {
      points <- points[points$avg_premium < remove_outliers, , drop = FALSE]
    }
  }

  p <- ggplot(
    prediction,
    aes(x = .data[["x"]], y = .data[[y_pred_col]])
  ) +
    geom_line(color = color_gam) +
    theme_bw(base_size = 12) +
    geom_vline(xintercept = splits, color = color_splits, linetype = 2) +
    labs(y = paste0("Predicted ", ylab), x = xlab)

  if (isTRUE(confidence) && has_conf) {
    ok_ci <- all(is.finite(prediction[[upr_col]])) &&
      all(prediction[[upr_col]] < 1e9, na.rm = TRUE)

    if (ok_ci) {
      p <- p + geom_ribbon(
        aes(
          ymin = .data[[lwr_col]],
          ymax = .data[[upr_col]]
        ),
        alpha = 0.12
      )
    }
  }

  if (isTRUE(show_observations)) {
    if (ylab == "frequency" && "frequency" %in% names(points)) {
      p <- p + geom_point(
        data = points,
        aes(x = .data[["x"]], y = .data[["frequency"]]),
        size = size_points,
        color = color_points
      )
    } else if (ylab == "severity" && "avg_claimsize" %in% names(points)) {
      p <- p + geom_point(
        data = points,
        aes(x = .data[["x"]], y = .data[["avg_claimsize"]]),
        size = size_points,
        color = color_points
      ) +
        scale_y_continuous(labels = scales::comma)
    } else if (ylab %in% c("pure_premium", "burning") &&
               "avg_premium" %in% names(points)) {
      p <- p + geom_point(
        data = points,
        aes(x = .data[["x"]], y = .data[["avg_premium"]]),
        size = size_points,
        color = color_points
      )
    }
  }

  if (isTRUE(rotate_labels)) {
    p <- p + theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
  }

  p
}

#' @export
autoplot.constructtariffclasses <- autoplot.tariff_classes
