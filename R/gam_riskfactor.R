#' Generalized Additive Model for Insurance Risk Factors
#'
#' @description
#' Fits a generalized additive model (GAM) to a continuous risk factor in one of
#' three contexts: claim frequency, claim severity, or burning cost (pure premium).
#'
#' @param data A data.frame containing the insurance portfolio.
#' @param nclaims Character, name of column in `data` with the number of claims.
#' @param x Character, name of column in `data` with the continuous risk factor.
#' @param exposure Character, name of column in `data` with the exposure.
#' @param amount (Optional) Character, column name in `data` with the claim
#'   amount. Required for `model = "severity"`.
#' @param pure_premium (Optional) Character, column name in `data` with the pure
#'   premium. Required for `model = "burning"`.
#' @param model Character string specifying the model type. One of
#'   `"frequency"`, `"severity"`, or `"burning"`. Default is `"frequency"`.
#' @param round_x (Optional) Numeric value to round the risk factor `x` to a
#'   multiple of `round_x`. Can speed up fitting for factors with many levels.
#'
#' @details
#' - **Frequency model**: Fits a Poisson GAM to the number of claims. The log of
#'   the exposure is used as an offset so the expected number of claims is
#'   proportional to exposure.
#'
#' - **Severity model**: Fits a lognormal GAM to the average claim size (total
#'   amount divided by number of claims). The number of claims is included as a
#'   weight.
#'
#' - **Burning cost model**: Fits a lognormal GAM to the pure premium (risk
#'   premium). Implemented by aggregating exposure-weighted pure premiums. This
#'   functionality is still experimental.
#'
#' ## Migration from `fit_gam()`
#'
#' The function [fit_gam()] is deprecated as of version 0.8.0 and replaced by
#' [riskfactor_gam()]. In addition to the name change, the interface has also
#' changed:
#'
#' - `fit_gam()` used **non-standard evaluation (NSE)**, so column names could be
#'   passed unquoted (e.g. `x = age_policyholder`).
#' - `riskfactor_gam()` uses **standard evaluation (SE)**, so column names must
#'   be passed as character strings (e.g. `x = "age_policyholder"`).
#'
#' This makes the function easier to use in programmatic workflows.
#'
#' `fit_gam()` is still available for backward compatibility but will emit a
#' deprecation warning and will be removed in a future release.
#'
#'
#' @importFrom mgcv gam predict.gam
#' @import ggplot2
#' @importFrom grDevices dev.off png
#' @importFrom graphics plot
#' @importFrom stats aggregate gaussian model.frame poisson predict qnorm setNames
#'
#' @references Antonio, K. and Valdez, E. A. (2012). Statistical concepts of a
#' priori and a posteriori risk classification in insurance. Advances in
#' Statistical Analysis, 96(2):187–224.
#' @references Henckaerts, R., Antonio, K., Clijsters, M. and Verbelen, R.
#' (2018). A data driven binning strategy for the construction of insurance
#' tariff classes. Scandinavian Actuarial Journal, 2018:8, 681–705.
#' @references Wood, S.N. (2011). Fast stable restricted maximum likelihood and
#' marginal likelihood estimation of semiparametric generalized linear models.
#' Journal of the Royal Statistical Society (B) 73(1):3–36.
#'
#' @return
#' A `list` of class `"fitgam"` with the following elements:
#' \item{prediction}{A data frame with predicted values and confidence intervals.}
#' \item{x}{Name of the continuous risk factor.}
#' \item{model}{The model type: `"frequency"`, `"severity"`, or `"burning"`.}
#' \item{data}{Merged data frame with predictions and observed values.}
#' \item{x_obs}{Observed values of the continuous risk factor.}
#'
#' @author Martin Haringa
#'
#' @examples
#' ## --- Recommended new usage (SE) ---
#' # Column names must be passed as strings
#' riskfactor_gam(MTPL,
#'                nclaims = "nclaims",
#'                x = "age_policyholder",
#'                exposure = "exposure")
#'
#' ## --- Deprecated usage (NSE) ---
#' # This still works but will show a warning
#' fit_gam(MTPL,
#'         nclaims = nclaims,
#'         x = age_policyholder,
#'         exposure = exposure)
#'
#' @export
riskfactor_gam <- function(data, nclaims, x, exposure, amount = NULL,
                           pure_premium = NULL, model = "frequency", round_x = NULL) {

  if (nrow(data) < 10) {
    stop("At least 10 datapoints are required. The spline smoothers assume a
         default of 10 degrees of freedom.", call. = FALSE)
  }

  if (!model %in% c("frequency", "severity", "burning")) {
    stop("Choose correct model specification: 'frequency', 'severity' or 'burning'.",
         call. = FALSE)
  }

  # kolomnamen als strings
  nclaims_col <- nclaims
  x_col <- x
  exposure_col <- exposure
  amount_col <- amount
  pure_col <- pure_premium

  if (!is.numeric(data[[x_col]])) stop("x should be numeric", call. = FALSE)
  if (!is.numeric(data[[exposure_col]])) stop("exposure should be numeric", call. = FALSE)

  if (model == "frequency") {
    df <- data.frame(
      nclaims = data[[nclaims_col]],
      x = data[[x_col]],
      exposure = data[[exposure_col]]
    )
    if (is.numeric(round_x)) df$x <- round(df$x / round_x) * round_x

    df <- aggregate(list(nclaims = df$nclaims,
                         exposure = df$exposure),
                    by = list(x = df$x), FUN = sum, na.rm = TRUE)
    df$frequency <- df$nclaims / df$exposure

    if (any(df$exposure == 0)) {
      stop("Exposures should be greater than zero.", call. = FALSE)
    }

    gam_x <- mgcv::gam(nclaims ~ s(x),
                       data = df,
                       family = poisson(),
                       offset = log(exposure))
  }

  if (model == "severity") {
    df <- data.frame(
      nclaims = data[[nclaims_col]],
      x = data[[x_col]],
      exposure = data[[exposure_col]],
      amount = data[[amount_col]]
    )
    if (is.numeric(round_x)) df$x <- round(df$x / round_x) * round_x

    df <- aggregate(list(nclaims = df$nclaims,
                         exposure = df$exposure,
                         amount = df$amount),
                    by = list(x = df$x), FUN = sum, na.rm = TRUE)
    df <- subset(df, nclaims > 0 & amount > 0)
    df$avg_claimsize <- df$amount / df$nclaims

    gam_x <- mgcv::gam(log(avg_claimsize) ~ s(x),
                       data = df,
                       family = gaussian,
                       weights = nclaims)
  }

  if (model == "burning") {
    df <- data.frame(
      x = data[[x_col]],
      exposure = data[[exposure_col]],
      pure_premium = data[[pure_col]]
    )
    df <- aggregate(list(exposure = df$exposure,
                         pure_premium = df$pure_premium,
                         weighted_premium = df$exposure * df$pure_premium),
                    by = list(x = df$x), FUN = sum, na.rm = TRUE)
    df <- subset(df, pure_premium > 0 & exposure > 0)
    df$avg_premium <- df$weighted_premium / df$exposure

    gam_x <- mgcv::gam(log(avg_premium) ~ s(x),
                       data = df,
                       family = gaussian,
                       weights = exposure)
  }

  newx <- data.frame(x = seq(min(data[[x_col]]), max(data[[x_col]]), length.out = 100))
  out <- confint_gam(gam_x, newx)

  counting <- sort(model.frame(gam_x)[["x"]])
  df_new <- data.frame(x = counting,
                       pred = as.numeric(mgcv::predict.gam(gam_x, data.frame(x = counting),
                                                           type = "response")))
  new <- merge(df_new, df, by = "x")

  return(structure(list(prediction = out,
                        x = x_col,
                        model = model,
                        data = new,
                        x_obs = data[[x_col]]),
                   class = "fitgam"))
}


#' @rdname riskfactor_gam
#' @description
#' [fit_gam()] is deprecated as of version 0.8.0.
#' Please use [riskfactor_gam()] instead.
#'
#' In addition, note that column arguments must now be passed as **strings**
#' (standard evaluation).
#'
#' @export
fit_gam <- function(data, nclaims, x, exposure, amount = NULL,
                    pure_premium = NULL, model = "frequency", round_x = NULL) {

  lifecycle::deprecate_warn(
    when = "0.8.0",
    what = "fit_gam()",
    with = "riskfactor_gam()",
    details =
"Please note that `riskfactor_gam()` requires **standard evaluation** (SE):
column names must be supplied as character strings, e.g.
`riskfactor_gam(df, nclaims = \"nclaims\", x = \"age\", exposure = \"exposure\")`.
The old NSE-style (`fit_gam(df, nclaims = nclaims, x = age, exposure = exposure)`)
is no longer supported."
  )

  riskfactor_gam(
    data = data,
    nclaims = deparse(substitute(nclaims)),
    x = deparse(substitute(x)),
    exposure = deparse(substitute(exposure)),
    amount = if (!is.null(substitute(amount))) deparse(substitute(
      amount)) else NULL,
    pure_premium = if (!is.null(substitute(pure_premium))) deparse(substitute(
      pure_premium)) else NULL,
    model = model,
    round_x = round_x
  )
}


#' Print method for fitgam objects
#'
#' @description
#' Prints the prediction component of a `fitgam` object created by
#' [riskfactor_gam()].
#'
#' @param x An object of class `"fitgam"`.
#' @param ... Further arguments passed to or from other methods (ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.fitgam <- function(x, ...) {
  cat("Predictions from fitgam object:\n")
  print(x$prediction)
  invisible(x)
}

#' Coerce fitgam objects to a data frame
#'
#' @description
#' Extracts the prediction component of a `fitgam` object and returns it
#' as a data frame.
#'
#' @param x An object of class `"fitgam"`.
#' @param ... Further arguments passed to [as.data.frame()].
#'
#' @return A `data.frame` containing the predictions.
#'
#' @export
as.data.frame.fitgam <- function(x, ...) {
  as.data.frame(x$prediction, ...)
}

#' Summary method for fitgam objects
#'
#' @description
#' Provides a concise summary of a `fitgam` object created by [riskfactor_gam()].
#' Shows the fitted model type, the risk factor, and basic information about the
#' prediction data.
#'
#' @param object An object of class `"fitgam"`.
#' @param ... Further arguments passed to or from other methods (ignored).
#'
#' @return Invisibly returns `object`.
#'
#' @examples
#' \dontrun{
#' fit <- riskfactor_gam(MTPL,
#'                       nclaims = "nclaims",
#'                       x = "age_policyholder",
#'                       exposure = "exposure")
#' summary(fit)
#' }
#'
#' @author Martin Haringa
#' @export
summary.fitgam <- function(object, ...) {
  if (!inherits(object, "fitgam")) {
    stop("Input must be of class 'fitgam'.", call. = FALSE)
  }

  cat("Generalized Additive Model for Insurance Risk Factors\n")
  cat("------------------------------------------------------\n")
  cat("Model type:   ", object$model, "\n")
  cat("Risk factor:  ", object$x, "\n")
  cat("Observations: ", length(object$x_obs), "\n")
  cat("Predictions:  ", nrow(object$prediction), " rows\n\n")

  cat("Prediction head:\n")
  print(utils::head(object$prediction, 5))

  invisible(object)
}

#' Autoplot for GAM Objects from `riskfactor_gam()`
#'
#' @description Generates a `ggplot2` visualization of a fitted GAM created with
#' [riskfactor_gam()]. The plot shows the fitted curve, and optionally confidence
#' intervals and observed data points.
#'
#' @param object An object of class `"fitgam"` returned by [riskfactor_gam()].
#' @param conf_int Logical. If `TRUE`, add 95% confidence intervals around the
#' fitted curve. Default is `FALSE`.
#' @param color_gam Color for the fitted GAM line, specified by name (e.g.,
#' `"red"`) or hex code (e.g., `"#FF1234"`). Default is `"steelblue"`.
#' @param x_stepsize Numeric. Step size for tick marks on the x-axis. If
#' `NULL`, breaks are determined automatically.
#' @param show_observations Logical. If `TRUE`, add observed frequency/severity
#' points corresponding to the underlying data.
#' @param size_points Numeric. Point size for observed data. Default is `1`.
#' @param color_points Color for the observed data points. Default is `"black"`.
#' @param rotate_labels Logical. If `TRUE`, rotate x-axis labels by 45 degrees
#' to reduce overlap.
#' @param remove_outliers Numeric. If specified, observations greater than this
#' threshold are omitted from the plot.
#' @param ... Additional arguments passed to underlying `ggplot2` functions.
#'
#' @return
#' A `ggplot` object representing the fitted GAM.
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' fit <- fit_gam(MTPL, nclaims = nclaims,
#'                x = age_policyholder,
#'                exposure = exposure)
#'
#' autoplot(fit, show_observations = TRUE)
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

  if (isTRUE(conf_int) && any(prediction$upr_95 > 1e9)) {
    message("Confidence intervals exceed 1e9 and will not be displayed.")
  }


  if (length(remove_outliers) == 1 && is.numeric(remove_outliers) &&
      isTRUE(show_observations)) {
    points <- switch(
      ylab,
      "frequency" = points[points$frequency < remove_outliers,],
      "severity" = points[points$avg_claimsize < remove_outliers,],
      "burning" = points[points$avg_premium < remove_outliers,],
      points  # default: nothing removed
    )
  }

  p <- ggplot(prediction, aes(x = x, y = predicted)) +
    geom_line(color = color_gam) +
    theme_bw(base_size = 12) +
    labs(y = paste0("Predicted ", ylab), x = xlab)

  if (isTRUE(conf_int) && !any(prediction$upr_95 > 1e9)) {
    p <- p + geom_ribbon(aes(ymin = lwr_95, ymax = upr_95), alpha = 0.12)
  }

  if (is.numeric(x_stepsize)) {
    p <- p + scale_x_continuous(breaks = seq(floor(min(prediction$x)),
                                             ceiling(max(prediction$x)),
                                             by = x_stepsize))
  }

  if (isTRUE(show_observations)) {
    yvar <- switch(ylab,
                   "frequency" = "frequency",
                   "severity"  = "avg_claimsize",
                   "burning"   = "avg_premium",
                   NULL)
    if (!is.null(yvar)) {
      p <- p + geom_point(data = points,
                          aes(x = x, y = .data[[yvar]]),
                          size = size_points, color = color_points)
    }
  }

  if (ylab == "severity") {
    p <- p + scale_y_continuous(labels = scales::comma)
  }

  if (isTRUE(rotate_labels)) {
    p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  }

  p
}
