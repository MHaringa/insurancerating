#' @keywords internal
fit_frequency_model <- function(df) {
  mgcv::gam(
    nclaims ~ s(x),
    data = df,
    family = poisson(),
    offset = log(exposure)
  )
}

#' @keywords internal
fit_severity_model <- function(df) {
  mgcv::gam(
    avg_claimsize ~ s(x),
    data = df,
    family = Gamma(link = "log"),
    weights = nclaims
  )
}

#' @keywords internal
fit_pure_premium_model <- function(df) {
  mgcv::gam(
    avg_premium ~ s(x),
    data = df,
    family = Gamma(link = "log"),
    weights = exposure
  )
}

#' @keywords internal
round_x_values <- function(x, round_x = NULL) {

  if (is.null(round_x)) {
    return(x)
  }

  if (!is.numeric(round_x) || length(round_x) != 1L || round_x <= 0) {
    stop("'round_x' must be a single positive numeric value.", call. = FALSE)
  }

  round(x / round_x) * round_x
}

#' @keywords internal
check_required_columns <- function(data, ...) {

  cols <- list(...)

  if (any(vapply(cols, is.null, logical(1)))) {
    stop("Required column arguments are missing.", call. = FALSE)
  }

  cols <- unlist(cols, use.names = FALSE)
  missing_cols <- setdiff(cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "The following columns are missing in 'data': %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' @keywords internal
normalise_gam_model <- function(model) {
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    stop("`model` must be one of 'frequency', 'severity', or 'pure_premium'.",
         call. = FALSE)
  }

  if (model == "burning") {
    lifecycle::deprecate_warn(
      "0.8.0",
      "risk_factor_gam(model = 'burning')",
      "risk_factor_gam(model = 'pure_premium')"
    )
    return("pure_premium")
  }

  if (!model %in% c("frequency", "severity", "pure_premium")) {
    stop("`model` must be one of 'frequency', 'severity', or 'pure_premium'.",
         call. = FALSE)
  }

  model
}

#' @keywords internal
legacy_gam_column_name <- function(expr) {
  if (identical(expr, quote(NULL))) {
    NULL
  } else if (is.character(expr) && length(expr) == 1L) {
    expr
  } else if (is.name(expr)) {
    as.character(expr)
  } else {
    deparse(expr)
  }
}


#' Estimate a smooth effect for a continuous risk factor
#'
#' @description
#' Estimate the relationship between a continuous risk factor and claim
#' frequency, average severity or risk premium with a generalized additive
#' model (GAM). The fitted curve is intended for exploratory risk-factor
#' analysis before selecting a functional form, applying refinement or deriving
#' categorical tariff segments.
#'
#' @param data A data frame containing portfolio observations.
#' @param risk_factor Character string. Numeric continuous risk-factor column
#'   in `data`.
#' @param claim_count Character string. Claim-count column. Required for
#'   `model = "frequency"` and `model = "severity"`.
#' @param exposure Character string. Exposure column used as an offset or
#'   aggregation weight.
#' @param claim_amount Optional character string. Total claim-amount column.
#'   Required for `model = "severity"`.
#' @param pure_premium Optional character string. Row-level risk-premium column.
#'   Required for `model = "pure_premium"` and aggregated using exposure
#'   weights.
#' @param model Character string. Response context: `"frequency"`,
#'   `"severity"` or `"pure_premium"`. The deprecated value `"burning"` maps
#'   to `"pure_premium"`.
#' @param round_risk_factor Optional positive numeric value. The continuous risk
#'   factor is rounded to multiples of this value before aggregation and model
#'   fitting. This can reduce computation and local volatility when the variable
#'   has many distinct values, but it also removes detail.
#' @param x,nclaims,amount,round_x Deprecated argument names. Use `risk_factor`,
#'   `claim_count`, `claim_amount`, and `round_risk_factor` instead.
#'
#' @details
#' ## Statistical specification
#'
#' - `"frequency"` fits a Poisson GAM to aggregated claim counts with
#'   `log(exposure)` as offset.
#'
#' - `"severity"` fits a Gamma GAM with log link to average claim amount. The
#'   response is total claim amount divided by claim count and claim count is
#'   used as model weight.
#'
#' - `"pure_premium"` fits a Gamma GAM with log link to exposure-weighted risk
#'   premium.
#'
#' Observations are first aggregated by the risk-factor value after optional
#' rounding. Predictions and pointwise confidence intervals are then evaluated
#' over the observed range.
#'
#' ## Actuarial interpretation
#'
#' The fitted curve describes the marginal pattern in the selected portfolio
#' data. It can reveal non-linearity, broad turning points and areas with sparse
#' support, but it is not by itself a final tariff structure. Correlation with
#' other risk factors, exposure concentration, claim volume, tail observations
#' and stability across periods should be considered before using the pattern
#' in a multivariate GLM.
#'
#' [autoplot.riskfactor_gam()] can be used to inspect the curve and observed
#' experience. [derive_tariff_segments()] can subsequently translate the smooth
#' pattern into candidate intervals. Alternatively, [add_smoothing()] supports
#' smoothing within the structured refinement workflow.
#'
#' ## Column interface and compatibility
#'
#' Column names are supplied as character strings. Deprecated [fit_gam()] and
#' [riskfactor_gam()] interfaces remain available for compatibility.
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
#' A list of class `"risk_factor_gam"` with compatibility classes
#' `"riskfactor_gam"` and `"fitgam"`. It contains:
#' \describe{
#'   \item{prediction}{Prediction grid with fitted values and pointwise
#'   confidence limits.}
#'   \item{x}{Name of the continuous risk factor.}
#'   \item{model}{Response context: `"frequency"`, `"severity"` or
#'   `"pure_premium"`.}
#'   \item{data}{Aggregated observed experience and fitted values at observed
#'   risk-factor values.}
#'   \item{x_obs}{Risk-factor values in the original portfolio row order, after
#'   optional rounding.}
#'   \item{round_risk_factor}{Rounding increment used for the risk factor, or
#'   `NULL` when no rounding was applied.}
#' }
#'
#' @author Martin Haringa
#'
#' @seealso [autoplot.riskfactor_gam()], [derive_tariff_segments()],
#'   [add_smoothing()]
#'
#' @examples
#' age_frequency <- risk_factor_gam(
#'   MTPL,
#'   risk_factor = "age_policyholder",
#'   claim_count = "nclaims",
#'   exposure = "exposure",
#'   model = "frequency"
#' )
#'
#' autoplot(age_frequency, show_observations = TRUE)
#'
#' @importFrom mgcv gam predict.gam
#' @import ggplot2
#' @importFrom grDevices dev.off png
#' @importFrom graphics plot
#' @importFrom stats aggregate gaussian model.frame poisson predict qnorm setNames
#'
#' @export
risk_factor_gam <- function(data, risk_factor = NULL, claim_count = NULL,
                            exposure = NULL, claim_amount = NULL,
                            pure_premium = NULL, model = "frequency",
                            round_risk_factor = NULL, x = NULL,
                            nclaims = NULL, amount = NULL,
                            round_x = NULL) {

  args <- resolve_risk_factor_gam_args(
    risk_factor = risk_factor,
    claim_count = claim_count,
    claim_amount = claim_amount,
    round_risk_factor = round_risk_factor,
    x = x,
    nclaims = nclaims,
    amount = amount,
    round_x = round_x
  )
  risk_factor <- args$risk_factor
  claim_count <- args$claim_count
  claim_amount <- args$claim_amount
  round_risk_factor <- args$round_risk_factor

  model <- normalise_gam_model(model)

  if (nrow(data) < 10) {
    stop("At least 10 datapoints are required. The spline smoothers assume a
         default of 10 degrees of freedom.", call. = FALSE)
  }

  check_required_columns(data, risk_factor, exposure)

  if (!is.numeric(data[[risk_factor]])) {
    stop("`risk_factor` should be numeric.", call. = FALSE)
  }
  if (!is.numeric(data[[exposure]])) {
    stop("`exposure` should be numeric.", call. = FALSE)
  }

  x_vals <- round_x_values(data[[risk_factor]], round_risk_factor)

  if (model == "frequency") {

    check_required_columns(data, claim_count, exposure)

    df <- aggregate(
      list(
        nclaims  = data[[claim_count]],
        exposure = data[[exposure]]
      ),
      by = list(x = x_vals),
      FUN = sum,
      na.rm = TRUE
    )

    if (any(df$exposure <= 0)) {
      stop("Exposures should be greater than zero.", call. = FALSE)
    }

    df$frequency <- df$nclaims / df$exposure

    gam_x <- fit_frequency_model(df)
  }

  if (model == "severity") {

    check_required_columns(data, claim_count, exposure, claim_amount)

    df <- aggregate(
      list(
        nclaims = data[[claim_count]],
        exposure = data[[exposure]],
        amount = data[[claim_amount]]
      ),
      by = list(x = x_vals),
      FUN = sum,
      na.rm = TRUE
    )

    df <- subset(df, nclaims > 0 & amount > 0)
    df$avg_claimsize <- df$amount / df$nclaims

    gam_x <- fit_severity_model(df)
  }

  if (model == "pure_premium") {

    check_required_columns(data, exposure, pure_premium)

    df <- aggregate(
      list(
        exposure = data[[exposure]],
        pure_premium = data[[pure_premium]],
        weighted_premium = data[[exposure]] * data[[pure_premium]]
      ),
      by = list(x = x_vals),
      FUN = sum,
      na.rm = TRUE
    )

    df <- subset(df, exposure > 0 & weighted_premium > 0)
    df$avg_premium <- df$weighted_premium / df$exposure

    gam_x <- fit_pure_premium_model(df)
  }

  prediction_grid <- data.frame(
    x = seq(min(data[[risk_factor]], na.rm = TRUE),
            max(data[[risk_factor]], na.rm = TRUE),
            length.out = 100)
  )

  out <- confint_gam(gam_x, prediction_grid)

  model_x <- sort(model.frame(gam_x)[["x"]])
  pred_x <- predict(gam_x,
                    newdata = data.frame(x = model_x),
                    type = "response")

  new <- merge(
    data.frame(
      x = model_x,
      pred = as.numeric(pred_x)
    ),
    df,
    by = "x"
  )

  return(structure(list(prediction = out,
                        x = risk_factor,
                        model = model,
                        data = new,
                        x_obs = x_vals,
                        round_risk_factor = round_risk_factor),
                   class = c("risk_factor_gam", "riskfactor_gam", "fitgam")))
}


resolve_risk_factor_gam_args <- function(risk_factor, claim_count,
                                         claim_amount, round_risk_factor, x,
                                         nclaims, amount, round_x) {
  if (!is.null(x)) {
    if (!is.null(risk_factor)) {
      stop("Use only one of `risk_factor` and deprecated `x`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "risk_factor_gam(x)",
                              "risk_factor_gam(risk_factor)")
    risk_factor <- x
  }
  if (!is.null(nclaims)) {
    if (!is.null(claim_count)) {
      stop("Use only one of `claim_count` and deprecated `nclaims`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "risk_factor_gam(nclaims)",
                              "risk_factor_gam(claim_count)")
    claim_count <- nclaims
  }
  if (!is.null(amount)) {
    if (!is.null(claim_amount)) {
      stop("Use only one of `claim_amount` and deprecated `amount`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "risk_factor_gam(amount)",
                              "risk_factor_gam(claim_amount)")
    claim_amount <- amount
  }
  if (!is.null(round_x)) {
    if (!is.null(round_risk_factor)) {
      stop("Use only one of `round_risk_factor` and deprecated `round_x`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "risk_factor_gam(round_x)",
                              "risk_factor_gam(round_risk_factor)")
    round_risk_factor <- round_x
  }

  list(
    risk_factor = risk_factor,
    claim_count = claim_count,
    claim_amount = claim_amount,
    round_risk_factor = round_risk_factor
  )
}


#' Deprecated alias for `risk_factor_gam()`
#'
#' @description
#' `riskfactor_gam()` is deprecated in favour of [risk_factor_gam()].
#'
#' @inheritParams risk_factor_gam
#' @param nclaims Deprecated. Use `claim_count` instead.
#' @param x Deprecated. Use `risk_factor` instead.
#' @param amount Deprecated. Use `claim_amount` instead.
#' @param round_x Deprecated. Use `round_risk_factor` instead.
#'
#' @return See [risk_factor_gam()].
#'
#' @export
#' @keywords internal
riskfactor_gam <- function(data, nclaims = NULL, x = NULL, exposure = NULL,
                           amount = NULL, pure_premium = NULL,
                           model = "frequency", round_x = NULL,
                           risk_factor = NULL, claim_count = NULL,
                           claim_amount = NULL, round_risk_factor = NULL) {
  lifecycle::deprecate_warn(
    "0.9.0",
    "riskfactor_gam()",
    "risk_factor_gam()"
  )

  if (!is.null(x)) risk_factor <- x
  if (!is.null(nclaims)) claim_count <- nclaims
  if (!is.null(amount)) claim_amount <- amount
  if (!is.null(round_x)) round_risk_factor <- round_x

  risk_factor_gam(
    data = data,
    risk_factor = risk_factor,
    claim_count = claim_count,
    exposure = exposure,
    claim_amount = claim_amount,
    pure_premium = pure_premium,
    model = model,
    round_risk_factor = round_risk_factor
  )
}


#' Deprecated NSE wrapper for `risk_factor_gam()`
#'
#' @description
#' [fit_gam()] is deprecated as of version 0.8.0.
#' Please use [risk_factor_gam()] instead.
#'
#' In addition, note that column arguments must now be passed as **strings**
#' (standard evaluation).
#' @inheritParams risk_factor_gam
#' @param nclaims Deprecated NSE argument for claim counts.
#' @param x Deprecated NSE argument for the continuous risk factor.
#' @param amount Deprecated NSE argument for claim amounts.
#' @param round_x Deprecated. Use `round_risk_factor` instead.
#'
#' @return See [risk_factor_gam()].
#'
#' @export
#' @keywords internal
fit_gam <- function(data, nclaims, x, exposure, amount = NULL,
                    pure_premium = NULL, model = "frequency", round_x = NULL) {
  claim_count <- legacy_gam_column_name(substitute(nclaims))
  risk_factor <- legacy_gam_column_name(substitute(x))
  exposure_column <- legacy_gam_column_name(substitute(exposure))
  claim_amount <- legacy_gam_column_name(substitute(amount))
  pure_premium_column <- legacy_gam_column_name(substitute(pure_premium))

  lifecycle::deprecate_warn(
    when = "0.8.0",
    what = "fit_gam()",
    with = "risk_factor_gam()",
    details =
      "Please note that `risk_factor_gam()` requires **standard evaluation** (SE):
column names must be supplied as character strings, e.g.
`risk_factor_gam(df, claim_count = \"nclaims\", risk_factor = \"age\", exposure = \"exposure\")`.
The old NSE-style (`fit_gam(df, nclaims = nclaims, x = age, exposure = exposure)`)
remains available through the deprecated wrapper."
  )

  risk_factor_gam(
    data = data,
    claim_count = claim_count,
    risk_factor = risk_factor,
    exposure = exposure_column,
    claim_amount = claim_amount,
    pure_premium = pure_premium_column,
    model = model,
    round_risk_factor = round_x
  )
}


#' @export
print.riskfactor_gam <- function(x, ...) {
  cat("Predictions from riskfactor_gam object:\n")
  print(x$prediction)
  invisible(x)
}

#' @export
print.risk_factor_gam <- print.riskfactor_gam

#' @export
print.fitgam <- print.riskfactor_gam

#' @export
as.data.frame.riskfactor_gam <- function(x, ...) {
  as.data.frame(x$prediction, ...)
}

#' @export
as.data.frame.risk_factor_gam <- as.data.frame.riskfactor_gam

#' @export
as.data.frame.fitgam <- as.data.frame.riskfactor_gam

#' @export
summary.riskfactor_gam <- function(object, ...) {
  if (!inherits(object, "risk_factor_gam") &&
      !inherits(object, "riskfactor_gam") &&
      !inherits(object, "fitgam")) {
    stop("Input must be of class 'risk_factor_gam'.", call. = FALSE)
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

#' @export
summary.risk_factor_gam <- summary.riskfactor_gam

#' @export
summary.fitgam <- summary.riskfactor_gam

.plot_risk_factor_curve <- function(prediction, points, risk_factor, model_type,
                                    confidence, color_gam,
                                    show_observations, x_stepsize,
                                    size_points, color_points,
                                    rotate_labels, remove_outliers,
                                    segment_boundaries = NULL,
                                    show_segments = FALSE,
                                    color_segments = "grey50") {
  if (!"fitted" %in% names(prediction)) {
    stop("The plot data must contain a `fitted` column.", call. = FALSE)
  }

  lower_candidates <- c("conf_low", "lwr_95", "lower_95", "lwr", "lower")
  upper_candidates <- c("conf_high", "upr_95", "upper_95", "upr", "upper")
  lower <- lower_candidates[lower_candidates %in% names(prediction)][1]
  upper <- upper_candidates[upper_candidates %in% names(prediction)][1]
  has_confidence <- !is.na(lower) && !is.na(upper)
  confidence_finite <- has_confidence &&
    all(is.finite(prediction[[lower]])) &&
    all(is.finite(prediction[[upper]])) &&
    all(prediction[[upper]] < 1e9)

  if (isTRUE(confidence) && !confidence_finite) {
    message(
      "Finite confidence intervals below 1e9 are not available and will not ",
      "be displayed."
    )
  }

  observed_column <- switch(
    model_type,
    "frequency" = "frequency",
    "severity" = "avg_claimsize",
    "pure_premium" = "avg_premium",
    "burning" = "avg_premium",
    NULL
  )

  if (is.numeric(remove_outliers) && length(remove_outliers) == 1L &&
      is.finite(remove_outliers) && isTRUE(show_observations) &&
      !is.null(observed_column) && observed_column %in% names(points)) {
    points <- points[
      points[[observed_column]] < remove_outliers,
      ,
      drop = FALSE
    ]
  }

  p <- ggplot(
    prediction,
    aes(x = .data[["x"]], y = .data[["fitted"]])
  ) +
    geom_line(color = color_gam) +
    theme_minimal() +
    .plot_grid_theme_ir() +
    labs(y = paste0("Predicted ", model_type), x = risk_factor)

  if (isTRUE(show_segments) && length(segment_boundaries) > 0L) {
    p <- p + geom_vline(
      xintercept = segment_boundaries,
      color = color_segments,
      linetype = 2
    )
  }

  if (isTRUE(confidence) && confidence_finite) {
    p <- p + geom_ribbon(
      aes(ymin = .data[[lower]], ymax = .data[[upper]]),
      alpha = 0.12
    )
  }

  if (!is.null(x_stepsize)) {
    if (!is.numeric(x_stepsize) || length(x_stepsize) != 1L ||
        is.na(x_stepsize) || !is.finite(x_stepsize) || x_stepsize <= 0) {
      stop("`x_stepsize` must be NULL or a positive finite number.",
           call. = FALSE)
    }
    p <- p + scale_x_continuous(
      breaks = seq(
        floor(min(prediction$x, na.rm = TRUE)),
        ceiling(max(prediction$x, na.rm = TRUE)),
        by = x_stepsize
      )
    )
  }

  if (isTRUE(show_observations) && !is.null(observed_column) &&
      observed_column %in% names(points)) {
    p <- p + geom_point(
      data = points,
      aes(x = .data[["x"]], y = .data[[observed_column]]),
      size = size_points,
      color = color_points
    )
  }

  if (identical(model_type, "severity")) {
    p <- p + scale_y_continuous(labels = scales::comma)
  }

  if (isTRUE(rotate_labels)) {
    p <- p + theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
  }

  p
}

#' Inspect smooth risk-factor effects and tariff-segment boundaries
#'
#' @description
#' Plot the smooth effect estimated by [risk_factor_gam()] or inspect that same
#' effect together with candidate boundaries returned by
#' [derive_tariff_segments()]. Both methods use the same curve, confidence
#' interval, observation and axis layers.
#'
#' @details
#' The fitted line is shown on its natural response scale: claim frequency,
#' average severity or risk premium. Optional observed points represent
#' portfolio experience aggregated at the continuous risk-factor values used
#' for fitting.
#'
#' For a `tariff_segments` object, vertical lines show the derived interval
#' boundaries. These lines support actuarial review of where the continuous
#' effect changes sufficiently to motivate a categorical tariff treatment.
#' Set `show_segments = FALSE` to inspect only the underlying smooth curve.
#'
#' Confidence intervals describe uncertainty in the fitted curve conditional on
#' the selected GAM specification. They do not include uncertainty from model
#' selection, omitted risk factors or future portfolio changes. Segment
#' boundaries do not by themselves demonstrate that adjacent segments are
#' statistically or commercially distinct. Exposure, claim volume, temporal
#' stability and operational tariff constraints should be considered
#' separately.
#'
#' `remove_outliers` affects displayed observed points only. It does not remove
#' observations from the fitted GAM or alter the prediction curve or segment
#' boundaries.
#'
#' @param object An object returned by [risk_factor_gam()] or
#'   [derive_tariff_segments()].
#' @param confidence Logical. If `TRUE`, add pointwise 95 percent confidence
#'   intervals where finite values are available.
#' @param conf_int Deprecated. Use `confidence` instead.
#' @param color_gam Colour for the fitted GAM line.
#' @param x_stepsize Optional positive numeric step size for x-axis tick marks.
#'   If `NULL`, breaks are determined automatically.
#' @param show_observations Logical. If `TRUE`, add the aggregated observed
#'   experience used for fitting.
#' @param size_points Numeric point size for observed experience.
#' @param color_points Colour for observed experience.
#' @param rotate_labels Logical. If `TRUE`, rotate x-axis labels by 45 degrees.
#' @param remove_outliers Optional single numeric upper display limit for
#'   observed points. The fitted curve remains unchanged.
#' @param color_splits Colour for segment boundaries. Used only for a
#'   `tariff_segments` object.
#' @param show_segments Logical. For a `tariff_segments` object, show the
#'   candidate segment boundaries when `TRUE`. Default is `TRUE`.
#' @param ... Additional arguments reserved for method compatibility.
#'
#' @return A `ggplot2` object.
#'
#' @examples
#' \dontrun{
#' fit <- risk_factor_gam(
#'   MTPL,
#'   risk_factor = "age_policyholder",
#'   claim_count = "nclaims",
#'   exposure = "exposure"
#' )
#'
#' # Inspect the continuous effect before deriving tariff segments.
#' autoplot(fit, confidence = TRUE, show_observations = TRUE)
#'
#' segments <- derive_tariff_segments(
#'   fit,
#'   segmentation_penalty = 10,
#'   seed = 1
#' )
#'
#' # Inspect the same effect with the candidate segment boundaries.
#' autoplot(segments, confidence = TRUE, show_observations = TRUE)
#' autoplot(segments, show_segments = FALSE)
#' }
#'
#' @author Martin Haringa
#'
#' @seealso [risk_factor_gam()], [derive_tariff_segments()],
#'   [add_tariff_segments()]
#'
#' @name autoplot.tariff_effect
#' @import ggplot2
NULL

#' @rdname autoplot.tariff_effect
#' @export
autoplot.riskfactor_gam <- function(object, confidence = FALSE,
                                    color_gam = "steelblue",
                                    show_observations = FALSE,
                                    x_stepsize = NULL, size_points = 1,
                                    color_points = "black",
                                    rotate_labels = FALSE,
                                    remove_outliers = NULL,
                                    conf_int = NULL, ...) {
  if (!is.null(conf_int)) {
    lifecycle::deprecate_warn("0.9.0", "autoplot(conf_int)",
                              "autoplot(confidence)")
    confidence <- conf_int
  }

  .plot_risk_factor_curve(
    prediction = object$prediction,
    points = object$data,
    risk_factor = object$x,
    model_type = object$model,
    confidence = confidence,
    color_gam = color_gam,
    show_observations = show_observations,
    x_stepsize = x_stepsize,
    size_points = size_points,
    color_points = color_points,
    rotate_labels = rotate_labels,
    remove_outliers = remove_outliers
  )
}

#' @export
autoplot.risk_factor_gam <- autoplot.riskfactor_gam

#' @export
autoplot.fitgam <- autoplot.riskfactor_gam
