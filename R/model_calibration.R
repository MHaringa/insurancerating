#' Calibrate the overall level of a refined pricing model
#'
#' @description
#' Adjust the overall prediction level of a fitted model returned by [refit()]
#' without re-estimating its relative tariff structure. Calibration is a final
#' model-level operation: all refinement decisions must be completed before
#' calling `calibrate_model()`.
#'
#' @details
#' For a refined GLM with a log link, calibration adds `log(factor)` to the
#' intercept. Consequently, every response-scale prediction is multiplied by
#' `factor`, while all non-intercept coefficients and tariff relativities remain
#' unchanged.
#'
#' The returned object is a copied, internally consistent fitted model. Its
#' coefficients, linear predictors, fitted values, working residuals, deviance
#' and AIC are updated to the calibrated level. The original refined model is
#' not modified. Calibration metadata store the factor, log shift, original and
#' calibrated intercept, creation time and call.
#'
#' ## Refinement and calibration
#'
#' Model refinement changes or constrains the relative tariff structure and is
#' evaluated through `prepare_refinement()`, one or more `add_*()` operations,
#' and [refit()]. Model calibration changes only the final overall level. A
#' calibrated model cannot be calibrated again or used as the starting point
#' for further refinement. Retain the `rating_refinement` specification and
#' recalibrate a newly refitted model if earlier decisions need to be revised.
#'
#' @param model A fitted refined GLM returned by [refit()]. It must inherit from
#'   `refitrestricted` or `refitsmooth` and use a log link.
#' @param factor Positive finite numeric scalar. `1` retains the prediction
#'   level, values above 1 increase it, and values below 1 decrease it.
#'
#' @return A fitted `glm` that also inherits from `calibrated_model`. Attributes
#'   `calibration_factor`, `calibration_original_intercept`,
#'   `calibration_intercept`, `calibration_log_shift`, `calibration_call` and
#'   `calibrated_at` record the calibration.
#'
#' @seealso [refit()], [rating_table()], [add_prediction()],
#'   [audit_refinement()]
#'
#' @examples
#' restrictions <- data.frame(
#'   zip = c(0, 1, 2, 3),
#'   zip_restricted = c(0.90, 1.00, 1.05, 1.10)
#' )
#'
#' mod_initial <- glm(
#'   nclaims ~ zip + offset(log(exposure)),
#'   family = poisson(),
#'   data = MTPL
#' )
#'
#' mod_refined <- mod_initial |>
#'   prepare_refinement() |>
#'   add_restriction(restrictions) |>
#'   refit(intercept_only = TRUE)
#'
#' mod_calibrated <- calibrate_model(mod_refined, factor = 1.05)
#'
#' rating_table(mod_calibrated)
#'
#' data_final <- mod_refined$data |>
#'   add_prediction(
#'     mod_calibrated,
#'     predictions = "net_risk_premium"
#'   )
#'
#' @export
calibrate_model <- function(model, factor) {
  if (inherits(model, "calibrated_model")) {
    stop(
      "This model has already been calibrated. Apply one final total ",
      "calibration factor to the refined model instead of calibrating it ",
      "repeatedly.",
      call. = FALSE
    )
  }
  if (!inherits(model, c("refitrestricted", "refitsmooth"))) {
    stop(
      "`model` must be a fitted refined model returned by `refit()`. ",
      "Complete model refinement before calling `calibrate_model()`.",
      call. = FALSE
    )
  }
  if (!inherits(model, "glm")) {
    stop("`model` must inherit from `glm`.", call. = FALSE)
  }
  if (!is.numeric(factor) || length(factor) != 1L || is.na(factor) ||
      !is.finite(factor) || factor <= 0) {
    stop("`factor` must be one finite numeric value greater than 0.",
         call. = FALSE)
  }
  if (!identical(model$family$link, "log")) {
    stop(
      "`calibrate_model()` currently requires a refined GLM with a log link, ",
      "so calibration can be represented as an intercept shift of ",
      "`log(factor)`.",
      call. = FALSE
    )
  }

  intercept_index <- match("(Intercept)", names(stats::coef(model)))
  if (is.na(intercept_index)) {
    stop("The refined model has no intercept to calibrate.", call. = FALSE)
  }

  calibrated <- model
  original_intercept <- unname(calibrated$coefficients[intercept_index])
  log_shift <- log(factor)
  calibrated_intercept <- original_intercept + log_shift
  calibrated$coefficients[intercept_index] <- calibrated_intercept
  calibrated$linear.predictors <- calibrated$linear.predictors + log_shift
  calibrated$fitted.values <- calibrated$family$linkinv(
    calibrated$linear.predictors
  )

  if (!is.null(calibrated$y)) {
    prior_weights <- calibrated$prior.weights %||%
      rep.int(1, length(calibrated$y))
    mu_eta <- calibrated$family$mu.eta(calibrated$linear.predictors)
    calibrated$residuals <-
      (calibrated$y - calibrated$fitted.values) / mu_eta
    calibrated$weights <- prior_weights * mu_eta^2 /
      calibrated$family$variance(calibrated$fitted.values)
    calibrated$deviance <- sum(calibrated$family$dev.resids(
      calibrated$y,
      calibrated$fitted.values,
      prior_weights
    ))
    calibrated$aic <- calibrated$family$aic(
      calibrated$y,
      rep.int(1, length(calibrated$y)),
      calibrated$fitted.values,
      prior_weights,
      calibrated$deviance
    ) + 2 * calibrated$rank
  }

  attr(calibrated, "calibration_factor") <- as.numeric(factor)
  attr(calibrated, "calibration_original_intercept") <- original_intercept
  attr(calibrated, "calibration_intercept") <- calibrated_intercept
  attr(calibrated, "calibration_log_shift") <- log_shift
  attr(calibrated, "calibration_call") <- match.call()
  attr(calibrated, "calibrated_at") <- Sys.time()
  class(calibrated) <- unique(c("calibrated_model", class(calibrated)))
  calibrated
}


#' @export
#' @noRd
print.calibrated_model <- function(x, ...) {
  cat("Calibrated refined generalized linear model\n\n")
  cat(
    "Calibration factor: ",
    format(attr(x, "calibration_factor"), trim = TRUE),
    "\nOriginal intercept: ",
    format(attr(x, "calibration_original_intercept"), trim = TRUE),
    "\nCalibrated intercept: ",
    format(attr(x, "calibration_intercept"), trim = TRUE),
    "\n\n",
    sep = ""
  )
  refined <- x
  class(refined) <- setdiff(class(refined), "calibrated_model")
  print(refined, ...)
  invisible(x)
}
