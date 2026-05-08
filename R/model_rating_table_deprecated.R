#' Include reference group in regression output
#'
#' @param model glm object produced by `glm()`
#' @param model_data Optional data.frame used to create glm object. If `NULL`,
#'   the function tries to use `model$data`.
#' @param exposure Logical or character. If `TRUE` (default), exposure is added
#'   if it can be inferred from the model. If `FALSE`, no exposure is added.
#'   If a character string is supplied, it is interpreted as the exposure column
#'   name.
#' @param exposure_name Optional name for the exposure column in the output.
#' @param colname name of coefficient column
#' @param exponentiate logical indicating whether or not to exponentiate the
#'   coefficient estimates. Defaults to TRUE.
#' @param round_exposure number of digits for exposure (defaults to 0)
#'
#' @description `r lifecycle::badge('deprecated')`
#'
#' Legacy interface. Prefer [rating_table()] for fitted models in the new
#' workflow, but this function remains available.
#'
#' @export
rating_factors2 <- function(model, model_data = NULL, exposure = TRUE,
                            exposure_name = NULL,
                            colname = "estimate",
                            exponentiate = TRUE, round_exposure = 0) {
  lifecycle::deprecate_warn("0.8.0", "rating_factors2()", "rating_table()")

  if (!missing(exposure) && is.symbol(substitute(exposure))) {
    exposure <- deparse(substitute(exposure))
  }

  .rating_table_one_model(
    model,
    model_data = model_data,
    exposure = exposure,
    exposure_output = exposure_name,
    colname = colname,
    exponentiate = exponentiate,
    round_exposure = round_exposure
  )
}

#' @rdname rating_table
#' @export
rating_factors <- function(..., model_data = NULL, exposure = TRUE,
                           exposure_name = NULL,
                           signif_stars = FALSE,
                           exponentiate = TRUE, round_exposure = 0) {
  lifecycle::deprecate_warn("0.8.0", "rating_factors()", "rating_table()")

  if (!missing(exposure) && is.symbol(substitute(exposure))) {
    exposure <- deparse(substitute(exposure))
  }

  rating_table(
    ...,
    model_data = model_data,
    exposure = exposure,
    exposure_output = exposure_name,
    exponentiate = exponentiate,
    significance = signif_stars,
    round_exposure = round_exposure
  )
}
