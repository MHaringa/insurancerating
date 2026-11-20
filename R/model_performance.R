#' Calculate response-scale prediction error
#'
#' @description
#' Calculate the root mean squared error (RMSE) between observed outcomes and
#' response-scale predictions from a fitted model. RMSE summarises the typical
#' absolute prediction error in the same unit as the model response.
#'
#' @param x A fitted model object, for example a `"glm"`.
#' @param data Optional data frame on which the observed response and predictions
#'   are evaluated. If `NULL`, the data stored with the fitted model are used.
#'
#' @details
#' RMSE is defined as
#'
#' \deqn{\sqrt{\frac{1}{n}\sum_{i=1}^{n}(y_i-\hat{y}_i)^2}.}
#'
#' In pricing work, RMSE can be used to compare alternative specifications for
#' the same response, portfolio and exposure treatment. Lower values indicate
#' smaller response-scale errors. Because errors are squared, individual large
#' deviations receive relatively high weight. This can be relevant for severity
#' models, but it also makes RMSE sensitive to large claims.
#'
#' RMSE values should not be compared across responses with different units or
#' scales. A value calculated on the estimation data is an in-sample diagnostic,
#' not an estimate of future predictive performance. Use resampling or separate
#' validation data when out-of-sample performance is required, and interpret
#' RMSE together with calibration, residual and distributional diagnostics.
#'
#' @return A numeric value: the root mean squared error.
#'
#' @author Martin Haringa
#'
#' @importFrom stats residuals predict formula model.response model.frame
#'
#' @examples
#' x <- glm(nclaims ~ area, offset = log(exposure),
#'          family = poisson(), data = MTPL2)
#' rmse(x, MTPL2)
#'
#' @seealso [model_performance()], [resample_performance()],
#'   [check_residuals()]
#'
#' @export
rmse <- function(x, data = NULL) {
  res_var <- stats::formula(x)[[2L]]
  resp <- eval(res_var, as.data.frame(data))
  .response_rmse(resp, stats::predict(x, data, type = "response"))
}


.response_rmse <- function(response, prediction) {
  sqrt(mean((response - prediction)^2, na.rm = TRUE))
}


#' Compare GLM fit and predictive error metrics
#'
#' @description
#' Compare GLM fit and predictive error metrics. The table reports AIC, BIC,
#' response-scale RMSE and residual deviance for one or more fitted GLMs.
#'
#' @param ... One or more objects of class `"glm"`.
#'
#' @details
#' The following measures are reported:
#' \describe{
#'   \item{AIC}{Akaike information criterion, balancing likelihood fit and
#'   model complexity.}
#'   \item{BIC}{Bayesian information criterion, applying a stronger
#'   sample-size-dependent complexity penalty.}
#'   \item{RMSE}{Root mean squared error between observed and response-scale
#'   predicted values.}
#'   \item{Residual_Deviance}{Residual deviance of the fitted GLM, as returned
#'   by [stats::deviance()].}
#' }
#'
#' AIC, BIC and residual deviance describe in-sample fit. RMSE measures
#' prediction error on the response scale, comparing observed responses with
#' fitted values from the model. It uses the model's stored data, or the
#' model frame if those data are unavailable. With the same records as in the
#' fit, RMSE is an in-sample measure of prediction error. None of these
#' metrics establishes out-of-sample predictive performance.
#'
#' For a Poisson frequency model with `offset(log(exposure))`, the fitted
#' values are expected claim counts for each observation's exposure. RMSE
#' therefore measures error in claim counts, not claim frequencies per unit
#' exposure.
#'
#' Compare models on the same response, estimation records, weights and
#' offsets. For residual deviance, also use the same model family.
#'
#' The table does not select a pricing model automatically. In actuarial model
#' assessment, statistical fit should be considered together with portfolio
#' calibration, residual behaviour, coefficient stability, exposure by level
#' and the practical interpretability of the resulting tariff structure.
#'
#' The implementation is adapted from `performance::model_performance()`.
#'
#' @return A data frame of class `"model_performance"`, with columns:
#' \describe{
#'   \item{Model}{Name of the model object as passed to the function.}
#'   \item{AIC}{AIC value.}
#'   \item{BIC}{BIC value.}
#'   \item{RMSE}{Response-scale root mean squared error.}
#'   \item{Residual_Deviance}{Residual deviance.}
#' }
#'
#' @author Martin Haringa
#'
#' @importFrom stats AIC BIC
#'
#' @examples
#' m1 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
#'           data = MTPL2)
#' m2 <- glm(nclaims ~ area + premium, offset = log(exposure), family = poisson(),
#'           data = MTPL2)
#' # RMSE is in claim counts for these exposure-offset models.
#' model_performance(m1, m2)
#'
#' @seealso [rmse()], [resample_performance()], [check_overdispersion()],
#'   [check_residuals()]
#'
#' @export
model_performance <- function(...) {
  objects <- list(...)
  object_names <- as.character(match.call(expand.dots = FALSE)$`...`)

  m <- mapply(function(mod, name) {
    # try to get model frame safely
    dat <- tryCatch(mod$data, error = function(e) NULL)
    if (is.null(dat)) {
      dat <- model.frame(mod)
    }

    out <- data.frame(
      Model = name,
      AIC = stats::AIC(mod),
      BIC = stats::BIC(mod),
      RMSE = rmse(mod, dat),
      Residual_Deviance = stats::deviance(mod),
      stringsAsFactors = FALSE
    )
    out
  }, objects, object_names, SIMPLIFY = FALSE)

  dfs <- do.call(rbind, m)
  class(dfs) <- c("model_performance", class(dfs))
  dfs
}


#' @export
print.model_performance <- function(x, digits = 3, ...) {
  orig_x <- x

  # Round numeric columns
  x[] <- lapply(x, function(i) if (is.numeric(i)) round(i, digits) else i)

  # Build a simple table
  header <- names(x)
  rows <- nrow(x)

  # Format columns as character
  x_fmt <- as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)

  # Determine column widths
  widths <- vapply(seq_along(header), function(j) {
    max(nchar(c(header[j], x_fmt[[j]])), na.rm = TRUE)
  }, numeric(1))

  # Print header
  cat("# Comparison of Model Performance Indices\n\n")
  cat(paste(mapply(function(h, w) format(h, width = w, justify = "centre"),
                   header, widths), collapse = " | "), "\n")
  cat(paste(mapply(function(w) paste(rep("-", w), collapse = ""), widths),
            collapse = "-+-"), "\n")

  # Print rows
  for (i in seq_len(rows)) {
    cat(paste(mapply(function(val, w) format(val, width = w, justify = "right"),
                     x_fmt[i, ], widths), collapse = " | "), "\n")
  }

  invisible(orig_x)
}
