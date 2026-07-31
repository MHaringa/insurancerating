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
#' @seealso [model_performance()], [bootstrap_performance()],
#'   [check_residuals()]
#'
#' @export
rmse <- function(x, data = NULL) {
  res_var <- stats::formula(x)[[2L]]
  resp <- eval(res_var, as.data.frame(data))
  res <- resp - stats::predict(x, data, type = "response")
  sqrt(mean(res^2, na.rm = TRUE))
}


#' Compare fitted GLMs using common performance measures
#'
#' @description
#' Compare one or more fitted GLMs using AIC, BIC and response-scale RMSE.
#' The resulting table provides a concise first comparison of alternative
#' pricing-model specifications fitted to the same portfolio outcome.
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
#' }
#'
#' Lower values are preferred within each measure, but the measures answer
#' different questions. AIC and BIC depend on the model likelihood, whereas
#' RMSE measures error on the response scale. Comparisons are therefore most
#' meaningful when models use the same response, estimation records, weights
#' and offsets.
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
#'   \item{RMSE}{Root mean squared error.}
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
#' model_performance(m1, m2)
#'
#' @seealso [rmse()], [bootstrap_performance()], [check_overdispersion()],
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
