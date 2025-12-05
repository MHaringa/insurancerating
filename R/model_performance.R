#' Root Mean Squared Error (RMSE)
#'
#' @description
#' Computes the root mean squared error (RMSE) for a fitted model, defined as the
#' square root of the mean of squared differences between predictions and observed values.
#'
#' @param object A fitted model object (e.g. of class `"glm"`).
#' @param data A data frame containing the variables used in the model. Required
#'   if not already stored in `object`.
#'
#' @details
#' The RMSE indicates the absolute fit of the model to the data.
#' It can be interpreted as the standard deviation of the unexplained variance,
#' and is expressed in the same units as the response variable.
#' Lower values indicate better model fit.
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
#' @export
rmse <- function(object, data = NULL) {
  res_var <- stats::formula(object)[[2L]]
  resp <- eval(res_var, as.data.frame(data))
  res <- resp - stats::predict(object, data, type = "response")
  sqrt(mean(res^2, na.rm = TRUE))
}


#' Performance of fitted GLMs
#'
#' @description
#' Computes model performance indices for one or more fitted GLMs.
#'
#' @param ... One or more objects of class `"glm"`.
#'
#' @details
#' The following indices are reported:
#' \describe{
#'   \item{AIC}{Akaike's Information Criterion.}
#'   \item{BIC}{Bayesian Information Criterion.}
#'   \item{RMSE}{Root mean squared error, computed from observed and predicted values.}
#' }
#'
#' This function is adapted from `performance::model_performance()`.
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


#' Print method for model_performance objects
#'
#' @description
#' Nicely formats and prints the results of [model_performance()], including
#' rounded numeric values, without requiring external packages.
#'
#' @param x An object of class `"model_performance"`.
#' @param digits Number of digits to round numeric columns. Default = 3.
#' @param ... Further arguments passed to or from other methods (ignored).
#'
#' @return Invisibly returns `x`.
#'
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

