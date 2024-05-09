#' Root Mean Squared Error
#'
#' @description Compute root mean squared error.
#'
#' @param object fitted model
#' @param data data.frame (defaults to NULL)
#'
#' @details The RMSE is the square root of the average of squared differences
#'   between prediction and actual observation and indicates
#'   the absolute fit of the model to the data. It can be interpreted as the
#'   standard deviation of the unexplained variance, and is in the same units
#'   as the response variable. Lower values indicate better model fit.
#'
#' @return numeric value
#'
#' @author Martin Haringa
#'
#' @importFrom stats residuals
#' @importFrom stats predict
#' @importFrom stats formula
#'
#' @examples
#' x <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
#'  data = MTPL2)
#' rmse(x, MTPL2)
#'
#' @export
rmse <- function(object, data) {
  res_var <- stats::formula(object)[[2L]]
  resp <- eval(res_var, as.data.frame(data))
  res <- resp - stats::predict(object, data, type = "response")
  sqrt(mean(res^2, na.rm = TRUE))
}


#' Performance of fitted GLMs
#'
#' @description Compute indices of model performance for (one or more) GLMs.
#'
#' @param ... One or more objects of class `glm`.
#'
#' @details The following indices are computed:
#' \describe{
#'   \item{AIC}{Akaike's Information Criterion}
#'   \item{BIC}{Bayesian Information Criterion}
#'   \item{RMSE}{Root mean squared error}
#' }
#'
#' @importFrom stats AIC
#' @importFrom stats BIC
#'
#' @return data frame
#'
#' @author Martin Haringa
#'
#' @details Adopted from `performance::model_performance()`.
#'
#' @examples
#' m1 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
#'           data = MTPL2)
#' m2 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
#'           data = MTPL2)
#' model_performance(m1, m2)
#'
#' @export
model_performance <- function(...) {

  objects <- list(...)
  object_names <- match.call(expand.dots = FALSE)$`...`

  m <- mapply(function(.x, .y) {
    dat <- .x$data
    out <- list()
    out$AIC <- stats::AIC(.x)
    out$BIC <- stats::BIC(.x)
    out$RMSE <- rmse(.x, dat)
    dat <- as.data.frame(out[!vapply(out, function(i) length(i) == 0 ||
                                       is.null(i) ||
                                       (length(i) == 1 & is.na(i)) ||
                                       any(i == "NULL"),
                                     FUN.VALUE = numeric(1))])
    row.names(dat) <- NULL
    cbind(data.frame(Model = as.character(.y), stringsAsFactors = FALSE), dat)},
    objects, object_names, SIMPLIFY = FALSE)

  dfs <- Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE), m)

  class(dfs) <- c("model_performance", class(dfs))
  return(dfs)
}


#' @importFrom insight export_table
#' @export
print.model_performance <- function(x, digits = 3, ...) {
  orig_x <- x
  cat(color_blue("# Comparison of Model Performance Indices\n\n"))
  x[] <- lapply(x, function(i) {
    if (is.numeric(i)) {
      round(i, digits = digits) } else {
        i
      }
  })
  cat(insight::export_table(x))
  invisible(orig_x)
}
