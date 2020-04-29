#' Root Mean Squared Error
#'
#' @description Compute root mean squared error.
#'
#' @param object fitted model
#' @param data data.frame (defaults to NULL)
#'
#' @details The RMSE is the square root of the average of squared differences
#'   between prediction and actual observation and indicates
#'   the absolute fit of the model to the data. It can be interpreted as the standard
#'   deviation of the unexplained variance, and is in the same units as the
#'   response variable. Lower values indicate better model fit.
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
#' x <- glm(nclaims ~ area, offset = log(exposure), family = poisson(), data = MTPL2)
#' rmse(x, MTPL2)
#'
#' @export
rmse <- function(object, data) {
  res_var <- stats::formula(object)[[2L]]
  resp <- eval(res_var, as.data.frame(data))
  res <- resp - stats::predict(object, data, type = "response")
  sqrt(mean(res^2, na.rm = TRUE))
}




