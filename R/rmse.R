#' Root Mean Squared Error
#'
#' @description Compute root mean squared error.
#'
#' @param object fitted model
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
#'
#' @examples
#' x <- glm(nclaims ~ area, offset = log(exposure), family = poisson(), data = MTPL2)
#' rmse(x)
#'
#' @export
rmse <- function(object) {
  res <- stats::residuals(object)
  x <- mean(res^2, na.rm = TRUE)
  rmse_val <- sqrt(x)
  return(rmse_val)
}


