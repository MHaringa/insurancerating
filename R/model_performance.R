#' Performance of fitted GLMs
#'
#' @description Compute indices of model performance for (one or more) GLMs.
#'
#' @param ... One or more objects of class \code{glm}.
#'
#' @details The following indices are computed:
#' \itemize{
#'   \item{\strong{AIC}} {Akaike's Information Criterion, see \code{\link[stats]{AIC}}}
#'   \item{\strong{BIC}} {Bayesian Information Criterion, see \code{\link[stats]{BIC}}}
#'   \item{\strong{RMSE}} {Root mean squared error, \code{\link{rmse}}}
#' }
#'
#' @importFrom stats AIC
#' @importFrom stats BIC
#'
#' @return data frame
#'
#' @author Martin Haringa
#'
#' @exportClass model_performance
#'
#' @details Adopted from \code{performance::model_performance()}.
#'
#' @examples
#' m1 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(), data = MTPL2)
#' m2 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(), data = MTPL2)
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
    dat <- as.data.frame(out[!sapply(out, function(i) length(i) == 0 || is.null(i) || (length(i) == 1 & is.na(i)) || any(i == "NULL"))])
    row.names(dat) <- NULL
    cbind(data.frame(Model = as.character(.y), stringsAsFactors = FALSE), dat)},
    objects, object_names, SIMPLIFY = FALSE)

  dfs <- Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE), m)

  class(dfs) <- c("model_performance", class(dfs))
  return(dfs)
}


#' @importFrom insight format_table
#' @importFrom insight print_color
#' @export
print.model_performance <- function(x, digits = 3, ...) {
  orig_x <- x
  insight::print_color("# Comparison of Model Performance Indices\n\n", "blue")

  x[] <- lapply(x, function(i) { if (is.numeric(i)) { round(i, digits = digits) } else { i } })
  cat(insight::format_table(x))
  invisible(orig_x)
}







