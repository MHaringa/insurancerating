#' Refitting Generalized Linear Models
#'
#' @description `r lifecycle::badge('experimental')`
#'  `refit_glm()` is used to refit generalized linear models, and must be
#'  preceded by `restrict_coef()`.
#'
#' @param x Object of class restricted or of class smooth
#'
#' @author Martin Haringa
#'
#' @importFrom stats glm
#' @importFrom utils modifyList
#'
#' @return Object of class GLM
#'
#' @export
refit_glm <- function(x) {

  .Deprecated("update_glm")

  if (!inherits(x, c("restricted", "smooth"))) {
    stop("Input must be of class restricted or of class smooth", call. = FALSE)
  }

  lst_call <- as.list(x$model_call)
  lst <- list(formula = x$formula_restricted, data = x$data_restricted,
              offset = NULL)
  y <- eval(as.call(modifyList(lst_call, lst)))
  y$call$formula <- lst$formula
  y$call$data <- quote(df_new)

  if (inherits(x, "smooth")) {
    attr(y, "new_rf") <- x[["new_rf"]]
    attr(y, "class") <- append(class(y), "refitsmooth")
  }

  if (inherits(x, "restricted")) {
    attr(y, "new_rf_rst") <- x[["rf_restricted_df"]]
    attr(y, "class") <- append(class(y), "refitrestricted")
  }

  y
}


#' @noRd
rating_factors1 <- function(model, model_data = NULL, exposure = NULL,
                            exponentiate = TRUE, round_exposure = 0) {

  .Deprecated("rating_factors")

  rating_factors(model, model_data = NULL, exposure = NULL,
                exponentiate = TRUE, signif_stars = FALSE,
                round_exposure = 0)
}
