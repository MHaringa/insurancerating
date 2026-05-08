#' Check overdispersion of a Poisson claim frequency model
#'
#' @description
#' Tests whether a fitted Poisson GLM shows overdispersion using Pearson's
#' chi-squared statistic.
#'
#' @param object A fitted model of class `"glm"` with family Poisson.
#'
#' @return An object of class `"overdispersion_check"` and `"overdispersion"`,
#' which is a list with elements:
#' \describe{
#'   \item{pearson_chisq}{Pearson's chi-squared statistic.}
#'   \item{dispersion_ratio}{Dispersion ratio, calculated as Pearson's
#'   chi-squared statistic divided by residual degrees of freedom.}
#'   \item{residual_df}{Residual degrees of freedom.}
#'   \item{p_value}{P-value from the chi-squared test.}
#' }
#' For backwards compatibility the object also contains the aliases `chisq`,
#' `ratio`, `rdf`, and `p`.
#'
#' @details
#' In Poisson claim frequency models, the variance is assumed to be equal to the
#' mean. A dispersion ratio above 1 indicates that the observed variation is
#' larger than expected under that assumption. In pricing work this can be a
#' useful diagnostic signal for omitted heterogeneity, clustering, outliers, or
#' model misspecification. It does not automatically mean that the model is
#' unusable.
#'
#' - A dispersion ratio close to 1 is broadly consistent with the Poisson
#'   variance assumption.
#' - A dispersion ratio above 1 suggests overdispersion.
#' - A p-value below 0.05 indicates statistically significant overdispersion.
#'
#' @references
#' Bolker B. et al. (2017). [GLMM FAQ](http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html)
#' See also: `performance::check_overdispersion()`.
#'
#' @author Martin Haringa
#'
#' @examples
#' x <- glm(nclaims ~ area, offset = log(exposure),
#'          family = poisson(), data = MTPL2)
#' check_overdispersion(x)
#'
#' @importFrom stats family pchisq df.residual
#' @export
check_overdispersion <- function(object) {

  if (!inherits(object, "glm")) {
    stop("`object` must be a fitted glm object.", call. = FALSE)
  }

  if (!identical(stats::family(object)$family, "poisson")) {
    stop("`object` must be fitted with a Poisson family.", call. = FALSE)
  }

  rdf <- stats::df.residual(object)
  if (!is.finite(rdf) || rdf <= 0) {
    stop("`object` must have positive residual degrees of freedom.",
         call. = FALSE)
  }

  rp <- stats::residuals(object, type = "pearson")
  pearson_chisq <- sum(rp^2, na.rm = TRUE)
  dispersion_ratio <- pearson_chisq / rdf
  p_value <- stats::pchisq(pearson_chisq, df = rdf, lower.tail = FALSE)

  return(structure(
    list(
      pearson_chisq = pearson_chisq,
      dispersion_ratio = dispersion_ratio,
      residual_df = rdf,
      p_value = p_value,
      chisq = pearson_chisq,
      ratio = dispersion_ratio,
      rdf = rdf,
      p = p_value
    ),
    class = c("overdispersion_check", "overdispersion")))
}


#' @export
print.overdispersion_check <- function(x, digits = 3, ...) {
  orig_x <- x
  dispersion_ratio <- if (!is.null(x$dispersion_ratio)) {
    x$dispersion_ratio
  } else {
    x$ratio
  }
  pearson_chisq <- if (!is.null(x$pearson_chisq)) {
    x$pearson_chisq
  } else {
    x$chisq
  }
  p_value <- if (!is.null(x$p_value)) {
    x$p_value
  } else {
    x$p
  }

  disp_ratio <- sprintf("%.*f", digits, dispersion_ratio)
  chisq_stat <- sprintf("%.*f", digits, pearson_chisq)

  pval_fmt <- if (p_value < .001) {
    "< 0.001"
  } else {
    sprintf("%.*f", digits, p_value)
  }

  maxlen <- max(nchar(disp_ratio), nchar(chisq_stat), nchar(pval_fmt))

  cat(sprintf("Dispersion ratio = %s\n",
              format(disp_ratio, justify = "right", width = maxlen)))
  cat(sprintf("Pearson's Chi-squared = %s\n",
              format(chisq_stat, justify = "right", width = maxlen)))
  cat(sprintf("p-value = %s\n\n",
              format(pval_fmt, justify = "right", width = maxlen)))

  if (p_value > 0.05) {
    message("No overdispersion detected.")
  } else {
    message("Overdispersion detected.")
  }

  invisible(orig_x)
}


#' @export
print.overdispersion <- function(x, digits = 3, ...) {
  print.overdispersion_check(x, digits = digits, ...)
}
