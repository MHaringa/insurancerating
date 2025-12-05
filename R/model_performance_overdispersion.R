#' Check overdispersion of a Poisson GLM
#'
#' @description
#' Tests whether a fitted Poisson regression model is overdispersed using
#' Pearson's chi-squared statistic.
#'
#' @param object A fitted model of class `"glm"` with family Poisson.
#'
#' @return An object of class `"overdispersion"`, which is a list with elements:
#' \describe{
#'   \item{chisq}{Pearson's chi-squared statistic.}
#'   \item{ratio}{Dispersion ratio (chisq / residual df).}
#'   \item{rdf}{Residual degrees of freedom.}
#'   \item{p}{P-value from chi-squared test.}
#' }
#'
#' @details
#' - A dispersion ratio close to 1 indicates a good Poisson fit.
#' - A dispersion ratio > 1 suggests overdispersion.
#' - A p-value < 0.05 indicates significant overdispersion.
#' - A dispersion ratio > 2 usually means a more serious lack of fit (e.g.
#'   outliers or misspecified model).
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

  if (stats::family(object)$family != "poisson") {
    stop("Family of object should be Poisson", call. = FALSE)
  }

  rdf <- stats::df.residual(object)
  rp <- residuals(object, type = "pearson")
  pearson_chisq <- sum(rp^2)
  prat <- pearson_chisq / rdf
  pval <- stats::pchisq(pearson_chisq, df = rdf, lower.tail = FALSE)

  return(structure(
    list(chisq = pearson_chisq,
         ratio = prat,
         rdf = rdf,
         p = pval),
    class = "overdispersion"))
}


#' @export
print.overdispersion <- function(x, digits = 3, ...) {
  orig_x <- x
  disp_ratio <- sprintf("%.*f", digits, x$ratio)
  chisq_stat <- sprintf("%.*f", digits, x$chisq)

  pval <- round(x$p, digits = digits)
  pval_fmt <- if (pval < .001) "< 0.001" else sprintf("%.*f", digits, pval)

  maxlen <- max(nchar(disp_ratio), nchar(chisq_stat), nchar(pval_fmt))

  cat(sprintf("Dispersion ratio = %s\n",
              format(disp_ratio, justify = "right", width = maxlen)))
  cat(sprintf("Pearson's Chi-squared = %s\n",
              format(chisq_stat, justify = "right", width = maxlen)))
  cat(sprintf("p-value = %s\n\n",
              format(pval_fmt, justify = "right", width = maxlen)))

  if (pval > 0.05) {
    message("No overdispersion detected.")
  } else {
    message("Overdispersion detected.")
  }

  invisible(orig_x)
}
