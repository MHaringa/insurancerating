#' @title Check overdispersion of Poisson GLM
#'
#' @description Check Poisson GLM for overdispersion.
#'
#' @param object fitted model of class \code{glm} and family Poisson
#'
#' @return A list with dispersion ratio, chi-squared statistic, and p-value.
#'
#' @details A dispersion ratio larger than one indicates overdispersion, this
#' occurs when the observed variance is higher than the variance of the theoretical
#' model. If the dispersion ratio is close to one, a Poisson model fits well to the data.
#' A p-value < .05 indicates overdispersion. Overdispersion > 2 probably means there
#' is a larger problem with the data: check (again) for outliers, obvious lack of fit.
#' Adopted from \code{performance::check_overdispersion()}.
#'
#' @author Martin Haringa
#'
#' @importFrom stats family
#' @importFrom stats pchisq
#' @importFrom stats df.residual
#'
#' @references \itemize{
#'  \item Bolker B et al. (2017): \href{http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html}{GLMM FAQ.}
#'  }
#'
#' @examples
#' x <- glm(nclaims ~ area, offset = log(exposure), family = poisson(), data = MTPL2)
#' check_overdispersion(x)
#'
#' @export
check_overdispersion <- function(object) {

  if ( stats::family(object)$family != "poisson") {
    stop("Family of object should be Poisson")
  }

  rdf <- stats::df.residual(object)
  rp <- residuals(object, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- stats::pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)

  return(structure(
    list(chisq = Pearson.chisq,
         ratio = prat,
         rdf = rdf,
         p = pval),
    class = "overdispersion"))
}


#' @export
print.overdispersion <- function(x, digits = 3, ...) {
  orig_x <- x

  x$dispersion_ratio <- sprintf("%.*f", digits, x$ratio)
  x$chisq_statistic <- sprintf("%.*f", digits, x$chisq)

  x$p_value <- pval <- round(x$p, digits = digits)
  if (x$p_value < .001) x$p_value <- "< 0.001"

  maxlen <- max(nchar(x$dispersion_ratio), nchar(x$chisq_statistic), nchar(x$p_value))

  cat(sprintf("       dispersion ratio = %s\n", format(x$dispersion_ratio, justify = "right", width = maxlen)))
  cat(sprintf("  Pearson's Chi-Squared = %s\n", format(x$chisq_statistic, justify = "right", width = maxlen)))
  cat(sprintf("                p-value = %s\n\n", format(x$p_value, justify = "right", width = maxlen)))

  if (pval > 0.05) {
    message("No overdispersion detected.")
  } else {
    message("Overdispersion detected.")
  }

  invisible(orig_x)
}
