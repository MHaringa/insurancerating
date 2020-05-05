#' @title Check model for (non-)normality of residuals (deprecated function; use 'check_residuals()' instead)
#' @name check_normality
#'
#' @description Check model for (non-)normality of residuals.
#'
#' @param object a model object.
#' @param simulate_residuals create scaled residuals by simulating from the fitted model
#' @param n_simulations number of simulations (defaults to 20)
#'
#' @return Invisibly returns the p-value of the test statistics. A p-value < 0.05 indicates a significant deviation from normal distribution
#'
#' @details \code{check_normality()} calls \code{\link[stats]{shapiro.test}}
#' and checks the standardized residuals for normal distribution. Note that
#' this formal test almost always yields significant results for the distribution
#' of residuals and visual inspections (e.g. Q-Q plots) are preferable.
#' Normality of deviance residuals is in general not expected under a Poisson;
#' and seeing deviance residuals (or any other standard residuals) that differ
#' from a straight line in a qqnorm plot is therefore in general no concern at all.
#' For large counts, QQ is approximately normally distributed, which is expected as
#' the Poisson approaches normality for large means. For small counts, there is a
#' notable deviation from normality.
#'
#' As explained in the vignette of \href{https://CRAN.R-project.org/package=DHARMa}{DHARMa},
#' neither deviance nor Pearson residuals are ideal for diagnosing Poisson models,
#' as they will appear visually inhomogeneous for low count rates, even if the model is entirely correct.
#' Instead, you can use \code{simulate_residuals = TRUE}, which implements the idea of randomized quantile residuals
#' by Dunn and Smyth (1996). This approach is adopted from DHARMa::simulateResiduals.
#'
#' @references Dunn, K. P., and Smyth, G. K. (1996). Randomized quantile residuals. Journal of Computational and Graphical Statistics 5, 1-10.
#' @references Gelman, A. & Hill, J. Data analysis using regression and multilevel/hierarchical models Cambridge University Press, 2006
#'
#' @author Martin Haringa
#'
#' @examples
#'
#' \dontrun{
#' m1 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_normality(m1)
#'
#' m1_norm <- check_normality(m1)
#' # QQ-plot
#' autoplot(m1_norm)
#'
#' # Density plot
#' autoplot(m1_norm, type = "density")
#' }
#'
#' @importFrom stats shapiro.test
#' @importFrom stats rstandard
#' @importFrom stats family
#' @importFrom insight print_color
#'
#' @export
check_normality <- function(object, simulate_residuals = TRUE, n_simulations = 20) {

  .Defunct("check_residuals")
  check_residuals(object = object, n_simulations = n_simulations)
}

#' Automatically create a ggplot for objects obtained from check_normality() (deprecated function; use 'autoplot.check_residuals()' instead)
#'
#' @description Takes an object produced by \code{check_normality()}, and plots the available input.
#'
#' @param object check_normality object produced by \code{check_normality()}
#' @param type character vector, "qq" (default) or "density"
#' @param data data (defaults to NULL)
#' @param ... other plotting parameters to affect the plot
#'
#' @return a ggplot object
#'
#' @author Martin Haringa
#'
#' @import ggplot2
#' @importFrom bayestestR estimate_density
#' @importFrom stats residuals
#' @importFrom stats rstudent
#' @importFrom stats fitted
#' @importFrom stats dnorm
#' @importFrom stats sd
#' @importFrom stats approxfun
#' @importFrom stats formula
#' @importFrom stats ks.test
#' @importFrom stats rpois
#' @importFrom stats runif
#'
#' @export
autoplot.check_normality <- function(object, type = c("qq", "density"), data = NULL, ...) {
  .Defunct("autoplot.check_residuals")

  autoplot.check_residuals(object = object, ...)
}






