#' @title Check model residuals
#'
#' @description Detect overall deviations from the expected distribution.
#'
#' @param object a model object
#' @param n_simulations number of simulations (defaults to 30)
#'
#' @details Misspecifications in GLMs cannot reliably be diagnosed with standard
#' residual plots, and GLMs are thus often not as thoroughly checked as LMs.
#' One reason why GLMs residuals are harder to interpret is that the expected
#' distribution of the data changes with the fitted values. As a result,
#' standard residual plots, when interpreted in the same way as for linear
#' models, seem to show all kind of problems, such as non-normality,
#' heteroscedasticity, even if the model is correctly specified.
#' `check_residuals()` aims at solving these problems by creating readily
#' interpretable residuals for GLMs that are standardized to values between
#' 0 and 1, and that can be interpreted as intuitively as residuals for the
#' linear model. This is achieved by a simulation-based approach, similar to the
#' Bayesian p-value or the parametric bootstrap, that transforms the residuals
#' to a standardized scale. This explanation is adopted from
#' [DHARMa::simulateResiduals()].
#'
#' @return Invisibly returns the p-value of the test statistics. A
#' p-value < 0.05 indicates a significant deviation from expected distribution.
#'
#' @author Martin Haringa
#'
#' @importFrom DHARMa simulateResiduals
#' @importFrom insight print_color
#' @importFrom stats approx
#' @importFrom stats ks.test
#'
#' @references Dunn, K. P., and Smyth, G. K. (1996). Randomized quantile
#' residuals. Journal of Computational and Graphical Statistics 5, 1-10.
#' @references Gelman, A. & Hill, J. Data analysis using regression and
#' multilevel/hierarchical models Cambridge University Press, 2006
#' @references Hartig, F. (2020). DHARMa: Residual Diagnostics for Hierarchical
#' (Multi-Level / Mixed) Regression Models. R package version 0.3.0.
#' <https://CRAN.R-project.org/package=DHARMa>
#'
#' @examples
#' \dontrun{
#' m1 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
#' data = MTPL2)
#' check_residuals(m1, n_simulations = 50) %>% autoplot()
#' }
#'
#' @export
check_residuals <- function(object, n_simulations = 30){

  suppressMessages({
    simout <- DHARMa::simulateResiduals(object, n = n_simulations)
  })

  u <- simout$scaledResiduals

  u <- u[!is.na(u)]
  n <- length(u)
  m <- (1:n) / (n+1)
  sx <- sort(m)
  sy <- sort(u)
  lenx <- length(sx)
  leny <- length(sy)
  if (leny < lenx) { sx <- stats::approx(1L:lenx, sx, n = leny)$y }
  if (leny > lenx) { sy <- stats::approx(1L:leny, sy, n = lenx)$y }

  dat <- data.frame(x = sx, y = sy)

  ts <- tryCatch(
    { stats::ks.test(unique(u), "punif", alternative = "two.sided") },
    error = function(e) { NULL }
  )

  p.val <- ts$p.value

  return(structure(list(df = dat,
                        p.val = p.val),
                   class = "check_residuals"))
}

#' @export
print.check_residuals <- function(x, ...) {
  p.val <- x$p.val
  if (p.val < 0.05) {
    insight::print_color(sprintf("Warning: deviations from the expected
                                 distribution detected (p = %.3f).", p.val),
                         "red")
  } else {
    insight::print_color(sprintf("OK: residuals appear as from the expected
                                 distribution (p = %.3f).", p.val), "green")
  }
}


#' Automatically create a ggplot for objects obtained from check_residuals()
#'
#' @description Takes an object produced by `check_residuals()`, and produces a
#' uniform quantile-quantile plot.#'
#'
#' @param object check_residuals object produced by `check_residuals()`
#' @param show_message show output from test (defaults to TRUE)
#' @param ... other plotting parameters to affect the plot
#'
#' @return a ggplot object
#'
#' @author Martin Haringa
#'
#' @import ggplot2
#'
#' @export
autoplot.check_residuals <- function(object, show_message = TRUE, ...) {

  if ( isTRUE(show_message) ){
    p.val <- object$p.val
    if (p.val < 0.05) {
      insight::print_color(sprintf("Warning: deviations from the expected
                                   distribution detected (p = %.3f).", p.val),
                           "red")
    } else {
      insight::print_color(sprintf("OK: residuals appear as from the expected
                                   distribution (p = %.3f).", p.val), "green")
    }
  }
  dat <- object$df

  if ( nrow(dat) > 1000 ){
    dat <- dat[sample(nrow(dat), 1000), ]
  }

  ggplot2::ggplot(data = dat, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(aes(slope = 1, intercept = 0), color = "dodgerblue") +
    ggplot2::labs(x = "Expected",
                  y = "Observed",
                  title = "QQ plot residuals",
                  subtitle = "Dots should be plotted along the line") +
    ggplot2::theme_minimal()
}





