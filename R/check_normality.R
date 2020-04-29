#' @title Check model for (non-)normality of residuals
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
#' m1 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_normality(m1)
#'
#' # QQ-plot
#' \dontrun{
#' m1_norm <- check_normality(m1)
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

  obj_family <- stats::family(object)$family
  obj_glm <- class(object)[1]

  if ( isTRUE(simulate_residuals) & obj_family == "poisson" & obj_glm == "glm" ){

    simulate_residuals <- create_residuals(simulatedResponse = simulate_poisson_glm(object, n_simulations),
                                           observedResponse = object$data[[all.vars(formula(object))[1]]],
                                           fittedPredictedResponse = predict(object))
    n_res <- length(simulate_residuals$scaledResiduals)
    m_res <- (1:n_res) / (n_res + 1)
    simulated_dat <- stats::na.omit(data.frame(x = sort(m_res), y = sort(simulate_residuals$scaledResiduals)))

    ts <- tryCatch(
      {
        ks.test(unique(simulate_residuals$scaledResiduals), "punif")
      },
      error = function(e) {
        NULL
      }
    )
    insight::print_color("'check_normality()' uses scaled residuals by simulating from the fitted model to test for normality.\nVisual inspection (e.g. Q-Q plots) is preferable for large data sets.\n", "blue")

  } else {
    ts <- tryCatch(
      {
        stats::shapiro.test(stats::rstandard(object)[0:5000])
      },
      error = function(e) {
        NULL
      }
    )

    simulated_dat <- NULL

    if ( length(stats::rstandard(object)) > 5000 ){
      insight::print_color("'check_normality()' only uses the first 5000 data points to test for normality.\nVisual inspection (e.g. Q-Q plots) is preferable for large data sets.\n", "blue")
    }
  }

  if (is.null(ts)) {
    insight::print_color(sprintf("'check_normality()' does not support models of class '%s'.\n", class(object)[1]), "red")
    return(NULL)
  }

  p.val <- ts$p.value

  if (p.val < 0.05) {
    insight::print_color(sprintf("Warning: Non-normality of residuals detected (p = %.3f).\n", p.val), "red")
  } else {
    insight::print_color(sprintf("OK: Residuals appear as normally distributed (p = %.3f).\n", p.val), "green")
  }

  attr(p.val, "object_name") <- deparse(substitute(object), width.cutoff = 500)
  attr(p.val, "simulated_data") <- simulated_dat
  class(p.val) <- unique(c("check_normality", class(p.val)))

  invisible(p.val)
}

#' Automatically create a ggplot for objects obtained from check_normality()
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
  type <- match.arg(type)

  if (is.null(data)) {
    model <- .retrieve_data(object)
  } else {
    model <- data
  }

  if (!is.null(attr(object, "simulated_data", exact = TRUE))){
    simulated_data <- attr(object, "simulated_data", exact = TRUE)
  } else{
    simulated_data <- NULL
  }

  if (type == "qq") {
    if (inherits(model, c("lme", "lmerMod", "merMod", "glmmTMB"))) {
      res_ <- sort(stats::residuals(model), na.last = NA)
    } else {
      res_ <- sort(stats::rstudent(model), na.last = NA)
    }

    fitted_ <- sort(stats::fitted(model), na.last = NA)

    if ( is.null(simulated_data )) {
      dat <- stats::na.omit(data.frame(x = fitted_, y = res_))
    } else{
      dat <- simulated_data
    }

    p1 <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::stat_smooth(method = "lm", size = .8, colour = "dodgerblue") + # "#16a085"
      ggplot2::geom_point(stroke = 0, size = 1) +
      ggplot2::theme_minimal(base_size = 10)

    if ( is.null(simulated_data)) {
      p1 + ggplot2::labs(
          title = "Non-normality of Residuals and Outliers",
          subtitle = "Dots should be plotted along the line",
          y = "(Studentized) Residuals",
          x = "Theoretical Quantiles"
        )
    } else{
      p1 + ggplot2::labs(
        title = "Non-normality of Simulated Residuals and Outliers",
        subtitle = "Dots should be plotted along the line",
        y = "(Studentized) Residuals",
        x = "Theoretical Quantiles"
      )
    }

  } else if (type == "density") {
    r <- stats::residuals(model)
    dat <- as.data.frame(bayestestR::estimate_density(r))

    dat$curve <- stats::dnorm(
      seq(min(dat$x), max(dat$x), length.out = nrow(dat)),
      mean(r),
      stats::sd(r)
    )

    ggplot2::ggplot(dat, ggplot2::aes(x = .data$x)) +
      ggplot2::geom_ribbon(
        mapping = ggplot2::aes(ymin = 0, ymax = .data$y),
        colour = "#7f8c8d",
        fill = "#E7B800",
        alpha = 0.2
      ) +
      ggplot2::geom_line(
        mapping = ggplot2::aes(y = .data$curve),
        colour = "#16a085",
        size = .8
      ) +
      ggplot2::labs(
        x = "Residuals",
        y = "Density",
        title = "Non-Normality of Residuals",
        subtitle = "Distribution should look like a normal curve"
      ) +
      ggplot2::theme_minimal(base_size = 10)
  }
}






