#' Check model residuals
#'
#' @description
#' Detect overall deviations of residuals from the expected distribution using
#' a simulation-based approach. Provides standardized residuals that are more
#' interpretable for GLMs than classical residual plots.
#'
#' @param object A fitted model object (e.g. of class `"glm"`).
#' @param n_simulations Number of simulations to generate residuals. Default = 30.
#'
#' @details
#' Misspecifications in GLMs cannot reliably be diagnosed with standard
#' residual plots, because the expected distribution of the data changes with
#' fitted values. `check_residuals()` uses a simulation-based approach
#' (similar to a parametric bootstrap or Bayesian p-value) to generate
#' standardized residuals between 0 and 1, which can be interpreted
#' intuitively like residuals from linear models.
#'
#' This function wraps [DHARMa::simulateResiduals()], adapted for convenience.
#'
#' Note: If all simulations for a data point have the same value (e.g. all
#' zeros), an error may occur (`Error in approxfun: need at least two non-NA values`).
#' Increasing `n_simulations` can help in such cases.
#'
#' @return An object of class `"check_residuals"`, which is a list with:
#' \describe{
#'   \item{df}{Data frame with theoretical quantiles (`x`) and observed residuals (`y`).}
#'   \item{p.val}{P-value from Kolmogorov-Smirnov test against uniform(0,1).}
#' }
#' Invisibly returns the object.
#'
#' @author Martin Haringa
#'
#' @importFrom DHARMa simulateResiduals
#' @importFrom stats approx ks.test
#'
#' @references Dunn, K. P., & Smyth, G. K. (1996). Randomized quantile residuals.
#' *JCGS*, 5, 1–10.
#' @references Gelman, A., & Hill, J. (2006).
#' *Data analysis using regression and multilevel/hierarchical models*.
#' Cambridge University Press.
#' @references Hartig, F. (2020). DHARMa: Residual Diagnostics for Hierarchical
#' (Multi-Level / Mixed) Regression Models. R package version 0.3.0.
#' <https://CRAN.R-project.org/package=DHARMa>
#'
#' @examples
#' \dontrun{
#' m1 <- glm(nclaims ~ area, offset = log(exposure),
#'           family = poisson(), data = MTPL2)
#' cr <- check_residuals(m1, n_simulations = 50)
#' autoplot(cr)
#' }
#'
#' @export
check_residuals <- function(object, n_simulations = 30) {
  suppressMessages({
    simout <- DHARMa::simulateResiduals(object, n = n_simulations)
  })

  u <- simout$scaledResiduals
  u <- u[!is.na(u)]

  n <- length(u)
  m <- (1:n) / (n + 1)
  sx <- sort(m)
  sy <- sort(u)

  lenx <- length(sx)
  leny <- length(sy)

  if (leny < lenx) {
    sx <- stats::approx(1L:lenx, sx, n = leny)$y
  }
  if (leny > lenx) {
    sy <- stats::approx(1L:leny, sy, n = lenx)$y
  }

  dat <- data.frame(x = sx, y = sy)

  ts <- tryCatch(
    stats::ks.test(unique(u), "punif", alternative = "two.sided"),
    error = function(e) NULL
  )

  p.val <- if (!is.null(ts)) ts$p.value else NA_real_

  structure(
    list(df = dat, p.val = p.val),
    class = "check_residuals"
  )
}

#' @export
print.check_residuals <- function(x, digits = 3, ...) {
  cat("Residual check for GLM\n")
  cat("----------------------\n")
  if (is.na(x$p.val)) {
    cat("Kolmogorov-Smirnov test could not be computed.\n")
  } else {
    cat("Kolmogorov-Smirnov p-value:",
        ifelse(x$p.val < .001, "< 0.001", round(x$p.val, digits)),
        "\n")
    if (x$p.val < 0.05) {
      message("Deviations detected: residuals differ from expected distribution.")
    } else {
      message("No significant deviations detected.")
    }
  }
  invisible(x)
}


#' Autoplot for check_residuals objects
#'
#' @description
#' `autoplot()` method for objects created by [check_residuals()].
#' Produces a simulation-based uniform QQ-plot of the residuals, with the
#' Kolmogorov-Smirnov p-value shown in the subtitle.
#' Optionally prints a message about whether deviations are detected.
#'
#' @param object An object of class `"check_residuals"`, produced by [check_residuals()].
#' @param show_message Logical. If TRUE (default), prints a short message based on
#'   the p-value from the KS test.
#' @param ... Additional arguments passed to [ggplot2::autoplot()].
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @author Martin Haringa
#'
#' @import ggplot2
#' @export
autoplot.check_residuals <- function(object, show_message = TRUE, ...) {
  if (!inherits(object, "check_residuals")) {
    stop("Input must be of class 'check_residuals'.", call. = FALSE)
  }

  p.val <- object$p.val
  dat <- object$df

  # print message if requested
  if (isTRUE(show_message) && !is.na(p.val)) {
    if (p.val < 0.05) {
      message(sprintf("\u26A0 Deviations detected (p = %.3f)", p.val))
    } else {
      message(
        sprintf(
          "\u2705 Residuals consistent with expected distribution (p = %.3f)",
          p.val
        )
      )
    }
  }

  # sample if very large
  if (nrow(dat) > 1000) {
    dat <- dat[sample(nrow(dat), 1000), ]
  }

  subtitle <- if (is.na(p.val)) {
    "KS test could not be computed"
  } else {
    paste0("Kolmogorov-Smirnov p-value: ",
           ifelse(p.val < .001, "< 0.001", signif(p.val, 3)))
  }

  ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(color = "steelblue", alpha = 0.7) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "grey40",
                         linetype = 2) +
    ggplot2::labs(
      x = "Theoretical quantiles (Uniform[0,1])",
      y = "Observed residuals",
      title = "Residual QQ-plot",
      subtitle = subtitle
    ) +
    ggplot2::theme_minimal()
}
