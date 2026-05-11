#' Check simulation-based model residuals
#'
#' @description
#' Checks whether a fitted model shows systematic residual deviations from the
#' distribution implied by the model. The function uses simulation-based
#' residuals from [DHARMa::simulateResiduals()], which are especially useful for
#' GLMs where classical residual plots can be hard to interpret.
#'
#' @param object A fitted `"glm"` object supported by
#' [DHARMa::simulateResiduals()].
#' @param n_simulations Number of simulations used to generate residuals.
#' Must be a positive whole number. Default is 30.
#'
#' @details
#' In insurance pricing, residual checks are used to assess whether a model is
#' behaving consistently across the portfolio. For example, a Poisson frequency
#' model may fit the average claim count well but still show structure in the
#' residuals because of omitted rating factors, unmodelled heterogeneity,
#' clustering, outliers, or an unsuitable distributional assumption.
#'
#' DHARMa simulates new responses from the fitted model and compares the
#' observed response with those simulations. The resulting scaled residuals are
#' approximately uniformly distributed on `[0, 1]` when the model is correctly
#' specified. This gives a common diagnostic scale for GLMs and related models,
#' where raw residuals are otherwise difficult to compare across different
#' fitted values, exposures, or expected claim amounts.
#'
#' `check_residuals()` returns the scaled residuals, QQ-plot data, and a
#' Kolmogorov-Smirnov p-value for a simple uniformity check. The p-value should
#' be read as a diagnostic signal, not as a pricing decision rule. A low p-value
#' indicates that the residual distribution differs from what the fitted model
#' implies and that the model specification may need review.
#'
#' @return An object of class `"residual_check"` and `"check_residuals"`,
#' which is a list with:
#' \describe{
#'   \item{qq_data}{Data frame with theoretical quantiles (`x`) and observed
#'   scaled residuals (`y`).}
#'   \item{scaled_residuals}{Numeric vector of DHARMa scaled residuals.}
#'   \item{p_value}{P-value from a Kolmogorov-Smirnov test against
#'   `uniform(0, 1)`.}
#' }
#' For backwards compatibility the object also contains the aliases `df` and
#' `p.val`.
#'
#' @author Martin Haringa
#'
#' @importFrom DHARMa simulateResiduals
#' @importFrom stats ks.test
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

  if (!inherits(object, "glm")) {
    stop("`object` must be a fitted glm object.", call. = FALSE)
  }
  if (!is.numeric(n_simulations) ||
      length(n_simulations) != 1L ||
      is.na(n_simulations) ||
      !is.finite(n_simulations) ||
      n_simulations <= 0 ||
      n_simulations != as.integer(n_simulations)) {
    stop("`n_simulations` must be a positive whole number.", call. = FALSE)
  }

  suppressMessages({
    simout <- DHARMa::simulateResiduals(object, n = n_simulations)
  })

  u <- simout$scaledResiduals
  u <- u[!is.na(u)]
  if (length(u) == 0L) {
    stop("No non-missing scaled residuals were returned by DHARMa.",
         call. = FALSE)
  }

  n <- length(u)
  dat <- data.frame(
    x = (seq_len(n)) / (n + 1),
    y = sort(u)
  )

  ts <- tryCatch(
    suppressWarnings(
      stats::ks.test(u, "punif", alternative = "two.sided")
    ),
    error = function(e) NULL
  )

  p_value <- if (!is.null(ts)) ts$p.value else NA_real_

  structure(
    list(
      qq_data = dat,
      scaled_residuals = u,
      p_value = p_value,
      df = dat,
      p.val = p_value
    ),
    class = c("residual_check", "check_residuals")
  )
}

#' @export
print.check_residuals <- function(x, digits = 3, ...) {
  p_value <- if (!is.null(x$p_value)) {
    x$p_value
  } else {
    x$p.val
  }

  cat("Simulation-based residual check\n")
  cat("--------------------------------\n")
  if (is.na(p_value)) {
    cat("Kolmogorov-Smirnov test could not be computed.\n")
  } else {
    cat("Kolmogorov-Smirnov p-value:",
        ifelse(p_value < .001, "< 0.001", round(p_value, digits)),
        "\n")
    if (p_value < 0.05) {
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
#' @param max_points Maximum number of QQ-plot points to display. If the
#' residual check contains more points, an evenly spaced subset is shown. Use
#' `Inf` to plot all points.
#' @param ... Additional arguments passed to [ggplot2::autoplot()].
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @author Martin Haringa
#'
#' @import ggplot2
#' @export
autoplot.check_residuals <- function(object, show_message = TRUE,
                                     max_points = 1000, ...) {
  if (!inherits(object, "check_residuals")) {
    stop("Input must be of class 'check_residuals'.", call. = FALSE)
  }
  if (!isTRUE(show_message) && !identical(show_message, FALSE)) {
    stop("`show_message` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(max_points) ||
      length(max_points) != 1L ||
      is.na(max_points) ||
      max_points <= 0 ||
      (!is.infinite(max_points) && max_points != as.integer(max_points))) {
    stop("`max_points` must be a positive whole number or Inf.",
         call. = FALSE)
  }

  p_value <- if (!is.null(object$p_value)) {
    object$p_value
  } else {
    object$p.val
  }
  dat <- if (!is.null(object$qq_data)) {
    object$qq_data
  } else {
    object$df
  }

  if (isTRUE(show_message) && !is.na(p_value)) {
    if (p_value < 0.05) {
      message(sprintf("Deviations detected (p = %.3f)", p_value))
    } else {
      message(
        sprintf(
          "Residuals consistent with expected distribution (p = %.3f)",
          p_value
        )
      )
    }
  }

  if (is.finite(max_points) && nrow(dat) > max_points) {
    idx <- unique(round(seq(1, nrow(dat), length.out = max_points)))
    dat <- dat[idx, , drop = FALSE]
  }

  subtitle <- if (is.na(p_value)) {
    "KS test could not be computed"
  } else {
    paste0("Kolmogorov-Smirnov p-value: ",
           ifelse(p_value < .001, "< 0.001", signif(p_value, 3)))
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
    ggplot2::theme_minimal() +
    .plot_grid_theme_ir()
}
