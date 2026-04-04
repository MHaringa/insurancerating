#' Truncated probability density function
#'
#' @description Computes the probability density function (PDF) of a
#' specified distribution, truncated to the interval \eqn{(a, b)}.
#'
#' The function takes a base distribution (e.g. \code{"norm"}, \code{"gamma"})
#' and returns the corresponding truncated density evaluated at the values in
#' \code{x}. Values outside the truncation interval have density zero.
#'
#' @param x A numeric vector of quantiles at which the truncated density is evaluated.
#' @param spec A character string specifying the distribution name (e.g.
#' \code{"norm"}, \code{"gamma"}, \code{"lnorm"}). The corresponding density
#' function \code{d<spec>} and distribution function \code{p<spec>} must exist.
#' @param a Numeric. Lower truncation bound. Defaults to \code{-Inf}.
#' @param b Numeric. Upper truncation bound. Defaults to \code{Inf}.
#' @param ... Additional arguments passed to the underlying distribution
#' functions (e.g. \code{mean}, \code{sd}, \code{shape}, \code{scale}).
#'
#' @details
#' The truncated density is defined as:
#' \deqn{
#' f_{trunc}(x) = \frac{f(x)}{F(b) - F(a)}, \quad \text{for } x \in (a, b)
#' }
#'
#' and zero outside the truncation interval, where \eqn{f} and \eqn{F} are the
#' density and cumulative distribution function of the original distribution.
#'
#' The function checks that the truncation interval lies within the support of
#' the distribution to avoid numerical issues.
#'
#' @return A numeric vector of the same length as \code{x}, containing the
#' truncated density values.
#'
#' @seealso \code{\link{ptrunc}} for the truncated cumulative distribution function.
#'
#' @keywords internal
dtrunc <- function(x, spec, a = -Inf, b = Inf, ...) {
  if (a >= b) {
    stop("argument a is greater than or equal to b")
  }

  tt <- rep(0, length(x))
  g <- get(paste0("d", spec), mode = "function")
  G <- get(paste0("p", spec), mode = "function")

  G.a <- G(a, ...)
  G.b <- G(b, ...)

  denom <- G.b - G.a
  if (!is.finite(denom) || denom <= 0) {
    return(tt)  # alles 0 --> loglik = -Inf
  }

  idx <- x >= a & x <= b
  tt[idx] <- g(x[idx], ...) / denom

  tt
}

#' Truncated cumulative distribution function
#'
#' @description Computes the cumulative distribution function (CDF) of a
#' specified distribution, truncated to the interval \eqn{(a, b)}.
#'
#' The function takes a base distribution (e.g. \code{"norm"}, \code{"gamma"})
#' and returns the corresponding truncated CDF evaluated at the values in
#' \code{q}. Values outside the truncation interval are mapped to the interval
#' boundaries.
#'
#' @param q A numeric vector of quantiles at which the truncated CDF is evaluated.
#' @param spec A character string specifying the distribution name (e.g.
#' \code{"norm"}, \code{"gamma"}, \code{"lnorm"}). The corresponding CDF
#' function \code{p<spec>} must exist.
#' @param a Numeric. Lower truncation bound. Defaults to \code{-Inf}.
#' @param b Numeric. Upper truncation bound. Defaults to \code{Inf}.
#' @param ... Additional arguments passed to the underlying distribution
#' function (e.g. \code{mean}, \code{sd}, \code{shape}, \code{scale}).
#'
#' @details
#' The truncated CDF is defined as:
#' \deqn{
#' F_{trunc}(q) = \frac{F(\min(\max(q, a), b)) - F(a)}{F(b) - F(a)}
#' }
#'
#' where \eqn{F} is the CDF of the original distribution.
#'
#' The function ensures numerical stability by checking that the truncation
#' interval lies within the support of the distribution.
#'
#' @return A numeric vector of the same length as \code{q}, containing the
#' truncated CDF values.
#'
#' @seealso \code{\link{dtrunc}} for the truncated density function.
#'
#' @keywords internal
ptrunc <- function(q, spec, a = -Inf, b = Inf, ...) {
  if (a >= b) {
    stop("argument a is greater than or equal to b")
  }

  aa <- rep(a, length(q))
  bb <- rep(b, length(q))
  G <- get(paste0("p", spec), mode = "function")

  G.a <- G(aa, ...)
  G.b <- G(bb, ...)

  denom <- G.b - G.a
  if (any(!is.finite(denom)) || any(denom <= 0)) {
    return(rep(NA_real_, length(q)))
  }

  qq <- pmax(pmin(q, bb), aa)
  tt <- G(qq, ...) - G.a

  tt / denom
}



#' @keywords internal
get_start_values_truncated <- function(y,
                                       dist = c("gamma", "lognormal"),
                                       n_variants = 1,
                                       n_shape_grid = 8,
                                       n_scale_grid = 8) {
  dist <- match.arg(dist)

  if (!is.numeric(y) || length(y) == 0) {
    stop("y must be a non-empty numeric vector")
  }

  y <- y[is.finite(y)]

  if (length(y) == 0) {
    stop("y contains no finite observations")
  }

  if (any(y <= 0)) {
    stop("y must be strictly positive for gamma and lognormal")
  }

  m <- mean(y, na.rm = TRUE)
  s <- stats::sd(y, na.rm = TRUE)
  v <- s^2

  starts <- list()

  add_start <- function(pars) {
    vals <- unlist(pars, use.names = FALSE)
    ok <- all(is.finite(vals)) && all(vals > 0)
    if (ok) {
      starts[[length(starts) + 1]] <<- pars
    }
  }

  if (dist == "gamma") {
    shape_mom <- m^2 / v
    scale_mom <- v / m

    add_start(list(scale = scale_mom, shape = shape_mom))

    fit_naive <- tryCatch(
      suppressWarnings(
        fitdistrplus::fitdist(
          y,
          "gamma",
          method = "mle",
          start = list(shape = shape_mom, scale = scale_mom),
          lower = c(1e-8, 1e-8),
          optim.method = "L-BFGS-B"
        )
      ),
      error = function(e) NULL
    )

    if (!is.null(fit_naive)) {
      add_start(as.list(fit_naive$estimate[c("scale", "shape")]))
    }

    mult_local <- switch(
      as.character(n_variants),
      "0" = 1,
      "1" = c(0.5, 0.8, 1, 1.2, 1.5, 2),
      c(0.25, 0.5, 0.8, 1, 1.25, 1.5, 2, 4)
    )

    base_starts <- starts

    for (st in base_starts) {
      for (a in mult_local) {
        for (b in mult_local) {
          add_start(list(
            scale = st$scale * a,
            shape = st$shape * b
          ))
        }
      }
    }

    shape_lower <- max(min(0.05, shape_mom / 20), 1e-8)
    shape_upper <- max(5, shape_mom * 20)
    scale_lower <- max(min(scale_mom / 20, stats::quantile(y, 0.1, na.rm = TRUE) / 10), 1e-8)
    scale_upper <- max(scale_mom * 20, stats::quantile(y, 0.9, na.rm = TRUE) * 2)

    shape_grid <- exp(seq(log(shape_lower), log(shape_upper), length.out = n_shape_grid))
    scale_grid <- exp(seq(log(scale_lower), log(scale_upper), length.out = n_scale_grid))

    for (sh in shape_grid) {
      for (sc in scale_grid) {
        add_start(list(scale = sc, shape = sh))
      }
    }
  }

  if (dist == "lognormal") {
    ly <- log(y)

    meanlog_emp <- mean(ly, na.rm = TRUE)
    sdlog_emp <- stats::sd(ly, na.rm = TRUE)
    add_start(list(meanlog = meanlog_emp, sdlog = sdlog_emp))

    meanlog_mom <- log(m^2 / sqrt(v + m^2))
    sdlog_mom <- sqrt(log(1 + v / m^2))
    add_start(list(meanlog = meanlog_mom, sdlog = sdlog_mom))

    fit_naive <- tryCatch(
      suppressWarnings(
        fitdistrplus::fitdist(y, "lnorm", method = "mle")
      ),
      error = function(e) NULL
    )

    if (!is.null(fit_naive)) {
      add_start(as.list(fit_naive$estimate[c("meanlog", "sdlog")]))
    }

    shift <- switch(
      as.character(n_variants),
      "0" = 0,
      "1" = c(-0.15, 0, 0.15),
      c(-0.4, -0.2, 0, 0.2, 0.4)
    )

    mult_sd <- switch(
      as.character(n_variants),
      "0" = 1,
      "1" = c(0.8, 1, 1.2),
      c(0.6, 0.8, 1, 1.2, 1.5)
    )

    base_starts <- starts

    for (st in base_starts) {
      for (a in shift) {
        for (b in mult_sd) {
          add_start(list(
            meanlog = st$meanlog + a,
            sdlog = st$sdlog * b
          ))
        }
      }
    }
  }

  starts_df <- unique(do.call(
    rbind,
    lapply(starts, function(z) as.data.frame(z, check.names = FALSE))
  ))

  lapply(seq_len(nrow(starts_df)), function(i) {
    as.list(starts_df[i, , drop = FALSE])
  })
}


#' Fit a distribution to truncated severity (loss) data
#'
#' @description `r lifecycle::badge('experimental')` Estimate the original
#' distribution from truncated data.
#'
#' @param y Vector with observations of losses.
#' @param dist Distribution for severity: \code{"gamma"} or
#' \code{"lognormal"}.
#' @param left Numeric. Observations below this threshold are not present in
#' the sample.
#' @param right Numeric. Observations above this threshold are not present in
#' the sample.
#' @param start Optional list of starting values. If \code{NULL}, a
#' multi-start strategy is used.
#' @param print_initial Deprecated logical retained for backward compatibility.
#' @param n_variants Controls how many local variations around base starts are
#' used.
#' @param n_shape_grid Number of grid points for gamma shape.
#' @param n_scale_grid Number of grid points for gamma scale.
#' @param trace Logical. If \code{TRUE}, prints periodic progress during the
#' fitting loop.
#' @param report Logical. If \code{TRUE}, prints a short summary at the end.
#'
#' @importFrom fitdistrplus fitdist
#'
#' @return An object of class \code{c("truncated_dist", "fitdist")} with
#' additional attributes describing the attempted fits.
#' @export
fit_truncated_dist <- function(y,
                               dist = c("gamma", "lognormal"),
                               left = NULL,
                               right = NULL,
                               start = NULL,
                               print_initial = TRUE,
                               n_variants = 1,
                               n_shape_grid = 8,
                               n_scale_grid = 8,
                               trace = FALSE,
                               report = TRUE) {
  dist <- match.arg(dist)

  if (!is.numeric(y) || length(y) == 0) {
    stop("y must be a non-empty numeric vector")
  }

  y <- y[is.finite(y)]

  if (length(y) == 0) {
    stop("y contains no finite observations")
  }

  if (is.null(left)) {
    left <- 0
  }
  if (is.null(right)) {
    right <- Inf
  }

  if (!is.numeric(left) || !is.numeric(right) ||
      length(left) != 1 || length(right) != 1) {
    stop("left and right must be numeric scalars")
  }

  if (left >= right) {
    stop("argument left is greater than or equal to right")
  }

  if (any(y <= left | y >= right)) {
    warning("Some observations lie outside the truncation interval")
  }

  dtruncated_gamma <- ptruncated_gamma <- NULL
  dtruncated_log_normal <- ptruncated_log_normal <- NULL

  if (dist == "gamma") {
    dtruncated_gamma <<- function(x, scale, shape) {
      dtrunc(x, "gamma", a = left, b = right, scale = scale, shape = shape)
    }
    ptruncated_gamma <<- function(q, scale, shape) {
      ptrunc(q, "gamma", a = left, b = right, scale = scale, shape = shape)
    }
    distt <- "truncated_gamma"
  }

  if (dist == "lognormal") {
    dtruncated_log_normal <<- function(x, meanlog, sdlog) {
      dtrunc(x, "lnorm", a = left, b = right,
             meanlog = meanlog, sdlog = sdlog)
    }
    ptruncated_log_normal <<- function(q, meanlog, sdlog) {
      ptrunc(q, "lnorm", a = left, b = right,
             meanlog = meanlog, sdlog = sdlog)
    }
    distt <- "truncated_log_normal"
  }

  if (is.null(start)) {
    start_list <- get_start_values_truncated(
      y = y,
      dist = dist,
      n_variants = n_variants,
      n_shape_grid = n_shape_grid,
      n_scale_grid = n_scale_grid
    )
  } else {
    n_start <- length(start[[1]])
    start_list <- lapply(seq_len(n_start), function(i) {
      lapply(start, `[[`, i)
    })
  }

  fits <- list()
  logliks <- numeric(0)
  fit_attempts <- vector("list", length(start_list))

  n_total <- length(start_list)
  n_success <- 0L
  n_failed <- 0L

  for (i in seq_along(start_list)) {
    start_e <- start_list[[i]]

    fit_i <- suppressWarnings(
      suppressMessages(
        tryCatch(
          fitdistrplus::fitdist(
            y,
            distt,
            method = "mle",
            start = start_e,
            lower = c(1e-8, 1e-8),
            optim.method = "L-BFGS-B"
          ),
          error = function(e) e
        )
      )
    )

    if (inherits(fit_i, "error")) {
      n_failed <- n_failed + 1L
      fit_attempts[[i]] <- list(
        index = i,
        status = "failed",
        start = start_e,
        loglik = NA_real_,
        message = conditionMessage(fit_i)
      )

      if (isTRUE(trace) && (i == 1L || i %% 25L == 0L || i == n_total)) {
        message(sprintf(
          "[%d/%d] processed (%d succeeded, %d failed)",
          i, n_total, n_success, n_failed
        ))
      }
      next
    }

    if (!is.null(fit_i) && is.finite(fit_i$loglik)) {
      n_success <- n_success + 1L
      fits[[length(fits) + 1L]] <- fit_i
      logliks <- c(logliks, fit_i$loglik)

      fit_attempts[[i]] <- list(
        index = i,
        status = "success",
        start = start_e,
        loglik = fit_i$loglik,
        message = NULL
      )
    } else {
      n_failed <- n_failed + 1L
      fit_attempts[[i]] <- list(
        index = i,
        status = "failed",
        start = start_e,
        loglik = NA_real_,
        message = "Fit returned no finite loglik"
      )
    }

    if (isTRUE(trace) && (i == 1L || i %% 25L == 0L || i == n_total)) {
      message(sprintf(
        "[%d/%d] processed (%d succeeded, %d failed)",
        i, n_total, n_success, n_failed
      ))
    }
  }

  if (length(fits) == 0L) {
    stop("No successful fit found")
  }

  best_pos <- which.max(logliks)
  out <- fits[[best_pos]]
  best_attempt_index <- which(vapply(
    fit_attempts,
    function(z) is.list(z) && identical(z$status, "success") &&
      isTRUE(all.equal(z$loglik, out$loglik)),
    logical(1)
  ))[1]

  if (isTRUE(report)) {
    message(sprintf(
      "Tried %d start combinations: %d succeeded, %d failed.",
      n_total, n_success, n_failed
    ))
    message(sprintf(
      "Best fit came from combination %d with loglik = %.2f.",
      best_attempt_index, out$loglik
    ))
  }

  class(out) <- append("truncated_dist", class(out))
  attr(out, "truncated_vec") <- y
  attr(out, "left") <- left
  attr(out, "right") <- right
  attr(out, "fit_attempts") <- fit_attempts
  attr(out, "n_attempts") <- n_total
  attr(out, "n_success") <- n_success
  attr(out, "n_failed") <- n_failed
  attr(out, "best_attempt_index") <- best_attempt_index

  out
}


#' Generate random samples from a truncated lognormal distribution
#'
#' @description Generates random observations from a lognormal distribution
#' truncated to the interval \eqn{(lower, upper)} using inverse transform
#' sampling.
#'
#' @param n Integer. Number of observations to generate.
#' @param meanlog Numeric. Mean of the underlying normal distribution.
#' @param sdlog Numeric. Standard deviation of the underlying normal distribution.
#' @param lower Numeric. Lower truncation bound.
#' @param upper Numeric. Upper truncation bound.
#'
#' @details
#' Random values are generated by sampling from a uniform distribution on the
#' interval \eqn{[F(lower), F(upper)]}, where \eqn{F} is the CDF of the
#' lognormal distribution, and then applying the inverse CDF.
#'
#' This approach ensures that the generated values follow the truncated
#' distribution exactly.
#'
#' The implementation is based on the inverse transform method as described in:
#' \url{https://www.r-bloggers.com/2020/08/generating-data-from-a-truncated-distribution/}
#'
#' @return A numeric vector of length \code{n} containing random draws from the
#' truncated lognormal distribution.
#'
#' @importFrom stats plnorm runif qlnorm
#'
#' @author Martin Haringa
#'
#' @export
rlnormt <- function(n, meanlog, sdlog, lower, upper) {
  if (lower >= upper) {
    stop("lower must be strictly less than upper")
  }

  f_a <- plnorm(lower, meanlog = meanlog, sdlog = sdlog)
  f_b <- plnorm(upper, meanlog = meanlog, sdlog = sdlog)

  u <- runif(n, min = f_a, max = f_b)
  qlnorm(u, meanlog = meanlog, sdlog = sdlog)
}


#' Generate random samples from a truncated gamma distribution
#'
#' @description Generates random observations from a gamma distribution
#' truncated to the interval \eqn{(lower, upper)} using inverse transform
#' sampling.
#'
#' @param n Integer. Number of observations to generate.
#' @param shape Numeric. Shape parameter of the gamma distribution.
#' @param scale Numeric. Scale parameter of the gamma distribution.
#' @param lower Numeric. Lower truncation bound.
#' @param upper Numeric. Upper truncation bound.
#'
#' @details
#' Random values are generated by sampling from a uniform distribution on the
#' interval \eqn{[F(lower), F(upper)]}, where \eqn{F} is the CDF of the gamma
#' distribution, and then applying the inverse CDF.
#'
#' This approach ensures that the generated values follow the truncated
#' distribution exactly.
#'
#' The implementation is based on the inverse transform method as described in:
#' \url{https://www.r-bloggers.com/2020/08/generating-data-from-a-truncated-distribution/}
#'
#' @return A numeric vector of length \code{n} containing random draws from the
#' truncated gamma distribution.
#'
#' @importFrom stats pgamma runif qgamma
#'
#' @author Martin Haringa
#'
#' @export
rgammat <- function(n, shape, scale, lower, upper) {
  if (lower >= upper) {
    stop("lower must be strictly less than upper")
  }

  f_a <- pgamma(lower, shape = shape, scale = scale)
  f_b <- pgamma(upper, shape = shape, scale = scale)

  if (!is.finite(f_a) || !is.finite(f_b) || f_a >= f_b) {
    stop("Invalid truncation interval for gamma distribution")
  }

  u <- runif(n, min = f_a, max = f_b)
  qgamma(u, shape = shape, scale = scale)
}


#' Automatically create a ggplot for objects obtained from fit_truncated_dist()
#'
#' @description Creates a plot of the empirical cumulative distribution function
#' (ECDF) of the observed truncated data together with the fitted truncated CDF.
#'
#' @param object An object produced by \code{fit_truncated_dist()}.
#' @param geom_ecdf Character string indicating how to display the ECDF.
#' Must be one of \code{"point"} or \code{"step"}.
#' @param xlab Title of the x axis. Defaults to \code{"severity"}.
#' @param ylab Title of the y axis. Defaults to \code{"cumulative proportion"}.
#' @param ylim Numeric vector of length 2 specifying y-axis limits.
#' @param xlim Numeric vector of length 2 specifying x-axis limits.
#' @param print_title Logical. If \code{TRUE}, print title and subtitle.
#' @param print_dig Integer. Number of digits for parameter estimates in the subtitle.
#' @param print_trunc Integer. Number of digits used for truncation bounds.
#' @param ... Currently unused.
#'
#' @return A \code{ggplot2} object.
#'
#' @importFrom ggplot2 ggplot aes stat_ecdf stat_function theme_minimal labs
#' @importFrom ggplot2 coord_cartesian scale_x_continuous
#' @importFrom rlang .data
#'
#' @author Martin Haringa
#'
#' @export
autoplot.truncated_dist <- function(object,
                                    geom_ecdf = c("point", "step"),
                                    xlab = NULL,
                                    ylab = NULL,
                                    ylim = c(0, 1),
                                    xlim = NULL,
                                    print_title = TRUE,
                                    print_dig = 2,
                                    print_trunc = 2,
                                    ...) {

  if (!inherits(object, "truncated_dist")) {
    stop("object must inherit from class 'truncated_dist'")
  }

  left <- attr(object, "left")
  right <- attr(object, "right")
  xvar_vec <- attr(object, "truncated_vec")

  if (is.null(left) || is.null(right) || is.null(xvar_vec)) {
    stop("object does not contain the required truncation attributes")
  }

  geom_ecdf <- match.arg(geom_ecdf)

  lch <- format(round(left, print_trunc), big.mark = " ", scientific = FALSE)
  rch <- format(round(right, print_trunc), big.mark = " ", scientific = FALSE)

  trunc_bounds <- if (left > 0 && right < Inf) {
    paste0(" (left truncation at ", lch, " and right truncation at ", rch, ")")
  } else if (left > 0 && right == Inf) {
    paste0(" (left truncation at ", lch, ")")
  } else if (left == 0 && right < Inf) {
    paste0(" (right truncation at ", rch, ")")
  } else {
    ""
  }

  ptruncated_gamma <- NULL
  ptruncated_log_normal <- NULL

  ptruncated_gamma <<- function(q, scale, shape) {
    ptrunc(q, "gamma", a = left, b = right, scale = scale, shape = shape)
  }

  ptruncated_log_normal <<- function(q, meanlog, sdlog) {
    ptrunc(q, "lnorm", a = left, b = right, meanlog = meanlog, sdlog = sdlog)
  }

  df <- data.frame(xvar = xvar_vec)

  if (is.null(xlim)) {
    xlim <- c(0, max(df$xvar, na.rm = TRUE))
  }

  if (is.null(xlab)) {
    xlab <- "severity"
  }

  if (is.null(ylab)) {
    ylab <- "cumulative proportion"
  }

  distt <- switch(
    object$distname,
    truncated_gamma = "gamma distribution",
    truncated_log_normal = "lognormal distribution",
    "distribution"
  )

  est <- object$estimate
  est_title <- paste0(
    names(est)[1], " = ", round(est[1], print_dig),
    " and ",
    names(est)[2], " = ", round(est[2], print_dig)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$xvar)) +
    ggplot2::stat_ecdf(geom = geom_ecdf) +
    ggplot2::theme_minimal() +
    ggplot2::coord_cartesian(ylim = ylim, xlim = xlim) +
    ggplot2::scale_x_continuous(
      labels = function(x) format(x, big.mark = " ", scientific = FALSE)
    )

  if (object$distname == "truncated_gamma") {
    p <- p +
      ggplot2::stat_function(
        fun = ptruncated_gamma,
        color = "dodgerblue",
        args = list(
          shape = unname(est["shape"]),
          scale = unname(est["scale"])
        )
      )
  }

  if (object$distname == "truncated_log_normal") {
    p <- p +
      ggplot2::stat_function(
        fun = ptruncated_log_normal,
        color = "dodgerblue",
        args = list(
          meanlog = unname(est["meanlog"]),
          sdlog = unname(est["sdlog"])
        )
      )
  }

  if (isTRUE(print_title)) {
    p <- p +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        title = paste0("CDF of truncated ", distt, trunc_bounds),
        subtitle = paste0("(", est_title, ")")
      )
  } else {
    p <- p + ggplot2::labs(x = xlab, y = ylab)
  }

  p
}
