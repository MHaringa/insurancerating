#' @noRd
dtrunc <- function(x, spec, a = -Inf, b = Inf, ...) {
  if (a >= b) {
    stop("`a` must be smaller than `b`.", call. = FALSE)
  }

  tt <- rep(0, length(x))
  g <- get(paste0("d", spec), mode = "function")
  G <- get(paste0("p", spec), mode = "function")

  G.a <- G(a, ...)
  G.b <- G(b, ...)

  denom <- G.b - G.a
  if (!is.finite(denom) || denom <= 0) {
    stop("Truncation interval has no positive probability mass.",
         call. = FALSE)
  }

  idx <- x >= a & x <= b
  tt[idx] <- g(x[idx], ...) / denom

  tt
}

#' @noRd
ptrunc <- function(q, spec, a = -Inf, b = Inf, ...) {
  if (a >= b) {
    stop("`a` must be smaller than `b`.", call. = FALSE)
  }

  aa <- rep(a, length(q))
  bb <- rep(b, length(q))
  G <- get(paste0("p", spec), mode = "function")

  G.a <- G(aa, ...)
  G.b <- G(bb, ...)

  denom <- G.b - G.a
  if (any(!is.finite(denom)) || any(denom <= 0)) {
    stop("Truncation interval has no positive probability mass.",
         call. = FALSE)
  }

  qq <- pmax(pmin(q, bb), aa)
  tt <- G(qq, ...) - G.a

  tt / denom
}



#' @noRd
.is_positive_whole_number <- function(x) {
  is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x) &&
    x > 0 &&
    x == as.integer(x)
}

#' @noRd
.is_nonnegative_whole_number <- function(x) {
  is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x) &&
    x >= 0 &&
    x == as.integer(x)
}

#' @noRd
.is_flag <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

#' @noRd
.validate_truncation_bounds <- function(lower_truncation, upper_truncation) {
  if (!is.numeric(lower_truncation) ||
      !is.numeric(upper_truncation) ||
      length(lower_truncation) != 1L ||
      length(upper_truncation) != 1L ||
      is.na(lower_truncation) ||
      is.na(upper_truncation)) {
    stop("`lower_truncation` and `upper_truncation` must be numeric scalars.",
         call. = FALSE)
  }
  if (lower_truncation >= upper_truncation) {
    stop("`lower_truncation` must be smaller than `upper_truncation`.",
         call. = FALSE)
  }
}

#' @noRd
.validate_probability_mass <- function(lower, upper, lower_cdf, upper_cdf,
                                       distribution) {
  if (!is.finite(lower_cdf) || !is.finite(upper_cdf) || lower_cdf >= upper_cdf) {
    stop(
      sprintf(
        "Truncation interval has no positive probability mass for the %s distribution.",
        distribution
      ),
      call. = FALSE
    )
  }
}

#' @noRd
.validate_truncated_start <- function(start_values, distribution) {
  if (!is.list(start_values) || length(start_values) == 0L ||
      is.null(names(start_values)) || any(names(start_values) == "")) {
    stop("`start_values` must be a named list of numeric starting values.",
         call. = FALSE)
  }

  required <- if (distribution == "gamma") {
    c("shape", "scale")
  } else {
    c("meanlog", "sdlog")
  }
  missing <- setdiff(required, names(start_values))
  if (length(missing) > 0L) {
    stop(
      sprintf("`start_values` must contain %s.", paste(missing, collapse = " and ")),
      call. = FALSE
    )
  }

  lens <- vapply(start_values[required], length, integer(1))
  if (any(lens == 0L) || length(unique(lens)) != 1L) {
    stop("All `start_values` entries must have the same positive length.",
         call. = FALSE)
  }
  vals <- unlist(start_values[required], use.names = FALSE)
  if (!is.numeric(vals) || any(!is.finite(vals))) {
    stop("`start_values` must contain finite numeric values.", call. = FALSE)
  }

  positive <- if (distribution == "gamma") {
    required
  } else {
    "sdlog"
  }
  if (any(unlist(start_values[positive], use.names = FALSE) <= 0)) {
    stop("Scale, shape, and standard deviation start values must be positive.",
         call. = FALSE)
  }
}

#' @noRd
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


#' Fit severity distributions to truncated claim data
#'
#' @description `r lifecycle::badge('experimental')`
#' Estimate an underlying claim severity distribution when the observed claims
#' are truncated.
#'
#' @param losses Numeric vector with observed claim severities.
#' @param distribution Severity distribution to fit: `"gamma"` or
#' `"lognormal"`.
#' @param lower_truncation Numeric lower truncation point. Claims at or below
#' this value are assumed not to be present in `losses`. Defaults to `0`.
#' @param upper_truncation Numeric upper truncation point. Claims at or above
#' this value are assumed not to be present in `losses`. Defaults to `Inf`.
#' @param start_values Optional named list of starting values. If `NULL`, a
#' multi-start strategy is used. For a gamma distribution use `shape` and
#' `scale`; for a lognormal distribution use `meanlog` and `sdlog`.
#' @param print_initial Deprecated logical retained for backward compatibility.
#' @param n_variants Controls how many local variations around base starts are
#' used.
#' @param n_shape_grid Number of grid points for gamma shape.
#' @param n_scale_grid Number of grid points for gamma scale.
#' @param show_progress Logical. If `TRUE`, prints periodic progress during the
#' fitting loop.
#' @param show_summary Logical. If `TRUE`, prints a short summary at the end.
#' @param y,dist,left,right,start,trace,report Deprecated argument names kept
#' for backward compatibility.
#'
#' @details
#' In insurance pricing, severity models are often fitted on claim amounts that
#' are not observed over the full range of possible losses. Small claims may be
#' absent because of a deductible, reporting threshold, or data extraction rule.
#' Very large claims may be capped, excluded, or modelled separately as large
#' losses. A standard gamma or lognormal fit on the remaining observed claims
#' treats that truncated sample as if it were complete, which can bias the
#' estimated severity distribution.
#'
#' `fit_truncated_dist()` fits the distribution conditional on the claim being
#' observed within the truncation interval. This means the fitted likelihood
#' uses the density divided by the probability mass between `lower_truncation`
#' and `upper_truncation`. The function is intended for truncation, where
#' claims outside the interval are absent from the data. This differs from
#' censoring, where claims outside a limit are still observed but their exact
#' amount is not known.
#'
#' Observed losses must lie strictly inside the truncation interval. Values
#' outside the interval indicate that the bounds do not describe the data and
#' therefore produce an error.
#'
#' @importFrom fitdistrplus fitdist
#'
#' @return An object of class `c("truncated_dist", "fitdist")`. The object
#' contains the fitted distribution parameters from [fitdistrplus::fitdist()]
#' and additional attributes:
#' \describe{
#'   \item{truncated_vec}{The observed losses used for fitting.}
#'   \item{lower_truncation, upper_truncation}{The truncation bounds.}
#'   \item{fit_attempts}{Metadata for each attempted start combination.}
#'   \item{n_attempts, n_success, n_failed}{Fit attempt counts.}
#'   \item{best_attempt_index}{Index of the selected start combination.}
#' }
#'
#' @examples
#' \dontrun{
#' observed <- MTPL2$amount[MTPL2$amount > 500 & MTPL2$amount < 10000]
#' fit <- fit_truncated_dist(
#'   losses = observed,
#'   distribution = "gamma",
#'   lower_truncation = 500,
#'   upper_truncation = 10000
#' )
#' autoplot(fit)
#' }
#'
#' @export
fit_truncated_dist <- function(losses = NULL,
                               distribution = c("gamma", "lognormal"),
                               lower_truncation = NULL,
                               upper_truncation = NULL,
                               start_values = NULL,
                               print_initial = TRUE,
                               n_variants = 1,
                               n_shape_grid = 8,
                               n_scale_grid = 8,
                               show_progress = FALSE,
                               show_summary = TRUE,
                               y = NULL,
                               dist = NULL,
                               left = NULL,
                               right = NULL,
                               start = NULL,
                               trace = NULL,
                               report = NULL) {

  if (!is.null(y)) {
    if (!is.null(losses)) {
      stop("Use only one of `losses` and deprecated `y`.", call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "fit_truncated_dist(y)",
                              "fit_truncated_dist(losses)")
    losses <- y
  }
  if (!is.null(dist)) {
    lifecycle::deprecate_warn("0.9.0", "fit_truncated_dist(dist)",
                              "fit_truncated_dist(distribution)")
    distribution <- dist
  }
  if (!is.null(left)) {
    if (!is.null(lower_truncation)) {
      stop("Use only one of `lower_truncation` and deprecated `left`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "fit_truncated_dist(left)",
                              "fit_truncated_dist(lower_truncation)")
    lower_truncation <- left
  }
  if (!is.null(right)) {
    if (!is.null(upper_truncation)) {
      stop("Use only one of `upper_truncation` and deprecated `right`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "fit_truncated_dist(right)",
                              "fit_truncated_dist(upper_truncation)")
    upper_truncation <- right
  }
  if (!is.null(start)) {
    if (!is.null(start_values)) {
      stop("Use only one of `start_values` and deprecated `start`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "fit_truncated_dist(start)",
                              "fit_truncated_dist(start_values)")
    start_values <- start
  }
  if (!is.null(trace)) {
    lifecycle::deprecate_warn("0.9.0", "fit_truncated_dist(trace)",
                              "fit_truncated_dist(show_progress)")
    show_progress <- trace
  }
  if (!is.null(report)) {
    lifecycle::deprecate_warn("0.9.0", "fit_truncated_dist(report)",
                              "fit_truncated_dist(show_summary)")
    show_summary <- report
  }
  if (!identical(print_initial, TRUE)) {
    lifecycle::deprecate_warn("0.9.0", "fit_truncated_dist(print_initial)")
  }

  distribution <- match.arg(distribution)

  if (!is.numeric(losses) || length(losses) == 0L) {
    stop("`losses` must be a non-empty numeric vector.", call. = FALSE)
  }

  losses <- losses[is.finite(losses)]

  if (length(losses) == 0L) {
    stop("`losses` must contain at least one finite observation.",
         call. = FALSE)
  }

  if (is.null(lower_truncation)) {
    lower_truncation <- 0
  }
  if (is.null(upper_truncation)) {
    upper_truncation <- Inf
  }

  .validate_truncation_bounds(lower_truncation, upper_truncation)
  if (any(losses <= lower_truncation | losses >= upper_truncation)) {
    stop("All `losses` must lie strictly inside the truncation interval.",
         call. = FALSE)
  }
  if (any(losses <= 0)) {
    stop("`losses` must be strictly positive for gamma and lognormal distributions.",
         call. = FALSE)
  }
  if (!.is_nonnegative_whole_number(n_variants)) {
    stop("`n_variants` must be a non-negative whole number.", call. = FALSE)
  }
  if (!.is_positive_whole_number(n_shape_grid)) {
    stop("`n_shape_grid` must be a positive whole number.", call. = FALSE)
  }
  if (!.is_positive_whole_number(n_scale_grid)) {
    stop("`n_scale_grid` must be a positive whole number.", call. = FALSE)
  }
  if (!.is_flag(show_progress)) {
    stop("`show_progress` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!.is_flag(show_summary)) {
    stop("`show_summary` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(start_values)) {
    .validate_truncated_start(start_values, distribution)
  }

  dtruncated_gamma <- ptruncated_gamma <- NULL
  dtruncated_log_normal <- ptruncated_log_normal <- NULL

  if (distribution == "gamma") {
    dtruncated_gamma <<- function(x, scale, shape) {
      dtrunc(x, "gamma", a = lower_truncation, b = upper_truncation,
             scale = scale, shape = shape)
    }
    ptruncated_gamma <<- function(q, scale, shape) {
      ptrunc(q, "gamma", a = lower_truncation, b = upper_truncation,
             scale = scale, shape = shape)
    }
    distt <- "truncated_gamma"
  }

  if (distribution == "lognormal") {
    dtruncated_log_normal <<- function(x, meanlog, sdlog) {
      dtrunc(x, "lnorm", a = lower_truncation, b = upper_truncation,
             meanlog = meanlog, sdlog = sdlog)
    }
    ptruncated_log_normal <<- function(q, meanlog, sdlog) {
      ptrunc(q, "lnorm", a = lower_truncation, b = upper_truncation,
             meanlog = meanlog, sdlog = sdlog)
    }
    distt <- "truncated_log_normal"
  }

  if (is.null(start_values)) {
    start_list <- get_start_values_truncated(
      y = losses,
      dist = distribution,
      n_variants = n_variants,
      n_shape_grid = n_shape_grid,
      n_scale_grid = n_scale_grid
    )
  } else {
    n_start <- length(start_values[[1]])
    start_list <- lapply(seq_len(n_start), function(i) {
      lapply(start_values, `[[`, i)
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
            losses,
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

      if (isTRUE(show_progress) && (i == 1L || i %% 25L == 0L || i == n_total)) {
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

    if (isTRUE(show_progress) && (i == 1L || i %% 25L == 0L || i == n_total)) {
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

  if (isTRUE(show_summary)) {
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
  attr(out, "truncated_vec") <- losses
  attr(out, "losses") <- losses
  attr(out, "left") <- lower_truncation
  attr(out, "right") <- upper_truncation
  attr(out, "lower_truncation") <- lower_truncation
  attr(out, "upper_truncation") <- upper_truncation
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
  if (!.is_positive_whole_number(n)) {
    stop("`n` must be a positive whole number.", call. = FALSE)
  }
  if (!is.numeric(meanlog) || length(meanlog) != 1L || !is.finite(meanlog)) {
    stop("`meanlog` must be a single finite number.", call. = FALSE)
  }
  if (!is.numeric(sdlog) || length(sdlog) != 1L ||
      !is.finite(sdlog) || sdlog <= 0) {
    stop("`sdlog` must be a single positive finite number.", call. = FALSE)
  }
  .validate_truncation_bounds(lower, upper)

  f_a <- stats::plnorm(lower, meanlog = meanlog, sdlog = sdlog)
  f_b <- stats::plnorm(upper, meanlog = meanlog, sdlog = sdlog)
  .validate_probability_mass(lower, upper, f_a, f_b, "lognormal")

  u <- stats::runif(n, min = f_a, max = f_b)
  stats::qlnorm(u, meanlog = meanlog, sdlog = sdlog)
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
  if (!.is_positive_whole_number(n)) {
    stop("`n` must be a positive whole number.", call. = FALSE)
  }
  if (!is.numeric(shape) || length(shape) != 1L ||
      !is.finite(shape) || shape <= 0) {
    stop("`shape` must be a single positive finite number.", call. = FALSE)
  }
  if (!is.numeric(scale) || length(scale) != 1L ||
      !is.finite(scale) || scale <= 0) {
    stop("`scale` must be a single positive finite number.", call. = FALSE)
  }
  .validate_truncation_bounds(lower, upper)

  f_a <- stats::pgamma(lower, shape = shape, scale = scale)
  f_b <- stats::pgamma(upper, shape = shape, scale = scale)
  .validate_probability_mass(lower, upper, f_a, f_b, "gamma")

  u <- stats::runif(n, min = f_a, max = f_b)
  stats::qgamma(u, shape = shape, scale = scale)
}


#' Plot a fitted truncated severity distribution
#'
#' @description Creates a plot of the empirical cumulative distribution
#' function (ECDF) of the observed truncated claim amounts together with the
#' fitted truncated CDF.
#'
#' @param object An object produced by \code{fit_truncated_dist()}.
#' @param ecdf_geom Character string indicating how to display the empirical
#' CDF. Must be one of `"point"` or `"step"`.
#' @param x_label Title of the x axis. Defaults to `"severity"`.
#' @param y_label Title of the y axis. Defaults to `"cumulative proportion"`.
#' @param y_limits Numeric vector of length 2 specifying y-axis limits.
#' @param x_limits Optional numeric vector of length 2 specifying x-axis limits.
#' @param show_title Logical. If `TRUE`, print title and subtitle.
#' @param digits Integer. Number of digits for parameter estimates in the
#' subtitle.
#' @param truncation_digits Integer. Number of digits used for truncation
#' bounds.
#' @param geom_ecdf,xlab,ylab,ylim,xlim,print_title,print_dig,print_trunc
#' Deprecated argument names kept for backward compatibility.
#' @param ... Currently unused.
#'
#' @details
#' The plot compares the empirical distribution of the observed, truncated
#' claim severities with the fitted distribution conditional on the same
#' truncation interval. This is a visual check of whether the selected severity
#' distribution is plausible for the part of the portfolio that is actually
#' observed.
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
                                    ecdf_geom = c("point", "step"),
                                    x_label = NULL,
                                    y_label = NULL,
                                    y_limits = c(0, 1),
                                    x_limits = NULL,
                                    show_title = TRUE,
                                    digits = 2,
                                    truncation_digits = 2,
                                    geom_ecdf = NULL,
                                    xlab = NULL,
                                    ylab = NULL,
                                    ylim = NULL,
                                    xlim = NULL,
                                    print_title = NULL,
                                    print_dig = NULL,
                                    print_trunc = NULL,
                                    ...) {

  if (!inherits(object, "truncated_dist")) {
    stop("`object` must inherit from class 'truncated_dist'.", call. = FALSE)
  }
  if (!is.null(geom_ecdf)) {
    lifecycle::deprecate_warn("0.9.0", "autoplot(geom_ecdf)",
                              "autoplot(ecdf_geom)")
    ecdf_geom <- geom_ecdf
  }
  if (!is.null(xlab)) {
    lifecycle::deprecate_warn("0.9.0", "autoplot(xlab)",
                              "autoplot(x_label)")
    x_label <- xlab
  }
  if (!is.null(ylab)) {
    lifecycle::deprecate_warn("0.9.0", "autoplot(ylab)",
                              "autoplot(y_label)")
    y_label <- ylab
  }
  if (!is.null(ylim)) {
    lifecycle::deprecate_warn("0.9.0", "autoplot(ylim)",
                              "autoplot(y_limits)")
    y_limits <- ylim
  }
  if (!is.null(xlim)) {
    lifecycle::deprecate_warn("0.9.0", "autoplot(xlim)",
                              "autoplot(x_limits)")
    x_limits <- xlim
  }
  if (!is.null(print_title)) {
    lifecycle::deprecate_warn("0.9.0", "autoplot(print_title)",
                              "autoplot(show_title)")
    show_title <- print_title
  }
  if (!is.null(print_dig)) {
    lifecycle::deprecate_warn("0.9.0", "autoplot(print_dig)",
                              "autoplot(digits)")
    digits <- print_dig
  }
  if (!is.null(print_trunc)) {
    lifecycle::deprecate_warn("0.9.0", "autoplot(print_trunc)",
                              "autoplot(truncation_digits)")
    truncation_digits <- print_trunc
  }
  if (!.is_flag(show_title)) {
    stop("`show_title` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!.is_nonnegative_whole_number(digits)) {
    stop("`digits` must be a non-negative whole number.", call. = FALSE)
  }
  if (!.is_nonnegative_whole_number(truncation_digits)) {
    stop("`truncation_digits` must be a non-negative whole number.",
         call. = FALSE)
  }
  if (!is.numeric(y_limits) || length(y_limits) != 2L ||
      any(!is.finite(y_limits)) || y_limits[1] >= y_limits[2]) {
    stop("`y_limits` must be a numeric vector of length 2 in increasing order.",
         call. = FALSE)
  }
  if (!is.null(x_limits) &&
      (!is.numeric(x_limits) || length(x_limits) != 2L ||
       any(!is.finite(x_limits)) || x_limits[1] >= x_limits[2])) {
    stop("`x_limits` must be NULL or a numeric vector of length 2 in increasing order.",
         call. = FALSE)
  }

  left <- attr(object, "lower_truncation")
  right <- attr(object, "upper_truncation")
  xvar_vec <- attr(object, "losses")
  if (is.null(left)) left <- attr(object, "left")
  if (is.null(right)) right <- attr(object, "right")
  if (is.null(xvar_vec)) xvar_vec <- attr(object, "truncated_vec")

  if (is.null(left) || is.null(right) || is.null(xvar_vec)) {
    stop("`object` does not contain the required truncation attributes.",
         call. = FALSE)
  }

  ecdf_geom <- match.arg(ecdf_geom)

  lch <- format(round(left, truncation_digits), big.mark = " ", scientific = FALSE)
  rch <- format(round(right, truncation_digits), big.mark = " ", scientific = FALSE)

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

  if (is.null(x_limits)) {
    x_limits <- c(0, max(df$xvar, na.rm = TRUE))
  }

  if (is.null(x_label)) {
    x_label <- "severity"
  }

  if (is.null(y_label)) {
    y_label <- "cumulative proportion"
  }

  distt <- switch(
    object$distname,
    truncated_gamma = "gamma distribution",
    truncated_log_normal = "lognormal distribution",
    "distribution"
  )

  est <- object$estimate
  est_title <- paste0(
    names(est)[1], " = ", round(est[1], digits),
    " and ",
    names(est)[2], " = ", round(est[2], digits)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$xvar)) +
    ggplot2::stat_ecdf(geom = ecdf_geom) +
    ggplot2::theme_minimal() +
    ggplot2::coord_cartesian(ylim = y_limits, xlim = x_limits) +
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

  if (isTRUE(show_title)) {
    p <- p +
      ggplot2::labs(
        x = x_label,
        y = y_label,
        title = paste0("CDF of truncated ", distt, trunc_bounds),
        subtitle = paste0("(", est_title, ")")
      )
  } else {
    p <- p + ggplot2::labs(x = x_label, y = y_label)
  }

  p
}
