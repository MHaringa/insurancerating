
#' @keywords internal
moments <- function(x, dist = c("gamma", "lognormal")) {

  dist <- match.arg(dist)
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  v <- s^2

  if (dist == "gamma") {
    scale <- m ^ 2 / s
    shape <- s / m
    return(list(scale = scale, shape = shape))
  }

  if (dist == "lognormal") {
    meanlog <- log(m ^ 2 / sqrt(v + m ^ 2))
    sdlog <- log(v / (m ^ 2) + 1)
    return(list(meanlog = meanlog, sdlog = sdlog))
  }
}

#' Fit a distribution to truncated severity (loss) data
#'
#' @description `r lifecycle::badge('experimental')` Estimate the original
#' distribution from truncated data. Truncated data arise frequently in
#' insurance studies. It is common that only claims above a certain threshold
#' are known.
#'
#' @param y vector with observations of losses
#' @param dist distribution for severity ("gamma" or "lognormal"). Defaults to
#' "gamma".
#' @param left numeric. Observations below this threshold are not present in the
#' sample.
#' @param right numeric. Observations above this threshold are not present in
#' the sample. Defaults to Inf.
#' @param start list of starting parameters for the algorithm.
#' @param print_initial print attempts for initial parameters.
#'
#' @importFrom stats sd
#' @importFrom utils capture.output
#' @importFrom fitdistrplus fitdist
#'
#' @return fitdist returns an object of class "fitdist"
#' @examples
#' \dontrun{
#' # Original observations for severity
#' set.seed(1)
#' e <- rgamma(1000, scale = 148099.5, shape = 0.4887023)
#'
#' # Truncated data (only claims above 30.000 euros)
#' threshold <- 30000
#' f <- e[e > threshold]
#'
#' library(dplyr)
#' library(ggplot2)
#' data.frame(value = c(e, f),
#' variable = rep(c("Original data", "Only claims above 30.000 euros"),
#'                c(length(e), length(f)))) %>%
#'                filter(value < 5e5) %>%
#'                mutate(value = value / 1000) %>%
#'                ggplot(aes(x = value)) +
#'                geom_histogram(colour = "white") +
#'                facet_wrap(~variable, ncol = 1) +
#'                labs(y = "Number of observations",
#'                     x = "Severity (x 1000 EUR)")
#'
#' # scale = 156259.7 and shape = 0.4588. Close to parameters of original
#' # distribution!
#' x <- fit_truncated_dist(f, left = threshold, dist = "gamma")
#'
#' # Print cdf
#' autoplot(x)
#'
#' # CDF with modifications
#' autoplot(x, print_dig = 5, xlab = "loss", ylab = "cdf", ylim = c(.9, 1))
#'
#' est_scale <- x$estimate[1]
#' est_shape <- x$estimate[2]
#'
#' # Generate data from truncated distribution (between 30k en 20 mln)
#' rg <- rgammat(10, scale = est_scale, shape = est_shape, lower = 3e4,
#'  upper = 20e6)
#'
#' # Calculate quantiles
#' quantile(rg, probs = c(.5, .9, .99, .995))
#' }
#'
#' @author Martin Haringa
#'
#' @export
fit_truncated_dist <- function(y, dist = c("gamma", "lognormal"), left = NULL,
                               right = NULL, start = NULL,
                               print_initial = TRUE) {

  dist <- match.arg(dist)
  x <- moments(y, dist)
  distt <- switch(dist, gamma = "truncated_gamma",
                  lognormal = "truncated_log_normal")
  if (is.null(left)) {
    left <- 0
  }
  if (is.null(right)) {
    right <- Inf
  }
  dtruncated_gamma <- ptruncated_gamma <- dtruncated_log_normal <-
    ptruncated_log_normal <- NULL
  if (dist == "gamma") {
    dtruncated_gamma <<- function(x, scale, shape) {
      dtrunc(x, "gamma", a = left, b = right,
             scale = scale, shape = shape)
    }
    ptruncated_gamma <<- function(q, scale, shape) {
      ptrunc(q, "gamma", a = left, b = right,
             scale = scale, shape = shape)
    }
    if (is.null(start)) {
      sc <- seq(1, x$scale, by = 100)
      sh <- seq(0.01, 1, length.out = 100)
      x_grid <- expand.grid(scale = c(x$scale,
                                      c(rbind(x$scale + sc, x$scale - sc))),
                            shape = c(x$shape,
                                      c(rbind(x$shape + sh, x$shape - sh))))
      start <- list(scale = x_grid$scale, shape = x_grid$shape)
    }
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
    if (is.null(start)) {
      m <- seq(0.1, x$meanlog, by = 0.5)
      sd <- seq(0.01, 1, length.out = 100)
      x_grid <- expand.grid(meanlog = c(x$meanlog,
                                        c(rbind(x$meanlog + m, x$meanlog - m))),
                            sdlog = c(x$sdlog,
                                      c(rbind(x$sdlog + sd, x$sdlog - sd))))
      start <- list(meanlog = x_grid$meanlog, sdlog = x_grid$sdlog)
    }
  }
  for (i in seq_along(start[[1]])) {
    start_e <- lapply(start, "[[", i)
    start_e_chr <- paste0(names(start_e)[1], " = ",
                          as.numeric(start_e[1]), ", and ", names(start_e)[2],
                          " = ", as.numeric(start_e[2]))
    capture.output(out <- tryCatch(expr = {
      fitdistrplus::fitdist(y, distt, method = "mle",
                            lower = c(0, 0), start = start_e)
    }, error = function(e) {
      if (isTRUE(print_initial)) {
        message("initial values: ", start_e_chr,
                ", attempt is not in the interior of the feasible region")
      }
      NULL
    }))
    if (!is.null(out)) {
      if (isTRUE(print_initial)) {
        message("initial values: ", start_e_chr, ", attempt returns:")
      }
      break
    }
  }
  class(out) <- append("truncated_dist", class(out))
  attr(out, "truncated_vec") <- y
  attr(out, "left") <- left
  attr(out, "right") <- right
  return(out)
}


#' Generate data from truncated lognormal distribution
#'
#' @description Random generation for the truncated log normal distribution
#' whose logarithm has mean equal to meanlog and standard deviation equal to
#' sdlog.
#'
#' @param n number of observations
#' @param meanlog mean of the distribution on the log scale
#' @param sdlog standard deviation of the distribution on the log scale
#' @param lower numeric. Observations below this threshold are not present in
#' the sample.
#' @param upper numeric. Observations above this threshold are not present in
#' the sample.
#'
#' @importFrom stats plnorm
#' @importFrom stats runif
#' @importFrom stats qlnorm
#'
#' @author Martin Haringa
#'
#' @return The length of the result is determined by `n`.
#'
#' @export
rlnormt <- function(n, meanlog, sdlog, lower, upper) {
  # www.r-bloggers.com/2020/08/generating-data-from-a-truncated-distribution/
  f_a <- plnorm(lower, meanlog = meanlog, sdlog = sdlog)
  f_b <- plnorm(upper, meanlog = meanlog, sdlog = sdlog)
  u <- runif(n, min = f_a, max = f_b)
  qlnorm(u, meanlog = meanlog, sdlog = sdlog)
}

#' Generate data from truncated gamma distribution
#'
#' @description Random generation for the truncated Gamma distribution with
#' parameters shape and scale.
#'
#' @param n number of observations
#' @param scale scale parameter
#' @param shape shape parameter
#' @param lower numeric. Observations below this threshold are not present in
#' the sample.
#' @param upper numeric. Observations above this threshold are not present in
#' the sample.
#'
#' @importFrom stats pgamma
#' @importFrom stats runif
#' @importFrom stats qgamma
#'
#' @author Martin Haringa
#'
#' @return The length of the result is determined by `n`.
#'
#' @export
rgammat <- function(n, scale = scale, shape = shape, lower, upper) {
  # www.r-bloggers.com/2020/08/generating-data-from-a-truncated-distribution/
  f_a <- pgamma(lower, scale = scale, shape = shape)
  f_b <- pgamma(upper, scale = scale, shape = shape)
  u <- runif(n, min = f_a, max = f_b)
  qgamma(u, scale = scale, shape = shape)
}


#' Automatically create a ggplot for objects obtained from fit_truncated_dist()
#'
#' @description Takes an object produced by `fit_truncated_dist()`, and plots
#' the available input.
#'
#' @param object object univariate object produced by `fit_truncated_dist()`
#' @param geom_ecdf the geometric object to use display the data (point or step)
#' @param xlab the title of the x axis
#' @param ylab the title of the y axis
#' @param ylim two numeric values, specifying the lower limit and the upper
#' limit of the scale
#' @param xlim two numeric values, specifying the left limit and the right
#' limit of the scale
#' @param print_title show title (default to TRUE)
#' @param print_dig number of digits for parameters in title (default 2)
#' @param print_trunc number of digits for truncation values to print
#' @param ... other plotting parameters to affect the plot
#'
#' @import ggplot2
#'
#' @author Martin Haringa
#'
#' @return a ggplot2 object
#'
#' @export
autoplot.truncated_dist <- function(object, geom_ecdf = c("point", "step"),
                                    xlab = NULL, ylab = NULL, ylim = c(0, 1),
                                    xlim = NULL, print_title = TRUE,
                                    print_dig = 2, print_trunc = 2, ...) {

  left <- attr(object, "left")
  right <- attr(object, "right")

  lch <- round(left, print_trunc)
  rch <- round(right, print_trunc)
  lch <- format(round(left, print_trunc), big.mark = " ", scientific = FALSE)

  trunc_bounds <- if (left > 0 && right < Inf) {
    paste0(" (left truncation at ", lch, " and right truncation at ", rch, ") ")
  } else if (left > 0 && right == Inf) {
    paste0(" (left truncation at ", lch, ") ")
  } else if (left == 0 && right < Inf) {
    paste0(" (right truncation at ", rch, ") ")
  } else {
    ""
  }

  ptruncated_gamma <- ptruncated_log_normal <- NULL

  ptruncated_gamma <<- function(q, scale, shape) { # x was q
    ptrunc(q, "gamma", a = left, b = right,
           scale = scale, shape = shape)
  }

  ptruncated_log_normal <<- function(q, meanlog, sdlog) {
    ptrunc(q, "lnorm", a = left, b = right,
           meanlog = meanlog, sdlog = sdlog)
  }

  geom_ecdf <- match.arg(geom_ecdf)

  xvar_vec <- attr(object, "truncated_vec")
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

  distt <- switch(object$distname,
                  truncated_gamma = "gamma distribution",
                  lognormal = "lognormal distribution")

  est <- object$estimate
  est_title <- paste0(names(est)[1], " = ", round(est[1], print_dig), " and ",
                      names(est)[2], " = ", round(est[2], print_dig))

  ggplot2::ggplot(data = df, aes(x = xvar)) +
    ggplot2::stat_ecdf(geom = geom_ecdf) + {
      if (object$distname == "truncated_gamma") {
        ggplot2::stat_function(
          fun = "ptruncated_gamma",
          color = "dodgerblue",
          args = list(shape = est[names(est) == "shape"],
                      scale = est[names(est) == "scale"]))
      }
    } + {
      if (object$distname == "truncated_log_normal") {
        ggplot2::stat_function(
          fun = "ptruncated_log_normal",
          color = "dodgerblue",
          args = list(meanlog = est[names(est) == "meanlog"],
                      sdlog = est[names(est) == "sdlog"]))
      }
    } +
    ggplot2::theme_minimal() + {
      if (isTRUE(print_title)) {
        ggplot2::labs(x = xlab, y = ylab,
                      title = paste0("CDF of truncated ", distt, trunc_bounds),
                      subtitle =  paste0("(", est_title, ")"))
      }
    } +
    ggplot2::coord_cartesian(ylim = ylim, xlim = xlim) +
    ggplot2::scale_x_continuous(
      labels = function(x) format(x, big.mark = " ", scientific = FALSE)
    )
}
