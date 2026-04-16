#' Bootstrapped model performance
#'
#' @description
#' Generate `n` bootstrap replicates to compute `n` root mean squared errors (RMSE).
#' This can be used to evaluate the predictive stability of a fitted model.
#'
#' @param model A fitted model object.
#' @param data Data used to fit the model object.
#' @param n Integer. Number of bootstrap replicates. Default = 50.
#' @param frac Fraction of the data used in the training set if cross-validation
#'   is applied. Must be in (0, 1]. Default = 1 (use all data).
#' @param show_progress Logical. Show progress bar during bootstrap iterations.
#'   Default = TRUE.
#' @param rmse_model Optional numeric RMSE of the fitted (original) model.
#'   If NULL (default), it is computed automatically.
#'
#' @details
#' To test the predictive ability of a fitted model it can be helpful to assess
#' the variation in the computed RMSE. The variation is calculated by refitting
#' the model on `n` bootstrap replicates and storing the resulting RMSE values.
#'
#' - If `frac = 1`, each bootstrap sample has the same size as the dataset.
#' - If `frac < 1`, a subset of the data is used as training, and the remainder
#'   as test set (cross-validation).
#'
#' @return An object of class `"bootstrap_performance"`, which is a list with components:
#' \describe{
#'   \item{rmse_bs}{Numeric vector with `n` bootstrap RMSE values.}
#'   \item{rmse_mod}{Root mean squared error for the original fitted model.}
#' }
#'
#' @author Martin Haringa
#'
#' @importFrom stats formula predict update
#'
#' @examples
#' \dontrun{
#' mod1 <- glm(nclaims ~ age_policyholder, data = MTPL,
#'             offset = log(exposure), family = poisson())
#'
#' # Use all records
#' x <- bootstrap_performance(mod1, MTPL, n = 80, show_progress = FALSE)
#' print(x)
#' autoplot(x)
#'
#' # Use 80% of records (cross-validation style)
#' x_frac <- bootstrap_performance(mod1, MTPL, n = 50, frac = .8, show_progress = FALSE)
#' autoplot(x_frac)
#' }
#'
#' @export
bootstrap_performance <- function(model, data, n = 50, frac = 1,
                                  show_progress = TRUE,
                                  rmse_model = NULL) {
  if (frac > 1 || frac <= 0) {
    stop("frac should be in interval (0, 1].", call. = FALSE)
  }

  data <- as.data.frame(data)
  n_sample <- floor(frac * nrow(data))

  if (is.null(rmse_model)) {
    rmse_model <- rmse(model, data)
  }

  rmse_vec <- numeric(n)

  if (isTRUE(show_progress)) {
    pb <- utils::txtProgressBar(max = n, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  for (i in seq_len(n)) {
    if (isTRUE(show_progress)) {
      utils::setTxtProgressBar(pb, i)
    }

    train_rows <- sample(seq_len(nrow(data)), size = n_sample, replace = TRUE)
    train <- data[train_rows, , drop = FALSE]
    test <- data[-train_rows, , drop = FALSE]

    model_train <- stats::update(model, . ~ ., data = train)
    resp_formula <- stats::formula(model_train)[[2L]]

    if (frac < 1) {
      resp <- eval(resp_formula, test)
      pred <- stats::predict(model_train, test, type = "response")
    } else {
      resp <- eval(resp_formula, train)
      pred <- stats::predict(model_train, train, type = "response")
    }

    rmse_vec[i] <- sqrt(mean((resp - pred)^2, na.rm = TRUE))
  }

  structure(
    list(
      rmse_bs = rmse_vec,
      rmse_mod = rmse_model
    ),
    class = "bootstrap_performance"
  )
}

#' @rdname bootstrap_performance
#' @description
#' `bootstrap_rmse()` is deprecated in favour of [bootstrap_performance()].
#'
#' @export
bootstrap_rmse <- function(model, data, n = 50, frac = 1, show_progress = TRUE,
                           rmse_model = NULL) {
  lifecycle::deprecate_warn(
    "0.9.0",
    "bootstrap_rmse()",
    "bootstrap_performance()"
  )

  bootstrap_performance(
    model = model,
    data = data,
    n = n,
    frac = frac,
    show_progress = show_progress,
    rmse_model = rmse_model
  )
}


#' @export
print.bootstrap_performance <- function(x, ...) {
  print(x$rmse_bs)
  invisible(x)
}


#' Coerce bootstrap_performance objects to a vector
#'
#' @description
#' Extracts the bootstrap RMSE values as a numeric vector.
#'
#' @param x An object of class `"bootstrap_performance"`.
#' @param ... Further arguments passed to [as.vector()].
#'
#' @return A numeric vector with the bootstrap RMSE values.
#'
#' @export
as.vector.bootstrap_performance <- function(x, ...) {
  as.vector(x$rmse_bs, ...)
}


#' Autoplot for bootstrap_performance objects
#'
#' @description
#' `autoplot()` method for objects created by [bootstrap_performance()].
#' Produces a histogram and density plot of the bootstrapped RMSE values,
#' with the RMSE of the original fitted model shown as a dashed vertical line.
#' Optionally, 95% quantile bounds are shown as dotted vertical lines.
#'
#' @param object An object of class `"bootstrap_performance"`, produced by
#'   [bootstrap_performance()].
#' @param fill Fill color of the histogram bars. Default = `"steelblue"`.
#' @param color Border color of the histogram bars. Default = `"black"`.
#' @param ... Additional arguments passed to [ggplot2::autoplot()].
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @author Martin Haringa
#'
#' @importFrom stats quantile
#' @importFrom ggplot2 after_stat
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' mod1 <- glm(nclaims ~ age_policyholder, data = MTPL,
#'             offset = log(exposure), family = poisson())
#' x <- bootstrap_performance(mod1, MTPL, n = 100, show_progress = FALSE)
#' autoplot(x)
#' }
#'
#' @export
autoplot.bootstrap_performance <- function(object,
                                           fill = "steelblue",
                                           color = "black",
                                           ...) {
  if (!inherits(object, "bootstrap_performance")) {
    stop("Input must be of class 'bootstrap_performance'.", call. = FALSE)
  }

  rmse_bs <- object$rmse_bs
  rmse_mod <- object$rmse_mod
  dat <- data.frame(x = rmse_bs)

  conf_bounds <- tryCatch(
    stats::quantile(rmse_bs, c(0.025, 0.975)),
    error = function(e) NULL
  )

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      fill = fill,
      color = color,
      bins = 30
    ) +
    ggplot2::geom_density(
      alpha = .3,
      fill = "antiquewhite3",
      color = "grey40"
    ) +
    ggplot2::geom_vline(xintercept = rmse_mod, linetype = 2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Bootstrapped model performance",
      x = "(Simulated) RMSE",
      y = "Density"
    )

  if (!is.null(conf_bounds)) {
    p <- p + ggplot2::geom_vline(xintercept = conf_bounds, linetype = 3)
  }

  p
}


#' @export
print.bootstrap_rmse <- function(x, ...) {
  print.bootstrap_performance(x, ...)
}

#' @export
as.vector.bootstrap_rmse <- function(x, ...) {
  as.vector.bootstrap_performance(x, ...)
}

#' @export
autoplot.bootstrap_rmse <- function(object,
                                    fill = "steelblue",
                                    color = "black",
                                    ...) {
  autoplot.bootstrap_performance(
    object = object,
    fill = fill,
    color = color,
    ...
  )
}
