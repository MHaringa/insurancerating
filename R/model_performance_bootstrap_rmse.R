#' Bootstrapped model performance
#'
#' @description
#' Generate repeated train/evaluation samples to compute model performance.
#' Currently, the supported metric is root mean squared error (RMSE).
#'
#' @param model A fitted model object.
#' @param data Data used to fit the model object.
#' @param n_resamples Integer. Number of resampling replicates. Default = 50.
#' @param sample_fraction Fraction of the data used in the training sample. Must
#'   be in `(0, 1]`. Default = 1.
#' @param metric Character. Performance metric to compute. Currently only
#'   `"rmse"` is supported.
#' @param sampling Character. Sampling scheme. `"bootstrap"` samples training
#'   rows with replacement and evaluates on out-of-bag rows when
#'   `sample_fraction < 1`.
#'   `"split"` samples training rows without replacement and evaluates on the
#'   remaining rows when `sample_fraction < 1`.
#' @param show_progress Logical. Show progress bar during bootstrap iterations.
#'   Default = TRUE.
#' @param rmse_model Optional numeric RMSE of the fitted (original) model.
#'   If NULL (default), it is computed automatically.
#' @param n,frac Deprecated argument names. Use `n_resamples` and
#'   `sample_fraction` instead.
#'
#' @details
#' To test the predictive stability of a fitted model it can be helpful to
#' assess the variation in a performance metric. The variation is calculated by
#' refitting the model on repeated samples and storing the resulting metric
#' values.
#'
#' - If `sample_fraction = 1`, the metric is evaluated on the sampled training
#'   data.
#' - If `sample_fraction < 1`, the metric is evaluated on rows that were not
#'   used for training.
#'
#' Character columns and factor columns are converted to factors with levels
#' taken from the full input data before resampling. For factor variables used
#' in the model, the training sample is augmented when needed so every observed
#' level is represented at least once. This prevents prediction failures when a
#' level is present in the evaluation data but absent from a particular training
#' sample.
#'
#' @return An object of class `"bootstrap_performance"`, which is a list with components:
#' \describe{
#'   \item{rmse_bs}{Numeric vector with `n_resamples` bootstrap RMSE values.}
#'   \item{rmse_mod}{Root mean squared error for the original fitted model.}
#'   \item{metric}{Metric name.}
#'   \item{sampling}{Sampling scheme.}
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
#' x <- bootstrap_performance(mod1, MTPL, n_resamples = 80,
#'                            show_progress = FALSE)
#' print(x)
#' autoplot(x)
#'
#' # Use 80% of records and evaluate on the remaining records
#' x_frac <- bootstrap_performance(mod1, MTPL, n_resamples = 50,
#'                                 sample_fraction = .8, sampling = "split",
#'                                 show_progress = FALSE)
#' autoplot(x_frac)
#' }
#'
#' @export
bootstrap_performance <- function(model, data, n_resamples = 50,
                                  sample_fraction = 1,
                                  metric = "rmse",
                                  sampling = c("bootstrap", "split"),
                                  show_progress = TRUE,
                                  rmse_model = NULL,
                                  n = NULL,
                                  frac = NULL) {
  if (!is.null(n)) {
    lifecycle::deprecate_warn("0.9.0", "bootstrap_performance(n)",
                              "bootstrap_performance(n_resamples)")
    n_resamples <- n
  }
  if (!is.null(frac)) {
    lifecycle::deprecate_warn("0.9.0", "bootstrap_performance(frac)",
                              "bootstrap_performance(sample_fraction)")
    sample_fraction <- frac
  }

  sampling <- match.arg(sampling)
  metric <- validate_bootstrap_performance_args(
    model = model,
    data = data,
    n_resamples = n_resamples,
    sample_fraction = sample_fraction,
    metric = metric,
    sampling = sampling,
    show_progress = show_progress,
    rmse_model = rmse_model
  )

  data <- preserve_model_levels(as.data.frame(data))
  level_cols <- intersect(names(model$xlevels), names(data))
  n_sample <- floor(sample_fraction * nrow(data))

  if (is.null(rmse_model)) {
    rmse_model <- rmse(model, data)
  }

  rmse_vec <- numeric(n_resamples)

  if (isTRUE(show_progress)) {
    pb <- utils::txtProgressBar(max = n_resamples, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  for (i in seq_len(n_resamples)) {
    if (isTRUE(show_progress)) {
      utils::setTxtProgressBar(pb, i)
    }

    replace <- identical(sampling, "bootstrap")
    train_rows <- sample(seq_len(nrow(data)), size = n_sample, replace = replace)
    train_rows <- ensure_training_levels(train_rows, data, level_cols)
    train <- data[train_rows, , drop = FALSE]
    eval_rows <- if (sample_fraction < 1) {
      setdiff(seq_len(nrow(data)), unique(train_rows))
    } else {
      train_rows
    }
    eval_data <- data[eval_rows, , drop = FALSE]

    if (nrow(eval_data) == 0) {
      stop(
        "No evaluation rows are available for this bootstrap sample. ",
        "Use a smaller `frac`, a larger dataset, or `sampling = 'split'.",
        call. = FALSE
      )
    }

    model_train <- stats::update(model, . ~ ., data = train)
    resp_formula <- stats::formula(model_train)[[2L]]
    resp <- eval(resp_formula, eval_data, parent.frame())
    pred <- stats::predict(model_train, eval_data, type = "response")

    rmse_vec[i] <- sqrt(mean((resp - pred)^2, na.rm = TRUE))
  }

  structure(
    list(
      rmse_bs = rmse_vec,
      rmse_mod = rmse_model,
      metric = metric,
      sampling = sampling,
      n_resamples = n_resamples,
      sample_fraction = sample_fraction,
      n = n_resamples,
      frac = sample_fraction
    ),
    class = "bootstrap_performance"
  )
}


validate_bootstrap_performance_args <- function(model,
                                                data,
                                                n_resamples,
                                                sample_fraction,
                                                metric,
                                                sampling,
                                                show_progress,
                                                rmse_model) {
  if (missing(model) || is.null(model)) {
    stop("`model` must be a fitted model object.", call. = FALSE)
  }
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (nrow(data) == 0) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }
  if (!is.numeric(n_resamples) || length(n_resamples) != 1L ||
      is.na(n_resamples) || n_resamples < 1 ||
      n_resamples != floor(n_resamples)) {
    stop("`n_resamples` must be a positive whole number.", call. = FALSE)
  }
  if (!is.numeric(sample_fraction) || length(sample_fraction) != 1L ||
      !is.finite(sample_fraction) || sample_fraction <= 0 ||
      sample_fraction > 1) {
    stop("`sample_fraction` must be a single number in the interval (0, 1].",
         call. = FALSE)
  }
  if (!is.character(metric) || length(metric) != 1L || is.na(metric)) {
    stop("`metric` must be 'rmse'.", call. = FALSE)
  }
  if (!metric %in% "rmse") {
    stop("`metric` must be 'rmse'.", call. = FALSE)
  }
  if (!sampling %in% c("bootstrap", "split")) {
    stop("`sampling` must be 'bootstrap' or 'split'.", call. = FALSE)
  }
  if (!is.logical(show_progress) || length(show_progress) != 1L ||
      is.na(show_progress)) {
    stop("`show_progress` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(rmse_model) &&
      (!is.numeric(rmse_model) || length(rmse_model) != 1L ||
       !is.finite(rmse_model))) {
    stop("`rmse_model` must be NULL or a single finite number.", call. = FALSE)
  }

  metric
}


preserve_model_levels <- function(data) {
  out <- data

  for (nm in names(out)) {
    if (is.character(out[[nm]])) {
      out[[nm]] <- factor(out[[nm]], levels = unique(out[[nm]]))
    } else if (is.factor(out[[nm]])) {
      out[[nm]] <- factor(out[[nm]], levels = levels(out[[nm]]))
    }
  }

  out
}


ensure_training_levels <- function(train_rows, data, level_cols) {
  if (length(level_cols) == 0) {
    return(train_rows)
  }

  for (nm in level_cols) {
    if (!is.factor(data[[nm]])) {
      next
    }

    train_levels <- unique(as.character(data[[nm]][train_rows]))
    missing_levels <- setdiff(levels(data[[nm]]), train_levels)

    if (length(missing_levels) == 0) {
      next
    }

    extra_rows <- unlist(lapply(missing_levels, function(level) {
      which(as.character(data[[nm]]) == level)[1]
    }), use.names = FALSE)
    train_rows <- c(train_rows, extra_rows[!is.na(extra_rows)])
  }

  train_rows
}

#' Deprecated alias for `bootstrap_performance()`
#'
#' @description
#' `bootstrap_rmse()` is deprecated in favour of [bootstrap_performance()].
#' Objects returned by `bootstrap_rmse()` keep class `"bootstrap_rmse"` for
#' backward compatibility and also inherit from `"bootstrap_performance"`.
#'
#' @inheritParams bootstrap_performance
#' @param n Deprecated. Use `n_resamples` in [bootstrap_performance()] instead.
#' @param frac Deprecated. Use `sample_fraction` in [bootstrap_performance()]
#'   instead.
#'
#' @return See [bootstrap_performance()].
#'
#' @export
#' @keywords internal
bootstrap_rmse <- function(model, data, n = 50, frac = 1,
                           metric = "rmse",
                           sampling = c("bootstrap", "split"),
                           show_progress = TRUE,
                           rmse_model = NULL) {
  lifecycle::deprecate_warn(
    "0.9.0",
    "bootstrap_rmse()",
    "bootstrap_performance()"
  )

  out <- bootstrap_performance(
    model = model,
    data = data,
    n_resamples = n,
    sample_fraction = frac,
    metric = metric,
    sampling = sampling,
    show_progress = show_progress,
    rmse_model = rmse_model
  )
  class(out) <- unique(c("bootstrap_rmse", class(out)))
  out
}


#' @export
print.bootstrap_performance <- function(x, ...) {
  print(x$rmse_bs)
  invisible(x)
}


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
#' @param fill Fill color of the histogram bars. Default = `"#E6E6E6"`.
#' @param color Border color of the histogram bars. Default = `NA`, which
#'   removes bar borders.
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
#' x <- bootstrap_performance(mod1, MTPL, n_resamples = 100,
#'                            show_progress = FALSE)
#' autoplot(x)
#' }
#'
#' @export
autoplot.bootstrap_performance <- function(object,
                                           fill = "#E6E6E6",
                                           color = NA,
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
      alpha = 0.75,
      bins = 30
    ) +
    ggplot2::geom_density(
      alpha = 0.12,
      fill = "#2C7FB8",
      color = "#2C7FB8",
      linewidth = 0.9
    )

  if (!is.null(conf_bounds)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = conf_bounds,
        linetype = 3,
        color = "grey75",
        linewidth = 0.3
      )
  }

  p <- p +
    ggplot2::geom_vline(
      xintercept = rmse_mod,
      linetype = 2,
      color = "#F28E2B",
      linewidth = 0.7
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.06))
    ) +
    ggplot2::theme_minimal() +
    .plot_grid_theme_ir() +
    ggplot2::labs(
      title = "Bootstrapped model performance",
      x = "(Simulated) RMSE",
      y = "Density"
    )

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
                                    fill = "#E6E6E6",
                                    color = NA,
                                    ...) {
  autoplot.bootstrap_performance(
    object = object,
    fill = fill,
    color = color,
    ...
  )
}
