#' Assess predictive performance across resampled datasets
#'
#' @description
#' Refit a pricing model on repeated samples to assess variability in predictive
#' performance. Unlike [model_performance()], which compares fitted models,
#' this function refits the model at each replicate and evaluates it on rows
#' selected by the resampling design.
#'
#' @param model A fitted model object that can be updated on resampled data.
#' @param data Data frame containing the model response and predictors.
#' @param n_resamples Positive whole number. Number of resampling replicates.
#'   Default is 50.
#' @param sample_fraction Fraction of the data used in the training sample. Must
#'   be in `(0, 1]`. Default is 1.
#' @param metric Legacy argument that accepts only `"rmse"`. Use `metrics`
#'   to select metrics.
#' @param metrics Character vector of distinct metrics in the desired output
#'   order: `"rmse"` (default), `"mae"`, or `"deviance"`.
#' @param sampling Character string. Use `"bootstrap"` (default) to sample
#'   training rows with replacement, `"split"` to sample without replacement,
#'   or `"rolling"` for expanding-window temporal validation.
#' @param method Alternative name for `sampling`. Supply only one of the two.
#' @param time For rolling validation, name of a column containing numeric
#'   periods, dates, date-times or an ordered factor. Missing periods are not
#'   allowed.
#' @param initial For rolling validation, number of distinct periods in the
#'   first training window.
#' @param horizon For rolling validation, number of subsequent distinct
#'   periods evaluated at each origin. Default is 1.
#' @param show_progress Logical. Show a progress bar during resampling.
#'   Default is `TRUE`.
#' @param rmse_model Optional finite numeric RMSE for the original fitted model.
#'   If `NULL` and RMSE is selected, it is calculated from `model` and `data`.
#' @param n,frac Deprecated argument names. Use `n_resamples` and
#'   `sample_fraction` instead.
#'
#' @details
#' ## Resampling design
#'
#' - With `sampling = "bootstrap"` and `sample_fraction = 1`, scores are
#'   evaluated on the resampled training rows (in sample, including duplicates).
#' - With `sampling = "bootstrap"` and `sample_fraction < 1`, scores are
#'   evaluated on out-of-bag rows (out of sample for each replicate).
#' - With `sampling = "split"` and `sample_fraction < 1`, scores are evaluated
#'   on the held-out rows of each split (out of sample for each replicate).
#'   At `sample_fraction = 1`, all rows are used for both fitting and evaluation.
#'
#' Character columns and factor columns are converted to factors with levels
#' taken from the full input data before resampling. For factor variables used
#' in the model, the training sample is augmented when needed so every observed
#' level is represented at least once. This prevents prediction failures when a
#' level is present in the evaluation data but absent from a particular training
#' sample. If this leaves no evaluation rows, use a smaller `sample_fraction`.
#'
#' ## Rolling-origin validation
#'
#' With `sampling = "rolling"`, fit the GLM to the earliest `initial` periods,
#' score the next `horizon` periods, then advance the origin by one observed
#' period at a time. Training and test periods never overlap within a window,
#' but test windows can overlap across origins when `horizon > 1`.
#' The number of windows follows from the available periods, not `n_resamples`
#' or `sample_fraction`.
#'
#' Supply an unrestricted [stats::glm()] as the model specification. It is
#' fitted on each training window. Transformations such as `scale()` learn
#' their parameters
#' from the training rows. Precomputed predictors must not use future data.
#' Supply weights and offsets using columns in `data`, and filter any intended
#' subset before calling the function. Observation-level starting values are
#' not supported. A factor level unseen in training produces an error; future
#' observations are not added to the training window.
#'
#' Rolling validation assesses a specification repeatedly fitted to history.
#' For temporal calibration of one fixed fitted model, use
#' [check_calibration()] with `by` set to the period column.
#'
#' ## Metrics and interpretation
#'
#' RMSE (root mean squared error) and MAE (mean absolute error) are unweighted
#' summaries of response-scale errors. For an exposure-offset Poisson model
#' these are errors in claim counts, not claim frequency per unit exposure.
#' For a binomial GLM, the errors are on the probability scale: two-column
#' responses are converted to proportions, and factor responses use the
#' fitted model's reference level to define zero.
#' Predictive deviance sums the fitted GLM family's deviance contributions on
#' the evaluation rows, using those rows' response-scale predictions (including
#' offsets) and observation weights. Specify weights and offsets using columns
#' in `data`, rather than separate row-aligned vectors, so the model can be
#' refitted and evaluated on each sample. Total deviance can vary with the
#' number and weight of evaluation rows as well as with prediction quality.
#' On the original estimation rows with the same predictions and weights, it
#' agrees with the residual deviance reported by [model_performance()].
#'
#' A wide score distribution indicates sensitivity to the sampled rows. Even
#' with held-out rows, resampling the observed portfolio is not independent
#' validation on a future or external portfolio.
#'
#' @return For rolling validation, a data frame with one row per origin:
#'   `iteration`, `train_start`, `train_end`, `test_start`, `test_end`,
#'   `n_train`, `n_test` and the requested metric columns. Counts refer to the
#'   input rows in each window. Scores are calculated on the test rows;
#'   binomial prediction errors use proportions for grouped responses.
#'
#'   For random resampling, a list of class `"bootstrap_performance"`. For RMSE
#'   alone, the original structure is unchanged: `rmse_bs` contains the scores,
#'   `rmse_mod` contains the original model's RMSE, and `metric`, `sampling`,
#'   `n_resamples`, `sample_fraction`, `n` and `frac` describe the run.
#'   When another metric is selected, `scores` is a data frame with one row
#'   per replicate and a column for each requested metric, and `original_scores`
#'   is a named numeric vector for the original model on `data`.
#'   `metric` records the selected metric names; `rmse_bs` and `rmse_mod` are
#'   `NULL` if RMSE was not selected. For these results, `as.vector()` returns
#'   scores for the first requested metric.
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
#' x <- resample_performance(mod1, MTPL, n_resamples = 80,
#'                           show_progress = FALSE)
#' print(x)
#' autoplot(x)
#'
#' # Use 80% of records and evaluate on the remaining records
#' x_frac <- resample_performance(mod1, MTPL, n_resamples = 50,
#'                                sample_fraction = .8, sampling = "split",
#'                                show_progress = FALSE,
#'                                metrics = c("rmse", "mae", "deviance"))
#' head(x_frac$scores)
#' autoplot(x_frac, metric = "deviance")
#' }
#'
#' # Illustrative years; MTPL2 does not contain real policy dates.
#' portfolio <- as.data.frame(MTPL2)
#' portfolio$year <- rep(2020:2025, length.out = nrow(portfolio))
#' model <- glm(nclaims ~ factor(area) + offset(log(exposure)),
#'              family = poisson(), data = portfolio)
#' resample_performance(
#'   model, portfolio, sampling = "rolling", time = "year", initial = 3,
#'   metrics = c("rmse", "mae", "deviance"), show_progress = FALSE
#' )
#'
#' @seealso [rmse()], [model_performance()],
#'   [autoplot.bootstrap_performance()]
#'
#' @export
resample_performance <- function(model, data, n_resamples = 50,
                                  sample_fraction = 1,
                                  metric = "rmse",
                                  sampling = c("bootstrap", "split"),
                                  show_progress = TRUE,
                                  rmse_model = NULL,
                                  n = NULL,
                                  frac = NULL,
                                  metrics = "rmse",
                                  method = NULL,
                                  time = NULL,
                                  initial = NULL,
                                  horizon = 1) {
  if (!is.null(method)) {
    if (!missing(sampling)) {
      stop("Supply either `sampling` or `method`, not both.", call. = FALSE)
    }
    sampling <- method
  }
  if (identical(sampling, c("bootstrap", "split"))) sampling <- "bootstrap"
  sampling <- match.arg(sampling, c("bootstrap", "split", "rolling"))
  rolling <- identical(sampling, "rolling")
  if (rolling) {
    if (!missing(n_resamples) || !missing(sample_fraction) ||
        !is.null(n) || !is.null(frac) || !is.null(rmse_model)) {
      stop("Rolling validation determines windows from periods; do not supply ",
           "`n_resamples`, `sample_fraction`, `n`, `frac` or `rmse_model`.",
           call. = FALSE)
    }
  } else if (!is.null(time) || !is.null(initial) || !missing(horizon)) {
    stop("`time`, `initial` and `horizon` are only used for rolling validation.",
         call. = FALSE)
  }

  if (!is.null(n)) {
    lifecycle::deprecate_warn("0.9.0", "resample_performance(n)",
                              "resample_performance(n_resamples)")
    n_resamples <- n
  }
  if (!is.null(frac)) {
    lifecycle::deprecate_warn("0.9.0", "resample_performance(frac)",
                              "resample_performance(sample_fraction)")
    sample_fraction <- frac
  }

  if (!rolling) {
    validate_bootstrap_performance_args(
      model = model,
      data = data,
      n_resamples = n_resamples,
      sample_fraction = sample_fraction,
      metric = metric,
      sampling = sampling,
      show_progress = show_progress,
      rmse_model = rmse_model
    )
  }
  if (!is.character(metrics) || !is.null(dim(metrics)) || !length(metrics) ||
      anyNA(metrics) || anyDuplicated(metrics) ||
      !all(metrics %in% c("rmse", "mae", "deviance"))) {
    stop("`metrics` must contain distinct values from 'rmse', 'mae', ",
         "and 'deviance'.", call. = FALSE)
  }
  metrics <- unname(metrics)
  if (rolling) {
    return(.resample_rolling(model, data, time, initial, horizon, metrics, show_progress))
  }

  data <- preserve_model_levels(as.data.frame(data))
  level_cols <- intersect(names(model$xlevels), names(data))
  n_sample <- floor(sample_fraction * nrow(data))

  if ("rmse" %in% metrics) {
    if (is.null(rmse_model)) rmse_model <- rmse(model, data)
  } else {
    rmse_model <- NULL
  }

  scores <- matrix(NA_real_, n_resamples, length(metrics),
                   dimnames = list(NULL, metrics))
  original_scores <- stats::setNames(numeric(length(metrics)), metrics)
  if ("rmse" %in% metrics) original_scores[["rmse"]] <- rmse_model
  extra_metrics <- setdiff(metrics, "rmse")
  if (length(extra_metrics)) {
    original_scores[extra_metrics] <- .resample_scores(
      model, data, stats::predict(model, data, type = "response"), extra_metrics
    )
  }

  if (isTRUE(show_progress)) {
    pb <- utils::txtProgressBar(max = n_resamples, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  binomial <- .is_binomial_performance_model(model)
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

    model_train <- if (binomial) {
      stats::update(model, . ~ ., data = train, model = TRUE)
    } else {
      stats::update(model, . ~ ., data = train)
    }
    pred <- stats::predict(model_train, eval_data, type = "response")

    scores[i, ] <- .resample_scores(model_train, eval_data, pred, metrics)
  }

  result <- structure(
    list(
      rmse_bs = if ("rmse" %in% metrics) scores[, "rmse"],
      rmse_mod = rmse_model,
      metric = if (identical(metrics, "rmse")) metric else metrics,
      sampling = sampling,
      n_resamples = n_resamples,
      sample_fraction = sample_fraction,
      n = n_resamples,
      frac = sample_fraction
    ),
    class = "bootstrap_performance"
  )
  if (!identical(metrics, "rmse")) {
    result$scores <- as.data.frame(scores)
    result$original_scores <- original_scores
  }
  result
}


.resample_scores <- function(model, data, predictions, metrics) {
  scores <- stats::setNames(numeric(length(metrics)), metrics)
  observed <- .performance_response_scale(model, .performance_observed(model, data))
  response <- observed$response
  if ("rmse" %in% metrics) {
    scores[["rmse"]] <- .response_rmse(response, predictions)
  }
  if ("mae" %in% metrics) {
    scores[["mae"]] <- mean(abs(response - predictions), na.rm = TRUE)
  }
  if ("deviance" %in% metrics) {
    scores[["deviance"]] <- .resample_deviance(model, data, response, predictions,
                                             observed$trials)
  }
  scores
}


.resample_deviance <- function(model, data, response, predictions, trials) {
  family <- stats::family(model)
  if (!is.function(family$dev.resids)) {
    stop("Predictive deviance requires a family with deviance residuals.",
         call. = FALSE)
  }
  weight_expr <- stats::getCall(model)[["weights"]]
  weights <- if (!is.null(weight_expr)) {
    eval(weight_expr, data, environment(stats::formula(model)))
  }
  if (is.null(weights)) weights <- rep.int(1, nrow(data))
  if (!is.numeric(weights) || length(weights) != nrow(data) ||
      any(!is.finite(weights[!is.na(weights)]) | weights[!is.na(weights)] < 0)) {
    stop("Evaluation weights must be finite, non-negative numeric values, ",
         "one per row (missing weights are excluded). Supply the model's ",
         "weight variables in `data`.", call. = FALSE)
  }
  if (!is.null(trials)) weights <- weights * trials
  complete <- stats::complete.cases(response, predictions, weights)
  if (!any(complete)) return(NA_real_)
  keep <- which(complete & weights > 0)
  if (!length(keep)) return(0)
  sum(family$dev.resids(response[keep], predictions[keep], weights[keep]))
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

#' Deprecated alias for `resample_performance()`
#'
#' @description
#' `bootstrap_performance()` is deprecated. Use [resample_performance()] instead.
#'
#' @inheritParams resample_performance
#' @param sampling Random sampling scheme: `"bootstrap"` or `"split"`. For
#'   rolling validation, call [resample_performance()] directly.
#' @return See [resample_performance()].
#' @keywords internal
#' @export
bootstrap_performance <- function(model, data, n_resamples = 50,
                                  sample_fraction = 1,
                                  metric = "rmse",
                                  sampling = c("bootstrap", "split"),
                                  show_progress = TRUE,
                                  rmse_model = NULL,
                                  n = NULL,
                                  frac = NULL,
                                  metrics = "rmse") {
  .Deprecated("resample_performance")
  call <- match.call()
  call[[1L]] <- resample_performance
  eval.parent(call)
}


#' Deprecated RMSE wrapper for `resample_performance()`
#'
#' @description
#' `bootstrap_rmse()` is deprecated in favour of [resample_performance()].
#' Objects returned by `bootstrap_rmse()` keep class `"bootstrap_rmse"` for
#' backward compatibility and also inherit from `"bootstrap_performance"`.
#'
#' @inheritParams resample_performance
#' @param sampling Random sampling scheme: `"bootstrap"` or `"split"`.
#' @param n Deprecated. Use `n_resamples` in [resample_performance()] instead.
#' @param frac Deprecated. Use `sample_fraction` in [resample_performance()]
#'   instead.
#'
#' @return See [resample_performance()].
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
    "resample_performance()"
  )

  out <- resample_performance(
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
  if (is.null(x$scores)) {
    print(x$rmse_bs)
  } else {
    print(x$scores, ...)
  }
  invisible(x)
}


#' @export
as.vector.bootstrap_performance <- function(x, ...) {
  as.vector(if (is.null(x$scores)) x$rmse_bs else x$scores[[1L]], ...)
}


#' Plot the resampled performance distribution
#'
#' @description
#' Display the empirical distribution of resampled scores from
#' [resample_performance()]. The histogram shows the observed resampling
#' distribution, while the density curve provides a smooth visual summary.
#'
#' @param object An object of class `"bootstrap_performance"`, produced by
#'   [resample_performance()].
#' @param fill Fill colour of the histogram bars. Default is `"#E6E6E6"`.
#' @param color Border colour of the histogram bars. Default is `NA`, which
#'   removes bar borders.
#' @param ... Currently unused.
#' @param metric Metric to plot. The default, `NULL`, uses the first requested
#'   metric, or RMSE for an RMSE-only result.
#'
#' @details
#' The dashed orange line marks the score of the original fitted model. The
#' subtitle identifies which rows were used to score the refits. Total
#' predictive deviance depends on the number and weight of evaluated rows,
#' so the orange line is not directly comparable when those differ.
#'
#' The dotted grey lines mark the 2.5 and 97.5 percent empirical quantiles of the
#' resampled values when these can be calculated. Their distance provides a
#' practical indication of sampling variability; it is not a formal prediction
#' interval for future portfolio performance.
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
#' x <- resample_performance(mod1, MTPL, n_resamples = 100,
#'                            show_progress = FALSE)
#' autoplot(x)
#' }
#'
#' @seealso [resample_performance()], [rmse()]
#'
#' @export
autoplot.bootstrap_performance <- function(object,
                                           fill = "#E6E6E6",
                                           color = NA,
                                           ...,
                                           metric = NULL) {
  if (!inherits(object, "bootstrap_performance")) {
    stop("Input must be of class 'bootstrap_performance'.", call. = FALSE)
  }

  available <- if (is.null(object$scores)) "rmse" else names(object$scores)
  if (is.null(metric)) metric <- available[[1L]]
  if (!is.character(metric) || length(metric) != 1L ||
      !metric %in% available) {
    stop("`metric` must be one of: ", paste(available, collapse = ", "),
         ".", call. = FALSE)
  }
  scores <- if (is.null(object$scores)) object$rmse_bs else object$scores[[metric]]
  original_score <- if (is.null(object$scores)) {
    object$rmse_mod
  } else {
    object$original_scores[[metric]]
  }
  dat <- data.frame(x = scores)
  split <- identical(object$sampling, "split")
  fraction <- if (is.null(object$sample_fraction)) object$frac else object$sample_fraction
  evaluation <- if (length(fraction) != 1L || is.na(fraction)) {
    NULL
  } else if (fraction == 1) {
    if (split) "Scores on the training rows" else "Scores on the resampled training rows"
  } else {
    if (split) "Scores on held-out rows" else "Scores on out-of-bag rows"
  }
  caption <- "Dashed orange line: reference score for the original model."
  if (metric == "deviance") {
    caption <- paste(caption, "Total deviance depends on the evaluation row count and weights.",
                     sep = "\n")
  }

  conf_bounds <- tryCatch(
    stats::quantile(scores, c(0.025, 0.975)),
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
      xintercept = original_score,
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
      title = if (split) {
        "Model performance across repeated splits"
      } else {
        "Bootstrapped model performance"
      },
      subtitle = evaluation,
      x = switch(metric, rmse = "RMSE", mae = "MAE", deviance = "Predictive deviance"),
      y = "Density",
      caption = caption
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
