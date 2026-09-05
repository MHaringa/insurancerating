#' Assess GLM coefficient stability by portfolio-row bootstrap
#'
#' @description
#' Refit a GLM on repeated bootstrap samples of the estimation portfolio and
#' retain the coefficient estimates from every successful refit. The resulting
#' distribution describes how sensitive individual model coefficients are to
#' sampling variation in the observed portfolio.
#'
#' @details
#' Each resample contains the same number of portfolio rows as the original
#' estimation data and is drawn with replacement. The function recovers these
#' data from `object`; a separate `data` argument is deliberately not required.
#' Rows omitted during the original model fit are excluded so the resampling
#' population remains aligned with the fitted GLM.
#'
#' Original factor levels, the model formula, offsets and model weights are
#' retained during refitting. A factor level may nevertheless be absent from a
#' particular bootstrap sample. Its coefficient can then be non-estimable and
#' is stored as `NA` for that replicate.
#'
#' A failed or non-converged GLM refit does not stop the procedure. The failed
#' replicate is recorded and the function continues. After resampling, an
#' informative message reports how many requested refits produced usable model
#' objects. [summary.bootstrap_coefficients()] reports the number of finite
#' estimates separately for each coefficient.
#'
#' ## Actuarial interpretation
#'
#' The bootstrap distribution can identify tariff effects that are sensitive to
#' the particular portfolio sample. Wide intervals, material bootstrap bias or
#' a low number of estimable replicates often indicate sparse levels, correlated
#' model terms or limited claim information. These diagnostics should be
#' considered alongside exposure, claim counts, coefficient interpretation and
#' stability across calendar periods.
#'
#' The row bootstrap represents sampling variation in the observed estimation
#' portfolio. It does not include future trend, parameter uncertainty caused by
#' model selection, structural changes in portfolio composition or dependence
#' between repeated records for the same policy. Where such dependence is
#' material, a cluster-level bootstrap would require a different resampling
#' design.
#'
#' @param object A fitted `glm` object. Refined GLMs are accepted when their
#'   estimation data can be recovered from the model object.
#' @param n_resamples Positive whole number. Number of bootstrap samples.
#'   Default is 500.
#' @param seed Optional single numeric seed for reproducible resampling.
#' @param show_progress Logical. If `TRUE`, display a text progress bar.
#'
#' @return An object of class `"bootstrap_coefficients"`. It contains the
#'   original coefficients, a coefficient matrix with one row per requested
#'   resample, indicators for successful model fits, recorded failure messages,
#'   and the resampling settings. Use [summary.bootstrap_coefficients()] for a
#'   coefficient-level data frame and `as_gt()` for a formatted table.
#'
#' @author Martin Haringa
#'
#' @seealso [summary.bootstrap_coefficients()], [bootstrap_performance()],
#'   [model_performance()], [as_gt()]
#'
#' @examples
#' model <- glm(
#'   nclaims ~ age_policyholder + zip + offset(log(exposure)),
#'   family = poisson(),
#'   data = MTPL
#' )
#'
#' \dontrun{
#' boot <- bootstrap_coefficients(
#'   model,
#'   n_resamples = 25,
#'   seed = 123,
#'   show_progress = FALSE
#' )
#'
#' summary(boot, scale = "link")
#' summary(boot, scale = "exponentiated")
#' summary(boot, scale = "relativity")
#'
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   as_gt(boot, scale = "relativity")
#' }
#' }
#'
#' @export
bootstrap_coefficients <- function(object, n_resamples = 500, seed = NULL,
                                   show_progress = interactive()) {
  .validate_bootstrap_coefficients_args(
    object = object,
    n_resamples = n_resamples,
    seed = seed,
    show_progress = show_progress
  )
  n_resamples <- as.integer(n_resamples)

  data <- .bootstrap_coefficients_model_data(object)
  original <- stats::coef(object)
  coefficient_names <- names(original)
  replicates <- matrix(
    NA_real_,
    nrow = n_resamples,
    ncol = length(original),
    dimnames = list(NULL, coefficient_names)
  )
  successful_fit <- rep(FALSE, n_resamples)
  failure_messages <- rep(NA_character_, n_resamples)
  warning_messages <- rep(NA_character_, n_resamples)

  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (isTRUE(show_progress)) {
    progress <- utils::txtProgressBar(max = n_resamples, style = 3)
    on.exit(close(progress), add = TRUE)
  }

  for (i in seq_len(n_resamples)) {
    if (isTRUE(show_progress)) {
      utils::setTxtProgressBar(progress, i)
    }
    rows <- sample.int(nrow(data), size = nrow(data), replace = TRUE)
    bootstrap_data <- data[rows, , drop = FALSE]
    fit_warnings <- character()
    fit <- withCallingHandlers(
      tryCatch(
        .bootstrap_coefficients_refit(object, bootstrap_data),
        error = identity
      ),
      warning = function(warning) {
        fit_warnings <<- c(fit_warnings, conditionMessage(warning))
        invokeRestart("muffleWarning")
      }
    )

    if (inherits(fit, "error")) {
      failure_messages[i] <- conditionMessage(fit)
      next
    }
    if (!inherits(fit, "glm")) {
      failure_messages[i] <- "The refit did not return a glm object."
      next
    }
    if (identical(fit$converged, FALSE)) {
      failure_messages[i] <- "The bootstrap GLM did not converge."
      next
    }

    fit_coefficients <- stats::coef(fit)
    common <- intersect(names(fit_coefficients), coefficient_names)
    replicates[i, common] <- fit_coefficients[common]
    successful_fit[i] <- TRUE
    if (length(fit_warnings) > 0L) {
      warning_messages[i] <- paste(unique(fit_warnings), collapse = " | ")
    }
  }

  n_successful <- sum(successful_fit)
  if (n_successful < n_resamples) {
    message(
      "Coefficient bootstrap completed ", n_successful, " of ", n_resamples,
      " requested GLM refits. Failed refits are retained as missing replicates."
    )
  }

  structure(
    list(
      original = original,
      replicates = replicates,
      successful_fit = successful_fit,
      failure_messages = failure_messages,
      warning_messages = warning_messages,
      n_resamples = n_resamples,
      n_successful = n_successful,
      n_observations = nrow(data),
      seed = seed,
      link = object$family$link,
      family = object$family$family,
      model_call = object$call,
      call = match.call()
    ),
    class = "bootstrap_coefficients"
  )
}

.validate_bootstrap_coefficients_args <- function(object, n_resamples, seed,
                                                   show_progress) {
  if (!inherits(object, "glm")) {
    stop("`object` must be a fitted `glm` object.", call. = FALSE)
  }
  if (!is.numeric(n_resamples) || length(n_resamples) != 1L ||
      is.na(n_resamples) || !is.finite(n_resamples) || n_resamples < 1 ||
      n_resamples != as.integer(n_resamples)) {
    stop("`n_resamples` must be a positive whole number.", call. = FALSE)
  }
  if (!is.null(seed) &&
      (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
       !is.finite(seed) || seed < 0 || seed != floor(seed) ||
       seed > .Machine$integer.max)) {
    stop(
      "`seed` must be NULL or a whole number between 0 and ",
      .Machine$integer.max, ".",
      call. = FALSE
    )
  }
  if (!is.logical(show_progress) || length(show_progress) != 1L ||
      is.na(show_progress)) {
    stop("`show_progress` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

.bootstrap_coefficients_refit <- function(object, bootstrap_data) {
  # The recovered data already contain exactly the observations used by the
  # original fit. Reapplying an original subset would subset them a second time.
  stats::update(object, . ~ ., data = bootstrap_data, subset = NULL)
}

.bootstrap_coefficients_model_data <- function(object) {
  data <- object$data
  if (is.null(data) && !is.null(object$call$data)) {
    data <- tryCatch(
      eval(object$call$data, envir = environment(stats::formula(object))),
      error = function(error) NULL
    )
  }
  if (is.null(data)) {
    data <- tryCatch(extract_model_data(object), error = function(error) NULL)
  }
  if (is.null(data) || !is.data.frame(data)) {
    stop(
      "The estimation data could not be recovered from `object`. Fit the GLM ",
      "with a data frame supplied through `data=` and retain that model object.",
      call. = FALSE
    )
  }
  data <- as.data.frame(data)

  model_frame <- stats::model.frame(object)
  used_rows <- row.names(model_frame)
  data_rows <- row.names(data)
  if (length(used_rows) > 0L && all(used_rows %in% data_rows)) {
    data <- data[match(used_rows, data_rows), , drop = FALSE]
  } else if (nrow(data) != nrow(model_frame)) {
    omitted <- stats::na.action(object)
    omitted_rows <- suppressWarnings(as.integer(omitted))
    if (length(omitted_rows) > 0L && all(is.finite(omitted_rows)) &&
        all(omitted_rows >= 1L) && all(omitted_rows <= nrow(data))) {
      data <- data[-omitted_rows, , drop = FALSE]
    }
  }
  if (nrow(data) != nrow(model_frame) || nrow(data) == 0L) {
    stop(
      "The observations used to fit `object` could not be aligned with its ",
      "stored estimation data.",
      call. = FALSE
    )
  }

  for (column in intersect(names(object$xlevels), names(data))) {
    data[[column]] <- factor(data[[column]], levels = object$xlevels[[column]])
  }

  data
}

#' Summarise bootstrap coefficient stability
#'
#' @description
#' Summarise the coefficient distributions returned by
#' [bootstrap_coefficients()] on the GLM link scale or after exponentiation.
#'
#' @param object A `bootstrap_coefficients` object.
#' @param scale Character string. `"link"` reports coefficients on their fitted
#'   GLM scale. `"exponentiated"` applies `exp()` to every original and
#'   bootstrap coefficient. `"relativity"` is an alias for
#'   `"exponentiated"`; this interpretation is most direct for a log-link GLM.
#'   For a logit-link model, exponentiated coefficients are odds ratios rather
#'   than response probabilities.
#' @param confidence Numeric scalar between 0 and 1 giving the confidence level.
#' @param interval Character string. `"percentile"` uses empirical bootstrap
#'   quantiles. `"normal"` uses the original estimate plus or minus a normal
#'   quantile times the bootstrap standard error.
#' @param ... Additional arguments are not used.
#'
#' @return A data frame with one row per original coefficient and columns:
#' \describe{
#'   \item{term}{Coefficient name.}
#'   \item{estimate}{Estimate from the original GLM.}
#'   \item{bootstrap_mean}{Mean of the finite bootstrap estimates.}
#'   \item{bias}{Bootstrap mean minus the original estimate.}
#'   \item{bootstrap_se}{Standard deviation of the bootstrap estimates.}
#'   \item{lower, upper}{Requested bootstrap interval.}
#'   \item{n_successful}{Number of finite bootstrap estimates for the term.}
#'   \item{n_requested}{Number of requested bootstrap samples.}
#'   \item{success_rate}{`n_successful / n_requested`.}
#' }
#'
#' @author Martin Haringa
#' @seealso [bootstrap_coefficients()], [as_gt()]
#' @keywords internal
#' @export
summary.bootstrap_coefficients <- function(object,
                                           scale = c(
                                             "link",
                                             "exponentiated",
                                             "relativity"
                                           ),
                                           confidence = 0.95,
                                           interval = c("percentile", "normal"),
                                           ...) {
  scale <- match.arg(scale)
  interval <- match.arg(interval)
  if (!is.numeric(confidence) || length(confidence) != 1L ||
      is.na(confidence) || !is.finite(confidence) || confidence <= 0 ||
      confidence >= 1) {
    stop("`confidence` must be a single number between 0 and 1.", call. = FALSE)
  }

  exponentiate <- scale %in% c("exponentiated", "relativity")
  estimates <- object$original
  replicates <- object$replicates
  if (exponentiate) {
    estimates <- exp(estimates)
    replicates <- exp(replicates)
  }

  alpha <- (1 - confidence) / 2
  result <- lapply(seq_along(estimates), function(i) {
    values <- replicates[, i]
    values <- values[is.finite(values)]
    n_successful <- length(values)
    bootstrap_mean <- if (n_successful > 0L) mean(values) else NA_real_
    bootstrap_se <- if (n_successful > 1L) stats::sd(values) else NA_real_
    limits <- if (n_successful == 0L) {
      c(NA_real_, NA_real_)
    } else if (identical(interval, "percentile")) {
      as.numeric(stats::quantile(values, c(alpha, 1 - alpha), names = FALSE))
    } else if (is.finite(bootstrap_se)) {
      estimates[i] + c(-1, 1) * stats::qnorm(1 - alpha) * bootstrap_se
    } else {
      c(NA_real_, NA_real_)
    }

    data.frame(
      term = names(estimates)[i],
      estimate = unname(estimates[i]),
      bootstrap_mean = bootstrap_mean,
      bias = bootstrap_mean - unname(estimates[i]),
      bootstrap_se = bootstrap_se,
      lower = limits[1],
      upper = limits[2],
      n_successful = n_successful,
      n_requested = object$n_resamples,
      success_rate = n_successful / object$n_resamples,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, result)
  row.names(result) <- NULL
  attr(result, "scale") <- if (exponentiate) "exponentiated" else "link"
  attr(result, "confidence") <- confidence
  attr(result, "interval") <- interval
  result
}

#' @export
#' @keywords internal
print.bootstrap_coefficients <- function(x, ...) {
  cat("Bootstrap coefficient stability\n")
  cat("Family: ", x$family, " (", x$link, " link)\n", sep = "")
  cat("Requested resamples: ", x$n_resamples, "\n", sep = "")
  cat("Successful GLM refits: ", x$n_successful, "\n", sep = "")
  invisible(x)
}

#' Present bootstrap coefficient stability as a gt table
#'
#' @description
#' Format the coefficient-level result from [bootstrap_coefficients()] as a
#' `gt` table. The table retains the requested link or exponentiated scale and
#' shows how many bootstrap estimates were available for each coefficient.
#'
#' @inheritParams summary.bootstrap_coefficients
#' @param locale Character string used for number and percentage formatting.
#' @param estimate_decimals Non-negative whole number. Number of decimals for
#'   estimates, bias, standard errors and interval limits.
#' @param success_decimals Non-negative whole number. Number of decimals for the
#'   success-rate percentage.
#' @param title Optional table title. If `NULL`, no header is added.
#' @param subtitle Optional table subtitle. If `NULL`, no subtitle is added.
#'
#' @author Martin Haringa
#' @rdname as_gt
#' @export
as_gt.bootstrap_coefficients <- function(x,
                                         scale = c(
                                           "link",
                                           "exponentiated",
                                           "relativity"
                                         ),
                                         confidence = 0.95,
                                         interval = c("percentile", "normal"),
                                         locale = "nl-NL",
                                         estimate_decimals = 3,
                                         success_decimals = 1,
                                         title = NULL,
                                         subtitle = NULL,
                                         ...) {
  rlang::check_installed("gt")
  scale <- match.arg(scale)
  interval <- match.arg(interval)
  validate_single_character(locale, "locale")
  validate_decimal_count(estimate_decimals, "estimate_decimals")
  validate_decimal_count(success_decimals, "success_decimals")
  if (!is.null(title)) validate_single_character(title, "title")
  if (!is.null(subtitle)) validate_single_character(subtitle, "subtitle")

  table_data <- summary(
    x,
    scale = scale,
    confidence = confidence,
    interval = interval
  )
  out <- gt::gt(table_data, locale = locale)
  out <- gt::cols_label(
    out,
    term = "Term",
    estimate = "Original",
    bootstrap_mean = "Mean",
    bias = "Bias",
    bootstrap_se = "SE",
    lower = "Lower",
    upper = "Upper",
    n_successful = "Successful",
    n_requested = "Requested",
    success_rate = "Success (%)"
  )
  out <- gt::tab_spanner(
    out,
    label = if (scale %in% c("exponentiated", "relativity")) {
      "Exponentiated coefficients"
    } else {
      "Link-scale coefficients"
    },
    columns = c(
      "estimate", "bootstrap_mean", "bias", "bootstrap_se", "lower", "upper"
    )
  )
  out <- gt::tab_spanner(
    out,
    label = "Bootstrap resamples",
    columns = c("n_successful", "n_requested", "success_rate")
  )
  out <- gt::fmt_number(
    out,
    columns = c(
      "estimate", "bootstrap_mean", "bias", "bootstrap_se", "lower", "upper"
    ),
    decimals = estimate_decimals,
    locale = locale
  )
  out <- gt::fmt_integer(
    out,
    columns = c("n_successful", "n_requested"),
    locale = locale
  )
  out <- gt::fmt_percent(
    out,
    columns = "success_rate",
    decimals = success_decimals,
    locale = locale
  )
  if (!is.null(title) || !is.null(subtitle)) {
    out <- gt::tab_header(out, title = title, subtitle = subtitle)
  }
  out
}
