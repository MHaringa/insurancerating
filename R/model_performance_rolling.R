.resample_rolling <- function(model, data, time, initial, horizon, metrics,
                              show_progress) {
  if (!is.data.frame(data) || !nrow(data)) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }
  if (!is.character(time) || length(time) != 1L || is.na(time) ||
      !nzchar(time) || !time %in% names(data)) {
    stop("`time` must name one period column in `data`.", call. = FALSE)
  }
  if (!.time_is_flag(show_progress)) {
    stop("`show_progress` must be TRUE or FALSE.", call. = FALSE)
  }
  for (arg in c("initial", "horizon")) {
    value <- if (arg == "initial") initial else horizon
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < 1 || value != floor(value)) {
      stop("`", arg, "` must be a positive whole number of distinct periods.",
           call. = FALSE)
    }
  }
  data <- as.data.frame(data)
  period <- data[[time]]
  if (anyNA(period)) {
    stop("The `time` column must not contain missing periods.", call. = FALSE)
  }
  if (!is.null(dim(period)) ||
      !(is.ordered(period) || inherits(period, c("Date", "POSIXt")) ||
        (is.numeric(period) && all(is.finite(period))))) {
    stop("The `time` column must contain numeric periods, dates, date-times, ",
         "or an ordered factor.", call. = FALSE)
  }
  periods <- sort(unique(period))
  if (length(periods) < initial + horizon) {
    stop("Insufficient distinct periods for the requested `initial` and `horizon`.",
         call. = FALSE)
  }
  ends <- seq.int(initial, length(periods) - horizon)
  result <- data.frame(
    iteration = seq_along(ends),
    train_start = rep(periods[1L], length(ends)),
    train_end = periods[ends],
    test_start = periods[ends + 1L],
    test_end = periods[ends + horizon],
    n_train = integer(length(ends)),
    n_test = integer(length(ends))
  )
  scores <- matrix(NA_real_, nrow(result), length(metrics),
                   dimnames = list(NULL, metrics))

  fit_call <- .rolling_glm_call(model, data)
  formula <- stats::formula(model)
  fit_env <- new.env(parent = environment(formula))
  fit_env$.rolling_formula <- formula
  fit_env$.rolling_family <- stats::family(model)
  period_id <- match(period, periods)
  if (show_progress) {
    pb <- utils::txtProgressBar(max = length(ends), style = 3)
    on.exit(close(pb), add = TRUE)
  }

  for (i in seq_along(ends)) {
    train <- period_id <= ends[i]
    test <- period_id > ends[i] & period_id <= ends[i] + horizon
    result$n_train[i] <- sum(train)
    result$n_test[i] <- sum(test)
    fit_env$.rolling_data <- data[train, , drop = FALSE]
    assessment <- data[test, , drop = FALSE]
    scores[i, ] <- tryCatch({
      fitted <- eval(fit_call, fit_env)
      predictions <- stats::predict(fitted, newdata = assessment, type = "response")
      .resample_scores(fitted, assessment, predictions, metrics)
    }, error = function(e) {
      stop("Rolling window ", i, " (test ", format(result$test_start[i]), " to ",
           format(result$test_end[i]), "): ", conditionMessage(e),
           " Training uses only historical rows.", call. = FALSE)
    })
    if (show_progress) utils::setTxtProgressBar(pb, i)
  }
  cbind(result, as.data.frame(scores))
}


.rolling_glm_call <- function(model, data) {
  if (!inherits(model, "glm") ||
      inherits(model, c("refitrestricted", "refitsmooth", "calibrated_model"))) {
    stop("Rolling validation requires an unrestricted model fitted with `stats::glm()`.",
         call. = FALSE)
  }
  call <- stats::getCall(model)
  if (is.null(call) ||
      (!identical(call[[1L]], quote(glm)) &&
       !identical(call[[1L]], quote(stats::glm)) &&
       !identical(call[[1L]], stats::glm))) {
    stop("Rolling validation requires a model fitted with `stats::glm()`.",
         call. = FALSE)
  }
  if (any(vapply(c("subset", "etastart", "mustart"), function(arg) {
    !is.null(call[[arg]])
  }, logical(1)))) {
    stop("For rolling validation, filter `data` before fitting instead of ",
         "using `subset`, and omit row-level starting values.", call. = FALSE)
  }
  rows <- list(call[["weights"]], call[["offset"]])
  if (any(vapply(rows, function(expr) {
    !is.null(expr) && !length(all.vars(expr))
  }, logical(1)))) {
    stop("For rolling validation, specify weights and offsets using columns ",
         "in `data`, not fixed vectors.", call. = FALSE)
  }
  required <- unique(c(all.vars(stats::formula(model)), unlist(lapply(rows, all.vars))))
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("Missing model variables in `data`: ", paste(missing, collapse = ", "),
         ".", call. = FALSE)
  }

  # Rebuild model terms on each training window, not from the full model frame.
  call[[1L]] <- quote(stats::glm)
  call$formula <- quote(.rolling_formula)
  call$family <- quote(.rolling_family)
  call$data <- quote(.rolling_data)
  if (.is_binomial_performance_model(model)) call$model <- TRUE
  call
}
