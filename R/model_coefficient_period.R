.coefficient_periods <- function(object, data, period, confidence, show_progress) {
  .validate_bootstrap_coefficients_args(object, 1L, NULL, show_progress)
  if (is.null(data)) data <- .bootstrap_coefficients_model_data(object)
  if (!is.data.frame(data) || !nrow(data) ||
      !.is_single_string(period) || !period %in% names(data)) {
    stop("`period` must identify a period column in non-empty `data`.", call. = FALSE)
  }
  if (!is.null(confidence) && (!is.numeric(confidence) || length(confidence) != 1L ||
      !is.finite(confidence) || confidence <= 0 || confidence >= 1)) {
    stop("`confidence` must be NULL or a number between zero and one.", call. = FALSE)
  }
  time <- data[[period]]
  if (anyNA(time) || !is.null(dim(time)) ||
      !(is.factor(time) || inherits(time, c("Date", "POSIXt")) ||
        (is.numeric(time) && all(is.finite(time))))) {
    stop("Periods must be non-missing numeric values, dates or factors.", call. = FALSE)
  }
  terms <- stats::terms(object)
  call <- stats::getCall(object)
  weight_expr <- call[["weights"]]
  offset_expr <- call[["offset"]]
  missing <- setdiff(unique(c(all.vars(stats::formula(object)), all.vars(weight_expr),
                              all.vars(offset_expr))), names(data))
  if (length(missing)) {
    stop("Missing model columns: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  get_column <- function(expr, default) {
    if (is.null(expr)) return(rep(default, nrow(data)))
    value <- eval(expr, data, environment(stats::formula(object)))
    if (!is.numeric(value) || length(value) != nrow(data) || !length(all.vars(expr))) {
      stop("Specify prior weights and offsets using columns in `data`.", call. = FALSE)
    }
    value
  }
  weights <- get_column(weight_expr, 1)
  offset <- get_column(offset_expr, 0)
  offsets <- lapply(attr(terms, "offset"), function(i) {
    attr(terms, "variables")[[i + 1L]][[2L]]
  })
  if (!is.null(offset_expr)) offsets <- c(offsets, list(offset_expr))
  exposure <- .calibration_exposure_column(
    object$family$link, offsets, weight_expr, stats::formula(object)[[2L]]
  )
  periods <- sort(unique(time))
  rows <- split(seq_len(nrow(data)), match(time, periods))
  original <- stats::coef(object)
  terms_coef <- c("(Intercept)", attr(terms, "term.labels"))[
    (object$assign %||% attr(stats::model.matrix(object), "assign")) + 1L
  ]
  log_link <- identical(object$family$link, "log")
  z <- if (is.null(confidence)) NA_real_ else stats::qnorm((1 + confidence) / 2)
  if (show_progress) {
    pb <- utils::txtProgressBar(max = length(rows), style = 3)
    on.exit(close(pb), add = TRUE)
  }
  results <- lapply(seq_along(rows), function(i) {
    idx <- rows[[i]]
    status <- rep("not_estimable", length(original))
    estimate <- se <- rep(NA_real_, length(original))
    diagnostics <- character()
    n_used <- 0L
    total_weight <- total_exposure <- NA_real_
    fit <- withCallingHandlers(tryCatch({
      frame <- stats::model.frame(terms, data[idx, , drop = FALSE],
                                  na.action = stats::na.pass, xlev = object$xlevels)
      x <- stats::model.matrix(terms, frame, contrasts.arg = object$contrasts)
      y <- stats::model.response(frame)
      off <- offset[idx] + (stats::model.offset(frame) %||% 0)
      w <- weights[idx]
      if (any(w < 0, na.rm = TRUE)) stop("Negative prior weights.")
      keep <- stats::complete.cases(x, y) & is.finite(off) & is.finite(w) & w > 0
      n_used <- sum(keep)
      total_weight <- sum(w[keep])
      if (!is.null(exposure)) total_exposure <- sum(data[[exposure]][idx[keep]])
      if (!n_used) stop("No complete, positively weighted observations.")
      if (!identical(colnames(x), names(original))) stop("Coefficient coding changed.")
      if (is.matrix(y)) y <- y[keep, , drop = FALSE] else y <- y[keep]
      stats::glm.fit(x[keep, , drop = FALSE], y, weights = w[keep], offset = off[keep],
                     family = object$family, control = object$control,
                     intercept = attr(terms, "intercept") == 1L)
    }, error = identity), warning = function(w) {
      diagnostics <<- c(diagnostics, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    if (inherits(fit, "error")) {
      status[] <- "fit_failed"
      diagnostics <- c(diagnostics, conditionMessage(fit))
    } else if (!isTRUE(fit$converged)) {
      status[] <- "not_converged"
    } else {
      estimable <- .period_estimable(fit) & is.finite(original)
      estimate[estimable] <- fit$coefficients[estimable]
      status[estimable] <- "estimated"
      fit_summary <- tryCatch(suppressWarnings(summary(structure(fit, class = c("glm", "lm")))),
                              error = function(e) NULL)
      if (!is.null(fit_summary)) {
        se[estimable] <- fit_summary$coefficients[
          match(names(original)[estimable], rownames(fit_summary$coefficients)), 2L]
      }
    }
    if (!is.null(confidence)) status[status == "estimated" & !is.finite(se)] <- "no_interval"
    if (show_progress) utils::setTxtProgressBar(pb, i)
    list(estimates = data.frame(
      period = periods[i], term = names(original), model_term = terms_coef,
      estimate = estimate, relativity = if (log_link) exp(estimate) else NA_real_,
      std_error = se, lower = estimate - z * se, upper = estimate + z * se,
      n = n_used, exposure = total_exposure, status = status
    ), diagnostics = data.frame(
      period = periods[i], n_input = length(idx), n = n_used,
      prior_weight = total_weight, exposure = total_exposure,
      message = paste(unique(diagnostics), collapse = "; ")
    ))
  })
  structure(list(estimates = do.call(rbind, lapply(results, `[[`, "estimates")),
                 diagnostics = do.call(rbind, lapply(results, `[[`, "diagnostics")),
                 confidence = confidence, log_link = log_link),
            class = "period_coefficients")
}

.period_estimable <- function(fit) {
  rank <- fit$rank
  p <- length(fit$coefficients)
  if (rank == p) return(rep(TRUE, p))
  if (!rank) return(rep(FALSE, p))
  r <- qr.R(fit$qr)
  kept <- seq_len(rank)
  aliases <- seq.int(rank + 1L, p)
  # A pivoted coefficient is not identifiable if a free column can change it.
  dependence <- backsolve(r[kept, kept, drop = FALSE], r[kept, aliases, drop = FALSE])
  estimable <- rep(FALSE, p)
  estimable[fit$qr$pivot[kept]] <- rowSums(abs(dependence) > 1e-8) == 0L
  estimable
}

#' Plot coefficients fitted separately by period
#' @param object A result from `coefficient_stability(method = "period")`.
#' @param ... Not used.
#' @param scale Coefficients on the link scale or exponentiated effects for a log link.
#' @param terms Optional coefficient or model-term names to plot.
#' @return A ggplot object.
#' @export
autoplot.period_coefficients <- function(object, ..., scale = c("link", "relativity"),
                                         terms = NULL) {
  .check_dots_empty(...)
  scale <- match.arg(scale)
  data <- object$estimates
  if (is.null(terms) && any(data$term != "(Intercept)")) {
    data <- data[data$term != "(Intercept)", , drop = FALSE]
  } else if (!is.null(terms)) {
    if (!length(terms) || !all(terms %in% c(data$term, data$model_term))) {
      stop("Choose coefficient or model-term names from `estimates`.", call. = FALSE)
    }
    data <- data[data$term %in% terms | data$model_term %in% terms, , drop = FALSE]
  }
  if (scale == "relativity") {
    if (!object$log_link) stop("Relativities require a log link.", call. = FALSE)
    data[c("estimate", "lower", "upper")] <- lapply(data[c("estimate", "lower", "upper")], exp)
  }
  p <- ggplot2::ggplot(data, ggplot2::aes(.data$period, .data$estimate,
                                         colour = .data$term, group = .data$term)) +
    ggplot2::geom_line(na.rm = TRUE) + ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::facet_wrap(~model_term, scales = "free_y") +
    ggplot2::labs(x = "Period", y = if (scale == "link") "Coefficient" else "Relativity",
                  colour = "Coefficient") +
    ggplot2::theme_minimal() + .plot_grid_theme_ir()
  if (!is.null(object$confidence)) {
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper), na.rm = TRUE
    )
  }
  reference <- data.frame(model_term = setdiff(unique(data$model_term), "(Intercept)"))
  if (nrow(reference)) {
    reference$neutral <- if (scale == "link") 0 else 1
    p <- p + ggplot2::geom_hline(data = reference, ggplot2::aes(yintercept = .data$neutral),
                                colour = "grey55", linetype = 2)
  }
  p
}
