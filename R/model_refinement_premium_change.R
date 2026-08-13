#' Interpret the premium effect of a smoothing curve
#'
#' @description
#' Translate an effective smoothing curve in a refinement specification into
#' concrete modelled-premium comparisons. By default, each selected value is
#' compared with twice that value. Supplying `step` instead compares each value
#' with a fixed increment above it.
#'
#' @details
#' `premium_change()` is an interpretation helper for smoothing created with
#' [add_smoothing()] and subsequently modified with [edit_smoothing()]. It is
#' not a smoothing method and does not change the refinement specification.
#'
#' For a multiplicative relativity curve \eqn{R(x)}, doubling reports
#' \eqn{R(2x) / R(x) - 1}. Fixed-step mode reports
#' \eqn{R(x+h) / R(x) - 1}, where \eqn{h} is `step`. If total modelled premium
#' can be written as \eqn{P(x,z)=C(z)R(x)}, all other multiplicative model
#' effects \eqn{C(z)} cancel in this ratio. No particular policy profile is
#' therefore required for the interpretation.
#'
#' The effective curve is reconstructed from the stored refinement history.
#' Consequently, `steps = "current"` reflects all smoothing edits recorded up
#' to the current state. Numeric step identifiers refer to positions in the
#' complete refinement sequence. If another type of refinement occurs after a
#' smoothing step, the previously effective smoothing is carried forward.
#'
#' With the default `basis = "curve"`, evaluation uses the continuous effective
#' smoothing line retained by the refinement system. It therefore describes
#' the shape and steepness of the estimated or edited curve at exactly \eqn{x}
#' and the corresponding comparison value; it does not use neighbouring
#' tariff-segment relativities.
#'
#' With `basis = "segments"`, both values are assigned to the effective tariff
#' intervals created by the smoothing. Their current segment relativities are
#' compared. This describes the premium effect of the implementable segmented
#' tariff. The result can be zero when both values fall in the same segment and
#' can change discretely when the comparison crosses a segment boundary.
#'
#' Values are never extrapolated. When `at = NULL`, six representative starting
#' values are selected from the common range for which both the starting and
#' comparison values are supported in every selected refinement state.
#'
#' Multiplying an entire curve by a common rebasing constant does not alter the
#' result because that constant cancels in the relativity ratio.
#'
#' @param x For `premium_change()`, a `rating_refinement` object containing at
#'   least one smoothing step. For `as_gt()`, an object returned by
#'   `premium_change()`.
#' @param variable Optional character string identifying the smoothed model
#'   variable or its continuous source variable. This may be omitted when the
#'   refinement contains exactly one smoothing lineage.
#' @param at Optional numeric vector of starting values. Each starting and
#'   comparison value must lie inside the supported smoothing range of every
#'   selected refinement state. Doubling retains the existing requirement that
#'   starting values are positive. If `NULL`, approximately six representative
#'   values are selected automatically.
#' @param change Character comparison mode. `"double"` (default) compares
#'   \eqn{x} with \eqn{2x}. When `step` is supplied, omit `change`; fixed-step
#'   mode is then selected automatically.
#' @param step Optional positive finite numeric increment. When supplied,
#'   compares \eqn{x} with \eqn{x + step}. It cannot be combined with an
#'   explicitly supplied `change` instruction.
#' @param steps Refinement states to evaluate. Use `"current"` for the latest
#'   state, `"all"` for every state from the selected smoothing onwards, or a
#'   numeric vector such as `c(1, 6)` for stored refinement positions.
#' @param basis Character string determining the interpretation basis.
#'   `"curve"`, the default, evaluates the continuous effective smoothing at
#'   the exact values. `"segments"` compares the effective relativities of the
#'   tariff intervals containing those values.
#' @param ... Reserved for future extensions. Additional arguments are not
#'   currently accepted.
#'
#' @return A tibble with class `premium_change` in long format, containing the
#'   variable, refinement state, starting and comparison values, evaluated
#'   relativities, and premium change as a decimal.
#'
#' @seealso [add_smoothing()], [edit_smoothing()],
#'   [autoplot.rating_refinement()], [as_gt()]
#'
#' @examples
#' age <- rep(seq(20, 70, by = 5), each = 5)
#' portfolio <- data.frame(
#'   claims = rep(c(0, 1, 0, 2, 1), length(age) / 5),
#'   exposure = 1,
#'   age = age
#' )
#' portfolio$age_band <- cut(
#'   portfolio$age,
#'   breaks = c(15, 30, 45, 60, 75),
#'   include.lowest = TRUE
#' )
#' model <- glm(
#'   claims ~ age_band + offset(log(exposure)),
#'   family = poisson(),
#'   data = portfolio
#' )
#' refinement <- prepare_refinement(model, data = portfolio) |>
#'   add_smoothing(
#'     model_variable = "age_band",
#'     source_variable = "age",
#'     breaks = seq(15, 75, by = 5),
#'     smoothing = "poly",
#'     degree = 2,
#'     weights = "exposure"
#'   )
#' premium_change(refinement, at = c(20, 25, 30))
#' premium_change(refinement, at = c(20, 25, 30), step = 5)
#' premium_change(refinement, at = c(20, 25, 30), basis = "segments")
#'
#' edited <- refinement |>
#'   edit_smoothing(
#'     model_variable = "age_band",
#'     from = 20,
#'     to = 60,
#'     adjustment = 1.05,
#'     transition = "linear"
#'   )
#' premium_change(edited, at = c(20, 25, 30), steps = c(1, 2))
#'
#' @export
premium_change <- function(x, variable = NULL, at = NULL,
                           change = "double", step = NULL,
                           steps = "current",
                           basis = c("curve", "segments"), ...) {
  change_missing <- missing(change)
  .check_dots_empty(...)
  .assert_refinement(x)
  basis <- match.arg(basis)
  comparison <- .premium_change_comparison(
    change = change,
    step = step,
    change_missing = change_missing
  )
  lineage <- .premium_change_lineage(x, variable)
  selected_steps <- .premium_change_steps(x, steps, lineage$step_index)
  states <- lapply(selected_steps, function(step_index) {
    smoothing <- preview_refinement(x, upto = step_index)$state$
      smoothing_states[[lineage$step_id]]
    if (is.null(smoothing) || is.null(smoothing$current_line)) {
      stop(
        "The effective smoothing for `", lineage$model_variable,
        "` is not available at refinement step ", step_index, ".",
        call. = FALSE
      )
    }
    smoothing
  })

  ranges <- vapply(states, function(state) {
    if (identical(basis, "curve")) {
      .premium_change_line_range(state)
    } else {
      .premium_change_segment_range(state$current_new)
    }
  }, numeric(2))
  common_min <- max(ranges[1, ])
  common_max <- min(ranges[2, ])
  valid_start_max <- if (identical(comparison$mode, "double")) {
    common_max / 2
  } else {
    common_max - comparison$step
  }
  at <- if (is.null(at)) {
    .premium_change_default_at(
      common_min, valid_start_max,
      comparison = comparison
    )
  } else {
    .premium_change_validate_at(
      at, common_min, valid_start_max,
      comparison = comparison
    )
  }
  to <- .premium_change_to(at, comparison)

  rows <- lapply(seq_along(selected_steps), function(i) {
    step_index <- selected_steps[i]
    values <- if (identical(basis, "curve")) {
      .premium_change_ratio_curve(
        states[[i]]$current_line,
        from = at,
        to = to,
        scale = states[[i]]$scale %||% "relativity"
      )
    } else {
      .premium_change_ratio(
        evaluator = function(values) {
          .premium_change_evaluate_segments(states[[i]]$current_new, values)
        },
        from = at,
        to = to
      )
    }
    data.frame(
      variable = lineage$source_variable,
      basis = basis,
      step_id = x$steps[[step_index]]$id,
      step_label = paste0("Step ", step_index),
      from = at,
      to = to,
      relativity_from = values$relativity_from,
      relativity_to = values$relativity_to,
      premium_change = values$premium_change,
      stringsAsFactors = FALSE
    )
  })

  out <- tibble::as_tibble(do.call(rbind, rows))
  class(out) <- c("premium_change", class(out))
  attr(out, "model_variable") <- lineage$model_variable
  attr(out, "source_variable") <- lineage$source_variable
  attr(out, "selected_steps") <- selected_steps
  attr(out, "basis") <- basis
  attr(out, "comparison") <- comparison$mode
  attr(out, "step") <- comparison$step
  out
}

.premium_change_comparison <- function(change, step, change_missing = FALSE) {
  if (!is.null(step)) {
    if (!isTRUE(change_missing)) {
      stop(
        "Supply either `change = \"double\"` or `step`, not both. Omit ",
        "`change` when requesting a fixed-step comparison.",
        call. = FALSE
      )
    }
    if (!is.numeric(step) || length(step) != 1L || is.na(step) ||
        !is.finite(step) || step <= 0) {
      stop("`step` must be one positive finite numeric value.", call. = FALSE)
    }
    return(list(mode = "step", step = as.numeric(step)))
  }
  if (!is.character(change) || length(change) != 1L || is.na(change) ||
      !identical(change, "double")) {
    stop("`change` must be \"double\".", call. = FALSE)
  }
  list(mode = "double", step = NULL)
}

.premium_change_to <- function(from, comparison) {
  if (identical(comparison$mode, "double")) 2 * from else from + comparison$step
}

.premium_change_lineage <- function(ref, variable) {
  smoothing_index <- which(vapply(
    ref$steps,
    function(step) identical(step$type, "smoothing"),
    logical(1)
  ))
  if (length(smoothing_index) == 0L) {
    stop("`x` does not contain smoothing created by `add_smoothing()`.",
         call. = FALSE)
  }
  if (!is.null(variable)) {
    if (!is.character(variable) || length(variable) != 1L ||
        is.na(variable) || !nzchar(variable)) {
      stop("`variable` must be one non-empty character string.", call. = FALSE)
    }
    matches <- smoothing_index[vapply(ref$steps[smoothing_index], function(step) {
      variable %in% c(step$model_variable %||% step$variable,
                      step$source_variable %||% step$x_org)
    }, logical(1))]
    if (length(matches) == 0L) {
      available <- unique(unlist(lapply(ref$steps[smoothing_index], function(step) {
        c(step$model_variable %||% step$variable,
          step$source_variable %||% step$x_org)
      })))
      stop(
        "No smoothing was found for `variable = \"", variable,
        "\"`. Available variables: ", paste(available, collapse = ", "), ".",
        call. = FALSE
      )
    }
    if (length(matches) > 1L) {
      stop("`variable` matches more than one smoothing lineage.", call. = FALSE)
    }
    smoothing_index <- matches
  } else if (length(smoothing_index) > 1L) {
    stop("The refinement contains multiple smoothed variables. Supply `variable =`.",
         call. = FALSE)
  }
  step <- ref$steps[[smoothing_index]]
  list(
    step_index = smoothing_index,
    step_id = step$id,
    model_variable = step$model_variable %||% step$variable,
    source_variable = step$source_variable %||% step$x_org
  )
}

.premium_change_steps <- function(ref, steps, smoothing_index) {
  n_steps <- length(ref$steps)
  if (is.character(steps) && length(steps) == 1L && !is.na(steps)) {
    if (identical(steps, "current")) return(n_steps)
    if (identical(steps, "all")) return(seq.int(smoothing_index, n_steps))
    stop("`steps` must be \"current\", \"all\" or numeric step positions.",
         call. = FALSE)
  }
  if (!is.numeric(steps) || length(steps) == 0L || anyNA(steps) ||
      any(!is.finite(steps)) || any(steps != as.integer(steps))) {
    stop("`steps` must be \"current\", \"all\" or numeric step positions.",
         call. = FALSE)
  }
  steps <- sort(unique(as.integer(steps)))
  if (any(steps < 1L | steps > n_steps)) {
    stop("Numeric `steps` must refer to positions in the refinement history.",
         call. = FALSE)
  }
  if (any(steps < smoothing_index)) {
    stop(
      "The selected smoothing is not yet available at refinement step(s): ",
      paste(steps[steps < smoothing_index], collapse = ", "), ".",
      call. = FALSE
    )
  }
  steps
}

.premium_change_line_xy <- function(line) {
  if (!is.data.frame(line) || ncol(line) < 2L || !"yhat" %in% names(line)) {
    stop("The stored smoothing line is unavailable or invalid.", call. = FALSE)
  }
  x_name <- setdiff(names(line), "yhat")[1L]
  x <- as.numeric(line[[x_name]])
  y <- as.numeric(line$yhat)
  valid <- is.finite(x) & is.finite(y)
  x <- x[valid]
  y <- y[valid]
  ordering <- order(x)
  x <- x[ordering]
  y <- y[ordering]
  keep <- !duplicated(x, fromLast = TRUE)
  list(x = x[keep], y = y[keep])
}

.premium_change_line_range <- function(smoothing) {
  xy <- .premium_change_line_xy(smoothing$current_line)
  if (length(xy$x) < 2L) {
    stop("The stored smoothing line must contain at least two distinct points.",
         call. = FALSE)
  }
  range(xy$x)
}

.premium_change_segment_data <- function(segments) {
  required <- c("breaks_min", "breaks_max", "yhat")
  if (!is.data.frame(segments) || !all(required %in% names(segments))) {
    stop("The stored smoothing segments are unavailable or invalid.",
         call. = FALSE)
  }
  out <- segments[, intersect(
    c("breaks_min", "breaks_max", "yhat", "start_oc", "end_oc"),
    names(segments)
  ), drop = FALSE]
  if (!"start_oc" %in% names(out)) {
    out$start_oc <- c("closed", rep("open", nrow(out) - 1L))
  }
  if (!"end_oc" %in% names(out)) out$end_oc <- "closed"
  valid <- is.finite(out$breaks_min) & is.finite(out$breaks_max) &
    is.finite(out$yhat) & out$breaks_min < out$breaks_max
  out <- out[valid, , drop = FALSE]
  out <- out[order(out$breaks_min, out$breaks_max), , drop = FALSE]
  if (nrow(out) == 0L) {
    stop("The stored smoothing segments contain no valid intervals.",
         call. = FALSE)
  }
  out
}

.premium_change_segment_range <- function(segments) {
  segments <- .premium_change_segment_data(segments)
  c(min(segments$breaks_min), max(segments$breaks_max))
}

.premium_change_evaluate_segments <- function(segments, values) {
  segments <- .premium_change_segment_data(segments)
  vapply(values, function(value) {
    left <- ifelse(
      segments$start_oc == "closed",
      value >= segments$breaks_min,
      value > segments$breaks_min
    )
    right <- ifelse(
      segments$end_oc == "closed",
      value <= segments$breaks_max,
      value < segments$breaks_max
    )
    matched <- which(left & right)
    if (length(matched) != 1L) {
      stop(
        "Value `", format(value, trim = TRUE, scientific = FALSE),
        "` could not be assigned to exactly one effective smoothing segment.",
        call. = FALSE
      )
    }
    segments$yhat[matched]
  }, numeric(1))
}

.premium_change_validate_at <- function(at, common_min, valid_start_max,
                                        comparison) {
  if (!is.numeric(at) || length(at) == 0L || anyNA(at) ||
      any(!is.finite(at)) ||
      (identical(comparison$mode, "double") && any(at <= 0))) {
    stop(
      if (identical(comparison$mode, "double")) {
        "`at` must contain finite numeric values greater than zero."
      } else {
        "`at` must contain finite numeric values."
      },
      call. = FALSE
    )
  }
  at <- sort(unique(as.numeric(at)))
  tolerance <- sqrt(.Machine$double.eps) *
    max(1, abs(common_min), abs(valid_start_max))
  invalid <- at < common_min - tolerance | at > valid_start_max + tolerance
  if (any(invalid)) {
    comparison_label <- if (identical(comparison$mode, "double")) {
      "doubling"
    } else {
      paste0("fixed-step comparison with `step = ",
             format(comparison$step, trim = TRUE, scientific = FALSE), "`")
    }
    stop(
      "The requested ", comparison_label, " for `at = ",
      paste(format(at[invalid], trim = TRUE, scientific = FALSE), collapse = ", "),
      "` falls outside the supported smoothing range. Use starting values from ",
      format(if (identical(comparison$mode, "double")) {
        max(common_min, .Machine$double.eps)
      } else {
        common_min
      }, trim = TRUE, scientific = FALSE),
      " through ", format(valid_start_max, trim = TRUE, scientific = FALSE),
      "; `premium_change()` never extrapolates.",
      call. = FALSE
    )
  }
  at
}

.premium_change_default_at <- function(common_min, valid_start_max,
                                       comparison) {
  lower <- if (identical(comparison$mode, "double")) {
    max(common_min, .Machine$double.eps)
  } else {
    common_min
  }
  if (!is.finite(lower) || !is.finite(valid_start_max) ||
      valid_start_max < lower) {
    stop(
      if (identical(comparison$mode, "double")) {
        "No positive starting value can be doubled within the common supported smoothing range."
      } else {
        paste0(
          "No starting value can be increased by `step = ",
          format(comparison$step, trim = TRUE, scientific = FALSE),
          "` within the common supported smoothing range."
        )
      },
      call. = FALSE
    )
  }
  if (isTRUE(all.equal(lower, valid_start_max))) return(lower)
  unique(as.numeric(seq(lower, valid_start_max, length.out = 6L)))
}

.premium_change_evaluate <- function(line, values, scale = NULL) {
  xy <- .premium_change_line_xy(line)
  scale <- scale %||% attr(line, "smoothing_scale", exact = TRUE) %||%
    "relativity"
  scale <- .validate_smoothing_scale(scale)
  if (identical(scale, "log_relativity")) {
    if (any(xy$y <= 0)) {
      stop("Log-relativity smoothing requires positive relativities.",
           call. = FALSE)
    }
    return(exp(stats::approx(
      xy$x, log(xy$y), xout = values, method = "linear",
      ties = "ordered", rule = 1
    )$y))
  }
  stats::approx(
    xy$x, xy$y, xout = values, method = "linear",
    ties = "ordered", rule = 1
  )$y
}

.premium_change_ratio <- function(evaluator, from, to) {
  relativity_from <- evaluator(from)
  relativity_to <- evaluator(to)
  list(
    relativity_from = relativity_from,
    relativity_to = relativity_to,
    premium_change = relativity_to / relativity_from - 1
  )
}

.premium_change_ratio_curve <- function(line, from, to, scale = NULL) {
  .premium_change_ratio(
    evaluator = function(values) {
      .premium_change_evaluate(line, values, scale = scale)
    },
    from = from,
    to = to
  )
}

.incremental_premium_change <- function(line, step, at = NULL,
                                        percent = FALSE, scale = NULL) {
  if (!is.numeric(step) || length(step) != 1L || is.na(step) ||
      !is.finite(step) || step <= 0) {
    stop("`step` must be one positive finite numeric value.", call. = FALSE)
  }
  xy <- .premium_change_line_xy(line)
  if (is.null(at)) at <- xy$x[xy$x + step <= max(xy$x)]
  if (!is.numeric(at) || anyNA(at) || any(!is.finite(at))) {
    stop("`at` must contain finite numeric values.", call. = FALSE)
  }
  if (any(at < min(xy$x)) || any(at + step > max(xy$x))) {
    stop(
      "Incremental premium change can only be evaluated where both `x` and ",
      "`x + step` are inside the supported smoothing range.",
      call. = FALSE
    )
  }
  values <- .premium_change_ratio_curve(
    line, from = at, to = at + step, scale = scale
  )
  change <- values$premium_change
  if (isTRUE(percent)) change <- 100 * change
  data.frame(x = at, relativity_from = values$relativity_from,
             relativity_to = values$relativity_to,
             incremental_change = change)
}

.premium_change_wide <- function(x) {
  labels <- unique(x$step_label)
  base <- unique(as.data.frame(x[c("from", "to")]))
  key <- paste(base$from, base$to, sep = "\r")
  for (label in labels) {
    selected <- x[x$step_label == label,
                  c("from", "to", "premium_change")]
    index <- match(key, paste(selected$from, selected$to, sep = "\r"))
    base[[label]] <- selected$premium_change[index]
  }
  if (length(labels) == 2L) {
    base$Difference <- base[[labels[2L]]] - base[[labels[1L]]]
  }
  base
}

.premium_change_format_number <- function(x) {
  format(round(x, 6), big.mark = ",", scientific = FALSE, trim = TRUE)
}

.premium_change_format_percent <- function(x, percentage_points = FALSE) {
  value <- 100 * x
  suffix <- if (percentage_points) " pp" else "%"
  paste0(ifelse(value > 0, "+", ""),
         formatC(value, format = "f", digits = 1), suffix)
}

#' @export
print.premium_change <- function(x, ...) {
  variable <- attr(x, "source_variable", exact = TRUE) %||%
    unique(x$variable)[1L]
  cat("Premium change for ", variable, "\n\n", sep = "")
  comparison <- attr(x, "comparison", exact = TRUE) %||% "double"
  if (identical(comparison, "step")) {
    cat("Increment: ", .premium_change_format_number(
      attr(x, "step", exact = TRUE)
    ), "\n", sep = "")
  } else {
    cat("Comparison: doubling\n")
  }
  basis <- attr(x, "basis", exact = TRUE) %||% unique(x$basis)[1L] %||% "curve"
  cat("Basis: ", if (identical(basis, "segments")) {
    "Tariff segments"
  } else {
    "Effective smoothing curve"
  }, "\n\n", sep = "")
  display <- .premium_change_wide(x)
  display$from <- .premium_change_format_number(display$from)
  display$to <- .premium_change_format_number(display$to)
  names(display)[1:2] <- c("From", "To")
  value_columns <- setdiff(names(display), c("From", "To", "Difference"))
  for (column in value_columns) {
    display[[column]] <- .premium_change_format_percent(display[[column]])
  }
  if ("Difference" %in% names(display)) {
    display$Difference <- .premium_change_format_percent(
      display$Difference,
      percentage_points = TRUE
    )
  }
  if (length(value_columns) == 1L) {
    names(display)[names(display) == value_columns] <- "Premium change"
  }
  print(display, row.names = FALSE, right = TRUE)
  invisible(x)
}

#' Present smoothing premium changes as a gt table
#'
#' @description
#' Format an object returned by `premium_change()`. One refinement state is
#' shown as a three-column table. Multiple states are shown side by side; when
#' exactly two are selected, their difference is added in percentage points.
#'
#' @param x For `premium_change()`, a `rating_refinement` object containing at
#'   least one smoothing step. For `as_gt()`, an object returned by
#'   `premium_change()`.
#' @param locale Character string passed to `gt` for numeric formatting.
#' @param decimals Non-negative integer. Number of decimal places for changes.
#' @param title Optional table title. The source-variable name is used by
#'   default.
#' @param subtitle Optional table subtitle.
#' @param ... Additional arguments are not accepted.
#'
#' @return A `gt_tbl` object.
#' @rdname premium_change
#' @export
as_gt.premium_change <- function(x, locale = "en-US", decimals = 1,
                                 title = NULL, subtitle = NULL, ...) {
  rlang::check_installed("gt")
  .check_dots_empty(...)
  if (!inherits(x, "premium_change")) {
    stop("`x` must be a `premium_change` object.", call. = FALSE)
  }
  if (!is.character(locale) || length(locale) != 1L || is.na(locale)) {
    stop("`locale` must be one character string.", call. = FALSE)
  }
  if (!is.numeric(decimals) || length(decimals) != 1L || is.na(decimals) ||
      decimals < 0 || decimals != as.integer(decimals)) {
    stop("`decimals` must be one non-negative whole number.", call. = FALSE)
  }
  data <- .premium_change_wide(x)
  names(data)[1:2] <- c("From", "To")
  step_columns <- setdiff(names(data), c("From", "To", "Difference"))
  if (length(step_columns) == 1L) {
    names(data)[names(data) == step_columns] <- "Premium change"
  }
  difference <- "Difference" %in% names(data)
  if (difference) data$Difference <- 100 * data$Difference
  out <- gt::gt(data = data, locale = locale)
  value_columns <- setdiff(names(data), c("From", "To", "Difference"))
  out <- gt::fmt_number(out, columns = c("From", "To"), decimals = 0,
                        use_seps = TRUE, locale = locale)
  out <- gt::fmt_percent(out, columns = value_columns, decimals = decimals,
                         locale = locale)
  if (difference) {
    out <- gt::fmt_number(out, columns = "Difference", decimals = decimals,
                          suffixing = FALSE, locale = locale)
    out <- gt::text_transform(
      out,
      locations = gt::cells_body(columns = "Difference"),
      fn = function(values) paste0(values, " pp")
    )
  }
  label_args <- stats::setNames(as.list(names(data)), names(data))
  out <- do.call(gt::cols_label, c(list(.data = out), label_args))
  if (is.null(title)) {
    variable <- attr(x, "source_variable", exact = TRUE)
    comparison <- attr(x, "comparison", exact = TRUE) %||% "double"
    title <- if (identical(comparison, "step")) {
      paste0(
        "Premium change for +",
        .premium_change_format_number(attr(x, "step", exact = TRUE)),
        " ", variable
      )
    } else {
      paste("Premium effect of doubling", variable)
    }
  }
  if (is.null(subtitle)) {
    basis <- attr(x, "basis", exact = TRUE) %||% unique(x$basis)[1L] %||%
      "curve"
    subtitle <- if (identical(basis, "segments")) {
      "Based on effective tariff-segment relativities"
    } else {
      "Based on the effective continuous smoothing curve"
    }
  }
  if (!is.null(title)) {
    out <- gt::tab_header(out, title = title, subtitle = subtitle)
  } else if (!is.null(subtitle)) {
    out <- gt::tab_header(out, subtitle = subtitle)
  }
  out
}
