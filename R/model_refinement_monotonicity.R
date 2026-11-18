# Factor contrasts (e.g. .L, .Q or sum contrasts) are not category effects.
# Recover the fitted term's contribution on its original intercept scale.
.categorical_refinement_effects <- function(model, table) {
  terms <- stats::terms(model)
  classes <- attr(terms, "dataClasses")
  labels <- attr(terms, "term.labels")
  variables <- intersect(names(classes)[classes %in% c("factor", "ordered")], labels)
  if (!length(variables)) return(table)

  frame <- stats::model.frame(model)
  design <- stats::model.matrix(model)
  for (variable in variables) {
    cols <- which(attr(design, "assign") == match(variable, labels))
    coefficients <- colnames(design)[cols]
    lv <- model$xlevels[[variable]]
    rows <- match(lv, as.character(frame[[variable]]))
    effects <- design[rows, cols, drop = FALSE] %*% stats::coef(model)[coefficients]
    contrast_rows <- table$risk_factor %in% setdiff(coefficients, labels)
    table <- table[!contrast_rows, , drop = FALSE]
    idx <- which(table$risk_factor == variable)
    table$estimate[idx] <- exp(as.numeric(effects))[match(table$level[idx], lv)]
  }
  table
}

# Weighted pool-adjacent-violators: each block is pushed and merged at most once.
.monotone_pava <- function(y, weights) {
  means <- y
  mass <- log(weights)
  ends <- integer(length(y))
  n <- 0L
  for (i in seq_along(y)) {
    n <- n + 1L
    means[n] <- y[i]
    mass[n] <- log(weights[i])
    ends[n] <- i
    while (n > 1L && means[n - 1L] > means[n]) {
      j <- n - 1L
      # A log-sum keeps pooled weights finite even for very large volumes.
      total <- max(mass[j], mass[n]) + log1p(exp(-abs(mass[j] - mass[n])))
      means[j] <- means[j] + exp(mass[n] - total) * (means[n] - means[j])
      mass[j] <- total
      ends[j] <- ends[n]
      n <- j
    }
  }
  blocks <- seq_len(n)
  rep(means[blocks], diff(c(0L, ends[blocks])))
}

.monotonicity_level_order <- function(group, variable) {
  lv <- levels(group)
  if (anyNA(lv) || length(lv) < 2L) {
    stop("`", variable, "` requires at least two categories with no missing levels.",
         call. = FALSE)
  }
  bounds <- .parse_rating_table_numeric_levels(lv)
  if (is.null(bounds)) {
    if (is.ordered(group)) return(lv)
    stop("`", variable, "` must have numeric level labels or numeric intervals, ",
         "or be an ordered factor with explicit levels before fitting the GLM. ",
         "Alphabetical order is not a tariff order.", call. = FALSE)
  }
  intervals <- lapply(lv, .parse_restriction_interval)
  is_interval <- !vapply(intervals, is.null, logical(1))
  idx <- order(bounds$lower, bounds$upper)
  if (all(is_interval)) {
    adjacent <- seq_len(length(idx) - 1L)
    overlap <- vapply(adjacent, function(i) {
      .restriction_intervals_overlap(intervals[[idx[i]]], intervals[[idx[i + 1L]]])
    }, logical(1))
    if (any(bounds$lower >= bounds$upper) || any(overlap)) {
      stop("Numeric intervals of `", variable, "` must be non-empty and ",
           "non-overlapping to define a monotonic order.", call. = FALSE)
    }
  } else if (any(is_interval) || anyDuplicated(bounds$lower)) {
    stop("Numeric levels of `", variable, "` must represent distinct values ",
         "or non-overlapping intervals; do not mix the two.", call. = FALSE)
  }
  if (is.ordered(group) && !identical(lv, lv[idx])) {
    warning("Using ascending numeric order for `", variable,
            "`; its ordered-factor levels have a different order. ",
            "`direction` applies to numeric values.", call. = FALSE)
  }
  lv[idx]
}

.monotonicity_values <- function(state, step) {
  variable <- step$variable
  group <- state$data[[variable]]
  lv <- step$level_order %||% levels(group)
  observed <- as.character(group)
  if (anyNA(observed) || anyNA(lv) || length(lv) < 2L ||
      !setequal(lv, observed)) {
    stop("Every level of `", variable, "` must be observed, with no ",
         "missing levels and at least two categories.", call. = FALSE)
  }

  if (step$replace_refinement_offset) {
    # Read the active offset, not the original GLM or an earlier display table.
    mapping <- unique(data.frame(level = observed, value = state$data[[step$model_term]]))
    if (anyDuplicated(mapping$level)) {
      stop("The active tariff does not have one relativity per ordered level.",
           call. = FALSE)
    }
    values <- mapping$value[match(lv, mapping$level)]
    previous <- state$rf_restricted_df
    if (any(previous$risk_factor %in% step$previous_display &
            !previous$level %in% lv)) {
      stop("A preceding refinement adds levels outside the declared order of `",
           variable, "`.", call. = FALSE)
    }
  } else {
    current <- .current_refinement_coefficients(state, variable)
    values <- current$relativity[match(lv, current$level)]
  }
  if (length(values) != length(lv) || any(!is.finite(values) | values <= 0)) {
    stop("Each ordered level requires a finite, positive, estimable relativity.",
         call. = FALSE)
  }

  w <- rep(1, length(lv))
  if (!is.null(step$weights)) {
    volume <- state$data[[step$weights]]
    if (!is.numeric(volume) || !is.null(dim(volume)) ||
        any(!is.finite(volume) | volume < 0)) {
      stop("`weights` must name finite, non-negative numeric row weights.",
           call. = FALSE)
    }
    totals <- tapply(volume, group, sum)
    w <- as.numeric(totals[lv])
    if (any(!is.finite(w) | w <= 0)) {
      stop("Aggregated weights must be finite and positive for every level.",
           call. = FALSE)
    }
  }
  idx <- seq_along(lv)
  if (step$direction == "decreasing") idx <- rev(idx)
  beta <- log(values[idx])
  adjusted <- switch(step$method,
    isotonic = .monotone_pava(beta, w[idx]),
    increase_only = cummax(beta),
    decrease_only = rev(cummin(rev(beta)))
  )
  projected <- values
  changed <- which(adjusted != beta)
  projected[idx[changed]] <- exp(adjusted[changed])
  data.frame(level = lv, original_relativity = values, weight = w,
             adjusted_relativity = projected, stringsAsFactors = FALSE)
}

.apply_monotonicity_step <- function(state, step) {
  values <- .monotonicity_values(state, step)
  step$restrictions[[2L]] <- values$adjusted_relativity
  state <- .apply_restriction_step(state, step)
  state$monotonicity_df <- values
  state
}

#' Enforce monotone tariff relativities for numeric bands or ordered categories
#'
#' Project the current level effects of a standalone categorical factor onto a
#' non-decreasing or non-increasing sequence. This is a post-estimation
#' adjustment, not likelihood-constrained GLM estimation. Unlike
#' [add_smoothing()], it needs no continuous source variable, draws no curve
#' between levels and does not impose smoothness.
#'
#' @param model A refinement specification created by [prepare_refinement()].
#' @param model_variable Name of a standalone factor in a log-link GLM with
#'   an intercept. Numeric labels (e.g. `"100"`, `"200"`, `"1000"`) and numeric
#'   intervals (e.g. levels created by `cut()`) are ordered numerically, not
#'   alphabetically, including for ordered factors. A conflicting ordered-factor
#'   level order produces a warning; numeric order is used for the projection.
#'   Intervals must not overlap, even for ordered factors. For non-numeric
#'   labels, declare the order with `ordered(x, levels = ...)` before fitting;
#'   that explicit order is retained.
#' @param direction Desired order of relativities in the numeric or declared level order.
#'   Both directions permit ties.
#' @param method `"isotonic"` minimizes the weighted sum of squared changes
#'   to log-relativities by pooling adjacent violating levels. Alternatively,
#'   `"increase_only"` raises only levels that must rise; `"decrease_only"`
#'   lowers only levels that must fall.
#' @param weights Optional name of a non-negative numeric volume column.
#'   Weights are summed by level; each total must be finite and positive.
#'   `NULL` gives each level equal weight, irrespective of its row count.
#'   Weights are recorded but do not affect the two one-sided methods.
#' @param output_variable Name of the new numeric tariff column used as a
#'   fixed offset at refit and displayed in [rating_table()]. Defaults to
#'   the model variable name followed by `_monotone`, with a unique suffix
#'   (`_1`, `_2`, ...) if needed. An explicitly supplied name must be new.
#'
#' @details For grouped insured amount, reconstruction value or similar
#'   variables, pass the existing factor used in the GLM, just as the
#'   `model_variable` of [add_smoothing()]. No `source_variable` or new `breaks`
#'   are needed: existing categories are retained and only their relativities
#'   are adjusted. A continuous numeric slope is not supported.
#'
#'   A treatment-coded reference level enters the projection with its
#'   current relativity (initially one); it can move. For polynomial contrasts
#'   the input is the exponentiated fitted level contribution, not an
#'   individual contrast coefficient. Projected values are not renormalized.
#'   [refit()] fixes their ratios and estimates the intercept and remaining
#'   free coefficients. Later explicit refinements can change these ratios.
#'
#'   Repeated calls for the same `model_variable` append separate steps and
#'   use the current relativities, including any intervening refinements.
#'   Earlier steps remain in the history; the latest projection replaces the
#'   preceding effect at refit, rather than multiplying it again. Applying the
#'   same direction to an already monotone effect leaves its values unchanged.
#'
#' @return A `rating_refinement` with an appended step; no GLM is fitted.
#'   The step's `values` table contains the ordered levels, input and adjusted
#'   relativities and level weights. Use [ggplot2::autoplot()] for a preview.
#' @seealso [add_restriction()], [add_smoothing()], [audit_refinement()]
#' @examples
#' portfolio <- data.frame(
#'   insured_amount = c(50000, 150000, 750000, 1500000, 3000000),
#'   claims = c(100, 115, 110, 135, 130), exposure = rep(1000, 5)
#' )
#' portfolio$amount_band <- cut(portfolio$insured_amount,
#'   breaks = c(0, 100000, 500000, 1000000, 2000000, Inf))
#' # cut() creates an ordinary factor; its numeric intervals provide the order.
#' fit <- glm(claims ~ amount_band + offset(log(exposure)), poisson(), portfolio)
#' spec <- prepare_refinement(fit) |>
#'   add_monotonicity("amount_band", weights = "exposure")
#' spec$steps[[1]]$values
#' rating_table(refit(spec))
#'
#' # Rechecking appends a step without overwriting the earlier specification.
#' rechecked <- spec |>
#'   add_monotonicity("amount_band", weights = "exposure")
#' vapply(rechecked$steps, function(step) step$output_variable, character(1))
#' @export
add_monotonicity <- function(model, model_variable,
                             direction = c("increasing", "decreasing"),
                             method = c("isotonic", "increase_only", "decrease_only"),
                             weights = NULL, output_variable = NULL) {
  .assert_refinement(model)
  direction <- match.arg(direction)
  method <- match.arg(method)
  fit <- model$base$model
  .assert_column_name(model_variable, "model_variable", model$base$data)
  if (!identical(fit$family$link, "log") || attr(stats::terms(fit), "intercept") != 1L) {
    stop("Monotonicity requires a log-link GLM with an intercept.", call. = FALSE)
  }
  labels <- attr(stats::terms(fit), "term.labels")
  related <- labels[vapply(labels, function(x) model_variable %in% all.vars(str2lang(x)),
                           logical(1))]
  if (length(related) != 1L || related != model_variable ||
      model_variable %in% all.vars(str2lang(model$base$offset %||% "0"))) {
    stop("Use a standalone categorical main effect, not an interaction, ",
         "transformed term or offset.", call. = FALSE)
  }
  if (!is.factor(model$base$data[[model_variable]]) ||
      !attr(stats::terms(fit), "dataClasses")[[model_variable]] %in% c("factor", "ordered")) {
    stop("`", model_variable, "` must be a grouped numeric factor or an ordered factor, ",
         "not a continuous numeric slope. Create the factor before fitting the GLM.",
         call. = FALSE)
  }
  state <- if (length(model$steps)) preview_refinement(model)$state else .make_exec_state(model)
  if (!identical(levels(state$data[[model_variable]]), fit$xlevels[[model_variable]])) {
    stop("The declared order and observed fitted levels must match.", call. = FALSE)
  }
  level_order <- .monotonicity_level_order(state$data[[model_variable]], model_variable)
  .assert_optional_column_name(weights, "weights", state$data)
  if (is.null(output_variable)) {
    output_variable <- utils::tail(make.unique(
      c(names(state$data), paste0(model_variable, "_monotone")), sep = "_"
    ), 1L)
  }
  if (!.is_single_string(output_variable) ||
      !identical(make.names(output_variable), output_variable) ||
      output_variable %in% names(state$data)) {
    stop("`output_variable` must be a new, syntactically valid column name.",
         call. = FALSE)
  }
  term <- .refinement_term_for_variable(model$steps, model_variable)
  replace <- !identical(term, model_variable) && term %in% all.vars(str2lang(state$offset %||% "0"))
  if (!replace && !term %in% attr(stats::terms(state$formula_no_offset), "term.labels")) {
    stop("The categorical main effect is no longer active in this refinement.", call. = FALSE)
  }
  map <- .restriction_variable_map(model$steps)
  step <- list(id = .next_step_id(model), type = "monotonicity",
               variable = model_variable, model_variable = model_variable,
               level_order = level_order,
               direction = direction, method = method, weights = weights,
               output_variable = output_variable, model_term = term,
               previous_display = c(model_variable, term,
                 map$restricted_variable[map$source_variable == model_variable]),
               replace_refinement_offset = replace)
  step$values <- .monotonicity_values(state, step)
  step$restrictions <- stats::setNames(step$values[c("level", "adjusted_relativity")],
                                       c(model_variable, output_variable))
  .add_step(model, step)
}
