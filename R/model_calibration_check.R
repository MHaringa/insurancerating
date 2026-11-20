#' Check observed versus expected insurance experience
#'
#' @description
#' Compare the observed and predicted totals from a fitted GLM, overall or
#' by calendar year, rating factor or predicted risk. The model is not changed.
#'
#' @param model A fitted Poisson, quasi-Poisson, Gamma or Tweedie GLM.
#' @param data A non-empty data frame with the response and variables used in
#'   the model, including any weight and offset variables. Responses,
#'   predictions and weights must be finite and non-negative.
#'   For a refined model's own portfolio, use [extract_model_data()] to
#'   include the columns created by the refinement.
#' @param by Optional name of one grouping column. Factor levels follow
#'   their declared order; other values are sorted. Missing values form
#'   a separate group at the end.
#' @param groups Optional number of groups formed by sorting records from
#'   lowest to highest predicted risk. Supply a positive whole number no
#'   larger than the number of rows.
#'   Use either `groups` or `by`, not both.
#'
#' @details
#' Overall totals show calibration-in-the-large. Risk groups check whether the
#' model over- or under-predicts at different predicted risk levels. A
#' calendar-year grouping checks temporal calibration. Changing O/E over
#' time does not necessarily mean the model has become worse at ranking risks.
#'
#' Predictions are on the response scale and use the offset in `data`.
#' For Poisson claim counts with a log-exposure offset, predictions are
#' expected counts and are not multiplied by exposure again. Other prior
#' weights are used for weighted totals, for example claim counts in a
#' weighted average-severity GLM or exposure in a claim-rate GLM. If a
#' recognised offset already uses the same column as the prior weights,
#' that column is not applied again; weights that use it in a more complex
#' expression are rejected as ambiguous. Specify weights using columns in
#' `data`; weights stored as a fixed vector cannot be matched to new rows.
#'
#' A simple `log(column)` offset in a log-link GLM identifies an exposure
#' column. It may be supplied in the formula, as `offset = log(column)`, or
#' as one term of an additive offset. Exposure must be positive. With
#' several candidate terms, a single column named `exposure` takes precedence.
#' Without an offset, explicit `weights = exposure`, or weights on the
#' denominator column of a response such as `I(claims / duration)`, identify
#' exposure used as prior weights. Other weight variables and unused columns
#' are not assumed to represent exposure.
#'
#' ## Calibration by predicted risk
#'
#' With `groups = 5`, group 1 contains the lowest predicted risks and group 5
#' the highest. With a recognised exposure offset, the ranking uses
#' predictions per unit of exposure. For a claim-count model this means
#' predicted claims per policy-year rather than predicted claims for the
#' observed period. A longer policy period therefore does not by itself
#' put a policy in a higher risk group. Without an offset, the
#' function ranks the response-scale predictions. If an offset cannot be
#' identified as exposure, use `by` or overall calibration instead.
#'
#' The sorted records are divided so each group has roughly the same total
#' exposure (about 20% each for five groups), without splitting any record.
#' If exposure is unavailable or totals zero, the aim is equal row counts.
#' Ties follow the input row order, so policies with the same prediction
#' on the ranking scale can appear in different groups.
#'
#' Compare the observed and expected totals within each group. For example,
#' 24 observed claims versus 20 expected means O/E is 1.2: the model
#' underpredicts for that group. Look for patterns across groups and
#' consider exposure and claim volume before interpreting a gap. This is
#' a calibration diagnostic, not a hypothesis test or a measure of how
#' well the model ranks individual risks. Use separate evaluation data to
#' assess calibration outside the estimation sample.
#'
#' ## Calibration after refinement
#'
#' Use the fitted model returned by [refit()], not the refinement plan.
#' `check_calibration(smoothed_model, extract_model_data(smoothed_model))`
#' evaluates the smoothed model on its own portfolio. To review the new
#' tariff bands, use the band column as `by`: for example, `by = "age_smooth"`
#' when smoothing from `source_variable = "age"`. The numeric relativity
#' column used by the model is a separate column. For a separate evaluation
#' portfolio, create both columns using the same refinement mappings.
#' When a grouping column is unavailable, the error can suggest the related
#' smoothing band or a similar column name. A valid `by` is never replaced:
#' `by = "age"` groups by the original ages, not the tariff bands.
#'
#' To compare observed outcomes with the expected outcomes before and after a
#' restriction, evaluate both fitted models on the same records and group
#' them by the same `by` column. Do not match `groups` by group number across
#' models: each model can rank the records differently.
#'
#' @return A data frame of class `calibration_result` with `Observed`,
#'   `Expected`, `OE` (observed divided
#'   by expected) and `N` (number of observations). If expected is zero,
#'   `OE` is `NA`. The grouping column or integer `Group` is included when
#'   applicable. `Exposure` is the sum of the identified exposure column,
#'   without applying any case weights.
#'
#' @examples
#' portfolio <- as.data.frame(MTPL2)
#' portfolio$area <- factor(portfolio$area)
#' # Illustrative calendar years; MTPL2 does not contain actual policy dates.
#' portfolio$year <- rep(2023:2024, length.out = nrow(portfolio))
#' model <- glm(nclaims ~ area + offset(log(exposure)),
#'              family = poisson(), data = portfolio)
#'
#' check_calibration(model, portfolio)
#' check_calibration(model, portfolio, by = "year")
#' check_calibration(model, portfolio, by = "area")
#' # Compare the lowest to highest predicted claim frequencies.
#' check_calibration(model, portfolio, groups = 5)
#'
#' @seealso [autoplot.calibration_result()], [model_performance()],
#'   [resample_performance()], [adjust_intercept()], [extract_model_data()],
#'   [audit_refinement()]
#' @export
check_calibration <- function(model, data, by = NULL, groups = NULL) {
  if (!inherits(model, "glm")) {
    stop("`model` must be a fitted GLM.", call. = FALSE)
  }
  family <- stats::family(model)
  family_name <- tolower(family$family)
  if (!family_name %in% c("poisson", "quasipoisson", "gamma") &&
      !grepl("^tweedie($|[ (])", family_name)) {
    stop("Calibration supports Poisson, quasi-Poisson, Gamma and Tweedie GLMs; ",
         "the model family is '", family$family, "'.", call. = FALSE)
  }
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }
  data <- as.data.frame(data)
  n <- nrow(data)
  smoothing <- attr(model, "mgd_smt", exact = TRUE)
  refinement_columns <- unique(c(
    attr(model, "new_col_nm", exact = TRUE),
    unlist(smoothing, use.names = FALSE)
  ))
  reserved <- c("Observed", "Expected", "OE", "N", "Exposure")
  if (!is.null(by) && !is.null(groups)) {
    stop("Supply either `by` or `groups`, not both.", call. = FALSE)
  }
  if (!is.null(by)) {
    if (!is.character(by) || length(by) != 1L || !is.null(dim(by)) ||
        is.na(by) || !nzchar(by)) {
      stop("`by` must name one column of `data`.", call. = FALSE)
    }
    if (!by %in% names(data)) {
      if (by %in% refinement_columns) .calibration_missing_refinement(by)
      suggested <- .calibration_smoothing_bands(by, smoothing)
      if (!length(suggested)) {
        bands <- vapply(smoothing, `[[`, character(1), 1L)
        groupable <- names(data)[vapply(data, function(x) {
          is.atomic(x) && is.null(dim(x))
        }, logical(1))]
        candidates <- setdiff(union(groupable, bands),
                              c(reserved, setdiff(refinement_columns, bands)))
        suggested <- .closest_refinement_value(by, candidates)
      }
      if (any(!suggested %in% names(data))) {
        .calibration_missing_refinement(by, suggested)
      }
      stop("`by` must name one column of `data`; `", by, "` was not found.",
           .calibration_by_hint(suggested), call. = FALSE)
    }
    if (by %in% reserved) {
      stop("Rename the `by` column to avoid a calibration output name: ", by, ".",
           call. = FALSE)
    }
    if (!is.atomic(data[[by]]) || !is.null(dim(data[[by]]))) {
      stop("The `by` column must be a vector of grouping values.", call. = FALSE)
    }
  }
  if (!is.null(groups) &&
      (!is.numeric(groups) || length(groups) != 1L || !is.null(dim(groups)) ||
       !is.finite(groups) ||
       groups < 1 || groups > n || groups != floor(groups))) {
    stop("`groups` must be a positive whole number no larger than nrow(data).",
         call. = FALSE)
  }

  formula <- stats::formula(model)
  model_call <- stats::getCall(model)
  weight_expr <- model_call[["weights"]]
  offset_expr <- model_call[["offset"]]
  if (!is.null(weight_expr) && !length(all.vars(weight_expr))) {
    stop("Specify the model's prior weights using columns in `data`, ",
         "rather than a fixed vector.", call. = FALSE)
  }
  vars <- unique(c(all.vars(formula), all.vars(weight_expr), all.vars(offset_expr)))
  absent <- setdiff(vars, names(data))
  if (length(absent)) {
    if (any(absent %in% refinement_columns)) {
      .calibration_missing_refinement(absent, .calibration_smoothing_bands(by, smoothing))
    }
    stop("Missing model variables in `data`: ", paste(absent, collapse = ", "),
         ".", call. = FALSE)
  }

  model_terms <- stats::terms(model)
  offsets <- lapply(attr(model_terms, "offset"), function(i) {
    attr(model_terms, "variables")[[i + 1L]][[2L]]
  })
  if (!is.null(offset_expr)) offsets <- c(offsets, list(offset_expr))
  if (length(refinement_columns) && length(offsets)) {
    # Fixed tariff relativities are prediction offsets, not exposure offsets.
    offsets <- unlist(lapply(offsets, .calibration_offset_terms), recursive = FALSE)
    offsets <- Filter(function(term) {
      columns <- all.vars(term)
      !length(columns) || !all(columns %in% refinement_columns)
    }, offsets)
  }
  exposure_column <- .calibration_exposure_column(
    family$link, offsets, weight_expr, formula[[2L]]
  )
  exposure <- if (!is.null(exposure_column)) data[[exposure_column]]
  if (!is.null(exposure_column)) {
    .calibration_check_values(exposure, n, "Exposure")
    if (length(offsets) && any(exposure <= 0)) {
      stop("Exposure used in a log offset must be positive.", call. = FALSE)
    }
  } else if (!is.null(groups) && length(offsets)) {
    stop("Risk grouping requires an identifiable exposure for models with an ",
         "offset. Use a simple log-exposure offset, `by`, or overall calibration.",
         call. = FALSE)
  }

  observed <- eval(formula[[2L]], data, environment(formula))
  expected <- stats::predict(model, newdata = data, type = "response")
  .calibration_check_values(observed, n, "Observed responses")
  .calibration_check_values(expected, n, "Predictions")
  risk <- expected

  if (!is.null(weight_expr)) {
    weights <- eval(weight_expr, data, environment(formula))
    .calibration_check_values(weights, n, "Prior weights")
    if (length(offsets) && !is.null(exposure_column) &&
        exposure_column %in% all.vars(weight_expr)) {
      if (!identical(weight_expr, as.name(exposure_column))) {
        stop("The prior weights reuse the exposure offset in an expression. ",
             "Use distinct case weights to avoid counting exposure twice.",
             call. = FALSE)
      }
    } else {
      observed <- observed * weights
      expected <- expected * weights
    }
  }

  labels <- NULL
  if (!is.null(by)) {
    group_values <- data[[by]]
    group_values[is.na(group_values)] <- NA
    labels <- sort(unique(group_values), na.last = TRUE)
    group_id <- match(group_values, labels)
  } else if (!is.null(groups)) {
    if (length(offsets)) {
      # Ignore rounding noise when undoing the exposure offset for ranking.
      risk <- signif(risk / exposure, digits = 14L)
    }
    group_id <- .calibration_risk_groups(risk, exposure, as.integer(groups))
    labels <- seq_len(groups)
  } else {
    group_id <- rep.int(1L, n)
  }

  totals <- rowsum(cbind(Observed = observed, Expected = expected,
                         Exposure = exposure), group_id)
  if (any(!is.finite(totals))) {
    stop("Calibration totals must be finite.", call. = FALSE)
  }
  out <- data.frame(
    Observed = totals[, "Observed"],
    Expected = totals[, "Expected"],
    OE = ifelse(totals[, "Expected"] > 0,
                totals[, "Observed"] / totals[, "Expected"], NA_real_),
    N = tabulate(group_id),
    row.names = NULL
  )
  if (!is.null(exposure)) {
    out$Exposure <- totals[, "Exposure"]
  }
  if (!is.null(labels)) {
    label <- if (is.null(by)) "Group" else by
    out[[label]] <- labels
    out <- out[c(label, setdiff(names(out), label))]
  }
  grouping <- if (!is.null(groups)) "risk" else if (!is.null(by)) "by" else "overall"
  structure(out, class = c("calibration_result", class(out)),
            calibration_grouping = grouping, calibration_by = by)
}


.calibration_smoothing_bands <- function(by, smoothing) {
  if (is.null(by)) return(character())
  unique(unlist(lapply(smoothing, function(pair) {
    source_names <- sub("_smooth$", "", pair)
    if (tolower(by) %in% tolower(source_names)) pair[1L]
  }), use.names = FALSE))
}


.calibration_by_hint <- function(suggestions) {
  if (!length(suggestions)) return("")
  paste0(" Did you mean ", paste0("`", suggestions, "`", collapse = " or "),
         " for `by`?")
}


.calibration_missing_refinement <- function(columns, suggested_by = NULL) {
  stop(
    "Missing columns for the refined model or grouping: ",
    paste0("`", columns, "`", collapse = ", "), ".",
    .calibration_by_hint(suggested_by),
    " For the refit's own portfolio, use `data = extract_model_data(model)`. ",
    "For a separate evaluation portfolio, apply the same refinement mappings first.",
    call. = FALSE
  )
}


.calibration_check_values <- function(x, n, label) {
  if (!is.numeric(x) || !is.null(dim(x)) || length(x) != n ||
      any(!is.finite(x)) || any(x < 0)) {
    stop(label, " must contain one finite, non-negative numeric value per row.",
         call. = FALSE)
  }
}


.calibration_exposure_column <- function(link, offsets, weights, response) {
  if (length(offsets)) {
    if (!identical(link, "log")) return(NULL)
    terms <- unlist(lapply(offsets, .calibration_offset_terms), recursive = FALSE)
    candidates <- vapply(terms, function(x) {
      if (is.call(x) && length(x) == 2L && is.symbol(x[[2L]]) &&
          (identical(x[[1L]], quote(log)) || identical(x[[1L]], quote(base::log)))) {
        return(as.character(x[[2L]]))
      }
      NA_character_
    }, character(1))
    index <- which(!is.na(candidates))
    if (length(index) > 1L) index <- which(candidates == "exposure")
    if (length(index) != 1L) return(NULL)
    column <- candidates[[index]]
    if (column %in% unlist(lapply(terms[-index], all.vars), use.names = FALSE)) {
      return(NULL)
    }
    return(column)
  }
  if (!is.symbol(weights)) return(NULL)
  if (identical(weights, quote(exposure))) return("exposure")
  if (is.call(response) && identical(response[[1L]], quote(I))) {
    response <- response[[2L]]
  }
  if (is.call(response) && length(response) == 3L &&
      identical(response[[1L]], quote(`/`)) && identical(response[[3L]], weights)) {
    as.character(weights)
  }
}


.calibration_offset_terms <- function(x) {
  if (is.call(x) && identical(x[[1L]], quote(`(`))) {
    return(.calibration_offset_terms(x[[2L]]))
  }
  if (is.call(x) && length(x) == 3L && identical(x[[1L]], quote(`+`))) {
    return(c(.calibration_offset_terms(x[[2L]]), .calibration_offset_terms(x[[3L]])))
  }
  list(x)
}


.calibration_risk_groups <- function(risk, exposure, groups) {
  n <- length(risk)
  ord <- order(risk, method = "radix")
  cuts <- round(seq(0, n, length.out = groups + 1L))
  if (!is.null(exposure) && any(exposure > 0) && groups > 1L) {
    cumulative <- c(0, cumsum(exposure[ord]))
    targets <- cumulative[n + 1L] * (seq_len(groups - 1L) / groups)
    left <- findInterval(targets, cumulative, all.inside = TRUE)
    right <- left + 1L
    ends <- ifelse(targets - cumulative[left] <= cumulative[right] - targets,
                   left - 1L, right - 1L)
    # Leave at least one row for each remaining group.
    for (i in seq_along(ends)) {
      cuts[i + 1L] <- max(cuts[i] + 1L, min(ends[i], n - groups + i))
    }
  }
  group <- integer(n)
  group[ord] <- rep(seq_len(groups), times = diff(cuts))
  group
}
