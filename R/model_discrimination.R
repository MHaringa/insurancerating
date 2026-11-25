#' Assess discrimination in insurance pricing
#'
#' @description
#' Compare how fitted models or externally supplied scores rank risk on the
#' same evaluation portfolio. Discrimination measures ranking, not calibration
#' or prediction error.
#'
#' @param model A GLM or named list of GLMs with comparable responses and weights.
#' @param data Evaluation data for the models.
#' @param observed Non-negative observed totals for the evaluation records.
#' @param predicted Non-negative predicted totals, or a named list of such vectors.
#' @param exposure Positive exposure per record. Defaults to one for supplied
#'   totals. For models it can be a vector or a column name in `data`; by
#'   default use exposure from the GLM or the prior weights for mean responses
#'   such as average severity. Supply totals, not per-exposure rates.
#' @param reference Optional positive reference totals or a reference GLM for
#'   ordered Lorenz curves. Use the same scale and records as the candidates.
#' @param n_groups Target number of approximately equal-exposure lift groups.
#'
#' @details
#' The concentration curve ranks records from low to high predicted total
#' divided by exposure. Its axes are cumulative shares of exposure and observed
#' outcomes. The signed Gini is one minus twice the trapezoidal area under this
#' curve. The classical Lorenz curve ranks by realised outcome per exposure
#' unit. Its Gini is the denominator of normalized Gini, not a feasible
#' predictive benchmark. A zero denominator or zero observed total gives
#' an undefined normalized Gini. Ginis from different cohorts, response scales
#' or aggregation levels need not be comparable.
#'
#' Lift groups are ordered from low to high risk. A tied score block stays
#' together, so fewer groups may result. Observed and predicted lift divide the
#' respective group rates by the corresponding portfolio rates.
#'
#' For an ordered Lorenz curve, order by candidate divided by reference
#' prediction; plot cumulative reference predictions against cumulative
#' observed outcomes. A positive relative Gini means that realised outcomes
#' tend to be concentrated where the candidate predicts more relative to the
#' reference. Proportional tariffs give zero. The comparison is directional,
#' not proof of profitability. Scores are rounded to 14 significant digits
#' before checking ties to suppress roundoff from exposure normalisation.
#'
#' The model interface supports Poisson, quasi-Poisson, Gamma and Tweedie GLMs.
#' The response totals use the same weighting rules as [calibration_analysis()].
#' Mean severity with claim-count weights therefore compares total losses
#' over exposure measured in claims. Supply complete evaluation records; the
#' function does not silently discard rows for one model but not another.
#'
#' @return A `discrimination_result` with `summary`, `concentration`,
#'   `lorenz`, `lift` and, when requested, `ordered_lorenz`.
#' @references
#' Frees, E. W., Meyers, G., and Cummings, A. D. (2014).
#' Insurance Ratemaking and a Gini Index. Journal of Risk and Insurance,
#' 81(2), 335--366. \doi{10.1111/j.1539-6975.2012.01507.x}
#'
#' @examples
#' result <- discrimination_analysis(
#'   observed = c(100, 200, 300, 800, 1100),
#'   predicted = c(200, 100, 300, 1100, 800),
#'   reference = rep(500, 5), n_groups = 3
#' )
#' result$summary
#' autoplot(result)
#' autoplot(result, type = "lift")
#' @seealso [calibration_analysis()], [model_performance()], [validate_performance()]
#' @export
discrimination_analysis <- function(model = NULL, data = NULL, observed = NULL,
                                    predicted = NULL, exposure = NULL,
                                    reference = NULL, n_groups = 10) {
  if (!is.numeric(n_groups) || length(n_groups) != 1L || !is.finite(n_groups) ||
      n_groups < 1 || n_groups != floor(n_groups) || n_groups > .Machine$integer.max) {
    stop("`n_groups` must be a positive whole number.", call. = FALSE)
  }
  if (!is.null(model)) {
    if (!is.null(observed) || !is.null(predicted)) {
      stop("Supply models or observed and predicted totals, not both.", call. = FALSE)
    }
    if (!is.data.frame(data) || !nrow(data)) {
      stop("Supply non-empty evaluation `data` with models.", call. = FALSE)
    }
    models <- if (inherits(model, "glm")) list(model) else model
    if (!is.list(models) || !length(models)) stop("Supply one or more GLMs.", call. = FALSE)
    evaluations <- lapply(models, .discrimination_model_values, data = data,
                          exposure = exposure)
    observed <- evaluations[[1L]]$observed
    mass <- evaluations[[1L]]$exposure
    comparable <- function(x) {
      isTRUE(all.equal(x$observed, observed, check.attributes = FALSE)) &&
        isTRUE(all.equal(x$exposure, mass, check.attributes = FALSE))
    }
    if (!all(vapply(evaluations, comparable, logical(1)))) {
      stop("Models must use the same observed totals and exposure basis.", call. = FALSE)
    }
    predicted <- lapply(evaluations, `[[`, "predicted")
    if (!is.null(reference)) {
      ref <- .discrimination_model_values(reference, data, exposure)
      if (!comparable(ref)) {
        stop("The reference must use the same observed totals and exposure basis.",
             call. = FALSE)
      }
      reference <- ref$predicted
    }
    exposure <- mass
  } else if (!is.null(data) || is.null(observed) || is.null(predicted)) {
    stop("Supply both `observed` and `predicted` totals, or supply a model and data.",
         call. = FALSE)
  }
  n <- length(observed)
  if (!n) stop("At least one evaluation row is required.", call. = FALSE)
  .calibration_check_values(observed, n, "Observed totals")
  exposure <- exposure %||% rep.int(1, n)
  .discrimination_check_exposure(exposure, n)
  predicted <- if (is.list(predicted)) predicted else list(predicted)
  if (!length(predicted)) stop("Supply at least one prediction.", call. = FALSE)
  names_pred <- names(predicted) %||% rep("", length(predicted))
  blank <- is.na(names_pred) | !nzchar(names_pred)
  names_pred[blank] <- paste("Model", which(blank))
  if (anyDuplicated(names_pred)) stop("Model names must be unique.", call. = FALSE)
  if (!is.null(reference)) {
    .calibration_check_values(reference, n, "Reference totals")
    if (any(reference <= 0)) stop("Reference predictions must be positive.", call. = FALSE)
  }
  perfect <- .discrimination_curve(.discrimination_blocks(observed / exposure,
                                                           exposure, observed))
  benchmark <- .curve_gini(perfect)
  result <- lapply(seq_along(predicted), function(i) {
    p <- predicted[[i]]
    .calibration_check_values(p, n, "Predicted totals")
    blocks <- .discrimination_blocks(p / exposure, exposure, observed, p)
    concentration <- .discrimination_curve(blocks)
    gini <- .curve_gini(concentration)
    ordered <- if (!is.null(reference)) {
      .discrimination_curve(.discrimination_blocks(p / reference, reference, observed))
    }
    lift <- .discrimination_lift(blocks, n_groups)
    list(summary = data.frame(
      Model = names_pred[i], Gini = gini,
      Normalized_Gini = .normalized_gini(gini, benchmark),
      Relative_Gini = if (is.null(ordered)) NA_real_ else .curve_gini(ordered),
      Exposure = sum(exposure), Observed = sum(observed), Expected = sum(p), N = n
    ), concentration = cbind(Model = names_pred[i], concentration),
    lift = cbind(Model = names_pred[i], lift),
    ordered_lorenz = if (!is.null(ordered)) cbind(Model = names_pred[i], ordered))
  })
  bind <- function(name) do.call(rbind, lapply(result, `[[`, name))
  structure(list(summary = bind("summary"), concentration = bind("concentration"),
                 lorenz = perfect, lift = bind("lift"), ordered_lorenz = bind("ordered_lorenz")),
            class = "discrimination_result")
}

.discrimination_model_values <- function(model, data, exposure = NULL,
                                         predictions = NULL) {
  if (!inherits(model, "glm") ||
      !grepl("^(poisson$|quasipoisson$|gamma$|tweedie($|[ (]))",
             tolower(stats::family(model)$family))) {
    stop("Discrimination supports Poisson, quasi-Poisson, Gamma and Tweedie GLMs, ",
         "or independently supplied totals.", call. = FALSE)
  }
  values <- .calibration_values(model, data, require_exposure = is.null(exposure),
                                 predictions = predictions)
  if (is.character(exposure)) {
    if (!.is_single_string(exposure) || !exposure %in% names(data)) {
      stop("`exposure` must name a column in `data`.", call. = FALSE)
    }
    exposure <- data[[exposure]]
  }
  if (is.null(exposure)) {
    exposure <- if (is.null(values$exposure)) {
      values$multiplier
    } else if (length(values$offsets)) {
      values$exposure * values$multiplier
    } else {
      values$exposure
    }
  }
  .discrimination_check_exposure(exposure, nrow(data))
  list(observed = values$observed, predicted = values$expected, exposure = exposure)
}

.discrimination_check_exposure <- function(exposure, n) {
  .calibration_check_values(exposure, n, "Exposure")
  if (any(exposure <= 0)) stop("Exposure must be strictly positive.", call. = FALSE)
}

.discrimination_blocks <- function(score, mass, observed, predicted = NULL) {
  if (any(!is.finite(score))) stop("Ranking scores must be finite.", call. = FALSE)
  score <- signif(score, digits = 14L)
  sorted <- order(score, method = "radix")
  ties <- cumsum(c(TRUE, diff(score[sorted]) != 0))
  out <- rowsum(cbind(Mass = mass[sorted], Observed = observed[sorted],
                     Predicted = predicted[sorted]), ties, reorder = FALSE)
  if (any(!is.finite(colSums(out)))) stop("Discrimination totals must be finite.",
                                        call. = FALSE)
  out
}

.discrimination_curve <- function(blocks) {
  x <- c(0, cumsum(blocks[, "Mass"]) / sum(blocks[, "Mass"]))
  observed <- blocks[, "Observed"]
  y <- cumsum(observed)
  y <- if (sum(observed) > 0) y / sum(observed) else rep(NA_real_, length(y))
  x[length(x)] <- 1
  if (is.finite(y[length(y)])) y[length(y)] <- 1
  data.frame(X = x, Loss_share = c(0, y))
}

.curve_gini <- function(curve) {
  y <- curve$Loss_share
  gini <- 1 - sum(diff(curve$X) * (y[-1L] + y[-length(y)]))
  if (is.finite(gini) && abs(gini) < 1e-14) 0 else gini
}

.normalized_gini <- function(gini, perfect) {
  if (is.finite(perfect) && perfect > 0) gini / perfect else NA_real_
}

.discrimination_metrics <- c("gini", "normalized_gini")

.discrimination_scores <- function(model, data, predictions) {
  d <- .discrimination_model_values(model, data, predictions = predictions)
  gini <- .curve_gini(.discrimination_curve(.discrimination_blocks(
    d$predicted / d$exposure, d$exposure, d$observed
  )))
  ideal <- .curve_gini(.discrimination_curve(.discrimination_blocks(
    d$observed / d$exposure, d$exposure, d$observed
  )))
  c(gini = gini, normalized_gini = .normalized_gini(gini, ideal))
}

.discrimination_lift <- function(blocks, n_groups) {
  w <- blocks[, "Mass"]
  group <- pmin(n_groups, floor(n_groups * ((cumsum(w) - w / 2) / sum(w))) + 1)
  totals <- rowsum(blocks, group, reorder = FALSE)
  observed <- totals[, "Observed"] / totals[, "Mass"]
  predicted <- totals[, "Predicted"] / totals[, "Mass"]
  ratio <- function(rate, total) {
    average <- sum(totals[, total]) / sum(w)
    if (average > 0) rate / average else rep(NA_real_, length(rate))
  }
  data.frame(Group = seq_len(nrow(totals)), Exposure = totals[, "Mass"],
             Observed = totals[, "Observed"], Expected = totals[, "Predicted"],
             Observed_rate = observed, Expected_rate = predicted,
             Observed_lift = ratio(observed, "Observed"),
             Expected_lift = ratio(predicted, "Predicted"),
             row.names = NULL)
}

#' Plot discrimination and lift
#' @param object A [discrimination_analysis()] result.
#' @param ... Not used.
#' @param type Plot of concentration curves, lift or ordered Lorenz curves.
#'   `"lorenz"` is an alias for `"concentration"`.
#' @param benchmark Show the classical Lorenz benchmark on concentration plots.
#' @return A ggplot object.
#' @export
autoplot.discrimination_result <- function(object, ...,
                                             type = c("concentration", "lift",
                                                      "ordered_lorenz", "lorenz"),
                                             benchmark = TRUE) {
  .check_dots_empty(...)
  type <- match.arg(type)
  .assert_single_logical(benchmark, "benchmark")
  if (type == "lift") {
    d <- object$lift
    lift <- rbind(
      data.frame(Model = d$Model, Group = d$Group, Series = "Observed", Lift = d$Observed_lift),
      data.frame(Model = d$Model, Group = d$Group, Series = "Expected", Lift = d$Expected_lift)
    )
    p <- ggplot2::ggplot(lift, ggplot2::aes(.data$Group, .data$Lift, colour = .data$Series)) +
      ggplot2::geom_hline(yintercept = 1, colour = "grey55", linetype = 2) +
      ggplot2::geom_line(na.rm = TRUE) + ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::facet_wrap(~Model) +
      ggplot2::labs(x = "Group (low to high predicted risk)", y = "Lift", colour = NULL)
  } else {
    ordered <- type == "ordered_lorenz"
    d <- if (ordered) object$ordered_lorenz else object$concentration
    if (is.null(d)) stop("Supply a reference to plot ordered Lorenz curves.", call. = FALSE)
    gini <- if (ordered) object$summary$Relative_Gini else object$summary$Gini
    d$Model <- paste0(d$Model, " (", format(round(gini[match(d$Model,
                                          object$summary$Model)], 3), trim = TRUE), ")")
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$X, .data$Loss_share, colour = .data$Model)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey55", linetype = 2)
    if (!ordered && benchmark) {
      p <- p + ggplot2::geom_line(
        data = object$lorenz, ggplot2::aes(.data$X, .data$Loss_share),
        inherit.aes = FALSE, colour = "grey45", linetype = 3, na.rm = TRUE
      )
    }
    p <- p + ggplot2::geom_line(na.rm = TRUE) + ggplot2::coord_fixed() +
      ggplot2::scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
      ggplot2::scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
      ggplot2::labs(x = if (ordered) "Share of reference predictions" else "Share of exposure",
                    y = "Share of observed outcomes",
                    colour = if (ordered) "Model (relative Gini)" else "Model (Gini)",
                    caption = if (!ordered && benchmark) "Dotted line: realised ranking.")
  }
  p + ggplot2::theme_minimal() + .plot_grid_theme_ir()
}
