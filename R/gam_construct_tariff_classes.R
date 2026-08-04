#' Derive candidate tariff segments from a smooth risk-factor effect
#'
#' @description
#' Approximate the smooth effect estimated by [risk_factor_gam()] with intervals
#' for a continuous risk factor. The resulting boundaries provide a candidate
#' categorical representation that can be inspected before inclusion in a
#' pricing GLM or tariff structure.
#'
#' @param object A `"risk_factor_gam"` object returned by
#'   [risk_factor_gam()]. Legacy `"riskfactor_gam"` and `"fitgam"` classes are
#'   accepted for compatibility.
#' @param segmentation_penalty Non-negative numeric penalty on additional tree
#'   splits. Larger values generally favour fewer tariff segments. The default
#'   `0` retains the historical behaviour and applies no explicit split
#'   penalty; it can therefore produce a relatively detailed candidate
#'   segmentation. There is no universal actuarial value: compare candidate
#'   penalties and assess the resulting volume and stability by segment.
#' @param seed Single finite whole number used to reproduce the evolutionary
#'   search.
#' @param max_iterations Positive integer. Maximum number of evolutionary search
#'   iterations. This is an advanced algorithm-control parameter.
#' @param population_size Positive integer. Number of candidate trees maintained
#'   during the evolutionary search. This is an advanced algorithm-control
#'   parameter.
#' @param complexity Deprecated. Use `segmentation_penalty` instead.
#' @param alpha Deprecated. Use `segmentation_penalty` instead.
#' @param niterations Deprecated. Use `max_iterations` instead.
#' @param ntrees Deprecated. Use `population_size` instead.
#'
#' @details
#' ## Method
#'
#' An evolutionary regression tree from [evtree::evtree()] is fitted to the
#' predicted GAM effect over the distinct observed risk-factor values. The tree
#' therefore approximates the estimated univariate curve; it is not fitted
#' directly to individual claim outcomes or portfolio loss. Internal tree split
#' points are translated into interval boundaries. If no internal split is
#' supported by the fitted search, one interval spanning the observed range is
#' returned.
#'
#' The method follows the data-driven binning approach described by Henckaerts
#' et al. (2018). `segmentation_penalty`, `population_size`, `max_iterations`
#' and `seed` control the stochastic search rather than an actuarial
#' minimum-volume rule. Reusing the same inputs and `seed` makes the result
#' reproducible.
#'
#' Each distinct observed risk-factor value has equal influence when the tree
#' approximates the fitted curve. Exposure, claim count or another actuarial
#' weight is deliberately not applied again in this step. The relevant
#' portfolio information has already influenced the curve through the
#' statistical specification used by [risk_factor_gam()], such as the exposure
#' offset in a frequency model, claim-count weights in a severity model or
#' exposure weights in a risk-premium model. Applying a second weight during
#' segmentation would introduce an additional portfolio-distribution choice
#' after the GAM has been estimated.
#'
#' Exposure and claim count remain available through `summary()`. They are
#' diagnostics for assessing the support and practical stability of candidate
#' segments, but they do not influence the estimated boundaries.
#'
#' ## Actuarial interpretation
#'
#' The returned segments approximate the shape of the fitted univariate GAM;
#' they are not automatically a final tariff classification. Before use in a
#' multivariate model, the boundaries should be assessed against exposure and
#' claim volume, stability across periods, operational rounding and the
#' interaction with other risk factors. Particular care is required for
#' boundaries in sparsely populated tails.
#'
#' `summary()` reports the number of portfolio records, number of
#' distinct risk-factor values and available exposure and claim volume within
#' each proposed segment. These diagnostics support actuarial review but do not
#' constitute an automatic acceptance rule. Minimum-volume requirements and
#' operational rounding should be selected with reference to portfolio size,
#' model purpose and governance standards.
#'
#' ## A staged GLM refinement workflow
#'
#' In practical pricing work, the candidate boundaries are often used to form
#' an initial set of relatively broad model groups. The actuary reviews
#' `summary()` and, where necessary, combines thinly populated segments or
#' increases `segmentation_penalty` until the groups have sufficient exposure
#' and claim information for stable estimation. The resulting factor can then
#' be included in an unrestricted GLM.
#'
#' This broad first-stage grouping avoids estimating a separate free GLM
#' coefficient for every fine tariff interval when observations are unevenly
#' distributed over the continuous risk factor. After fitting the GLM,
#' [add_smoothing()] can use the broad model effect together with the original
#' continuous variable to construct a regularised pattern over finer breaks.
#' These finer breaks may reflect operational or commercial tariff boundaries,
#' while their relativities remain linked through the smoothing specification
#' rather than being estimated independently for every small segment.
#'
#' The staged approach therefore separates statistical support from final
#' tariff granularity: broad groups provide the information used by the GLM,
#' while smoothing can translate that information into a finer and more regular
#' tariff structure. Smoothing does not create additional observations, so the
#' resulting classes should still be assessed for stability, extrapolation and
#' commercial suitability.
#'
#' The first and last boundaries equal the observed range used by the GAM.
#' Applying the segmentation to new data outside that range results in an
#' informative error rather than silent extrapolation.
#'
#' Use [autoplot.tariff_segments()] to compare the smooth curve and boundaries.
#' Use [add_tariff_segments()] to apply the resulting boundaries to portfolio
#' data using the original continuous risk factor.
#'
#' @return A `list` of class `"tariff_segments"` with components:
#' \describe{
#'   \item{gam_prediction}{Data frame with the fitted GAM curve.}
#'   \item{risk_factor}{Name of the continuous risk factor.}
#'   \item{model_type}{Model type: `"frequency"`, `"severity"`, or `"pure_premium"`.}
#'   \item{classification_data}{Data frame used to derive the segments.}
#'   \item{risk_factor_values}{Observed risk factor values in portfolio row order.}
#'   \item{segment_boundaries}{Numeric vector with segment boundaries.}
#'   \item{assigned_segments}{Factor with the tariff segment assigned to each
#'   observed risk factor value.}
#'   \item{segment_summary}{Data frame with portfolio counts, distinct
#'   risk-factor values and the observed response components for each candidate
#'   segment. Use `summary()` as the public interface for this table.}
#'   \item{segmentation_penalty}{Penalty applied to additional tree splits.}
#' }
#' For backward compatibility, the old components `prediction`, `x`, `model`,
#' `data`, `x_obs`, `splits`, `class_boundaries`, `assigned_groups`, and
#' `tariff_classes` are also returned.
#'
#' @author Martin Haringa
#'
#' @seealso [risk_factor_gam()], [autoplot.tariff_segments()],
#'   [add_tariff_segments()], [prepare_refinement()], [add_smoothing()]
#'
#' @references Antonio, K. and Valdez, E. A. (2012). Statistical concepts of a
#' priori and a posteriori risk classification in insurance. *Advances in
#' Statistical Analysis*, 96(2), 187–224. \doi{doi:10.1007/s10182-011-0152-7}
#'
#' @references Grubinger, T., Zeileis, A., and Pfeiffer, K.-P. (2014). *evtree:
#' Evolutionary learning of globally optimal classification and regression trees
#' in R*. Journal of Statistical Software, 61(1), 1–29.
#' \doi{doi:10.18637/jss.v061.i01}
#'
#' @references Henckaerts, R., Antonio, K., Clijsters, M., & Verbelen, R.
#' (2018). A data driven binning strategy for the construction of insurance
#' tariff classes. *Scandinavian Actuarial Journal*, 2018(8), 681–705.
#' \doi{doi:10.1080/03461238.2018.1429300}
#'
#' @references Wood, S.N. (2011). Fast stable restricted maximum likelihood and
#' marginal likelihood estimation of semiparametric generalized linear models.
#' *JRSS B*, 73(1), 3–36. \doi{doi:10.1111/j.1467-9868.2010.00749.x}
#'
#' @examples
#' \dontrun{
#' age_segments <- risk_factor_gam(
#'   MTPL,
#'   risk_factor = "age_policyholder",
#'   claim_count = "nclaims",
#'   exposure = "exposure"
#' ) |>
#'   derive_tariff_segments(
#'     segmentation_penalty = 10,
#'     seed = 1
#'   )
#'
#' autoplot(age_segments, show_observations = TRUE)
#' summary(age_segments)
#'
#' MTPL |>
#'   add_tariff_segments(age_segments, name = "age_policyholder_segment")
#' }
#'
#' @importFrom evtree evtree evtree.control
#'
#' @export
derive_tariff_segments <- function(object, segmentation_penalty = 0,
                                   seed = 1, max_iterations = 10000,
                                   population_size = 200, complexity = NULL,
                                   alpha = NULL, niterations = NULL,
                                   ntrees = NULL) {
  segmentation_penalty_supplied <- !missing(segmentation_penalty)
  max_iterations_supplied <- !missing(max_iterations)
  population_size_supplied <- !missing(population_size)

  if (!inherits(object, "risk_factor_gam") &&
      !inherits(object, "riskfactor_gam") && !inherits(object, "fitgam")) {
    stop(
      "`object` must be a `risk_factor_gam` object returned by ",
      "`risk_factor_gam()`.",
      call. = FALSE
    )
  }

  if (!is.null(complexity)) {
    lifecycle::deprecate_warn(
      "0.8.2",
      "derive_tariff_segments(complexity = )",
      "derive_tariff_segments(segmentation_penalty = )"
    )
    if (segmentation_penalty_supplied) {
      stop(
        "Use only one of `segmentation_penalty` and deprecated `complexity`.",
        call. = FALSE
      )
    }
    segmentation_penalty <- complexity
    segmentation_penalty_supplied <- TRUE
  }

  if (!is.null(alpha)) {
    lifecycle::deprecate_warn(
      "0.8.0",
      "derive_tariff_segments(alpha = )",
      "derive_tariff_segments(segmentation_penalty = )"
    )
    if (segmentation_penalty_supplied) {
      stop(
        "Use only one of `segmentation_penalty` and deprecated `alpha`.",
        call. = FALSE
      )
    }
    segmentation_penalty <- alpha
  }
  if (!is.null(niterations)) {
    lifecycle::deprecate_warn(
      "0.8.0",
      "derive_tariff_segments(niterations = )",
      "derive_tariff_segments(max_iterations = )"
    )
    if (max_iterations_supplied) {
      stop(
        "Use only one of `max_iterations` and deprecated `niterations`.",
        call. = FALSE
      )
    }
    max_iterations <- niterations
  }
  if (!is.null(ntrees)) {
    lifecycle::deprecate_warn(
      "0.8.0",
      "derive_tariff_segments(ntrees = )",
      "derive_tariff_segments(population_size = )"
    )
    if (population_size_supplied) {
      stop(
        "Use only one of `population_size` and deprecated `ntrees`.",
        call. = FALSE
      )
    }
    population_size <- ntrees
  }

  validate_tariff_segment_control(
    segmentation_penalty = segmentation_penalty,
    max_iterations = max_iterations,
    population_size = population_size,
    seed = seed
  )

  data_used <- object$data
  x_obs <- object$x_obs

  required_components <- c("prediction", "x", "model", "data", "x_obs")
  missing_components <- required_components[
    !vapply(required_components, function(x) !is.null(object[[x]]), logical(1))
  ]
  if (length(missing_components) > 0L) {
    stop(
      "`object` is not a complete `risk_factor_gam` object. Missing component",
      if (length(missing_components) == 1L) ": " else "s: ",
      paste0("`", missing_components, "`", collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (!is.data.frame(data_used) ||
      !all(c("x", "pred") %in% names(data_used))) {
    stop(
      "`object$data` must contain the numeric columns `x` and `pred` created ",
      "by `risk_factor_gam()`.",
      call. = FALSE
    )
  }

  if (!is.numeric(x_obs)) {
    stop("`object$x_obs` must be numeric.", call. = FALSE)
  }

  invalid_x_obs <- sum(is.na(x_obs) | !is.finite(x_obs))
  if (invalid_x_obs > 0L) {
    stop(
      "The risk factor `", object$x, "` contains ", invalid_x_obs,
      " missing or non-finite ",
      if (invalid_x_obs == 1L) "value" else "values",
      " in `object$x_obs`. Remove or impute these values before deriving ",
      "tariff segments.",
      call. = FALSE
    )
  }

  if (!is.numeric(data_used$x) || !is.numeric(data_used$pred)) {
    stop("`object$data$x` and `object$data$pred` must be numeric.",
         call. = FALSE)
  }
  invalid_classification <- !is.finite(data_used$x) |
    !is.finite(data_used$pred) | is.na(data_used$x) | is.na(data_used$pred)
  if (any(invalid_classification)) {
    stop(
      "`object$data` contains ", sum(invalid_classification),
      " rows with missing or non-finite GAM values. Tariff segmentation ",
      "requires finite risk-factor values and fitted effects.",
      call. = FALSE
    )
  }

  x_range <- range(x_obs, na.rm = TRUE)
  if (!all(is.finite(x_range)) || x_range[1] == x_range[2]) {
    stop(
      "Cannot derive tariff segments because the risk factor has fewer than ",
      "two distinct finite values.",
      call. = FALSE
    )
  }

  split_x <- tryCatch(
    {
    tree_x <- evtree::evtree(
      pred ~ x,
      data = data_used,
      control = evtree::evtree.control(
        alpha = segmentation_penalty,
        ntrees = population_size,
        niterations = max_iterations,
        seed = seed
      )
    )
    split_obtained <- get_splits(tree_x)
    split_obtained[split_obtained > x_range[1] & split_obtained < x_range[2]]
    },
    error = function(e) {
      stop(
        "Could not derive tariff segments with evtree: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (length(split_x) == 0) {
    warning(
      "No internal tariff segment split was found; returning one interval.",
      call. = FALSE
    )
  }

  # Add min and max to binning
  splits <- sort(unique(c(x_range[1], split_x, x_range[2])))
  cuts <- cut(x_obs, breaks = splits, include.lowest = TRUE)
  classification_segments <- cut(
    data_used$x,
    breaks = splits,
    include.lowest = TRUE
  )
  segment_summary <- .summarise_tariff_segments(
    data_used = data_used,
    x_obs = x_obs,
    assigned_segments = cuts,
    classification_segments = classification_segments,
    model_type = object$model
  )

  structure(
    list(
      gam_prediction = object$prediction,
      risk_factor = object$x,
      model_type = object$model,
      classification_data = data_used,
      risk_factor_values = x_obs,
      segment_boundaries = splits,
      assigned_segments = cuts,
      segment_summary = segment_summary,
      segmentation_penalty = segmentation_penalty,
      round_risk_factor = object$round_risk_factor %||% NULL,
      class_boundaries = splits,
      assigned_groups = cuts,
      prediction = object$prediction,
      x = object$x,
      model = object$model,
      data = data_used,
      x_obs = x_obs,
      splits = splits,
      tariff_classes = cuts
    ),
    class = c("tariff_segments", "tariff_classes", "constructtariffclasses")
  )
}


#' Deprecated alias for `derive_tariff_segments()`
#'
#' @description
#' `construct_tariff_classes()` is deprecated as of version 0.9.0. Use
#' [derive_tariff_segments()] instead.
#'
#' @inheritParams derive_tariff_segments
#' @return See [derive_tariff_segments()].
#'
#' @export
#' @keywords internal
construct_tariff_classes <- function(object, complexity = 0,
                                     max_iterations = 10000,
                                     population_size = 200, seed = 1,
                                     alpha = NULL, niterations = NULL,
                                     ntrees = NULL) {
  lifecycle::deprecate_warn(
    "0.9.0",
    "construct_tariff_classes()",
    "derive_tariff_segments()"
  )
  if (!is.null(alpha)) {
    complexity <- alpha
  }
  if (!is.null(niterations)) {
    max_iterations <- niterations
  }
  if (!is.null(ntrees)) {
    population_size <- ntrees
  }
  derive_tariff_segments(
    object = object,
    segmentation_penalty = complexity,
    max_iterations = max_iterations,
    population_size = population_size,
    seed = seed
  )
}

validate_tariff_segment_control <- function(segmentation_penalty,
                                            max_iterations,
                                            population_size, seed) {
  if (!is.numeric(segmentation_penalty) ||
      length(segmentation_penalty) != 1L ||
      !is.finite(segmentation_penalty) || segmentation_penalty < 0) {
    stop(
      "`segmentation_penalty` must be a single non-negative number.",
      call. = FALSE
    )
  }
  if (!is.numeric(max_iterations) || length(max_iterations) != 1L ||
      !is.finite(max_iterations) || max_iterations <= 0 ||
      max_iterations != floor(max_iterations)) {
    stop("`max_iterations` must be a single positive whole number.",
         call. = FALSE)
  }
  if (!is.numeric(population_size) || length(population_size) != 1L ||
      !is.finite(population_size) || population_size <= 0 ||
      population_size != floor(population_size)) {
    stop("`population_size` must be a single positive whole number.",
         call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed != floor(seed)) {
    stop("`seed` must be a single finite whole number.", call. = FALSE)
  }
  invisible(NULL)
}

.summarise_tariff_segments <- function(data_used, x_obs, assigned_segments,
                                       classification_segments, model_type) {
  segment_levels <- levels(assigned_segments)
  sum_by_segment <- function(values) {
    vapply(
      segment_levels,
      function(level) sum(values[classification_segments == level]),
      numeric(1)
    )
  }

  out <- data.frame(
    segment = factor(segment_levels, levels = segment_levels),
    portfolio_records = as.integer(table(
      factor(assigned_segments, levels = segment_levels)
    )),
    risk_factor_values = as.integer(table(
      factor(classification_segments, levels = segment_levels)
    )),
    stringsAsFactors = FALSE
  )

  if (identical(model_type, "frequency")) {
    out$exposure <- sum_by_segment(data_used$exposure)
    out$claim_count <- sum_by_segment(data_used$nclaims)
    out$frequency <- out$claim_count / out$exposure
  } else if (identical(model_type, "severity")) {
    out$claim_count <- sum_by_segment(data_used$nclaims)
    out$claim_amount <- sum_by_segment(data_used$amount)
    out$average_severity <- out$claim_amount / out$claim_count
  } else if (model_type %in% c("pure_premium", "burning")) {
    out$exposure <- sum_by_segment(data_used$exposure)
    out$risk_premium_amount <- sum_by_segment(data_used$weighted_premium)
    out$risk_premium <- out$risk_premium_amount / out$exposure
  }

  row.names(out) <- NULL
  out
}

#' @export
print.tariff_segments <- function(x, ...) {
  boundaries <- x$segment_boundaries %||% x$class_boundaries
  cat("Tariff segmentation\n")
  cat("Risk factor:", x$risk_factor %||% x$x, "\n")
  cat("Candidate segments:", length(boundaries) - 1L, "\n")
  cat("Segmentation penalty:", x$segmentation_penalty %||% NA_real_, "\n")
  cat("Boundaries:\n")
  print(boundaries)
  invisible(x)
}

#' @export
print.tariff_classes <- print.tariff_segments

#' @export
print.constructtariffclasses <- print.tariff_segments

#' Summarise candidate tariff segments
#'
#' @description
#' Return the portfolio diagnostics stored when [derive_tariff_segments()]
#' created the candidate segmentation. The summary can be used to assess
#' whether the proposed intervals contain sufficient exposure and claim
#' information before they are used in a GLM or tariff structure.
#'
#' @param object A `"tariff_segments"` object returned by
#'   [derive_tariff_segments()].
#' @param ... Additional arguments reserved for method compatibility.
#'
#' @return A data frame with one row per candidate segment and the columns:
#' \describe{
#'   \item{segment}{Candidate tariff interval.}
#'   \item{portfolio_records}{Number of portfolio rows assigned to the
#'   interval.}
#'   \item{risk_factor_values}{Number of distinct observed risk-factor values
#'   represented by the interval.}
#'   \item{exposure}{Total exposure represented in a frequency or risk-premium
#'   GAM.}
#'   \item{claim_count}{Total observed claim count for a frequency or severity
#'   GAM.}
#'   \item{frequency}{Observed claim frequency, calculated as `claim_count /
#'   exposure`, for a frequency GAM.}
#'   \item{claim_amount}{Total observed claim amount for a severity GAM.}
#'   \item{average_severity}{Observed average severity, calculated as
#'   `claim_amount / claim_count`, for a severity GAM.}
#'   \item{risk_premium_amount}{Total exposure-weighted risk-premium amount for
#'   a risk-premium GAM.}
#'   \item{risk_premium}{Observed risk premium, calculated as
#'   `risk_premium_amount / exposure`, for a risk-premium GAM.}
#' }
#' The response columns are model dependent. The returned table therefore
#' contains the numerator, denominator and observed y-axis measure relevant to
#' the model used by [risk_factor_gam()].
#'
#' @author Martin Haringa
#'
#' @seealso [derive_tariff_segments()], [add_tariff_segments()]
#'
#' @keywords internal
#' @export
summary.tariff_segments <- function(object, ...) {
  out <- object$segment_summary
  if (!is.data.frame(out)) {
    stop(
      "`object` does not contain segment diagnostics. Recreate it with ",
      "`derive_tariff_segments()` before calling `summary()`.",
      call. = FALSE
    )
  }
  out
}

#' @export
summary.tariff_classes <- summary.tariff_segments

#' @export
summary.constructtariffclasses <- summary.tariff_segments

#' @export
as.vector.tariff_segments <- function(x, ...) {
  as.vector(x$segment_boundaries %||% x$class_boundaries)
}

#' @export
as.vector.tariff_classes <- as.vector.tariff_segments

#' @export
as.vector.constructtariffclasses <- as.vector.tariff_segments


#' Add derived tariff segments to portfolio data
#'
#' @description
#' Adds the tariff segments derived by [derive_tariff_segments()] as a new factor
#' column to a portfolio data set. The stored boundaries are applied to the
#' original continuous risk-factor column, so the result does not depend on the
#' row order used when the GAM was fitted.
#'
#' The helper does not re-estimate the GAM or derive new boundaries. It can be
#' used after filtering or reordering the original portfolio and on new data
#' whose risk-factor values remain within the range used to derive the
#' segmentation.
#'
#' @param data A data frame to which the tariff segments should be added.
#' @param segments Object of class `"tariff_segments"`, produced by
#'   [derive_tariff_segments()]. Old `"tariff_classes"` objects are accepted for
#'   backward compatibility.
#' @param name Character string. Name of the new output column. If `NULL`, the
#'   name is based on the risk factor name, for example
#'   `"age_policyholder_segment"`.
#' @param overwrite Logical. If `FALSE`, the function stops when `name` already
#'   exists in `data`.
#'
#' @return A data frame with the derived tariff segment column added.
#'
#' @details
#' The risk-factor name and optional rounding increment are taken from
#' `segments`. The risk-factor column in `data` must be numeric and contain only
#' finite, non-missing values. Values outside the original segmentation range
#' produce an error because their tariff treatment has not been supported by
#' the fitted GAM. The resulting factor can be used in a GLM or retained as a
#' candidate grouping for further actuarial review.
#'
#' @author Martin Haringa
#'
#' @examples
#' \dontrun{
#' age_segments <- risk_factor_gam(
#'   MTPL,
#'   risk_factor = "age_policyholder",
#'   claim_count = "nclaims",
#'   exposure = "exposure"
#' ) |>
#'   derive_tariff_segments()
#'
#' MTPL |>
#'   add_tariff_segments(age_segments, name = "age_policyholder_segment")
#' }
#'
#' @seealso [risk_factor_gam()], [derive_tariff_segments()]
#'
#' @export
add_tariff_segments <- function(data, segments, name = NULL, overwrite = FALSE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!inherits(segments, "tariff_segments") &&
      !inherits(segments, "tariff_classes")) {
    stop("`segments` must be an object returned by `derive_tariff_segments()`.",
         call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }

  risk_factor <- segments$risk_factor %||% segments$x
  boundaries <- segments$segment_boundaries %||%
    segments$class_boundaries %||% segments$splits
  if (!.is_single_string(risk_factor)) {
    stop(
      "`segments` does not contain a valid risk-factor name.",
      call. = FALSE
    )
  }
  if (!is.numeric(boundaries) || length(boundaries) < 2L ||
      anyNA(boundaries) || any(!is.finite(boundaries)) ||
      is.unsorted(boundaries, strictly = TRUE)) {
    stop("`segments` does not contain valid, strictly increasing boundaries.",
         call. = FALSE)
  }
  if (!risk_factor %in% names(data)) {
    stop(
      "Column `", risk_factor, "`, used to derive the tariff segments, was ",
      "not found in `data`.",
      call. = FALSE
    )
  }
  if (!is.numeric(data[[risk_factor]])) {
    stop("The risk-factor column `", risk_factor, "` must be numeric.",
         call. = FALSE)
  }

  risk_factor_values <- round_x_values(
    data[[risk_factor]],
    segments$round_risk_factor %||% NULL
  )
  invalid <- is.na(risk_factor_values) | !is.finite(risk_factor_values)
  if (any(invalid)) {
    stop(
      "The risk-factor column `", risk_factor, "` contains ", sum(invalid),
      " missing or non-finite ", if (sum(invalid) == 1L) "value" else "values",
      ". Remove or impute these values before adding tariff segments.",
      call. = FALSE
    )
  }

  below <- sum(risk_factor_values < boundaries[1])
  above <- sum(risk_factor_values > boundaries[length(boundaries)])
  if (below > 0L || above > 0L) {
    outside <- c(
      if (below > 0L) paste0(below, " below the first boundary"),
      if (above > 0L) paste0(above, " above the last boundary")
    )
    stop(
      "The risk-factor column `", risk_factor,
      "` contains values outside the tariff-segment range ", boundaries[1],
      " to ", boundaries[length(boundaries)], ": ",
      paste(outside, collapse = " and "),
      ". Review the boundaries before applying them to this portfolio.",
      call. = FALSE
    )
  }

  assigned_segments <- cut(
    risk_factor_values,
    breaks = boundaries,
    include.lowest = TRUE
  )

  if (is.null(name)) {
    name <- paste0(risk_factor, "_segment")
  }
  if (!is.character(name) || length(name) != 1L || is.na(name) || name == "") {
    stop("`name` must be a single non-empty character string.", call. = FALSE)
  }
  if (name %in% names(data) && !isTRUE(overwrite)) {
    stop("Column `", name, "` already exists. Use `overwrite = TRUE` to replace it.",
         call. = FALSE)
  }

  out <- data
  out[[name]] <- assigned_segments
  out
}


#' @rdname autoplot.tariff_effect
#' @export
autoplot.tariff_segments <- function(object,
                                   confidence = FALSE,
                                   color_gam = "steelblue",
                                   show_observations = FALSE,
                                   color_splits = "grey50",
                                   size_points = 1,
                                   color_points = "black",
                                   rotate_labels = FALSE,
                                   remove_outliers = NULL,
                                   conf_int = NULL,
                                   x_stepsize = NULL,
                                   show_segments = TRUE,
                                   ...) {
  if (!is.null(conf_int)) {
    lifecycle::deprecate_warn("0.9.0", "autoplot(conf_int)",
                              "autoplot(confidence)")
    confidence <- conf_int
  }

  if (!inherits(object, "tariff_segments") &&
      !inherits(object, "tariff_classes") &&
      !inherits(object, "constructtariffclasses")) {
    stop("Input must be of class 'tariff_segments'.", call. = FALSE)
  }
  if (!is.logical(show_segments) || length(show_segments) != 1L ||
      is.na(show_segments)) {
    stop("`show_segments` must be TRUE or FALSE.", call. = FALSE)
  }

  .plot_risk_factor_curve(
    prediction = object$gam_prediction %||% object$prediction,
    points = object$classification_data %||% object$data,
    risk_factor = object$risk_factor %||% object$x,
    model_type = object$model_type %||% object$model,
    confidence = confidence,
    color_gam = color_gam,
    show_observations = show_observations,
    x_stepsize = x_stepsize,
    size_points = size_points,
    color_points = color_points,
    rotate_labels = rotate_labels,
    remove_outliers = remove_outliers,
    segment_boundaries = object$segment_boundaries %||%
      object$class_boundaries %||% object$splits,
    show_segments = show_segments,
    color_segments = color_splits
  )
}

#' @export
autoplot.tariff_classes <- autoplot.tariff_segments

#' @export
autoplot.constructtariffclasses <- autoplot.tariff_segments
