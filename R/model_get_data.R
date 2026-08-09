#' Recover the portfolio data used by a fitted model
#'
#' @description
#' Recover the estimation data and pricing metadata stored with a fitted GLM or
#' a model produced by the refinement workflow. The result provides a
#' reproducible basis for rating grids, coefficient tables and portfolio-level
#' model diagnostics.
#'
#' `model_data()` is kept as a deprecated compatibility wrapper.
#'
#' @param x An object of class `"glm"`, `"refitsmooth"`, or `"refitrestricted"`.
#'
#' @details
#' ## Data represented by the result
#'
#' For an ordinary GLM, the function recovers the data stored with the model or
#' its model frame and records the response, model terms, risk factors, weights
#' and offsets. The recovered data represent the observations available to the
#' fitted model. Rows omitted during fitting, for example because of missing
#' model variables, may therefore not be present.
#'
#' For a refined model, technical columns used to construct smoothing and
#' restriction terms are removed from the returned data. The mappings required
#' to interpret the refined coefficients are retained as attributes.
#'
#' ## Actuarial use
#'
#' The extracted object is intended for downstream calculations that must remain
#' consistent with the fitted pricing model, such as [rating_grid()] and
#' [rating_table()]. It should not be interpreted as a replacement for the
#' original raw portfolio extract: preprocessing, filtering and missing-value
#' handling applied before or during model fitting remain part of the data
#' provenance.
#'
#' @return A `data.frame` of class `"model_data"` with additional attributes:
#' \itemize{
#'   \item `response`: response variable in the model;
#'   \item `rf`: names of risk factors in the model;
#'   \item `offweights`: weight and offset variables if present;
#'   \item `terms`: model terms object for plain GLMs;
#'   \item `mgd_rst`, `mgd_smt`: merged restrictions and smooths for refit
#'   objects;
#'   \item `new_nm`, `old_nm`: new and old column names for refit objects.
#' }
#'
#' @author Martin Haringa
#'
#' @examples
#' \dontrun{
#' library(insurancerating)
#'
#' pmodel <- glm(
#'   breaks ~ wool + tension,
#'   data = warpbreaks,
#'   family = poisson(link = "log")
#' )
#'
#' extract_model_data(pmodel)
#' }
#'
#' @seealso [rating_grid()], [rating_table()], [prepare_refinement()]
#'
#' @export
extract_model_data <- function(x) {

  if (!inherits(x, c("refitsmooth", "refitrestricted", "glm"))) {
    stop(
      "Input must be of class refitsmooth, glm or of class refitrestricted",
      call. = FALSE
    )
  }

  as_df <- function(z) {
    if (inherits(z, "data.table")) z <- as.data.frame(z)
    as.data.frame(z, stringsAsFactors = FALSE)
  }

  if (inherits(x, c("refitsmooth", "refitrestricted"))) {
    xdf <- as_df(x$data)
    xdf_nm <- names(xdf)

    rem_nm <- c(
      "breaks_min", "breaks_max", "start_oc", "end_oc",
      "start_", "end_", "avg_", "risk_factor"
    )

    keep_nm <- xdf_nm[!xdf_nm %in% rem_nm]
    out <- xdf[, keep_nm, drop = FALSE]
    out <- as_df(out)

    attr(out, "new_nm") <- attr(x, "new_col_nm")
    attr(out, "old_nm") <- attr(x, "old_col_nm")

    rf <- attr(x, "rf")
    mgd_smt <- attr(x, "mgd_smt")

    for (i in seq_along(mgd_smt)) {
      zsm <- gsub("_smooth$", "", mgd_smt[[i]][2])
      rf[rf == zsm] <- mgd_smt[[i]][1]
    }

    attr(out, "rf") <- rf
    attr(out, "mgd_rst") <- attr(x, "mgd_rst")
    attr(out, "mgd_smt") <- mgd_smt
    attr(out, "offweights") <- attr(x, "offweights")
  } else {
    out <- if (!is.null(x$data)) {
      as_df(x$data)
    } else {
      as_df(stats::model.frame(x))
    }

    terms_obj <- stats::terms(x)
    term_labels <- attr(terms_obj, "term.labels")
    rf <- if (length(term_labels) > 0) {
      unique(all.vars(stats::as.formula(paste("~", paste(term_labels, collapse = "+")))))
    } else {
      character(0)
    }

    lst_call <- as.list(x$call)
    weight_vars <- if (!is.null(lst_call$weights)) {
      all.vars(lst_call$weights)
    } else {
      character(0)
    }
    offset_vars <- if (!is.null(lst_call$offset)) {
      all.vars(lst_call$offset)
    } else {
      character(0)
    }

    offset_idx <- attr(terms_obj, "offset")
    if (!is.null(offset_idx)) {
      term_vars <- attr(terms_obj, "variables")
      offset_vars <- unique(c(
        offset_vars,
        unlist(
          lapply(offset_idx, function(i) all.vars(term_vars[[i + 1L]])),
          use.names = FALSE
        )
      ))
    }

    response_var <- all.vars(stats::formula(x))[1]

    attr(out, "response") <- response_var
    attr(out, "offweights") <- unique(c(weight_vars, offset_vars))
    attr(out, "rf") <- setdiff(rf, unique(c(response_var, weight_vars, offset_vars)))
    attr(out, "terms") <- terms_obj
    attr(out, "term.labels") <- term_labels
  }

  out <- as_df(out)
  class(out) <- c("model_data", class(out))
  out
}


#' Deprecated alias for `extract_model_data()`
#'
#' @description
#' `model_data()` is deprecated in favour of [extract_model_data()].
#'
#' @inheritParams extract_model_data
#' @return See [extract_model_data()].
#'
#' @export
#' @keywords internal
model_data <- function(x) {
  lifecycle::deprecate_warn(
    "0.9.0",
    "model_data()",
    "extract_model_data()"
  )

  extract_model_data(x)
}


.rating_grid_sum <- function(df, by_vars, sum_vars) {
  df <- data.table::as.data.table(df)

  if (length(by_vars) == 0) {
    return(df[, lapply(.SD, sum, na.rm = TRUE), .SDcols = sum_vars])
  }

  out <- df[, lapply(.SD, sum, na.rm = TRUE),
            by = by_vars, .SDcols = sum_vars]
  data.table::setorderv(out, by_vars)
  out
}


.rating_grid_count <- function(df, by_vars) {
  df <- data.table::as.data.table(df)

  if (length(by_vars) == 0) {
    return(data.frame(count = nrow(df)))
  }

  out <- df[, list(count = .N), by = by_vars]
  data.table::setorderv(out, by_vars)
  out
}


.rating_grid_merge <- function(x, y, by_vars) {
  x <- data.table::as.data.table(x)
  y <- data.table::as.data.table(y)

  if (length(by_vars) == 0) {
    return(cbind(x, y))
  }

  out <- merge(x, y, by = by_vars, all = TRUE, sort = FALSE)
  data.table::setorderv(out, by_vars)
  out
}


.rating_grid_wide <- function(df, group_vars, split_var, value_var, prefix) {
  df <- data.table::as.data.table(df)
  temporary_group <- ".rating_grid_group"

  if (length(group_vars) == 0L) {
    while (temporary_group %in% names(df)) {
      temporary_group <- paste0(temporary_group, "_")
    }
    df[, (temporary_group) := 1L]
    cast_groups <- temporary_group
  } else {
    cast_groups <- group_vars
  }

  lhs <- Reduce(
    function(left, right) call("+", left, right),
    lapply(cast_groups, as.name)
  )
  wide_formula <- stats::as.formula(
    call("~", lhs, as.name(split_var)),
    env = parent.frame()
  )

  out <- data.table::dcast(
    df,
    formula = wide_formula,
    value.var = value_var,
    drop = TRUE
  )
  if (length(group_vars) == 0L) {
    out[, (temporary_group) := NULL]
  }

  value_cols <- setdiff(names(out), cast_groups)
  data.table::setnames(
    out,
    old = value_cols,
    new = paste0(prefix, "_", value_cols)
  )
  if (length(group_vars) > 0L) {
    data.table::setorderv(out, group_vars)
  }

  out
}


.rating_grid_add_refinement <- function(out, xdf, refinement_pairs) {
  out <- data.table::as.data.table(out)
  xdf <- data.table::as.data.table(xdf)
  refinement_pairs <- refinement_pairs[vapply(refinement_pairs, length, integer(1)) >= 2]

  for (pair in refinement_pairs) {
    old_col <- pair[[1]]
    new_col <- pair[[2]]

    if (!old_col %in% names(out) || !new_col %in% names(xdf) || new_col %in% names(out)) {
      next
    }

    mapping <- unique(xdf[, c(old_col, new_col), with = FALSE])
    if (any(duplicated(mapping[[old_col]]))) {
      warning(
        "Refinement column `", new_col, "` has multiple values per `", old_col,
        "` and was not added to the rating grid.",
        call. = FALSE
      )
      next
    }

    out[, (new_col) := mapping[[new_col]][match(
      out[[old_col]],
      mapping[[old_col]]
    )]]
  }

  out
}


#' Construct observed rating-grid points
#'
#' @description
#' Collapse portfolio records with identical risk-factor combinations into
#' observed rating-grid points. Exposure and other numeric measures can be
#' aggregated alongside the combinations for prediction, tariff comparison and
#' portfolio diagnostics.
#'
#' Together with [merge_date_ranges()], this function belongs to the portfolio
#' reduction workflow. Both functions reduce row-level portfolio data while
#' retaining selected totals. `rating_grid()` reduces across identical
#' risk-factor combinations; [merge_date_ranges()] reduces temporally connected
#' records within the same policy, risk or portfolio segment.
#'
#' The function returns only combinations that are actually observed in the input
#' data. It does **not** create the full Cartesian product of all unique values.
#' This keeps the output compact and suitable for model diagnostics, portfolio
#' summaries, and prediction analysis.
#'
#' When `x` is an object returned by [extract_model_data()], the function uses
#' the extracted model metadata to determine the grouping variables if
#' `group_by` is not supplied. When `x` is a plain `data.frame`, it is
#' recommended to supply `group_by` explicitly.
#'
#' @param x A `data.frame`, an object of class `"model_data"` returned by
#'   [extract_model_data()], or a fitted model that can be passed to
#'   [extract_model_data()].
#' @param group_by Optional character vector with the variables that define the
#'   rating-grid points. If `NULL` and `x` is a `"model_data"` object, the
#'   risk-factor variables stored in the object are used. If `NULL` and `x` is a
#'   plain `data.frame`, all columns except those listed in `exposure`,
#'   `exposure_by`, and `aggregate_cols` are used.
#' @param exposure Optional character; name of the exposure column to aggregate.
#' @param exposure_by Optional character; name of a column used to split
#'   exposure or counts, for example a year variable.
#' @param aggregate_cols Optional character vector with additional numeric
#'   columns to aggregate using `sum(na.rm = TRUE)`.
#' @param drop_na Logical; if `TRUE`, rows with missing values in `group_by`
#'   are removed before aggregation. If `FALSE`, missing values define an
#'   explicit observed group and are retained. Default is `FALSE`.
#' @param group_vars,agg_cols Deprecated argument names. Use `group_by` and
#'   `aggregate_cols` instead.
#'
#' @details
#' ## Portfolio reduction
#'
#' `rating_grid()` performs categorical portfolio reduction. It combines rows
#' with the same observed `group_by` values and retains the corresponding totals.
#' Use [merge_date_ranges()] for the complementary temporal reduction of
#' connected coverage periods.
#'
#' ## Observed combinations
#'
#' The grid represents the combinations present in the supplied portfolio or
#' model data. It deliberately does not construct combinations that were not
#' observed. This avoids creating artificial model points and is particularly
#' relevant when risk factors are structurally related, such as product,
#' coverage and distribution channel.
#'
#' Each output row therefore represents one observed combination of the
#' variables in `group_by`. Exposure and `aggregate_cols` are summed over the
#' source records belonging to that combination. This is a categorical
#' reduction of the portfolio; no date intervals are combined.
#'
#' ## Estimating a GLM on aggregated data
#'
#' For a standard Poisson frequency GLM, aggregation before model fitting can
#' preserve the coefficient estimates exactly. This applies when records are
#' grouped by every predictor used in the model, claim counts are summed, earned
#' exposure is summed, and the aggregated model uses `offset(log(exposure))`.
#' Within such a group all records have the same linear predictor. Their
#' contribution to the coefficient estimation therefore depends on total claims
#' and total exposure, which are retained by the rating grid.
#'
#' This equivalence is conditional, not a general property of every GLM.
#' Aggregating over a model variable changes the model, and row-level weights,
#' interactions, offsets or non-additive quantities must be retained correctly.
#' Binomial, severity, quasi-likelihood and dispersion analyses require their
#' own sufficient totals and weights. Row-level residuals and influence
#' diagnostics are also no longer available after aggregation, even when the
#' fitted Poisson coefficients are unchanged.
#'
#' In practice, a rating grid is particularly useful before estimating a
#' frequency model on a large portfolio. It can reduce repeated policy records
#' to a much smaller table, lowering memory use and fitting time. Keep the
#' unaggregated data when policy-level predictions, sampling, validation or
#' diagnostics are required, and verify on a representative sample that the
#' selected aggregation retains all inputs needed by the intended model.
#'
#' ## Estimating a severity GLM on aggregated data
#'
#' Claim count and claim amount are additive portfolio measures and can be
#' supplied through `aggregate_cols`. Frequency is then calculated as total
#' claim count divided by total exposure. Average severity is total claim amount
#' divided by total claim count.
#'
#' A Gamma severity GLM fitted to grouped average severities can produce the same
#' coefficient estimates as a model fitted to the underlying individual claims.
#' This requires grouping by every predictor in the severity model, calculating
#' `average_severity = claim_amount / claim_count`, using `claim_count` as the
#' model weight, and applying the same family and link. Within each grid row all
#' underlying claims then have the same linear predictor, while the weight
#' retains the number of claims represented by the average.
#'
#' This equivalence concerns the coefficient estimates, not the complete model
#' output. Aggregation removes claim-level residuals and outlier information.
#' Deviance, residual degrees of freedom, estimated dispersion, standard errors
#' and significance tests can therefore differ from a claim-level fit. Claim-
#' level data should remain available for severity-distribution checks,
#' influential-claim analysis and model validation. The equivalence is also
#' lost if a severity predictor varies within a grid row or if the totals and
#' weights do not represent the underlying claims correctly.
#'
#' If `exposure_by` is supplied, exposure or row counts are split across levels
#' of that variable and returned in wide format, for example
#' `"exposure_2020"` or `"count_2020"`.
#'
#' For objects returned by [extract_model_data()], refinement mappings are joined
#' by their original factor column. They are not cross-joined onto every row.
#'
#' Aggregation, reshaping and refinement joins are performed internally with
#' [data.table::data.table()] to support large pricing portfolios. A local copy
#' is used, so the supplied object is not modified by reference. The output is
#' a regular `data.frame`, irrespective of the class of the input data.
#'
#' When the row-level portfolio does not fit comfortably in R memory, use
#' [rating_grid_db()] to perform the grouped reduction in a database and collect
#' only the resulting grid.
#'
#' @return
#' A `data.frame` with one row per observed rating-grid point.
#'
#' @author Martin Haringa
#'
#' @examples
#' portfolio <- data.frame(
#'   policy_id = 1:10,
#'   sector = rep(c("Industry", "Retail"), each = 5),
#'   region = rep(c("North", "South"), 5),
#'   underwriting_year = rep(c(2024, 2025), each = 5),
#'   earned_exposure = c(1, 0.8, 1, 0.5, 1, 1, 0.7, 1, 0.9, 1),
#'   claim_count = c(0, 1, 2, 0, 1, 0, 1, 0, 2, 1),
#'   claim_amount = c(0, 2500, 18000, 0, 6000, 0, 4500, 0, 22000, 9000)
#' )
#'
#' # Aggregate policy records into observed combinations of sector and region.
#' # The resulting exposure is the total earned exposure in each combination.
#' rating_grid(
#'   portfolio,
#'   group_by = c("sector", "region"),
#'   exposure = "earned_exposure"
#' )
#'
#' # Split earned exposure by underwriting year. This is useful when reviewing
#' # whether the portfolio mix within each rating combination changes over time.
#' rating_grid(
#'   portfolio,
#'   group_by = c("sector", "region"),
#'   exposure = "earned_exposure",
#'   exposure_by = "underwriting_year"
#' )
#'
#' # Claim count and claim amount remain additive totals in the rating grid.
#' # Frequency and average severity can subsequently be derived from them.
#' claims_grid <- rating_grid(
#'   portfolio,
#'   group_by = c("sector", "region"),
#'   exposure = "earned_exposure",
#'   aggregate_cols = c("claim_count", "claim_amount")
#' )
#'
#' claims_grid$frequency <-
#'   claims_grid$claim_count / claims_grid$earned_exposure
#' claims_grid$average_severity <- ifelse(
#'   claims_grid$claim_count > 0,
#'   claims_grid$claim_amount / claims_grid$claim_count,
#'   NA_real_
#' )
#' claims_grid
#'
#' # Fit a severity model to grouped average claim amounts. Grid rows without
#' # claims are excluded because average severity is undefined for those rows.
#' severity_model_grid <- glm(
#'   average_severity ~ sector + region,
#'   weights = claim_count,
#'   family = Gamma(link = "log"),
#'   data = subset(claims_grid, claim_count > 0)
#' )
#' coef(severity_model_grid)
#'
#' # For a fitted GLM, extract_model_data() retains the model variables and
#' # exposure information required to construct the observed rating grid.
#' mtpl_portfolio <- MTPL
#' mtpl_portfolio$zip <- factor(mtpl_portfolio$zip)
#'
#' frequency_model <- glm(
#'   nclaims ~ bm + zip + offset(log(exposure)),
#'   family = poisson(link = "log"),
#'   data = mtpl_portfolio
#' )
#'
#' frequency_model |>
#'   extract_model_data() |>
#'   rating_grid()
#'
#' # For this Poisson frequency model, fitting on the corresponding aggregated
#' # grid gives the same coefficient estimates as fitting on the policy rows.
#' frequency_grid <- rating_grid(
#'   mtpl_portfolio,
#'   group_by = c("bm", "zip"),
#'   exposure = "exposure",
#'   aggregate_cols = "nclaims"
#' )
#'
#' frequency_model_grid <- glm(
#'   nclaims ~ bm + zip + offset(log(exposure)),
#'   family = poisson(link = "log"),
#'   data = frequency_grid
#' )
#'
#' isTRUE(all.equal(
#'   unname(coef(frequency_model)),
#'   unname(coef(frequency_model_grid)),
#'   tolerance = 1e-8
#' ))
#'
#' @seealso [rating_grid_db()], [merge_date_ranges()],
#'   [merge_date_ranges_db()], [extract_model_data()], [rating_table()]
#'
#' @export
rating_grid <- function(x,
                        group_by = NULL,
                        exposure = NULL,
                        exposure_by = NULL,
                        aggregate_cols = NULL,
                        drop_na = FALSE,
                        group_vars = NULL,
                        agg_cols = NULL) {

  if (!is.null(group_vars)) {
    if (!is.null(group_by)) {
      stop("Use only one of `group_by` and deprecated `group_vars`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "rating_grid(group_vars)",
                              "rating_grid(group_by)")
    group_by <- group_vars
  }
  if (!is.null(agg_cols)) {
    if (!is.null(aggregate_cols)) {
      stop("Use only one of `aggregate_cols` and deprecated `agg_cols`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "rating_grid(agg_cols)",
                              "rating_grid(aggregate_cols)")
    aggregate_cols <- agg_cols
  }

  if (inherits(x, c("glm", "refitsmooth", "refitrestricted"))) {
    x <- extract_model_data(x)
  }

  if (inherits(x, "rating_refinement")) {
    stop(
      "Input is a 'rating_refinement' object. Call refit() first, then use rating_grid().",
      call. = FALSE
    )
  }

  if (!inherits(x, "model_data") && !inherits(x, "data.frame")) {
    stop(
      "Input must be a data.frame, an object returned by extract_model_data(), or a fitted model.",
      call. = FALSE
    )
  }

  if (!is.null(group_by) && !is.character(group_by)) {
    stop("`group_by` must be NULL or a character vector.", call. = FALSE)
  }
  if (!is.null(exposure) && (!is.character(exposure) || length(exposure) != 1)) {
    stop("`exposure` must be NULL or a single character string.", call. = FALSE)
  }
  if (!is.null(exposure_by) &&
      (!is.character(exposure_by) || length(exposure_by) != 1)) {
    stop("`exposure_by` must be NULL or a single character string.", call. = FALSE)
  }
  if (!is.null(aggregate_cols) && !is.character(aggregate_cols)) {
    stop("`aggregate_cols` must be NULL or a character vector.", call. = FALSE)
  }
  if (!is.logical(drop_na) || length(drop_na) != 1 || is.na(drop_na)) {
    stop("`drop_na` must be TRUE or FALSE.", call. = FALSE)
  }

  xdf <- data.table::as.data.table(data.table::copy(
    as.data.frame(x, stringsAsFactors = FALSE)
  ))
  offweights <- NULL
  agg_cols_all <- aggregate_cols

  if (inherits(x, "model_data")) {
    offweights <- unique(attr(x, "offweights"))
    default_group_vars <- attr(x, "rf")

    if (is.null(default_group_vars) || length(default_group_vars) == 0) {
      term_labels <- attr(x, "term.labels")
      if (!is.null(term_labels)) {
        default_group_vars <- intersect(term_labels, names(xdf))
      }
    }

    if (is.null(default_group_vars) || length(default_group_vars) == 0) {
      terms_obj <- attr(x, "terms")
      if (!is.null(terms_obj)) {
        term_labels <- attr(terms_obj, "term.labels")
        default_group_vars <- intersect(term_labels, names(xdf))
      }
    }

    if (is.null(default_group_vars) || length(default_group_vars) == 0) {
      response_var <- attr(x, "response")
      cols_excluded <- unique(c(
        response_var,
        offweights,
        exposure,
        exposure_by,
        aggregate_cols
      ))
      default_group_vars <- setdiff(names(xdf), cols_excluded)
    }

    if (length(default_group_vars) == 0) {
      stop(
        "Could not determine grouping variables from `model_data`. Supply `group_by` explicitly.",
        call. = FALSE
      )
    }

    if (is.null(group_by)) {
      group_by <- default_group_vars
    }

    if (!is.null(exposure) && exposure %in% group_by) {
      stop("Column in `exposure` is already used as grouping variable.", call. = FALSE)
    }
    if (!is.null(exposure_by) && exposure_by %in% group_by) {
      stop("Column in `exposure_by` is already used as grouping variable.", call. = FALSE)
    }
    if (!is.null(aggregate_cols) && any(aggregate_cols %in% offweights)) {
      stop("Column in `aggregate_cols` is already used in model.", call. = FALSE)
    }

    if (!is.null(exposure) &&
        !is.null(offweights) &&
        exposure %in% c(group_by, offweights) &&
        is.null(exposure_by)) {
      warning("Column in `exposure` is already used in model.", call. = FALSE)
    }

    if (!is.null(exposure) &&
        !is.null(offweights) &&
        identical(offweights, exposure)) {
      offweights_tmp <- paste0(offweights, "_99")
      xdf[[offweights_tmp]] <- xdf[[exposure]]
      offweights <- offweights_tmp
    }

    if (!is.null(offweights) && !is.null(aggregate_cols) &&
        offweights %in% aggregate_cols) {
      offweights <- NULL
    }

    agg_cols_all <- unique(c(aggregate_cols, offweights))
  } else if (is.null(group_by)) {
    cols_excluded <- c(aggregate_cols, exposure, exposure_by)
    group_by <- setdiff(names(xdf), cols_excluded)
  }

  if (!all(group_by %in% names(xdf))) {
    missing_cols <- setdiff(group_by, names(xdf))
    stop(
      "The following `group_by` columns are not present in `x`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.null(exposure) && !exposure %in% names(xdf)) {
    stop("Column in `exposure` not found in `x`.", call. = FALSE)
  }
  if (!is.null(exposure_by) && !exposure_by %in% names(xdf)) {
    stop("Column in `exposure_by` not found in `x`.", call. = FALSE)
  }
  if (!is.null(agg_cols_all) && !all(agg_cols_all %in% names(xdf))) {
    missing_cols <- setdiff(agg_cols_all, names(xdf))
    stop(
      "The following `aggregate_cols` are not present in `x`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (drop_na) {
    if (length(group_by) > 0L) {
      xdf <- xdf[stats::complete.cases(xdf[, group_by, with = FALSE])]
    }
  }

  if (is.null(exposure)) {
    if (is.null(exposure_by)) {
      out <- .rating_grid_count(xdf, group_by)

      if (length(agg_cols_all) > 0) {
        extra <- .rating_grid_sum(xdf, group_by, agg_cols_all)
        out <- .rating_grid_merge(out, extra, group_by)
      }
    } else {
      counts_df <- .rating_grid_count(xdf, c(group_by, exposure_by))
      out <- .rating_grid_wide(
        counts_df,
        group_vars = group_by,
        split_var = exposure_by,
        value_var = "count",
        prefix = "count"
      )

      if (length(agg_cols_all) > 0) {
        extra <- .rating_grid_sum(xdf, group_by, agg_cols_all)
        out <- .rating_grid_merge(out, extra, group_by)
      }
    }
  } else {
    sum_vars <- unique(c(agg_cols_all, exposure))

    if (is.null(exposure_by)) {
      out <- .rating_grid_sum(xdf, group_by, sum_vars)
    } else {
      agg0 <- .rating_grid_sum(xdf, c(group_by, exposure_by), sum_vars)
      out <- .rating_grid_wide(
        agg0,
        group_vars = group_by,
        split_var = exposure_by,
        value_var = exposure,
        prefix = exposure
      )

      if (length(agg_cols_all) > 0) {
        extra <- .rating_grid_sum(xdf, group_by, agg_cols_all)
        out <- .rating_grid_merge(out, extra, group_by)
      }
    }

    if (inherits(x, "model_data") && !is.null(offweights)) {
      names(out) <- gsub("_99$", "", names(out))
    }
  }

  if (inherits(x, "model_data")) {
    refinement_pairs <- c(attr(x, "mgd_rst"), attr(x, "mgd_smt"))
    if (length(refinement_pairs) > 0) {
      out <- .rating_grid_add_refinement(out, xdf, refinement_pairs)
    }
  }

  out <- as.data.frame(out, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}


#' Deprecated alias for `rating_grid()`
#'
#' @description
#' `construct_model_points()` is deprecated in favour of [rating_grid()].
#'
#' @inheritParams rating_grid
#' @return See [rating_grid()].
#'
#' @export
#' @keywords internal
construct_model_points <- function(x,
                                   group_by = NULL,
                                   exposure = NULL,
                                   exposure_by = NULL,
                                   aggregate_cols = NULL,
                                   drop_na = FALSE,
                                   group_vars = NULL,
                                   agg_cols = NULL) {
  lifecycle::deprecate_warn(
    "0.9.0",
    "construct_model_points()",
    "rating_grid()"
  )

  rating_grid(
    x = x,
    group_by = if (!is.null(group_vars)) group_vars else group_by,
    exposure = exposure,
    exposure_by = exposure_by,
    aggregate_cols = if (!is.null(agg_cols)) agg_cols else aggregate_cols,
    drop_na = drop_na
  )
}
