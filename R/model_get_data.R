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
#' A factor created by [add_relativities()] replaces its parent in the
#' default rating-grid grouping. Its numeric relativity columns are mapped to
#' the new segment levels using the stored refinement specification; the
#' original portfolio columns remain available for explicit grouping and audits.
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
#'   \item `rf`: risk factors used for default rating-grid grouping, including
#'   the output factors of split refinements;
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
#' portfolio <- as.data.frame(MTPL2)
#' portfolio$area <- factor(portfolio$area)
#' model <- glm(
#'   nclaims ~ area + offset(log(exposure)),
#'   family = poisson(), data = portfolio
#' )
#'
#' refined <- prepare_refinement(model, data = portfolio) |>
#'   add_restriction(c("3" = 1.10), model_variable = "area") |>
#'   refit()
#'
#' # Includes exposure and the column added by the restriction.
#' evaluation <- extract_model_data(refined)
#' names(evaluation)
#' calibration_analysis(refined, evaluation, by = "area")
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

    grid_metadata <- .refinement_grid_metadata(
      attr(x, "rf"), attr(x, "mgd_rst"), attr(x, "refinement_steps"),
      names(out)
    )
    rf <- grid_metadata$rf
    mgd_smt <- attr(x, "mgd_smt")

    for (i in seq_along(mgd_smt)) {
      zsm <- gsub("_smooth$", "", mgd_smt[[i]][2])
      rf[rf == zsm] <- mgd_smt[[i]][1]
    }

    attr(out, "rf") <- rf
    attr(out, "mgd_rst") <- grid_metadata$mgd_rst
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
  if (length(by_vars) == 0) {
    return(df[, lapply(.SD, sum, na.rm = TRUE), .SDcols = sum_vars])
  }

  out <- df[, lapply(.SD, sum, na.rm = TRUE),
            by = c(by_vars), .SDcols = sum_vars]
  data.table::setorderv(out, by_vars)
  out
}


.rating_grid_count <- function(df, by_vars) {
  if (length(by_vars) == 0) {
    return(data.frame(count = nrow(df)))
  }

  out <- df[, list(count = .N), by = c(by_vars)]
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


.refinement_grid_metadata <- function(risk_factors, restriction_pairs, steps,
                                       data_columns) {
  for (step in steps) {
    if (!identical(step$type, "relativities")) {
      next
    }
    source <- step$source_model_variable %||% step$model_variable %||%
      step$risk_factor
    output <- step$output_variable %||% step$display_risk_factor
    relativity <- step$derived_model_variable
    if (!all(vapply(list(source, output, relativity), .is_single_string,
                    logical(1))) || !output %in% data_columns) {
      next
    }

    # A split's numeric effect belongs to its output levels, not the parent
    # levels. Resolve stored mappings too, including those from earlier fits.
    risk_factors[risk_factors == source] <- output
    risk_factors <- unique(c(risk_factors, output))
    restriction_pairs <- lapply(restriction_pairs, function(pair) {
      if (length(pair) >= 2L && identical(pair[[2]], relativity)) {
        pair[[1]] <- output
      }
      pair
    })
  }
  list(rf = risk_factors, mgd_rst = restriction_pairs)
}


.rating_grid_add_refinement <- function(out, xdf, refinement_pairs) {
  out <- data.table::as.data.table(out)
  if (!data.table::is.data.table(xdf)) {
    xdf <- data.table::as.data.table(xdf)
  }
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


#' Aggregate portfolio records into observed model points
#'
#' @description
#' Combine records with identical grouping values into one model point per
#' observed combination. Sum exposure and other additive quantities within
#' each model point for model fitting, prediction or portfolio analysis.
#'
#' Together with [merge_date_ranges()], this function belongs to the portfolio
#' reduction workflow. Both functions reduce row-level portfolio data while
#' retaining selected totals. `rating_grid()` reduces across identical
#' risk-factor combinations; [merge_date_ranges()] reduces temporally connected
#' records within the same policy, risk or portfolio segment.
#'
#' The rating grid contains only combinations observed in the input data.
#' It does **not** create the full Cartesian product of all unique values.
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
#'   model points. If `NULL` and `x` is a `"model_data"` object, the
#'   risk-factor variables stored in the object are used. If `NULL` and `x` is a
#'   plain `data.frame`, all columns except those listed in `aggregate_cols`,
#'   `split` and `split_by` are used.
#' @param ... Reserved for legacy positional arguments. Name all arguments
#'   after `group_by` in new code.
#' @param aggregate_cols Optional character vector with additional numeric
#'   columns to sum per model point using `sum(na.rm = TRUE)`. These totals
#'   are not split across output columns.
#' @param split Optional character string naming the numeric column to sum
#'   instead of counting source rows. For example, use `split = "exposure"`
#'   to sum exposure. If `split_by` is not supplied, the total is returned
#'   in a single column with the original name.
#' @param split_by Optional character string naming the column whose values
#'   become separate output columns. With `split = "exposure"` and
#'   `split_by = "year"`, these might be `exposure_2024` and
#'   `exposure_2025`. Without `split`, they contain source row counts
#'   (for example, `count_2024` and `count_2025`).
#' @param drop_na Logical. With `FALSE` (default), missing values in
#'   `group_by` form a distinct grouping value and those rows still contribute
#'   to the totals. With `TRUE`, a missing value in any `group_by` column
#'   excludes the entire row from counts, exposure and other totals.
#'   Only `group_by` columns are checked; missing values in the split or
#'   aggregation columns do not trigger this filter.
#' @param exposure,exposure_by Deprecated argument names. Use `split` and
#'   `split_by` instead.
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
#' variables in `group_by`. The additional columns in `aggregate_cols` are
#' summed over the source records in that combination. Such a row is a model point:
#' one observed covariate combination together with its aggregated additive
#' portfolio quantities. This is a categorical reduction of the portfolio; no
#' date intervals are combined.
#'
#' To calculate all totals separately by year, include the year column in
#' `group_by`. Use `split_by` when you want to keep one row per model point
#' and spread only the selected measure (or row counts) across year columns.
#'
#' ## Estimating a GLM on aggregated data
#'
#' For an unweighted Poisson log-link GLM with claim counts as the response
#' and `offset(log(exposure))` as the only offset, grouping by all model
#' predictors and summing counts and exposure preserves the coefficient
#' estimates, up to numerical tolerance. Records within a group share the same
#' expected claim rate, but their expected counts can differ if their exposures
#' differ. The grouped fit uses the log of the summed exposure as its offset.
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
#' After [add_relativities()] creates an output factor, the default grid uses
#' that factor instead of its parent. Split relativities and subsequent
#' shrinkage or rebasing columns are joined by the output segment, not by the
#' original parent level. Ordinary one-to-one restriction mappings continue
#' to use their existing grouping variables.
#'
#' For objects returned by [extract_model_data()], refinement mappings are joined
#' by the factor key recorded in the refinement metadata. They are not
#' cross-joined onto every row.
#'
#' On wide portfolios, supply `group_by` explicitly to avoid grouping by
#' unused columns. The input is not modified, and the output is always a
#' regular `data.frame`.
#'
#' When the row-level portfolio does not fit comfortably in R memory, use
#' [rating_grid_db()] to perform the grouped reduction in a database and collect
#' only the resulting grid.
#'
#' @return
#' A `data.frame` with one row per observed model point.
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
#'   split = "earned_exposure"
#' )
#'
#' # Split earned exposure by underwriting year. This is useful when reviewing
#' # whether the portfolio mix within each rating combination changes over time.
#' rating_grid(
#'   portfolio,
#'   group_by = c("sector", "region"),
#'   split = "earned_exposure",
#'   split_by = "underwriting_year"
#' )
#'
#' # Claim count and claim amount remain additive totals in the rating grid.
#' # Frequency and average severity can subsequently be derived from them.
#' claims_grid <- rating_grid(
#'   portfolio,
#'   group_by = c("sector", "region"),
#'   aggregate_cols = c("claim_count", "claim_amount"),
#'   split = "earned_exposure"
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
#'   aggregate_cols = "nclaims",
#'   split = "exposure"
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
                        ...,
                        aggregate_cols = NULL,
                        split = NULL,
                        split_by = NULL,
                        drop_na = FALSE,
                        exposure = NULL,
                        exposure_by = NULL,
                        group_vars = NULL,
                        agg_cols = NULL) {

  if (...length() > 0L) {
    dots <- rlang::dots_list(..., .ignore_empty = "none", .preserve_empty = TRUE)
    unknown <- names(dots)[nzchar(names(dots))]
    if (length(unknown)) {
      stop("Unused argument(s): ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    if (!missing(split) || !missing(split_by)) {
      stop("Do not mix the old positional order with `split` or `split_by`. ",
           "Name all arguments after `group_by`.", call. = FALSE)
    }
    legacy <- list(split, split_by, aggregate_cols, drop_na, group_vars, agg_cols)
    supplied <- c(
      !missing(exposure), !missing(exposure_by),
      !missing(aggregate_cols), !missing(drop_na),
      !missing(group_vars), !missing(agg_cols)
    )
    available <- which(!supplied)
    if (length(dots) > length(available)) {
      stop("Too many positional arguments. Name arguments after `group_by`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0", "rating_grid(...)",
      details = "Name all arguments after `group_by` in new calls."
    )
    positions <- which(!vapply(dots, rlang::is_missing, logical(1)))
    legacy[available[positions]] <- dots[positions]
    split <- legacy[[1L]]
    split_by <- legacy[[2L]]
    aggregate_cols <- legacy[[3L]]
    drop_na <- legacy[[4L]]
    group_vars <- legacy[[5L]]
    agg_cols <- legacy[[6L]]
  }

  if (!is.null(exposure)) {
    if (!is.null(split)) {
      stop("Use only one of `split` and deprecated `exposure`.", call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "rating_grid(exposure)",
                              "rating_grid(split)")
    split <- exposure
  }
  if (!is.null(exposure_by)) {
    if (!is.null(split_by)) {
      stop("Use only one of `split_by` and deprecated `exposure_by`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "rating_grid(exposure_by)",
                              "rating_grid(split_by)")
    split_by <- exposure_by
  }

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
  if (!is.null(split) && !.is_single_string(split)) {
    stop("`split` must be NULL or a single column name.", call. = FALSE)
  }
  if (!is.null(split_by) && !.is_single_string(split_by)) {
    stop("`split_by` must be NULL or a single column name.", call. = FALSE)
  }
  if (!is.null(aggregate_cols) && !is.character(aggregate_cols)) {
    stop("`aggregate_cols` must be NULL or a character vector.", call. = FALSE)
  }
  if (!is.logical(drop_na) || length(drop_na) != 1 || is.na(drop_na)) {
    stop("`drop_na` must be TRUE or FALSE.", call. = FALSE)
  }

  xdf <- if (data.table::is.data.table(x)) {
    x
  } else {
    as.data.frame(x, stringsAsFactors = FALSE)
  }
  # Matrix-like columns can expand to new names during conversion.
  if (any(vapply(xdf, function(column) !is.null(dim(column)), logical(1)))) {
    xdf <- data.table::as.data.table(as.data.frame(xdf, stringsAsFactors = FALSE))
  }
  available_cols <- names(xdf)
  offweights <- NULL
  offweights_tmp <- NULL
  agg_cols_all <- aggregate_cols
  refinement_pairs <- NULL

  if (inherits(x, "model_data")) {
    offweights <- unique(attr(x, "offweights"))
    default_group_vars <- attr(x, "rf")

    if (is.null(default_group_vars) || length(default_group_vars) == 0) {
      term_labels <- attr(x, "term.labels")
      if (!is.null(term_labels)) {
        default_group_vars <- intersect(term_labels, available_cols)
      }
    }

    if (is.null(default_group_vars) || length(default_group_vars) == 0) {
      terms_obj <- attr(x, "terms")
      if (!is.null(terms_obj)) {
        term_labels <- attr(terms_obj, "term.labels")
        default_group_vars <- intersect(term_labels, available_cols)
      }
    }

    if (is.null(default_group_vars) || length(default_group_vars) == 0) {
      response_var <- attr(x, "response")
      cols_excluded <- unique(c(
        response_var,
        offweights,
        split,
        split_by,
        aggregate_cols
      ))
      default_group_vars <- setdiff(available_cols, cols_excluded)
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

    if (!is.null(split) && split %in% group_by) {
      stop("Column in `split` is already used as grouping variable.", call. = FALSE)
    }
    if (!is.null(split_by) && split_by %in% group_by) {
      stop("Column in `split_by` is already used as grouping variable.", call. = FALSE)
    }
    if (!is.null(aggregate_cols) && any(aggregate_cols %in% offweights)) {
      stop("Column in `aggregate_cols` is already used in model.", call. = FALSE)
    }

    if (!is.null(split) &&
        !is.null(offweights) &&
        split %in% c(group_by, offweights) &&
        is.null(split_by)) {
      warning("Column in `split` is already used in model.", call. = FALSE)
    }

    if (!is.null(split) &&
        !is.null(offweights) &&
        identical(offweights, split)) {
      offweights_tmp <- paste0(offweights, "_99")
      if (split %in% available_cols) {
        available_cols <- union(available_cols, offweights_tmp)
      } else {
        available_cols <- setdiff(available_cols, offweights_tmp)
      }
      offweights <- offweights_tmp
    }

    if (!is.null(offweights) && !is.null(aggregate_cols) &&
        any(offweights %in% aggregate_cols)) {
      offweights <- NULL
    }

    agg_cols_all <- unique(c(aggregate_cols, offweights))
    refinement_pairs <- c(attr(x, "mgd_rst"), attr(x, "mgd_smt"))
  } else if (is.null(group_by)) {
    cols_excluded <- c(aggregate_cols, split, split_by)
    group_by <- setdiff(available_cols, cols_excluded)
  }

  if (!all(group_by %in% available_cols)) {
    missing_cols <- setdiff(group_by, available_cols)
    stop(
      "The following `group_by` columns are not present in `x`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.null(split) && !split %in% available_cols) {
    stop("Column in `split` not found in `x`.", call. = FALSE)
  }
  if (!is.null(split_by) && !split_by %in% available_cols) {
    stop("Column in `split_by` not found in `x`.", call. = FALSE)
  }
  if (!is.null(agg_cols_all) && !all(agg_cols_all %in% available_cols)) {
    missing_cols <- setdiff(agg_cols_all, available_cols)
    stop(
      "The following `aggregate_cols` are not present in `x`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  grid_cols <- intersect(names(xdf), c(
    group_by, split, split_by, agg_cols_all,
    unlist(refinement_pairs, use.names = FALSE)
  ))
  if (!length(grid_cols)) {
    return(data.frame(count = if (length(xdf)) nrow(xdf) else 0L))
  }
  if (data.table::is.data.table(xdf)) {
    xdf <- xdf[, grid_cols, with = FALSE]
  } else {
    xdf <- data.table::as.data.table(xdf[grid_cols])
  }
  if (!is.null(offweights_tmp)) {
    xdf[[offweights_tmp]] <- xdf[[split]]
  }

  if (drop_na) {
    if (length(group_by) > 0L) {
      xdf <- xdf[stats::complete.cases(xdf[, group_by, with = FALSE])]
    }
  }

  if (is.null(split)) {
    if (is.null(split_by)) {
      out <- .rating_grid_count(xdf, group_by)

      if (length(agg_cols_all) > 0) {
        extra <- .rating_grid_sum(xdf, group_by, agg_cols_all)
        out <- .rating_grid_merge(out, extra, group_by)
      }
    } else {
      counts_df <- .rating_grid_count(xdf, c(group_by, split_by))
      out <- .rating_grid_wide(
        counts_df,
        group_vars = group_by,
        split_var = split_by,
        value_var = "count",
        prefix = "count"
      )

      if (length(agg_cols_all) > 0) {
        extra <- .rating_grid_sum(xdf, group_by, agg_cols_all)
        out <- .rating_grid_merge(out, extra, group_by)
      }
    }
  } else {
    sum_vars <- unique(c(agg_cols_all, split))

    if (is.null(split_by)) {
      out <- .rating_grid_sum(xdf, group_by, sum_vars)
    } else {
      agg0 <- .rating_grid_sum(xdf, c(group_by, split_by), split)
      out <- .rating_grid_wide(
        agg0,
        group_vars = group_by,
        split_var = split_by,
        value_var = split,
        prefix = split
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

  if (length(refinement_pairs) > 0) {
    out <- .rating_grid_add_refinement(out, xdf, refinement_pairs)
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
#' @param group_by See [rating_grid()]. The legacy `exposure` and `exposure_by`
#'   arguments correspond to `split` and `split_by`.
#' @param exposure,exposure_by See `split` and `split_by` in [rating_grid()].
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
    aggregate_cols = if (!is.null(agg_cols)) agg_cols else aggregate_cols,
    split = exposure,
    split_by = exposure_by,
    drop_na = drop_na
  )
}
