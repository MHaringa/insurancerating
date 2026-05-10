#' Extract model data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' `extract_model_data()` retrieves the modelling data and metadata from fitted
#' models. It works for objects of class `"glm"`, as well as objects produced by
#' refitting procedures (`"refitsmooth"` or `"refitrestricted"`).
#'
#' `model_data()` is kept as a deprecated compatibility wrapper.
#'
#' @param x An object of class `"glm"`, `"refitsmooth"`, or `"refitrestricted"`.
#'
#' @details
#' For GLM objects, the function returns the model data and attaches attributes
#' with the response, rating factors, terms object, and any weights or offsets.
#'
#' For refit objects, the function removes auxiliary columns used during
#' smoothing or restriction and attaches attributes with rating factors, merged
#' smooths, restrictions, and offsets.
#'
#' @return A `data.frame` of class `"model_data"` with additional attributes:
#' \itemize{
#'   \item `response` — response variable in the model;
#'   \item `rf` — names of risk factors in the model;
#'   \item `offweights` — weight and offset variables if present;
#'   \item `terms` — model terms object for plain GLMs;
#'   \item `mgd_rst`, `mgd_smt` — merged restrictions/smooths for refit objects;
#'   \item `new_nm`, `old_nm` — new and old column names for refit objects.
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
        unlist(lapply(offset_idx, function(i) all.vars(term_vars[[i]])), use.names = FALSE)
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
    out <- as.data.frame(as.list(vapply(
      df[, sum_vars, drop = FALSE],
      sum,
      numeric(1),
      na.rm = TRUE
    )))
    return(out)
  }

  out <- stats::aggregate(
    df[, sum_vars, drop = FALSE],
    by = df[, by_vars, drop = FALSE],
    FUN = sum,
    na.rm = TRUE
  )
  as.data.frame(out, stringsAsFactors = FALSE)
}


.rating_grid_count <- function(df, by_vars) {
  df$.rating_grid_count <- 1L

  if (length(by_vars) == 0) {
    return(data.frame(count = nrow(df)))
  }

  out <- stats::aggregate(
    df[".rating_grid_count"],
    by = df[, by_vars, drop = FALSE],
    FUN = length
  )
  names(out)[names(out) == ".rating_grid_count"] <- "count"
  as.data.frame(out, stringsAsFactors = FALSE)
}


.rating_grid_merge <- function(x, y, by_vars) {
  if (length(by_vars) == 0) {
    return(cbind(x, y))
  }

  merge(x, y, by = by_vars, all = TRUE, sort = FALSE)
}


.rating_grid_wide <- function(df, group_vars, split_var, value_var, prefix) {
  levels_split <- unique(as.character(df[[split_var]]))
  out <- unique(df[, group_vars, drop = FALSE])

  for (level in levels_split) {
    level_rows <- as.character(df[[split_var]]) == level
    tmp <- df[level_rows, c(group_vars, value_var), drop = FALSE]
    names(tmp)[names(tmp) == value_var] <- paste0(prefix, "_", level)
    out <- .rating_grid_merge(out, tmp, group_vars)
  }

  out
}


.rating_grid_add_refinement <- function(out, xdf, refinement_pairs) {
  refinement_pairs <- refinement_pairs[vapply(refinement_pairs, length, integer(1)) >= 2]

  for (pair in refinement_pairs) {
    old_col <- pair[[1]]
    new_col <- pair[[2]]

    if (!old_col %in% names(out) || !new_col %in% names(xdf) || new_col %in% names(out)) {
      next
    }

    mapping <- unique(xdf[, c(old_col, new_col), drop = FALSE])
    if (any(duplicated(mapping[[old_col]]))) {
      warning(
        "Refinement column `", new_col, "` has multiple values per `", old_col,
        "` and was not added to the rating grid.",
        call. = FALSE
      )
      next
    }

    out <- merge(out, mapping, by = old_col, all.x = TRUE, sort = FALSE)
  }

  out
}


#' Construct observed rating-grid points from model data or a data frame
#'
#' @description
#' `rating_grid()` constructs rating-grid points by collapsing rows with
#' identical combinations of grouping variables to a single row.
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
#'   are removed before aggregation. Default is `FALSE`.
#' @param group_vars,agg_cols Deprecated argument names. Use `group_by` and
#'   `aggregate_cols` instead.
#'
#' @details
#' The implementation uses base R only. Output is always a regular
#' `data.frame`, not a tibble or data.table.
#'
#' If `exposure_by` is supplied, exposure or row counts are split across levels
#' of that variable and returned in wide format, for example
#' `"exposure_2020"` or `"count_2020"`.
#'
#' For objects returned by [extract_model_data()], refinement mappings are joined
#' by their original factor column. They are not cross-joined onto every row.
#'
#' @return
#' A `data.frame` with one row per observed rating-grid point.
#'
#' @author Martin Haringa
#'
#' @examples
#' \dontrun{
#' rating_grid(mtcars, group_by = c("cyl", "vs"))
#'
#' rating_grid(
#'   mtcars,
#'   group_by = c("cyl", "vs"),
#'   exposure = "disp",
#'   exposure_by = "gear",
#'   aggregate_cols = "mpg"
#' )
#'
#' pmodel <- glm(
#'   breaks ~ wool + tension,
#'   data = warpbreaks,
#'   family = poisson(link = "log")
#' )
#'
#' pmodel |>
#'   extract_model_data() |>
#'   rating_grid()
#' }
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

  xdf <- as.data.frame(x, stringsAsFactors = FALSE)
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
    xdf <- xdf[stats::complete.cases(xdf[, group_by, drop = FALSE]), , drop = FALSE]
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
