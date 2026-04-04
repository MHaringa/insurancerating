#' Extract model data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' `extract_model_data()` retrieves underlying data from fitted models. It works
#' for objects of class `"glm"`, as well as objects produced by refitting
#' procedures (`"refitsmooth"` or `"refitrestricted"`).
#'
#' The wrapper [model_data()] is deprecated as of version 0.8.0; please use
#' [extract_model_data()] instead.
#'
#' @param x An object of class `"glm"`, `"refitsmooth"`, or `"refitrestricted"`.
#'
#' @details
#' For GLM objects, the function:
#' - returns the original data used in the model,
#' - attaches attributes with the relevant rating factors and any weights/offsets.
#'
#' For refit objects, the function:
#' - strips out auxiliary columns used for smoothing/restrictions,
#' - attaches attributes with information about rating factors, merged smooths,
#'   restrictions, and offsets.
#'
#' @return A `data.frame` of class `"model_data"`, containing the cleaned model
#' data with additional attributes:
#' \itemize{
#'   \item `rf` — names of risk factors in the model
#'   \item `offweights` — weights or offsets if present
#'   \item `mgd_rst`, `mgd_smt` — merged restrictions/smooths (refit objects only)
#'   \item `new_nm`, `old_nm` — new and old column names (refit objects only)
#' }
#'
#' @author Martin Haringa
#'
#' @examples
#' \dontrun{
#' library(insurancerating)
#' library(dplyr)
#'
#' # Fit GAM for claim frequency
#' age_policyholder_frequency <- riskfactor_gam(data = MTPL,
#'                                              nclaims = "nclaims",
#'                                              x = "age_policyholder",
#'                                              exposure = "exposure")
#'
#' # Determine clusters
#' clusters_freq <- construct_tariff_classes(age_policyholder_frequency)
#'
#' # Add clusters to MTPL portfolio
#' dat <- MTPL |>
#' mutate(age_policyholder_freq_cat = clusters_freq$tariff_classes) |>
#' mutate(across(where(is.character), as.factor)) |>
#' mutate(across(where(is.factor), ~biggest_reference(., exposure)))
#'
#' # Fit frequency and severity model
#' freq <- glm(nclaims ~ bm + age_policyholder_freq_cat, offset = log(exposure),
#'             family = poisson(), data = dat)
#' sev <- glm(amount ~ bm + zip, weights = nclaims,
#'            family = Gamma(link = "log"), data = dat |> filter(amount > 0))
#'
#' # Add predictions for freq and sev to data, and calculate premium
#' premium_df <- dat |>
#' add_prediction(freq, sev) |>
#' mutate(premium = pred_nclaims_freq * pred_amount_sev)
#'
#' # Fit unrestricted model
#' burn_unrestricted <- glm(premium ~ zip + bm + age_policyholder_freq_cat,
#' weights = exposure, family = Gamma(link = "log"), data = premium_df)
#'
#' # Impose smoothing and refit model
#' burn_restricted <- burn_unrestricted |>
#' add_smoothing(x_cut = "age_policyholder_freq_cat",
#' x_org = "age_policyholder",
#' breaks = seq(18, 95, 5)) |>
#' refit_glm()
#'
#' # Extract model data
#' extract_model_data(burn_restricted)
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

  cls <- class(x)

  if (cls[length(cls)] == "glm") {
    out <- as_df(x$data)

    rf <- rating_table(x, signif_stars = FALSE)$df
    rf <- as_df(rf)
    colnames(rf)[3] <- "estimate"

    rf2_nm <- unique(rf$risk_factor[rf$risk_factor != "(Intercept)"])

    lst_call <- as.list(x$call)
    offweights <- NULL

    if (!is.null(lst_call$weights)) {
      offweights <- append(offweights, as.character(lst_call$weights))
    }
    if (!is.null(lst_call$offset)) {
      offweights <- append(offweights, as.character(lst_call$offset)[2])
    }

    attr(out, "offweights") <- offweights
    attr(out, "rf") <- rf2_nm
  } else {

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
  }

  out <- as_df(out)
  class(out) <- c("model_data", class(out))
  out
}



#' @rdname extract_model_data
#' @export
model_data <- function(x) {
  lifecycle::deprecate_warn("0.7.5.9000", "model_data()",
                            "extract_model_data()")
  extract_model_data(x)
}


#' Construct observed model points from extracted model data or a data frame
#'
#' @description
#' `construct_model_points()` constructs model points by collapsing rows with
#' identical combinations of grouping variables to a single row.
#'
#' The function is intended for analytical use cases where it is convenient to
#' work with one row per observed combination of risk factors or model
#' variables. In many raw datasets, the same combination occurs multiple times.
#' For model diagnostics, portfolio summaries, and prediction analysis, it is
#' often more useful to aggregate these repeated combinations than to keep all
#' original rows.
#'
#' By default, the function returns only combinations that are actually observed
#' in the input data. It does **not** create the full Cartesian product of all
#' unique values, because that can become very large and is often not needed for
#' analysis.
#'
#' In other words, the function:
#' \itemize{
#'   \item identifies the model-point dimensions;
#'   \item groups rows with identical combinations of these variables;
#'   \item aggregates exposure and optional numeric columns to one row per
#'   observed combination.
#' }
#'
#' This is especially useful for:
#' \itemize{
#'   \item analysing model structure,
#'   \item summarising portfolios at model-point level,
#'   \item comparing observed and fitted values,
#'   \item creating compact input for further analysis or plotting.
#' }
#'
#' When `x` is an object returned by [extract_model_data()], the function uses
#' the extracted model metadata to determine the grouping variables if
#' `group_vars` is not supplied. When `x` is a plain `data.frame`, it is
#' recommended to supply `group_vars` explicitly.
#'
#' @param x A `data.frame` or an object of class `"model_data"` returned by
#'   [extract_model_data()].
#' @param group_vars Optional character vector with the variables that define the
#'   model points. If `NULL` and `x` is a `"model_data"` object, the risk-factor
#'   variables stored in the object are used. If `NULL` and `x` is a plain
#'   `data.frame`, all columns except those listed in `exposure`,
#'   `exposure_by`, and `agg_cols` are used.
#' @param exposure Optional character; name of the exposure column to aggregate.
#' @param exposure_by Optional character; name of a column used to split
#'   exposure or counts, for example a year variable.
#' @param agg_cols Optional character vector with additional numeric columns to
#'   aggregate using `sum(na.rm = TRUE)`.
#' @param drop_na Logical; if `TRUE`, rows with missing values in `group_vars`
#'   are removed before aggregation. Default is `FALSE`.
#'
#' @details
#' The function does **not** construct all theoretically possible model-point
#' combinations. Instead, it only keeps combinations that actually occur in the
#' input data and aggregates duplicates.
#'
#' If `exposure_by` is supplied, exposure or row counts are split across levels
#' of that variable and returned in wide format, for example
#' `"exposure_2020"` or `"count_2020"`.
#'
#' For objects returned by [extract_model_data()], additional refinement
#' variables stored in the object attributes may be retained when they are not
#' already part of the regular grouping variables.
#'
#' @return
#' A `data.frame` with one row per observed model point.
#'
#' @author Martin Haringa
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # With a data.frame
#' mtcars |>
#'   dplyr::select(cyl, vs) |>
#'   construct_model_points(group_vars = c("cyl", "vs"))
#'
#' mtcars |>
#'   dplyr::select(cyl, vs, disp) |>
#'   construct_model_points(
#'     group_vars = c("cyl", "vs"),
#'     exposure = "disp"
#'   )
#'
#' mtcars |>
#'   dplyr::select(cyl, vs, disp, gear) |>
#'   construct_model_points(
#'     group_vars = c("cyl", "vs"),
#'     exposure = "disp",
#'     exposure_by = "gear"
#'   )
#'
#' mtcars |>
#'   dplyr::select(cyl, vs, disp, gear, mpg) |>
#'   construct_model_points(
#'     group_vars = c("cyl", "vs"),
#'     exposure = "disp",
#'     exposure_by = "gear",
#'     agg_cols = c("mpg")
#'   )
#'
#' # With extracted model data
#' pmodel <- glm(
#'   breaks ~ wool + tension,
#'   data = warpbreaks,
#'   family = poisson(link = "log")
#' )
#'
#' pmodel |>
#'   extract_model_data() |>
#'   construct_model_points()
#' }
#'
#' @importFrom tidyr pivot_wider drop_na
#' @importFrom dplyr all_of
#'
#' @export
construct_model_points <- function(x,
                                   group_vars = NULL,
                                   exposure = NULL,
                                   exposure_by = NULL,
                                   agg_cols = NULL,
                                   drop_na = FALSE) {

  if (!inherits(x, "model_data") && !inherits(x, "data.frame")) {
    stop(
      "Input must be a data.frame or an object returned by extract_model_data().",
      call. = FALSE
    )
  }

  if (!is.null(group_vars) && !is.character(group_vars)) {
    stop("`group_vars` must be NULL or a character vector.", call. = FALSE)
  }

  if (!is.null(exposure) && (!is.character(exposure) || length(exposure) != 1)) {
    stop("`exposure` must be NULL or a single character string.", call. = FALSE)
  }

  if (!is.null(exposure_by) &&
      (!is.character(exposure_by) || length(exposure_by) != 1)) {
    stop("`exposure_by` must be NULL or a single character string.", call. = FALSE)
  }

  if (!is.null(agg_cols) && !is.character(agg_cols)) {
    stop("`agg_cols` must be NULL or a character vector.", call. = FALSE)
  }

  xdf <- as.data.frame(x)
  offweights <- NULL

  xdf <- as.data.frame(x)
  offweights <- NULL

  if (inherits(x, "model_data")) {

    offweights <- unique(attr(x, "offweights"))
    default_group_vars <- attr(x, "rf")

    # fallback 1: explicit term labels
    if (is.null(default_group_vars) || length(default_group_vars) == 0) {
      term_labels <- attr(x, "term.labels")
      if (!is.null(term_labels)) {
        default_group_vars <- intersect(term_labels, names(xdf))
      }
    }

    # fallback 2: terms object
    if (is.null(default_group_vars) || length(default_group_vars) == 0) {
      terms_obj <- attr(x, "terms")
      if (!is.null(terms_obj)) {
        term_labels <- attr(terms_obj, "term.labels")
        if (!is.null(term_labels)) {
          default_group_vars <- intersect(term_labels, names(xdf))
        }
      }
    }

    # fallback 3: infer from available columns
    if (is.null(default_group_vars) || length(default_group_vars) == 0) {
      response_var <- attr(x, "response")
      cols_excluded <- unique(c(
        response_var,
        offweights,
        exposure,
        exposure_by,
        agg_cols
      ))
      default_group_vars <- setdiff(names(xdf), cols_excluded)
    }

    if (length(default_group_vars) == 0) {
      stop(
        "Could not determine grouping variables from `model_data`. Supply `group_vars` explicitly.",
        call. = FALSE
      )
    }

    if (is.null(group_vars)) {
      group_vars <- default_group_vars
    }

    if (!is.null(exposure) && exposure %in% group_vars) {
      stop("Column in `exposure` is already used as grouping variable.", call. = FALSE)
    }

    if (!is.null(exposure_by) && exposure_by %in% group_vars) {
      stop("Column in `exposure_by` is already used as grouping variable.", call. = FALSE)
    }

    if (!is.null(agg_cols) && any(agg_cols %in% offweights)) {
      stop("Column in `agg_cols` is already used in model.", call. = FALSE)
    }

    if (!is.null(exposure) &&
        !is.null(offweights) &&
        exposure %in% c(group_vars, offweights) &&
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

    if (!is.null(offweights) && !is.null(agg_cols) && offweights %in% agg_cols) {
      offweights <- NULL
    }

    agg_cols_all <- unique(c(agg_cols, offweights))

  } else {
    if (is.null(group_vars)) {
      cols_excluded <- c(agg_cols, exposure, exposure_by)
      group_vars <- setdiff(names(xdf), cols_excluded)
    }
    agg_cols_all <- agg_cols
  }

  if (!all(group_vars %in% names(xdf))) {
    missing_cols <- setdiff(group_vars, names(xdf))
    stop(
      "The following `group_vars` are not present in `x`: ",
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
      "The following `agg_cols` are not present in `x`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (drop_na) {
    xdf <- xdf |>
      tidyr::drop_na(dplyr::all_of(group_vars))
  }

  aggregate_base <- function(df, by_vars, sum_vars = NULL) {
    if (length(sum_vars) == 0 || is.null(sum_vars)) {
      df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(by_vars))) |>
        dplyr::summarise(count = dplyr::n(), .groups = "drop")
    } else {
      df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(by_vars))) |>
        dplyr::summarise(
          dplyr::across(dplyr::all_of(sum_vars), ~ sum(.x, na.rm = TRUE)),
          .groups = "drop"
        )
    }
  }

  if (is.null(exposure)) {

    if (is.null(exposure_by)) {

      if (length(agg_cols_all) == 0) {
        out <- aggregate_base(xdf, group_vars, NULL)
      } else {
        out <- aggregate_base(xdf, group_vars, agg_cols_all) |>
          dplyr::left_join(
            aggregate_base(xdf, group_vars, NULL),
            by = group_vars
          )
      }

    } else {

      agg0 <- aggregate_base(
        xdf,
        c(group_vars, exposure_by),
        if (length(agg_cols_all) == 0) NULL else agg_cols_all
      )

      counts_df <- xdf |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, exposure_by)))) |>
        dplyr::summarise(count = dplyr::n(), .groups = "drop")

      if (length(agg_cols_all) > 0) {
        agg0 <- dplyr::left_join(
          agg0,
          counts_df,
          by = c(group_vars, exposure_by)
        )
      } else {
        agg0 <- counts_df
      }

      out_wide <- agg0 |>
        dplyr::mutate(name = paste0("count_", .data[[exposure_by]])) |>
        dplyr::select(dplyr::all_of(group_vars), name, count) |>
        tidyr::pivot_wider(names_from = name, values_from = count)

      if (length(agg_cols_all) > 0) {
        extra <- aggregate_base(xdf, group_vars, agg_cols_all)
        out <- dplyr::left_join(out_wide, extra, by = group_vars)
      } else {
        out <- out_wide
      }
    }

  } else {

    sum_vars <- unique(c(agg_cols_all, exposure))

    if (is.null(exposure_by)) {

      out <- aggregate_base(xdf, group_vars, sum_vars)

    } else {

      agg0 <- aggregate_base(xdf, c(group_vars, exposure_by), sum_vars)

      out_wide <- agg0 |>
        dplyr::mutate(name = paste0(exposure, "_", .data[[exposure_by]])) |>
        dplyr::select(dplyr::all_of(group_vars), name, dplyr::all_of(exposure)) |>
        tidyr::pivot_wider(names_from = name, values_from = dplyr::all_of(exposure))

      if (length(agg_cols_all) > 0) {
        extra <- aggregate_base(xdf, group_vars, agg_cols_all)
        out <- dplyr::left_join(out_wide, extra, by = group_vars)
      } else {
        out <- out_wide
      }
    }

    if (inherits(x, "model_data") && !is.null(offweights)) {
      names(out) <- gsub("_99$", "", names(out))
    }
  }

  if (inherits(x, "model_data")) {
    mgd_rst <- attr(x, "mgd_rst")
    mgd_smt <- attr(x, "mgd_smt")
    refinement_nm <- unique(c(mgd_rst, mgd_smt))
    refinement_nm <- refinement_nm[!is.na(refinement_nm)]

    if (length(refinement_nm) > 0) {
      refinement_df <- lapply(refinement_nm, function(y) {
        cols <- intersect(y, names(xdf))
        cols <- setdiff(cols, names(out))

        if (length(cols) == 0) {
          return(NULL)
        }

        unique(xdf[, cols, drop = FALSE])
      })

      refinement_df <- refinement_df[!vapply(refinement_df, is.null, logical(1))]

      if (length(refinement_df) > 0) {
        refinement_grid <- refinement_df[[1]]

        if (length(refinement_df) > 1) {
          for (j in 2:length(refinement_df)) {
            refinement_grid <- merge(refinement_grid, refinement_df[[j]], by = NULL)
          }
        }

        new_cols <- setdiff(names(refinement_grid), names(out))

        if (length(new_cols) > 0) {
          refinement_grid <- unique(refinement_grid[, new_cols, drop = FALSE])
          out <- merge(out, refinement_grid, by = NULL)
        }
      }
    }
  }

  rownames(out) <- NULL
  out
}
