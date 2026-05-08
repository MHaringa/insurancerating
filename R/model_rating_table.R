# -----------------------------------------------------------------------------
# Rating table helpers
# -----------------------------------------------------------------------------

#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom stats terms
#' @importFrom utils stack
#'
#' @keywords internal
.collect_refit_new_rf <- function(model) {

  out <- list()

  x_rst <- attr(model, "new_rf_rst")
  x_smt <- attr(model, "new_rf")

  if (!is.null(x_rst) && nrow(x_rst) > 0) {
    out[[length(out) + 1]] <- x_rst
  }

  if (!is.null(x_smt) && nrow(x_smt) > 0) {
    out[[length(out) + 1]] <- x_smt
  }

  if (length(out) == 0) {
    return(NULL)
  }

  x <- do.call(rbind, out)
  x <- unique(x)
  rownames(x) <- NULL
  x
}


#' @keywords internal
.normalize_level_key <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.numeric(x)) {
    x <- format(x, trim = TRUE, scientific = FALSE, digits = 15)
    x <- sub("\\.?0+$", "", x)
    x[x == ""] <- "0"
    return(x)
  }

  trimws(as.character(x))
}


#' @keywords internal
.get_restriction_map <- function(model) {
  out <- attr(model, "restriction_map")
  if (is.null(out)) {
    return(NULL)
  }

  if (!is.data.frame(out)) {
    return(NULL)
  }

  needed <- c("source_var", "risk_factor")
  if (!all(needed %in% names(out))) {
    return(NULL)
  }

  out$source_var <- as.character(out$source_var)
  out$risk_factor <- as.character(out$risk_factor)
  unique(out[, needed, drop = FALSE])
}


#' @keywords internal
.map_exposure_source_var <- function(model, risk_factor, model_data_names) {

  rf <- as.character(risk_factor)

  # 1. explicit restriction map from refit object
  # IMPORTANT: this must come before exact-match lookup, because the refit
  # data often also contains the restricted column itself (e.g. zip_rst),
  # while exposure should still be aggregated on the original source variable
  # (e.g. zip).
  rst_map <- .get_restriction_map(model)
  if (!is.null(rst_map)) {
    hit <- rst_map[rst_map$risk_factor == rf, , drop = FALSE]
    if (nrow(hit) > 0) {
      src <- hit$source_var[1]
      if (!is.na(src) && src %in% model_data_names) {
        return(src)
      }
    }
  }

  # 2. exact match
  if (rf %in% model_data_names) {
    return(rf)
  }

  # 3. fallback for legacy naming conventions
  rf_base <- sub("_rst99$", "", rf)
  rf_base <- sub("_rst$", "", rf_base)
  if (rf_base %in% model_data_names) {
    return(rf_base)
  }

  # 4. smoothing fallback
  rf_base <- sub("_smooth$", "", rf)
  if (rf_base %in% model_data_names) {
    return(rf_base)
  }

  NULL
}


#' @keywords internal
.resolve_rating_table_model_data <- function(model, model_data = NULL) {
  if (!is.null(model_data)) {
    return(as.data.frame(model_data))
  }

  if (!is.null(model$data)) {
    return(as.data.frame(model$data))
  }

  NULL
}


#' @keywords internal
.resolve_rating_table_model_data_name <- function(model, model_data = NULL) {
  if (!is.null(model_data)) {
    return(deparse(substitute(model_data)))
  }

  if (!is.null(model$data)) {
    return("model$data")
  }

  NULL
}


#' @keywords internal
.infer_exposure_col <- function(model, model_data) {

  if (is.null(model_data)) {
    return(NULL)
  }

  candidates <- character(0)

  # 1. weights from model call
  w_call <- tryCatch(model$call$weights, error = function(e) NULL)

  if (!is.null(w_call)) {
    w_chr <- deparse(w_call)
    if (length(w_chr) == 1 && w_chr %in% names(model_data)) {
      candidates <- c(candidates, w_chr)
    }
  }

  # 2. refit metadata
  offweights <- attr(model, "offweights")

  if (!is.null(offweights)) {
    offweights <- offweights[offweights %in% names(model_data)]
    if (length(offweights) > 0) {
      candidates <- c(candidates, offweights)
    }
  }

  candidates <- unique(candidates)

  if (length(candidates) == 0) {
    return(NULL)
  }

  # keep only numeric columns
  candidates <- candidates[vapply(
    candidates,
    function(nm) is.numeric(model_data[[nm]]),
    logical(1)
  )]

  if (length(candidates) == 0) {
    return(NULL)
  }

  # strong preference for a column literally called "exposure"
  if ("exposure" %in% candidates) {
    return("exposure")
  }

  candidates[1]
}


#' @keywords internal
.resolve_exposure_spec <- function(model, model_data, exposure = TRUE,
                                   exposure_output = NULL) {

  if (isFALSE(exposure)) {
    return(list(
      use_exposure = FALSE,
      exposure_col = NULL,
      exposure_out = NULL
    ))
  }

  if (isTRUE(exposure)) {
    exposure_col <- .infer_exposure_col(model, model_data)
  } else if (is.character(exposure) && length(exposure) == 1) {
    exposure_col <- exposure
  } else {
    stop(
      "`exposure` must be TRUE, FALSE, or a single character string.",
      call. = FALSE
    )
  }

  if (is.null(exposure_col)) {
    return(list(
      use_exposure = FALSE,
      exposure_col = NULL,
      exposure_out = NULL
    ))
  }

  if (is.null(model_data)) {
    return(list(
      use_exposure = FALSE,
      exposure_col = NULL,
      exposure_out = NULL
    ))
  }

  if (!exposure_col %in% names(model_data)) {
    stop(
      "Exposure column '", exposure_col, "' is not present in model data.",
      call. = FALSE
    )
  }

  if (!is.numeric(model_data[[exposure_col]])) {
    stop(
      "Exposure column '", exposure_col, "' must be numeric.",
      call. = FALSE
    )
  }

  exposure_out <- if (is.null(exposure_output)) exposure_col else exposure_output

  list(
    use_exposure = TRUE,
    exposure_col = exposure_col,
    exposure_out = exposure_out
  )
}


#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom stats terms
#' @importFrom utils stack
#'
#' @keywords internal
.rating_table_one_model <- function(model,
                                    model_data = NULL,
                                    exposure = TRUE,
                                    exposure_output = NULL,
                                    colname = "estimate",
                                    exponentiate = TRUE,
                                    round_exposure = 0) {

  # ---------------------------------------------------------------------------
  # Block pre-refit workflow objects
  # ---------------------------------------------------------------------------
  if (inherits(model, "rating_refinement")) {
    stop(
      "Input is a 'rating_refinement' object. Call refit() first, then use rating_table().",
      call. = FALSE
    )
  }

  if (inherits(model, c("restricted", "smooth"))) {
    stop(
      "Model is not refitted yet. Please call refit_glm() first.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Acceptable classes
  # ---------------------------------------------------------------------------
  if (!inherits(model, c("glm", "refitsmooth", "refitrestricted"))) {
    stop("Input must be a glm or a refit_glm()/refit() object.", call. = FALSE)
  }

  model_data_use <- .resolve_rating_table_model_data(model, model_data)
  model_data_name <- .resolve_rating_table_model_data_name(model, model_data)

  exposure_info <- .resolve_exposure_spec(
    model = model,
    model_data = model_data_use,
    exposure = exposure,
    exposure_output = exposure_output
  )

  use_exposure <- exposure_info$use_exposure
  exposure_col <- exposure_info$exposure_col
  exposure_out <- exposure_info$exposure_out

  # ---------------------------------------------------------------------------
  # Extract original xlevels from glm
  # ---------------------------------------------------------------------------
  xl <- model$xlevels
  xl_names <- character(0)

  xl_df <- data.frame(
    risk_factor = character(),
    level = character(),
    ind_values = character(),
    stringsAsFactors = FALSE
  )

  if (length(xl) > 0) {
    xl_names <- names(xl)
    tmp <- utils::stack(xl)
    tmp[] <- lapply(tmp, as.character)
    names(tmp) <- c("level", "risk_factor")
    tmp$ind_values <- paste0(tmp$risk_factor, tmp$level)
    xl_df <- rbind(xl_df, tmp)
  }

  # ---------------------------------------------------------------------------
  # Add refit() / refit_glm() attributes
  # IMPORTANT: collect BOTH new_rf_rst and new_rf
  # ---------------------------------------------------------------------------
  x_new <- NULL

  if (inherits(model, c("refitsmooth", "refitrestricted"))) {
    x_new <- .collect_refit_new_rf(model)

    if (!is.null(x_new) && nrow(x_new) > 0) {
      x_new$risk_factor <- as.character(x_new$risk_factor)
      x_new$level <- as.character(x_new$level)
      x_new$ind_values <- paste0(x_new$risk_factor, x_new$level)

      x2 <- x_new[, c("risk_factor", "level", "ind_values"), drop = FALSE]
      xl_df <- rbind(xl_df, x2)
      xl_names <- c(xl_names, unique(x_new$risk_factor))
    }
  }

  # ---------------------------------------------------------------------------
  # Exposure by factor level
  # ---------------------------------------------------------------------------
  if (isTRUE(use_exposure) && !is.null(model_data_use)) {

    model_data_use <- as.data.frame(model_data_use)

    rf_all <- unique(xl_df$risk_factor)

    rf_map <- data.frame(
      risk_factor = rf_all,
      source_var = vapply(
        rf_all,
        function(rf) {
          out <- .map_exposure_source_var(model, rf, names(model_data_use))
          if (is.null(out)) "" else out
        },
        character(1)
      ),
      stringsAsFactors = FALSE
    )

    rf_map_in  <- rf_map[rf_map$source_var != "", , drop = FALSE]
    rf_map_out <- rf_map[rf_map$source_var == "", , drop = FALSE]

    if (nrow(rf_map_out) > 0 && !is.null(model_data_name)) {
      message(
        paste(rf_map_out$risk_factor, collapse = ", "),
        " not in ",
        model_data_name
      )
    }

    if (nrow(rf_map_in) > 0) {
      listexp <- lapply(seq_len(nrow(rf_map_in)), function(i) {
        rf_i  <- rf_map_in$risk_factor[i]
        src_i <- rf_map_in$source_var[i]

        tmp <- stats::aggregate(
          model_data_use[[exposure_col]],
          by = list(level = as.character(model_data_use[[src_i]])),
          FUN = sum,
          na.rm = TRUE
        )

        names(tmp)[names(tmp) == "x"] <- exposure_col
        tmp$risk_factor <- rf_i
        tmp
      })

      dfexp <- if (length(listexp) > 0) do.call(rbind, listexp) else NULL

      if (!is.null(dfexp)) {
        dfexp$level <- as.character(dfexp$level)
        dfexp$risk_factor <- as.character(dfexp$risk_factor)

        if (!identical(exposure_col, exposure_out)) {
          names(dfexp)[names(dfexp) == exposure_col] <- exposure_out
        }

        xl_df$level <- as.character(xl_df$level)
        xl_df$risk_factor <- as.character(xl_df$risk_factor)

        xl_df <- dplyr::left_join(
          xl_df,
          dfexp,
          by = c("level", "risk_factor")
        )
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Coefficients and p-values
  # ---------------------------------------------------------------------------
  ret <- coefficients(summary(model))
  ret <- cbind(ind = rownames(ret), data.frame(ret, row.names = NULL))

  all_coefs <- stats::coef(model)

  if (length(all_coefs) != nrow(ret)) {
    coefs_df <- utils::stack(all_coefs)
    colnames(coefs_df)[colnames(coefs_df) == "values"] <- "Estimate"
    ret <- merge(x = coefs_df, y = ret, by = c("ind", "Estimate"), all.x = TRUE)
  }

  coef_vals <- coefficients(model)
  vals <- utils::stack(coef_vals)

  vals$pvalues <- as.numeric(ret[, 5])
  vals$pvalues <- ifelse(is.na(vals$pvalues), -9e9, vals$pvalues)
  vals$ind <- as.character(vals$ind)

  new_col_nm0 <- attr(model, "new_col_nm")

  if (inherits(model, c("refitsmooth", "refitrestricted")) &&
      !is.null(x_new) && nrow(x_new) > 0) {
    x_rf_names <- unique(x_new$risk_factor)
    new_col_nm0 <- unique(c(new_col_nm0, x_rf_names))
  }

  if (inherits(model, c("refitsmooth", "refitrestricted")) &&
      !is.null(x_new) && nrow(x_new) > 0) {
    x2 <- x_new[, c("yhat", "ind_values"), drop = FALSE]
    colnames(x2) <- c("values", "ind")
    x2$pvalues <- NA
    x2$values <- log(x2$values)
    vals <- rbind(vals, x2)
  }

  uit <- dplyr::full_join(xl_df, vals, by = c("ind_values" = "ind"))

  uit$values <- ifelse(
    is.na(uit$pvalues) &
      !(endsWith(uit$risk_factor, "_smooth") |
          uit$risk_factor %in% new_col_nm0),
    0,
    uit$values
  )

  int <- attr(stats::terms(model), "intercept")

  # Intercept
  intercept_condition <- int == 1 & uit$ind_values == "(Intercept)"
  uit$level[intercept_condition] <- "(Intercept)"
  uit$risk_factor[intercept_condition] <- "(Intercept)"

  # Continuous factors
  level_condition <- is.na(uit$level) & is.na(uit$risk_factor)
  uit$level[level_condition] <- uit$ind_values[level_condition]
  uit$risk_factor[is.na(uit$risk_factor)] <- uit$ind_values[is.na(uit$risk_factor)]

  # Exponentiate if needed
  if (isTRUE(exponentiate)) {
    uit$values <- exp(uit$values)
  }

  # Intercept first
  if (int == 1) {
    intercept_ix <- which(uit$risk_factor == "(Intercept)")
    uit <- uit[c(intercept_ix, setdiff(seq_len(nrow(uit)), intercept_ix)), , drop = FALSE]
  }

  row.names(uit) <- NULL

  # Select columns
  if (isTRUE(use_exposure) &&
      !is.null(exposure_out) &&
      exposure_out %in% names(uit)) {
    selected_columns <- c("risk_factor", "level", "values", exposure_out, "pvalues")
    selected_columns <- intersect(selected_columns, names(uit))
    uit <- uit[, selected_columns, drop = FALSE]
    uit[[exposure_out]] <- round(uit[[exposure_out]], round_exposure)
  } else {
    selected_columns <- c("risk_factor", "level", "values", "pvalues")
    selected_columns <- intersect(selected_columns, names(uit))
    uit <- uit[, selected_columns, drop = FALSE]
  }

  names(uit)[names(uit) == "values"] <- colname

  # p-values -> stars
  uit$pvalues[uit$pvalues < 0] <- NA
  uit$pvalues <- vapply(uit$pvalues, make_stars, FUN.VALUE = character(1))

  # Cleanup
  uit$risk_factor <- sub("_rst99$", "", uit$risk_factor)

  # intercept_only handling
  io <- attr(model, "intercept_only")
  cf <- attr(model, "continuous_factors")

  if (isTRUE(io) && !is.null(cf) && nrow(cf) > 0) {
    cf$pvalues <- NA_character_

    missing_cols <- setdiff(names(uit), names(cf))
    if (length(missing_cols) > 0) {
      for (nm in missing_cols) {
        cf[[nm]] <- NA
      }
    }

    cf <- cf[, names(uit), drop = FALSE]
    uit <- rbind(uit, cf)
  }

  attr(uit, "exposure_out") <- exposure_out
  attr(uit, "model_data_name") <- model_data_name

  uit
}


#' Build rating tables from fitted pricing models
#'
#' @description
#' `rating_table()` extracts model coefficients in tariff-table form. It adds
#' the reference level for factor variables, can exponentiate GLM coefficients
#' into relativities, and can add exposure by risk-factor level when the model
#' data are available.
#'
#' `rating_table()` is intended for fitted models:
#' - plain `glm` objects
#' - models obtained after `refit()`
#' - models obtained after `refit_glm()`
#'
#' For pre-refit objects (`rating_refinement`, `restricted`, `smooth`) use
#' `print()`, `summary()` and `autoplot()` instead.
#'
#' @param ... glm object(s) produced by `glm()`, `refit()` or `refit_glm()`
#' @param model_data Optional data.frame used to create the model(s). If `NULL`,
#'   the function tries to use `model$data` for each supplied model.
#' @param exposure Logical or character. If `TRUE` (default), exposure is added
#'   if it can be inferred from the model. If `FALSE`, no exposure is added.
#'   If a character string is supplied, it is interpreted as the exposure column
#'   name.
#' @param exposure_output Optional name for the exposure column in the output.
#'   If `NULL`, the original exposure column name is used.
#' @param exponentiate logical indicating whether or not to exponentiate the
#'   coefficient estimates. Defaults to TRUE.
#' @param significance Logical; if `TRUE`, show significance stars for p-values.
#' @param round_exposure number of digits for exposure (defaults to 0)
#' @param exposure_name Deprecated. Use `exposure_output` instead.
#' @param signif_stars Deprecated. Use `significance` instead.
#'
#' @return Object of class `"rating_table"` and legacy class `"riskfactor"`.
#'
#' @importFrom dplyr full_join
#' @importFrom utils stack
#' @importFrom stats coefficients
#'
#' @export
rating_table <- function(..., model_data = NULL, exposure = TRUE,
                         exposure_output = NULL,
                         exponentiate = TRUE, significance = FALSE,
                         round_exposure = 0, exposure_name = NULL,
                         signif_stars = NULL) {

  mc <- match.call(expand.dots = FALSE)
  models <- list(...)

  if (!missing(exposure_name)) {
    lifecycle::deprecate_warn(
      when = "0.9.0",
      what = "rating_table(exposure_name)",
      with = "rating_table(exposure_output)"
    )
    if (!is.null(exposure_output)) {
      stop("Use only one of 'exposure_output' and deprecated 'exposure_name'.",
           call. = FALSE)
    }
    exposure_output <- exposure_name
  }

  if (!missing(signif_stars)) {
    lifecycle::deprecate_warn(
      when = "0.9.0",
      what = "rating_table(signif_stars)",
      with = "rating_table(significance)"
    )
    if (!missing(significance)) {
      stop("Use only one of 'significance' and deprecated 'signif_stars'.",
           call. = FALSE)
    }
    significance <- signif_stars
  }

  if (!is.logical(significance) || length(significance) != 1 || is.na(significance)) {
    stop("'significance' must be TRUE or FALSE.", call. = FALSE)
  }

  if (length(models) == 0) {
    stop("At least one model must be supplied.", call. = FALSE)
  }

  bad_refinement <- vapply(models, inherits, logical(1), what = "rating_refinement")
  if (any(bad_refinement)) {
    stop(
      "rating_table() only works after refit(). One or more inputs are 'rating_refinement' objects.",
      call. = FALSE
    )
  }

  bad_legacy <- vapply(models, function(x) inherits(x, c("restricted", "smooth")), logical(1))
  if (any(bad_legacy)) {
    stop(
      "rating_table() only works after refit_glm()/refit(). One or more inputs are legacy 'restricted'/'smooth' objects.",
      call. = FALSE
    )
  }

  ok_classes <- vapply(
    models,
    function(x) inherits(x, c("glm", "refitsmooth", "refitrestricted")),
    logical(1)
  )
  if (!all(ok_classes)) {
    stop(
      "All inputs to rating_table() must be glm, refitsmooth or refitrestricted objects.",
      call. = FALSE
    )
  }

  cols <- .rating_table_model_names(models, mc)

  rf_list <- vector("list", length(models))
  exposure_out_nm <- NULL
  model_data_name_out <- NULL

  for (i in seq_along(models)) {
    df <- .rating_table_one_model(
      models[[i]],
      model_data = model_data,
      exposure = exposure,
      exposure_output = exposure_output,
      exponentiate = exponentiate,
      round_exposure = round_exposure
    )

    if (is.null(exposure_out_nm)) {
      exposure_out_nm <- attr(df, "exposure_out")
    }
    if (is.null(model_data_name_out)) {
      model_data_name_out <- attr(df, "model_data_name")
    }

    names(df)[names(df) == "estimate"] <- paste0("est_", cols[i])
    names(df)[names(df) == "pvalues"]  <- paste0("signif_", cols[i])

    rf_list[[i]] <- df
  }

  if (length(rf_list) == 1) {
    rf_fj <- rf_list[[1]]
  } else if (!is.null(exposure_out_nm)) {
    rf_fj <- Reduce(function(d1, d2) {
      dplyr::full_join(d1, d2, by = c("risk_factor", "level", exposure_out_nm))
    }, rf_list)

    keep_cols <- c(
      "risk_factor", "level",
      paste0("est_", cols),
      paste0("signif_", cols),
      exposure_out_nm
    )
    rf_fj <- rf_fj[, intersect(keep_cols, names(rf_fj)), drop = FALSE]

  } else {
    rf_fj <- Reduce(function(d1, d2) {
      dplyr::full_join(d1, d2, by = c("risk_factor", "level"))
    }, rf_list)

    keep_cols <- c(
      "risk_factor", "level",
      paste0("est_", cols),
      paste0("signif_", cols)
    )
    rf_fj <- rf_fj[, intersect(keep_cols, names(rf_fj)), drop = FALSE]
  }

  if (!isTRUE(significance)) {
    drop_cols <- paste0("signif_", cols)
    rf_fj <- rf_fj[, !(names(rf_fj) %in% drop_cols), drop = FALSE]
  }

  model_data_use <- if (!is.null(model_data)) {
    as.data.frame(model_data)
  } else if (!is.null(models[[1]]$data)) {
    as.data.frame(models[[1]]$data)
  } else {
    NULL
  }

  if (!is.null(model_data_use)) {
    lst_order <- lapply(names(model_data_use), function(x) {
      attributes(model_data_use[[x]])$xoriginal
    })
    names(lst_order) <- names(model_data_use)
    lst_order <- lst_order[lengths(lst_order) != 0]

    if (length(lst_order) > 0) {
      df_order <- utils::stack(lst_order)
      names(df_order) <- c("level", "risk_factor")
      df_order$risk_factor <- as.character(df_order$risk_factor)

      df_order <- df_order[df_order$risk_factor %in% unique(rf_fj$risk_factor), , drop = FALSE]
      rf_fj$risk_factor <- as.character(rf_fj$risk_factor)

      uit <- dplyr::full_join(df_order, rf_fj, by = c("risk_factor", "level"))
      rf_fj <- uit[order(match(uit$risk_factor, df_order$risk_factor)), , drop = FALSE]
      rownames(rf_fj) <- NULL

      # Put intercept first
      if ("(Intercept)" %in% rf_fj$risk_factor) {
        intercept_ix <- which(rf_fj$risk_factor == "(Intercept)")
        rf_fj <- rf_fj[c(intercept_ix, setdiff(seq_len(nrow(rf_fj)),
                                               intercept_ix)), , drop = FALSE]
        rownames(rf_fj) <- NULL
      }
    }
  }

  rf_fj_stars <- NULL
  signif_levels <- NULL

  if (isTRUE(significance)) {
    signif_levels <-
      "Significance levels: *** p < 0.001; ** p < 0.01; * p < 0.05; . p < 0.1"

    rf_fj_stars <- rf_fj

    for (i in seq_along(cols)) {
      est_num  <- round(rf_fj_stars[[paste0("est_", cols[i])]], 6)
      est_char <- format(est_num, digits = 6, nsmall = 2)

      stars_char <- rf_fj_stars[[paste0("signif_", cols[i])]]
      stars_char[is.na(stars_char)] <- ""

      rf_fj_stars[[paste0("est_", cols[i])]] <-
        format(paste0(est_char, " ", stars_char), justify = "left")
    }

    drop_cols <- paste0("signif_", cols)
    rf_fj_stars <- rf_fj_stars[, !(names(rf_fj_stars) %in% drop_cols), drop = FALSE]
  }

  structure(
    list(
      df = rf_fj,
      df_stars = rf_fj_stars,
      models = cols,
      exposure = exposure_out_nm,
      model_data = model_data_name_out,
      expon = exponentiate,
      significance = significance,
      signif_stars = significance,
      signif_levels = signif_levels
    ),
    class = c("rating_table", "riskfactor")
  )
}


# -----------------------------------------------------------------------------
# riskfactor print / summary / dataframe
# -----------------------------------------------------------------------------

#' @export
print.riskfactor <- function(x, ...) {
  significance <- isTRUE(x$significance) || isTRUE(x$signif_stars)
  if (significance && !is.null(x$df_stars)) {
    if (!is.null(x$signif_levels)) {
      cat("\033[34m", x$signif_levels, "\033[39m\n", sep = "")
    }
    print(x$df_stars, ...)
  } else {
    print(x$df, ...)
  }
  invisible(x)
}

#' @export
print.rating_table <- print.riskfactor

#' @export
as.data.frame.riskfactor <- function(x, ...) {
  significance <- isTRUE(x$significance) || isTRUE(x$signif_stars)
  if (significance && !is.null(x$df_stars)) {
    df <- x$df_stars
  } else {
    df <- x$df
  }
  as.data.frame(df)
}

#' @export
as.data.frame.rating_table <- as.data.frame.riskfactor

#' @export
summary.riskfactor <- function(object, ...) {
  out <- list(
    models = object$models,
    exposure = object$exposure,
    model_data = object$model_data,
    exponentiate = object$expon,
    significance = isTRUE(object$significance) || isTRUE(object$signif_stars),
    signif_stars = isTRUE(object$significance) || isTRUE(object$signif_stars),
    n_rows = if (!is.null(object$df)) nrow(object$df) else 0L
  )
  class(out) <- c("summary.rating_table", "summary.riskfactor")
  out
}

#' @export
summary.rating_table <- summary.riskfactor

#' @export
print.summary.riskfactor <- function(x, ...) {
  cat("rating_table summary\n\n")
  cat("Models: ", paste(x$models, collapse = ", "), "\n", sep = "")
  cat("Exposure column: ", if (is.null(x$exposure)) "none" else x$exposure, "\n", sep = "")
  cat("Model data: ", if (is.null(x$model_data)) "none" else x$model_data, "\n", sep = "")
  cat("Exponentiate: ", x$exponentiate, "\n", sep = "")
  cat("Significance: ", x$significance, "\n", sep = "")
  cat("Rows: ", x$n_rows, "\n", sep = "")
  invisible(x)
}

#' @export
print.summary.rating_table <- print.summary.riskfactor
