# -----------------------------------------------------------------------------
# Rating table helpers
# -----------------------------------------------------------------------------

#' @importFrom data.table data.table
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


#' @importFrom data.table data.table
#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom stats terms
#' @importFrom utils stack
#'
#' @keywords internal
rating_table_simple <- function(model, model_data = NULL, exposure = NULL,
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

  # ---------------------------------------------------------------------------
  # Exposure requires model_data
  # ---------------------------------------------------------------------------
  if (!is.null(exposure) && is.null(model_data)) {
    warning(
      "Argument 'exposure' was provided, but 'model_data' is missing. ",
      "Exposure cannot be determined without 'model_data'."
    )
  }

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
  x <- NULL

  if (inherits(model, c("refitsmooth", "refitrestricted"))) {
    x <- .collect_refit_new_rf(model)

    if (!is.null(x) && nrow(x) > 0) {
      x$risk_factor <- as.character(x$risk_factor)
      x$level <- as.character(x$level)
      x$ind_values <- paste0(x$risk_factor, x$level)

      x2 <- x[, c("risk_factor", "level", "ind_values"), drop = FALSE]
      xl_df <- rbind(xl_df, x2)
      xl_names <- c(xl_names, unique(x$risk_factor))
    }
  }

  # ---------------------------------------------------------------------------
  # Exposure by factor level
  # ---------------------------------------------------------------------------
  model_data_name <- deparse(substitute(model_data))

  if (!is.null(model_data)) {
    xl_names_in  <- intersect(xl_names, names(model_data))
    xl_names_out <- setdiff(xl_names, xl_names_in)
  } else {
    xl_names_in  <- character(0)
    xl_names_out <- xl_names
  }

  if (!is.null(model_data) && !is.null(exposure)) {

    if (!exposure %in% names(model_data)) {
      stop(exposure, " is unknown in ", model_data_name, call. = FALSE)
    }

    if (!is.numeric(model_data[[exposure]])) {
      stop(exposure, " should be numeric", call. = FALSE)
    }

    if (length(xl_names_out) > 0) {
      message(paste(xl_names_out, collapse = ", "), " not in ", model_data_name)
    }

    if (length(xl_names_in) > 0) {
      model_data <- as.data.frame(model_data)

      listexp <- lapply(xl_names_in, function(v) {
        exposure_by_factor(v, model_data, exposure)
      })

      dfexp <- if (length(listexp) > 0) do.call(rbind, listexp) else NULL

      if (!is.null(dfexp)) {
        dfexp$level <- as.character(dfexp$level)
        xl_df <- dplyr::left_join(xl_df, dfexp, by = c("level", "risk_factor"))
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

  if (inherits(model, c("refitsmooth", "refitrestricted")) && !is.null(x) && nrow(x) > 0) {
    x_rf_names <- unique(x$risk_factor)
    new_col_nm0 <- unique(c(new_col_nm0, x_rf_names))
  }

  if (inherits(model, c("refitsmooth", "refitrestricted")) && !is.null(x) && nrow(x) > 0) {
    x2 <- x[, c("yhat", "ind_values"), drop = FALSE]
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
  if (!is.null(model_data) && !is.null(exposure) && length(xl_names_in) > 0) {
    selected_columns <- c("risk_factor", "level", "values", exposure, "pvalues")
    selected_columns <- intersect(selected_columns, names(uit))
    uit <- uit[, selected_columns, drop = FALSE]
    uit[[exposure]] <- round(uit[[exposure]], round_exposure)
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

  uit
}


#' Include reference group in regression output
#'
#' @param model glm object produced by `glm()`
#' @param model_data data.frame used to create glm object
#' @param exposure column in `model_data` with exposure
#' @param colname name of coefficient column
#' @param exponentiate logical indicating whether or not to exponentiate the
#'   coefficient estimates. Defaults to TRUE.
#' @param round_exposure number of digits for exposure (defaults to 0)
#'
#' @description `r lifecycle::badge('deprecated')`
#'
#' Legacy interface. Prefer [rating_table()] for fitted models in the new
#' workflow, but this function remains available.
#'
#' @export
rating_factors2 <- function(model, model_data = NULL, exposure = NULL,
                            colname = "estimate",
                            exponentiate = TRUE, round_exposure = 0) {
  lifecycle::deprecate_warn("0.8.0", "rating_factors2()", "rating_table2()")

  if (!is.null(substitute(exposure))) {
    exposure <- deparse(substitute(exposure))
  }

  rating_table_simple(
    model,
    model_data = model_data,
    exposure = exposure,
    colname = colname,
    exponentiate = exponentiate,
    round_exposure = round_exposure
  )
}


#' Include reference group in regression output
#'
#' @description Extract coefficients in terms of the original levels of the
#' coefficients rather than the coded variables.
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
#' @param model_data data.frame used to create glm object(s), this should only
#'   be specified in case the exposure is desired in the output, default value
#'   is NULL
#' @param exposure column in `model_data` with exposure, default value is NULL
#' @param exponentiate logical indicating whether or not to exponentiate the
#'   coefficient estimates. Defaults to TRUE.
#' @param signif_stars show significance stars for p-values (defaults to FALSE)
#' @param round_exposure number of digits for exposure (defaults to 0)
#'
#' @return Object of class `riskfactor`
#'
#' @importFrom dplyr full_join
#' @importFrom utils stack
#' @importFrom stats coefficients
#'
#' @export
rating_table <- function(..., model_data = NULL, exposure = NULL,
                         exponentiate = TRUE, signif_stars = FALSE,
                         round_exposure = 0) {

  mc <- match.call(expand.dots = FALSE)
  models <- list(...)

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

  # This should work for glm as well
  ok_classes <- vapply(models, function(x) inherits(x, c("glm", "refitsmooth", "refitrestricted")), logical(1))
  if (!all(ok_classes)) {
    stop(
      "All inputs to rating_table() must be glm, refitsmooth or refitrestricted objects.",
      call. = FALSE
    )
  }

  cols <- .rating_table_model_names(models, mc)

  rf_list <- vector("list", length(models))

  for (i in seq_along(models)) {
    df <- rating_table_simple(
      models[[i]],
      model_data = model_data,
      exposure = exposure,
      exponentiate = exponentiate,
      round_exposure = round_exposure
    )

    names(df)[names(df) == "estimate"] <- paste0("est_", cols[i])
    names(df)[names(df) == "pvalues"]  <- paste0("signif_", cols[i])

    rf_list[[i]] <- df
  }

  if (length(rf_list) == 1) {
    rf_fj <- rf_list[[1]]
  } else if (!is.null(model_data) && !is.null(exposure)) {
    rf_fj <- Reduce(function(d1, d2) {
      dplyr::full_join(d1, d2, by = c("risk_factor", "level", exposure))
    }, rf_list)

    keep_cols <- c(
      "risk_factor", "level",
      paste0("est_", cols),
      paste0("signif_", cols),
      exposure
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

  if (!isTRUE(signif_stars)) {
    drop_cols <- paste0("signif_", cols)
    rf_fj <- rf_fj[, !(names(rf_fj) %in% drop_cols), drop = FALSE]
  }

  if (!is.null(model_data)) {
    lst_order <- lapply(names(model_data), function(x) {
      attributes(model_data[[x]])$xoriginal
    })
    names(lst_order) <- names(model_data)
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
    }
  }

  rf_fj_stars <- NULL
  signif_levels <- NULL

  if (isTRUE(signif_stars)) {
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
      exposure = exposure,
      model_data = deparse(substitute(model_data)),
      expon = exponentiate,
      signif_stars = signif_stars,
      signif_levels = signif_levels
    ),
    class = "riskfactor"
  )
}


#' @rdname rating_table
#' @export
rating_factors <- function(..., model_data = NULL, exposure = NULL,
                           signif_stars = FALSE,
                           exponentiate = TRUE, round_exposure = 0) {
  lifecycle::deprecate_warn("0.8.0", "rating_factors()", "rating_table()")

  if (!is.null(substitute(exposure))) {
    exposure <- deparse(substitute(exposure))
  }

  rating_table(
    ...,
    model_data = model_data,
    exposure = exposure,
    exponentiate = exponentiate,
    signif_stars = signif_stars,
    round_exposure = round_exposure
  )
}


# -----------------------------------------------------------------------------
# riskfactor print / summary / dataframe
# -----------------------------------------------------------------------------

#' @export
print.riskfactor <- function(x, ...) {
  if (isTRUE(x$signif_stars) && !is.null(x$df_stars)) {
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
as.data.frame.riskfactor <- function(x, ...) {
  if (isTRUE(x$signif_stars) && !is.null(x$df_stars)) {
    df <- x$df_stars
  } else {
    df <- x$df
  }
  as.data.frame(df)
}

#' @export
summary.riskfactor <- function(object, ...) {
  out <- list(
    models = object$models,
    exposure = object$exposure,
    model_data = object$model_data,
    exponentiate = object$expon,
    signif_stars = object$signif_stars,
    n_rows = if (!is.null(object$df)) nrow(object$df) else 0L
  )
  class(out) <- "summary.riskfactor"
  out
}

#' @export
print.summary.riskfactor <- function(x, ...) {
  cat("riskfactor summary\n\n")
  cat("Models: ", paste(x$models, collapse = ", "), "\n", sep = "")
  cat("Exposure column: ", x$exposure %||% "none", "\n", sep = "")
  cat("Model data: ", x$model_data, "\n", sep = "")
  cat("Exponentiate: ", x$exponentiate, "\n", sep = "")
  cat("Significance stars: ", x$signif_stars, "\n", sep = "")
  cat("Rows: ", x$n_rows, "\n", sep = "")
  invisible(x)
}


#' Plot risk factor effects from `rating_table()` results
#'
#' @description
#' Create a ggplot visualisation of a `riskfactor` object produced by
#' [rating_table()]. Estimates are plotted per risk factor, with optional
#' exposure bars and, optionally, an additional univariate line.
#'
#' When `univariate_scale = "reference"`, the univariate line is scaled to the
#' model reference group. The reference group is determined as the level with
#' model coefficient equal to 1. If no level is exactly equal to 1, the level
#' with coefficient closest to 1 is used.
#'
#' @param object A `riskfactor` object returned by [rating_table()].
#' @param risk_factors Character vector specifying which risk factors to plot.
#'   Defaults to all risk factors.
#' @param ncol Number of columns in the patchwork layout. Default is 1.
#' @param labels Logical; if `TRUE`, show exposure values as labels on the bars.
#'   Default is `TRUE`.
#' @param dec.mark Character; decimal separator, either `","` (default) or `"."`.
#' @param ylab Character; label for the y-axis. Default is `"Relativity"`.
#' @param fill Fill color for the exposure bars. If `NULL`, taken from the
#'   internal palette.
#' @param color Optional override for model line colors. If `NULL`, colors are
#'   taken from the internal discrete palette.
#' @param linetype Logical; if `TRUE`, use different line types for models.
#'   Default is `FALSE`.
#' @param univariate Optional `univariate` object returned by
#'   [univariate_summary()]. If supplied, the selected univariate statistic is
#'   added as an extra line.
#' @param univariate_var Character; statistic from `univariate` to plot.
#'   Default is `"risk_premium"`.
#' @param univariate_name Character; legend label for the univariate line.
#'   Default is `"Univariate"`.
#' @param univariate_color Optional override for the univariate line color.
#'   If `NULL`, the internal risk premium color is used.
#' @param univariate_scale Character; scaling applied to the univariate line.
#'   One of `"reference"` (default) or `"mean"`.
#' @param rotate_angle Numeric value for angle of labels on the x-axis (degrees).
#' @param custom_theme List with customised theme options.
#' @param remove_underscores Logical; remove underscores from labels.
#' @param ... Additional arguments passed to ggplot2 layers.
#'
#' @return A `ggplot`/`patchwork` object.
#'
#' @import ggplot2
#' @importFrom patchwork wrap_plots
#' @export
autoplot.riskfactor <- function(object, risk_factors = NULL, ncol = 1,
                                labels = TRUE,
                                dec.mark = ",",
                                ylab = "Relativity",
                                fill = NULL,
                                color = NULL,
                                linetype = FALSE,
                                univariate = NULL,
                                univariate_var = "risk_premium",
                                univariate_name = "Univariate",
                                univariate_color = NULL,
                                univariate_scale = c("reference", "mean"),
                                rotate_angle = NULL,
                                custom_theme = NULL,
                                remove_underscores = FALSE,
                                ...) {

  univariate_scale <- match.arg(univariate_scale)

  df_full <- object$df
  models <- object$models
  models_nm <- paste0("est_", models)
  exposure_nm <- object$exposure
  expon <- object$expon

  plot_palette <- function() {
    list(
      frequency        = "#2C7FB8",
      average_severity = "#41AB5D",
      risk_premium     = "#F28E2B",
      loss_ratio       = "#8C6BB1",
      average_premium  = "#2CB1A1",
      bg_bar           = "#E6E6E6",
      discrete = c(
        "#2C7FB8",
        "#41AB5D",
        "#8C6BB1",
        "#2CB1A1",
        "#E15759",
        "#B6992D",
        "#6B6B6B"
      )
    )
  }

  plot_grid_theme <- function() {
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.major = ggplot2::element_line(color = "#F2F2F2", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border     = ggplot2::element_blank(),
      axis.text.y.right  = ggplot2::element_text(color = "#9E9E9E", size = 8),
      axis.title.y.right = ggplot2::element_text(color = "#9E9E9E", size = 9),
      axis.title.y       = ggplot2::element_text(size = 10)
    )
  }

  discrete_palette_values <- function(n) {
    pal <- plot_palette()$discrete
    rep_len(pal, n)
  }

  get_reference_level <- function(df_full, rf_name, model_name = NULL, expon = TRUE) {
    df_ref <- df_full[df_full$risk_factor == rf_name, , drop = FALSE]
    est_cols <- grep("^est_", names(df_ref), value = TRUE)

    if (length(est_cols) == 0 || nrow(df_ref) == 0) {
      return(NULL)
    }

    if (!is.null(model_name)) {
      est_col <- paste0("est_", model_name)
      if (!est_col %in% est_cols) {
        est_col <- est_cols[1]
      }
    } else {
      est_col <- est_cols[1]
    }

    est_vals <- df_ref[[est_col]]

    if (!isTRUE(expon)) {
      est_vals <- exp(est_vals)
    }

    idx <- which(is.finite(est_vals) & abs(est_vals - 1) < 1e-8)

    if (length(idx) == 0) {
      idx <- which.min(abs(est_vals - 1))
    }

    if (length(idx) == 0 || is.infinite(idx) || is.na(idx)) {
      return(NULL)
    }

    as.character(df_ref$level[idx[1]])
  }

  pal <- plot_palette()
  grid_theme <- plot_grid_theme()

  final_fill <- if (is.null(fill)) pal$bg_bar else fill
  final_uni_color <- if (is.null(univariate_color)) pal$risk_premium else univariate_color

  # remove reference categories from plotted model lines
  df <- df_full[df_full$risk_factor != df_full$level, , drop = FALSE]

  df_long <- data.table::melt(
    data.table::setDT(df),
    id.vars = names(df)[!names(df) %in% models_nm],
    measure.vars = models_nm,
    variable.name = "model",
    value.name = "est"
  ) |>
    data.table::setDF()

  df_long$model <- gsub("^est_", "", df_long$model)

  if (!isTRUE(expon)) {
    df_long$est <- exp(df_long$est)
  }

  uni_df <- NULL

  if (!is.null(univariate)) {
    xvar_uni <- attr(univariate, "xvar")
    if (length(xvar_uni) > 1) {
      xvar_uni <- xvar_uni[1]
    }

    if (!univariate_var %in% names(univariate)) {
      stop("`univariate_var` not found in `univariate` object.", call. = FALSE)
    }

    uni_df <- as.data.frame(univariate)

    if (!xvar_uni %in% names(uni_df)) {
      stop("The selected `univariate` object does not contain the expected x variable.",
           call. = FALSE)
    }

    names(uni_df)[names(uni_df) == xvar_uni] <- "level"

    if (!"risk_factor" %in% names(uni_df)) {
      uni_df$risk_factor <- xvar_uni
    }

    uni_df$level <- as.character(uni_df$level)

    ref_levels <- vapply(
      unique(uni_df$risk_factor),
      function(rf) {
        ref <- get_reference_level(df_full, rf_name = rf, expon = expon)
        if (is.null(ref)) NA_character_ else ref
      },
      character(1)
    )

    ref_lookup <- data.frame(
      risk_factor = names(ref_levels),
      ref_level = unname(ref_levels),
      stringsAsFactors = FALSE
    )

    uni_df <- uni_df |>
      dplyr::left_join(ref_lookup, by = "risk_factor") |>
      dplyr::group_by(.data[["risk_factor"]]) |>
      dplyr::mutate(
        est = dplyr::case_when(
          univariate_scale == "mean" ~ .data[[univariate_var]] /
            mean(.data[[univariate_var]], na.rm = TRUE),
          univariate_scale == "reference" &
            !is.na(.data[["ref_level"]]) ~ .data[[univariate_var]] /
            .data[[univariate_var]][.data[["level"]] == .data[["ref_level"]]][1],
          univariate_scale == "reference" &
            is.na(.data[["ref_level"]]) ~ .data[[univariate_var]] /
            mean(.data[[univariate_var]], na.rm = TRUE)
        ),
        model = univariate_name
      ) |>
      dplyr::ungroup() |>
      dplyr::select("risk_factor", "level", "model", "est")
  }

  sep_fn <- if (dec.mark == ",") {
    function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  } else {
    function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)
  }

  if (is.null(risk_factors)) {
    rf_names <- unique(df$risk_factor)
  } else {
    rf_diff <- setdiff(risk_factors, unique(df$risk_factor))
    if (length(rf_diff) > 0) {
      stop(paste(rf_diff, collapse = ", "), " unknown risk_factor(s)", call. = FALSE)
    }
    rf_names <- risk_factors
  }

  fig_list <- list()
  missing_exposure_rf <- character(0)

  for (i in seq_along(rf_names)) {
    rf_i <- rf_names[i]

    df1 <- df_long[df_long$risk_factor == rf_i, , drop = FALSE]
    df1$level <- factor(df1$level, levels = unique(df1$level))

    if (!is.null(uni_df)) {
      uni1 <- uni_df[uni_df$risk_factor == rf_i, , drop = FALSE]

      if (nrow(uni1) > 0) {
        uni1$level <- factor(uni1$level, levels = levels(df1$level))
        df1 <- dplyr::bind_rows(df1, uni1)
      }
    }

    df1 <- df1[!is.na(df1$est), , drop = FALSE]

    df1_bar <- NULL
    has_valid_exposure <- FALSE

    if (!is.null(exposure_nm) && exposure_nm %in% names(df_long)) {
      df1_bar <- unique(
        df_long[df_long$risk_factor == rf_i,
                c("risk_factor", "level", exposure_nm), drop = FALSE]
      )

      df1_bar$level <- factor(df1_bar$level, levels = levels(df1$level))

      has_valid_exposure <- nrow(df1_bar) > 0 &&
        any(!is.na(df1_bar[[exposure_nm]]))

      if (has_valid_exposure) {
        df1_bar <- df1_bar[!is.na(df1_bar[[exposure_nm]]), , drop = FALSE]

        max_exposure <- max(df1_bar[[exposure_nm]], na.rm = TRUE)
        max_est <- max(df1$est, na.rm = TRUE)
        exposure_scale <- max_exposure / max_est

        df1_bar$s_axis_scale <- df1_bar[[exposure_nm]] / max_exposure * max_est
        df1_bar$y_print <- round(df1_bar[[exposure_nm]], 0)
      } else {
        missing_exposure_rf <- c(missing_exposure_rf, rf_i)
      }
    }

    model_levels <- unique(df1$model)

    model_cols <- discrete_palette_values(sum(model_levels != univariate_name))
    names(model_cols) <- model_levels[model_levels != univariate_name]

    if (!is.null(color)) {
      model_cols[] <- color
    }

    if (univariate_name %in% model_levels) {
      model_cols <- c(model_cols, setNames(final_uni_color, univariate_name))
    }

    p <- ggplot2::ggplot(data = df1) +
      ggplot2::theme_minimal() +
      grid_theme

    if (!has_valid_exposure) {
      p <- p + ggplot2::scale_y_continuous(
        labels = sep_fn,
        limits = c(0, NA),
        expand = ggplot2::expansion(mult = c(0, 0.06))
      )
    } else {
      p <- p +
        ggplot2::geom_bar(
          data = df1_bar,
          mapping = ggplot2::aes(
            x = .data[["level"]],
            y = .data[["s_axis_scale"]]
          ),
          stat = "identity",
          color = "white",
          fill = final_fill,
          alpha = 0.9
        ) +
        ggplot2::scale_y_continuous(
          labels = sep_fn,
          limits = c(0, NA),
          expand = ggplot2::expansion(mult = c(0, 0.06)),
          sec.axis = ggplot2::sec_axis(
            ~ . * exposure_scale,
            name = exposure_nm,
            labels = sep_fn
          )
        )
    }

    if (isTRUE(linetype)) {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(
            x = .data[["level"]],
            y = .data[["est"]],
            group = .data[["model"]],
            color = .data[["model"]],
            linetype = .data[["model"]]
          ),
          linewidth = 0.8
        )
    } else {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(
            x = .data[["level"]],
            y = .data[["est"]],
            group = .data[["model"]],
            color = .data[["model"]]
          ),
          linewidth = 0.8
        )
    }

    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(
          x = .data[["level"]],
          y = .data[["est"]],
          group = .data[["model"]],
          color = .data[["model"]]
        ),
        shape = 21,
        stroke = 0.7,
        fill = "white",
        size = 2.2
      )

    if (isTRUE(labels) && isTRUE(has_valid_exposure)) {
      p <- p +
        ggplot2::geom_text(
          data = df1_bar,
          ggplot2::aes(
            x = .data[["level"]],
            y = .data[["s_axis_scale"]],
            label = sep_fn(.data[["y_print"]])
          ),
          vjust = "inward",
          size = 3,
          color = "#6B6B6B"
        )
    }

    p <- p +
      ggplot2::scale_color_manual(values = model_cols) +
      ggplot2::labs(
        x = if (remove_underscores) gsub("_", " ", rf_i) else rf_i,
        y = ylab
      ) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank()
      )

    if (length(unique(df1$model)) == 1) {
      p <- p + ggplot2::theme(legend.position = "none")
    }

    if (!is.null(rotate_angle)) {
      p <- p +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = rotate_angle, hjust = 1)
        )
    }

    if (!is.null(custom_theme)) {
      p <- p + do.call(ggplot2::theme, custom_theme)
    }

    fig_list[[paste0("p", i)]] <- p
  }

  missing_exposure_rf <- unique(missing_exposure_rf)
  if (length(missing_exposure_rf) > 0 && !is.null(exposure_nm)) {
    message(
      "No valid exposure available for risk factor(s): ",
      paste(missing_exposure_rf, collapse = ", "),
      ". Plotting without exposure bars."
    )
  }

  patchwork::wrap_plots(fig_list, ncol = ncol, guides = "collect")
}

