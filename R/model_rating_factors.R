#' @importFrom data.table data.table
#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom stats terms
#' @importFrom utils stack
#'
#' @keywords internal
rating_table_simple <- function(model, model_data = NULL, exposure = NULL,
                                colname = "estimate",
                                exponentiate = TRUE, round_exposure = 0) {

  # Block raw restricted/smooth objects
  if (inherits(model, c("restricted", "smooth"))) {
    stop("Model is restricted/smoothed. Please call refit_glm() first.",
         call. = FALSE)
  }

  # Acceptable classes
  if (!inherits(model, c("glm", "refitsmooth", "refitrestricted"))) {
    stop("Input must be a glm or a refit_glm() object.", call. = FALSE)
  }

  # Exposure requires model_data
  if (!is.null(exposure) && is.null(model_data)) {
    warning("Argument 'exposure' was provided, but 'model_data' is missing. ",
            "Exposure cannot be determined without 'model_data'.")
  }

  # Extract factor levels from glm
  xl <- model$xlevels
  xl_names <- character(0)

  # Init empty df with final column names
  xl_df <- data.frame(risk_factor = character(),
                      level = character(),
                      ind_values = character(),
                      stringsAsFactors = FALSE)

  if (length(xl) > 0) {
    xl_names <- names(xl)
    tmp <- stack(xl)
    tmp[] <- lapply(tmp, as.character)
    names(tmp) <- c("level", "risk_factor")
    tmp$ind_values <- paste0(tmp$risk_factor, tmp$level)
    xl_df <- rbind(xl_df, tmp)
  }

  # Add refit_glm attributes
  if (inherits(model, c("refitsmooth", "refitrestricted"))) {
    x <- if (inherits(model, "refitsmooth")) {
      attr(model, "new_rf")
    } else {
      attr(model, "new_rf_rst")
    }

    x$risk_factor <- as.character(x$risk_factor)
    x$level <- as.character(x$level)
    x$ind_values <- paste0(x$risk_factor, x$level)

    x2 <- x[, c("risk_factor", "level", "ind_values")]
    xl_df <- rbind(xl_df, x2)
    xl_names <- c(xl_names, unique(x$risk_factor))
  }

  model_data_name <- deparse(substitute(model_data))

  xl_names_in  <- intersect(xl_names, names(model_data))
  xl_names_out <- setdiff(xl_names, xl_names_in)

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

      dfexp   <- if (length(listexp) > 0) do.call(rbind, listexp) else NULL

      if (!is.null(dfexp)) {
        dfexp$level <- as.character(dfexp$level)
        xl_df <- dplyr::left_join(xl_df, dfexp, by = c("level", "risk_factor"))
      }
    }
  }

  ret <- coefficients(summary(model))
  ret <- cbind(ind = rownames(ret), data.frame(ret, row.names = NULL))

  all_coefs <- stats::coef(model)

  # Corrigeer mismatch tussen aantal coefs en summary
  if (length(all_coefs) != nrow(ret)) {
    coefs_df <- stack(all_coefs)
    colnames(coefs_df)[colnames(coefs_df) == "values"] <- "Estimate"
    ret <- merge(x = coefs_df, y = ret, by = c("ind", "Estimate"), all.x = TRUE)
  }

  coef <- coefficients(model)
  vals <- stack(coef)

  # gebruik ret[, 5] voor p-values
  vals$pvalues <- as.numeric(ret[, 5])
  vals$pvalues <- ifelse(is.na(vals$pvalues), -9e9, vals$pvalues)
  vals$ind <- as.character(vals$ind)

  new_col_nm0 <- attr(model, "new_col_nm")

  if (inherits(model, c("refitsmooth", "refitrestricted"))) {
    x2 <- x[, c("yhat", "ind_values")]
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
    0, uit$values
  )

  int <- attr(terms(model), "intercept")

  # Intercept behandelen
  intercept_condition <- int == 1 & uit$ind_values == "(Intercept)"
  uit$level[intercept_condition] <- "(Intercept)"
  uit$risk_factor[intercept_condition] <- "(Intercept)"

  # Continue factoren
  level_condition <- is.na(uit$level) & is.na(uit$risk_factor)
  uit$level[level_condition] <- uit$ind_values[level_condition]
  uit$risk_factor[is.na(uit$risk_factor)] <- uit$ind_values[is.na(uit$risk_factor)]

  # Eventueel exponentiëren
  if (isTRUE(exponentiate)) {
    uit$values <- exp(uit$values)
  }

  # Intercept als eerste
  if (int == 1) {
    intercept_ix <- which(uit$risk_factor == "(Intercept)")
    uit <- uit[c(intercept_ix, setdiff(seq_len(nrow(uit)), intercept_ix)), ]
  }

  row.names(uit) <- NULL

  # Kolommen selecteren
  if (!is.null(model_data) && !is.null(exposure) && length(xl_names_in) > 0) {
    selected_columns <- c("risk_factor", "level", "values", exposure, "pvalues")
    selected_columns <- intersect(selected_columns, names(uit))
    uit <- uit[, selected_columns]
    uit[[exposure]] <- round(uit[[exposure]], round_exposure)
  } else {
    selected_columns <- c("risk_factor", "level", "values", "pvalues")
    selected_columns <- intersect(selected_columns, names(uit))
    uit <- uit[, selected_columns]
  }

  names(uit)[names(uit) == "values"] <- colname

  # P-values en stars
  uit$pvalues[uit$pvalues < 0] <- NA
  uit$pvalues <- vapply(uit$pvalues, make_stars, FUN.VALUE = character(1))

  # Opruimen
  uit$risk_factor <- sub("_rst99$", "", uit$risk_factor)

  io <- attr(model, "intercept_only")
  cf <- attr(model, "continuous_factors")
  if (isTRUE(io)) {
    cf$pvalues <- numeric(0)
    names(cf) <- names(uit)
    uit <- rbind(uit, cf)
  }

  uit
}


#' Include reference group in regression output
#'
#' @param model glm object(s) produced by `glm()`
#' @param model_data data.frame used to create glm object(s), this should only
#'   be specified in case the exposure is desired in the output, default value
#'   is NULL.
#' @param exposure column in `model_data` with exposure, default value is NULL
#' @param colname name of column.
#' @param exponentiate logical indicating whether or not to exponentiate the
#'   coefficient estimates. Defaults to TRUE.
#' @param round_exposure number of digits for exposure (defaults to 0)
#'
#' @description `r lifecycle::badge('deprecated')`
#'
#' @export
rating_factors2 <- function(model, model_data = NULL, exposure = NULL,
                            colname = "estimate",
                            exponentiate = TRUE, round_exposure = 0) {
  lifecycle::deprecate_warn("0.8.0", "rating_factors2()", "rating_table2()")
  if (!is.null(substitute(exposure))) {
    exposure <- deparse(substitute(exposure))
  }
  rating_table_simple(model,
                      model_data = model_data,
                      exposure = exposure,
                      colname = colname,
                      exponentiate = exponentiate,
                      round_exposure = round_exposure)
}


#' Include reference group in regression output
#'
#' @description Extract coefficients in terms of the original levels of the
#'   coefficients rather than the coded variables.
#'
#' @param ... glm object(s) produced by `glm()`
#' @param model_data data.frame used to create glm object(s), this should only
#'   be specified in case the exposure is desired in the output, default value
#'   is NULL
#' @param exposure column in `model_data` with exposure, default value is NULL
#' @param exponentiate logical indicating whether or not to exponentiate the
#'   coefficient estimates. Defaults to TRUE.
#' @param signif_stars show significance stars for p-values (defaults to TRUE)
#' @param round_exposure number of digits for exposure (defaults to 0)
#'
#' @details A fitted linear model has coefficients for the contrasts of the
#'   factor terms, usually one less in number than the number of levels. This
#'   function re-expresses the coefficients in the original coding. This
#'   function is adopted from dummy.coef(). Our adoption prints a data.frame as
#'   output. Use rating_factors_() for standard evaluation.
#'
#' @return data.frame
#'
#' @importFrom dplyr full_join
#' @importFrom utils stack
#' @importFrom stats coefficients
#'
#' @author Martin Haringa
#'
#' @examples
#' df <- MTPL2 |>
#' dplyr::mutate(dplyr::across(c(area), as.factor)) |>
#' dplyr::mutate(dplyr::across(c(area), ~biggest_reference(., exposure)))
#'
#' mod1 <- glm(nclaims ~ area + premium, offset = log(exposure),
#' family = poisson(), data = df)
#' mod2 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
#' data = df)
#'
#' rating_table(mod1, mod2, model_data = df, exposure = "exposure")
#'
#' @export
rating_table <- function(..., model_data = NULL, exposure = NULL,
                         exponentiate = TRUE, signif_stars = FALSE,
                         round_exposure = 0) {

  mc <- match.call(expand.dots = FALSE)
  models <- list(...)

  cols <- .rating_table_model_names(models, mc)

  rf_list <- list()
  for (i in seq_along(models)) {
    coef_name <- paste0("m_", i)

    df <- rating_table_simple(models[[i]],
                              model_data,
                              exposure = exposure,
                              exponentiate = exponentiate,
                              round_exposure = round_exposure)

    names(df)[names(df) == "estimate"] <- paste0("est_", cols[i])
    names(df)[names(df) == "pvalues"]  <- paste0("signif_", cols[i])

    rf_list[[coef_name]] <- df
  }

  if (length(rf_list) == 0) {
    rf_fj <- NULL
  } else if (!is.null(model_data) && !is.null(exposure)) {
    rf_fj <- Reduce(function(d1, d2) {
      dplyr::full_join(d1, d2, by = c("risk_factor", "level", exposure))
    }, rf_list)

    keep_cols <- c("risk_factor", "level",
                   paste0("est_", cols),
                   paste0("signif_", cols),
                   exposure)
    rf_fj <- rf_fj[, intersect(keep_cols, names(rf_fj))]

  } else {
    rf_fj <- Reduce(function(d1, d2) {
      dplyr::full_join(d1, d2, by = c("risk_factor", "level"))
    }, rf_list)

    keep_cols <- c("risk_factor", "level",
                   paste0("est_", cols),
                   paste0("signif_", cols))
    rf_fj <- rf_fj[, intersect(keep_cols, names(rf_fj))]
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
      df_order <- stack(lst_order)
      names(df_order) <- c("level", "risk_factor")
      df_order$risk_factor <- as.character(df_order$risk_factor)

      df_order <- df_order[df_order$risk_factor %in% unique(rf_fj$risk_factor),]
      rf_fj$risk_factor <- as.character(rf_fj$risk_factor)

      uit <- dplyr::full_join(df_order, rf_fj, by = c("risk_factor", "level"))

      # sorteer volgens originele volgorde
      rf_fj <- uit[order(match(uit$risk_factor, df_order$risk_factor)), ]
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
    rf_fj_stars <- rf_fj_stars[, !(names(rf_fj_stars) %in% drop_cols),
                               drop = FALSE]
  }

  return(structure(list(df = rf_fj,
                        df_stars = rf_fj_stars,
                        models = cols,
                        exposure = exposure,
                        model_data = deparse(substitute(model_data)),
                        expon = exponentiate,
                        signif_stars = signif_stars,
                        signif_levels = signif_levels),
                   class = "riskfactor"))
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
  rating_table(...,
               model_data = model_data,
               exposure = exposure,
               exponentiate = exponentiate,
               signif_stars = signif_stars,
               round_exposure = round_exposure)
}



#' @export
print.riskfactor <- function(x, ...) {
  if (isTRUE(x$signif_stars) && !is.null(x$df_stars)) {
    # Print uitleg bij de significantieniveaus
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

  if (isTRUE(x$signif_stars)) {
    df <- x$df_stars
  } else {
    df <- x$df
  }

  as.data.frame(df)
}


#' Plot risk factor effects from `rating_table()` results
#'
#' @description Create a ggplot visualization of a `riskfactor` object produced
#'   by [rating_table()]. Estimates are plotted per risk factor, with optional
#'   exposure bars and significance stars if available.
#'
#' @param object A `riskfactor` object returned by [rating_table()].
#' @param risk_factors Character vector specifying which risk factors to plot.
#'   Defaults to all risk factors.
#' @param ncol Number of columns in the patchwork layout. Default is 1.
#' @param labels Logical; if `TRUE`, show exposure values as labels on the bars.
#'   Default is `TRUE`.
#' @param dec.mark Character; decimal separator, either `","` (default) or `"."`.
#'   Controls formatting of axis labels.
#' @param ylab Character; label for the y-axis. Default is `"rate"`.
#' @param fill Fill color for the exposure bars. If `NULL`, derived from `color`.
#' @param color Line color for model estimates. Defaults to `"skyblue"`.
#' @param linetype Logical; if `TRUE`, use different line types for models.
#'   Default is `FALSE`.
#' @param ... Additional arguments passed to ggplot2 layers.
#'
#' @return A `ggplot`/`patchwork` object.
#'
#' @seealso [rating_table()]
#'
#' @author Martin Haringa
#'
#' @examples
#' library(dplyr)
#' df <- MTPL2 |>
#'   mutate(area = as.factor(area)) |>
#'   mutate(area = biggest_reference(area, exposure))
#'
#' mod1 <- glm(nclaims ~ area + premium, offset = log(exposure),
#'             family = poisson(), data = df)
#' mod2 <- glm(nclaims ~ area, offset = log(exposure),
#'             family = poisson(), data = df)
#'
#' x <- rating_table(mod1, mod2, model_data = df, exposure = "exposure")
#' autoplot(x)
#'
#' @import ggplot2
#' @importFrom patchwork wrap_plots
#' @export
autoplot.riskfactor <- function(object, risk_factors = NULL, ncol = 1,
                                labels = TRUE,
                                dec.mark = ",",
                                ylab = "rate",
                                fill = "lightskyblue",
                                color = "white",
                                linetype = FALSE, ...) {

  df <- object$df
  models <- object$models
  models_nm <- paste0("est_", models)
  exposure_nm <- object$exposure
  expon <- object$expon

  # remove reference categories
  df <- df[df$risk_factor != df$level, ]
  df_long <- data.table::melt(
    data.table::setDT(df),
    id.vars = names(df)[!names(df) %in% models_nm],
    measure.vars = models_nm,
    variable.name = "model",
    value.name = "est"
  ) |> data.table::setDF()

  df_long$model <- gsub("^est_", "", df_long$model)

  # exponentiate if necessary
  if (!isTRUE(expon)) {
    df_long$est <- exp(df_long$est)
  }

  # separator function
  sep_fn <- if (dec.mark == ",") {
    function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  } else {
    function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)
  }

  # risk factor selection
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
  for (i in seq_along(rf_names)) {
    df1 <- df_long[df_long$risk_factor == rf_names[i], ]
    df1$level <- factor(df1$level, levels = unique(df1$level))

    if (!is.null(exposure_nm)) {
      df1$s_axis_scale <- df1[[exposure_nm]] / max(df1[[exposure_nm]], na.rm = TRUE) *
        max(df1$est, na.rm = TRUE)
      df1$y_print <- round(df1[[exposure_nm]], 0)
      df1 <- df1[!is.na(df1$est), ]
      df1_bar <- unique(df1[, c("risk_factor", "level", exposure_nm,
                                "s_axis_scale", "y_print")])
    }

    p <- ggplot2::ggplot(data = df1) +
      ggplot2::theme_minimal()

    if (is.null(exposure_nm)) {
      p <- p + ggplot2::scale_y_continuous(labels = sep_fn,
                                           limits = c(0, NA),
                                           expand = ggplot2::expansion(mult = c(0, 0.02)))
    } else {
      p <- p +
        ggplot2::geom_bar(
          data = df1_bar,
          mapping = ggplot2::aes(x = .data[["level"]],
                                 y = .data[["s_axis_scale"]]),
          stat = "identity", color = color, fill = fill, alpha = .7
        ) +
        ggplot2::scale_y_continuous(
          labels = sep_fn,
          limits = c(0, NA),
          expand = ggplot2::expansion(mult = c(0, 0.02)),
          sec.axis = ggplot2::sec_axis(
            ~ . * max(df1_bar[[exposure_nm]]) / max(df1$est),
            name = exposure_nm, labels = sep_fn
          )
        )
    }

    p <- p + ggplot2::geom_point(
      ggplot2::aes(x = .data[["level"]], y = .data[["est"]],
                   group = .data[["model"]], color = .data[["model"]])
    )

    if (length(models) == 1) {
      p <- p + ggplot2::theme(legend.position = "none")
    }

    if (isTRUE(linetype)) {
      p <- p + ggplot2::geom_line(
        ggplot2::aes(x = .data[["level"]], y = .data[["est"]],
                     group = .data[["model"]], color = .data[["model"]],
                     linetype = .data[["model"]])
      )
    } else {
      p <- p + ggplot2::geom_line(
        ggplot2::aes(x = .data[["level"]], y = .data[["est"]],
                     group = .data[["model"]], color = .data[["model"]])
      )
    }

    if (isTRUE(labels) && !is.null(exposure_nm)) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(x = .data[["level"]], y = .data[["s_axis_scale"]],
                     label = sep_fn(.data[["y_print"]])),
        vjust = "inward", size = 3
      )
    }

    p <- p + ggplot2::labs(x = rf_names[i], y = ylab) +
      ggplot2::theme(legend.title = ggplot2::element_blank())

    fig_list[[paste0("p", i)]] <- p
  }

  patchwork::wrap_plots(fig_list, ncol = ncol)
}
