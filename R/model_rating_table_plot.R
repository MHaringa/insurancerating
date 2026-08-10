#' Compare fitted risk-factor effects graphically
#'
#' @description
#' Plot the coefficients or relativities stored in a [rating_table()] object by
#' risk factor. Multiple fitted models can be compared, exposure can be shown as
#' background bars, and observed portfolio experience attached with
#' [add_portfolio_experience()] can be added as a separate line.
#'
#' @details
#' ## Plot contents
#'
#' One panel is produced for each selected risk factor. Model effects use the
#' primary y-axis. When exposure is available, bars are rescaled to the plotting
#' range and the original exposure scale is shown on the secondary y-axis.
#' Panel and level order follow the input [rating_table()] object. This keeps
#' the reference level and any explicit actuarial review order consistent
#' between the data frame, [as_gt()] and the plot.
#'
#' Observed experience is plotted only after it has been attached with
#' [add_portfolio_experience()]. The selected `metric` is converted to the
#' relative scale recorded in that object, using either the model reference
#' level or the portfolio mean.
#'
#' ## Actuarial interpretation
#'
#' The plot supports comparison of fitted tariff effects, portfolio volume and
#' unadjusted observed experience. Differences between the observed and modelled
#' lines may indicate portfolio-mix effects, sparse levels, model smoothing or
#' genuine lack of fit. The chart does not separate these explanations and
#' should be reviewed together with claim counts, residual diagnostics and
#' stability across periods.
#'
#' When models are compared, the analyst should ensure that response
#' definitions, link functions and relativity scales are sufficiently
#' comparable. Exposure bars provide volume context but are not confidence
#' intervals.
#'
#' @param object A `"rating_table"` object returned by [rating_table()].
#' @param risk_factors Optional character vector specifying the risk factors to
#'   plot. If `NULL`, all available risk factors are shown.
#' @param metric Optional character string. Observed-experience metric to plot
#'   when observed experience has been attached with
#'   [add_portfolio_experience()]. Common choices are `"frequency"`,
#'   `"severity"`/`"average_severity"` and `"risk_premium"`.
#' @param ncol Positive integer specifying the number of columns in the
#'   patchwork layout.
#' @param legend_position Character string specifying the legend position.
#'   The default, `"auto"`, hides the legend when only one fitted model is
#'   shown and no observed-experience line is present. It places the legend on
#'   the right when multiple fitted models or an observed-experience comparison
#'   are shown. Use `"right"`, `"bottom"`, `"top"`, `"left"` or `"none"` to
#'   override this behaviour.
#' @param show_exposure_labels Logical. If `TRUE`, print exposure values on the
#'   background bars.
#' @param decimal_mark Character string, either `","` or `"."`, controlling
#'   number labels.
#' @param y_label Character string for the primary y-axis.
#' @param bar_fill Optional colour for exposure bars. If `NULL`, the package
#'   palette is used.
#' @param model_color Optional single colour overriding the model-line palette.
#' @param use_linetype Logical. If `TRUE`, distinguish fitted models by line
#'   type as well as colour.
#' @param abbreviate_labels Logical. If `TRUE`, long risk-factor level labels
#'   are shortened to `label_width` characters. A shortened label ends in one
#'   period; for example, `"Bouwnijverheid"` becomes `"Bouwn."` when
#'   `label_width = 6`. Only the displayed axis labels are changed.
#' @param label_width Positive whole number of at least 2. Maximum number of
#'   characters in automatically shortened level labels.
#' @param label_abbreviations Optional named character vector with explicit
#'   display labels, for example
#'   `c("Bouwnijverheid" = "Bouwn.", "Onroerend goed" = "Onr. goed")`.
#'   Explicit labels take precedence over automatic shortening.
#' @param rotate_angle Optional numeric angle for risk-factor level labels.
#' @param custom_theme Optional named list passed to [ggplot2::theme()].
#' @param remove_underscores Logical. If `TRUE`, replace underscores with spaces
#'   in risk-factor axis labels.
#' @param labels Deprecated alias for `show_exposure_labels`.
#' @param dec.mark Deprecated alias for `decimal_mark`.
#' @param ylab Deprecated alias for `y_label`.
#' @param fill Deprecated alias for `bar_fill`.
#' @param color Deprecated alias for `model_color`.
#' @param linetype Deprecated alias for `use_linetype`.
#' @param ... Additional arguments reserved for method compatibility.
#'
#' @return A `patchwork` object containing one `ggplot2` panel per selected risk
#' factor.
#'
#' @author Martin Haringa
#'
#' @seealso [rating_table()], [add_portfolio_experience()],
#'   [factor_analysis()], [as_gt.rating_table()]
#'
#' @examples
#' portfolio <- MTPL
#' portfolio$zip <- as.factor(portfolio$zip)
#'
#' frequency <- glm(
#'   nclaims ~ bm + zip + offset(log(exposure)),
#'   family = poisson(),
#'   data = portfolio
#' )
#'
#' effects <- rating_table(
#'   frequency,
#'   model_data = portfolio,
#'   exposure = "exposure"
#' )
#'
#' autoplot(effects, risk_factors = "zip", show_exposure_labels = FALSE)
#'
#' @import ggplot2
#' @importFrom patchwork wrap_plots
#' @export
autoplot.rating_table <- function(object,
                                  risk_factors = NULL,
                                  metric = NULL,
                                  ncol = 1,
                                  legend_position = c(
                                    "auto", "right", "bottom", "top", "left",
                                    "none"
                                  ),
                                  show_exposure_labels = TRUE,
                                  decimal_mark = ",",
                                  y_label = "Relativity",
                                  bar_fill = NULL,
                                  model_color = NULL,
                                  use_linetype = FALSE,
                                  abbreviate_labels = TRUE,
                                  label_width = 20,
                                  label_abbreviations = NULL,
                                  rotate_angle = NULL,
                                  custom_theme = NULL,
                                  remove_underscores = FALSE,
                                  labels = NULL,
                                  dec.mark = NULL,
                                  ylab = NULL,
                                  fill = NULL,
                                  color = NULL,
                                  linetype = NULL,
                                  ...) {

  legend_position <- match.arg(legend_position)

  old_args <- resolve_autoplot_rating_table_args(
    show_exposure_labels = show_exposure_labels,
    decimal_mark = decimal_mark,
    y_label = y_label,
    bar_fill = bar_fill,
    model_color = model_color,
    use_linetype = use_linetype,
    show_exposure_labels_supplied = !missing(show_exposure_labels),
    decimal_mark_supplied = !missing(decimal_mark),
    y_label_supplied = !missing(y_label),
    bar_fill_supplied = !missing(bar_fill),
    model_color_supplied = !missing(model_color),
    use_linetype_supplied = !missing(use_linetype),
    labels = labels,
    dec.mark = dec.mark,
    ylab = ylab,
    fill = fill,
    color = color,
    linetype = linetype
  )
  show_exposure_labels <- old_args$show_exposure_labels
  decimal_mark <- old_args$decimal_mark
  y_label <- old_args$y_label
  bar_fill <- old_args$bar_fill
  model_color <- old_args$model_color
  use_linetype <- old_args$use_linetype

  format_discrete_axis_labels(
    character(),
    abbreviate_labels = abbreviate_labels,
    label_width = label_width,
    label_abbreviations = label_abbreviations
  )

  df_full <- .rating_table_data(object)
  models <- .rating_table_metadata(object, "models")
  models_nm <- paste0("est_", models)
  exposure_nm <- .rating_table_metadata(object, "exposure")
  expon <- .rating_table_metadata(object, "expon")

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
      axis.title.y       = ggplot2::element_text(size = 10),
      axis.line.x = ggplot2::element_line(colour = "grey55", linewidth = 0.3),
      axis.line.y.left = ggplot2::element_line(colour = "grey55", linewidth = 0.3),
      axis.ticks = ggplot2::element_line(colour = "grey55", linewidth = 0.3),
      axis.ticks.y.right = ggplot2::element_line(colour = "grey75", linewidth = 0.25)
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

    if (length(idx) > 0) {
      idx <- idx[1L]
    }

    if (length(idx) == 0 || is.infinite(idx) || is.na(idx)) {
      return(NULL)
    }

    as.character(df_ref$level[idx])
  }

  pal <- plot_palette()
  grid_theme <- plot_grid_theme()

  final_fill <- if (is.null(bar_fill)) pal$bg_bar else bar_fill
  observed <- .rating_table_metadata(object, "observed_experience")
  observed_label <- if (!is.null(observed)) observed$label else NULL
  observed_color <- if (!is.null(observed) && !is.null(observed$color)) {
    observed$color
  } else {
    pal$risk_premium
  }

  if (identical(legend_position, "auto")) {
    n_displayed_series <- length(models) + as.integer(!is.null(observed_label))
    legend_position <- if (n_displayed_series > 1L) "right" else "none"
  }

  # remove reference categories from plotted model lines
  df <- df_full[df_full$risk_factor != df_full$level, , drop = FALSE]

  df_long <- stats::reshape(
    df,
    varying = models_nm,
    v.names = "est",
    timevar = "model",
    times = models_nm,
    direction = "long"
  )

  rownames(df_long) <- NULL

  df_long$model <- gsub("^est_", "", df_long$model)

  if (!isTRUE(expon)) {
    df_long$est <- exp(df_long$est)
  }

  observed_df <- NULL

  if (!is.null(observed)) {
    if (!is.null(observed$data)) {
      observed_df <- observed$data
    } else if (!is.null(observed$experience)) {
      observed_df <- normalize_rating_table_observed_experience(observed$experience)
    } else {
      stop("The attached observed experience has an unsupported structure.",
           call. = FALSE)
    }
    observed_metric <- resolve_rating_table_observed_metric(
      metric %||% observed$metric,
      observed_df,
      allow_null = FALSE
    )
    observed_scale <- observed$scale %||% "reference"

    ref_levels <- vapply(
      unique(observed_df$risk_factor),
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

    observed_df <- observed_df |>
      dplyr::left_join(ref_lookup, by = "risk_factor") |>
      dplyr::group_by(.data[["risk_factor"]]) |>
      dplyr::mutate(
        est = dplyr::case_when(
          observed_scale == "mean" ~ .data[[observed_metric]] /
            mean(.data[[observed_metric]], na.rm = TRUE),
          observed_scale == "reference" &
            !is.na(.data[["ref_level"]]) ~ .data[[observed_metric]] /
            .data[[observed_metric]][.data[["level"]] == .data[["ref_level"]]][1],
          observed_scale == "reference" &
            is.na(.data[["ref_level"]]) ~ .data[[observed_metric]] /
            mean(.data[[observed_metric]], na.rm = TRUE)
        ),
        model = observed_label
      ) |>
      dplyr::ungroup() |>
      dplyr::select("risk_factor", "level", "model", "est")
  }

  sep_fn <- if (decimal_mark == ",") {
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

    if (!is.null(observed_df)) {
      uni1 <- observed_df[observed_df$risk_factor == rf_i, , drop = FALSE]

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

    model_names <- if (is.null(observed_label)) {
      model_levels
    } else {
      model_levels[model_levels != observed_label]
    }

    model_cols <- discrete_palette_values(length(model_names))
    names(model_cols) <- model_names

    if (!is.null(model_color)) {
      model_cols[] <- model_color
    }

    if (!is.null(observed_label) && observed_label %in% model_levels) {
      model_cols <- c(model_cols, setNames(observed_color, observed_label))
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

    if (isTRUE(use_linetype)) {
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

    if (isTRUE(show_exposure_labels) && isTRUE(has_valid_exposure)) {
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
      ggplot2::scale_x_discrete(
        labels = function(x) {
          format_discrete_axis_labels(
            x,
            abbreviate_labels = abbreviate_labels,
            label_width = label_width,
            label_abbreviations = label_abbreviations
          )
        }
      ) +
      ggplot2::labs(
        x = if (remove_underscores) gsub("_", " ", rf_i) else rf_i,
        y = y_label
      ) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank()
      )

    if (!is.null(rotate_angle)) {
      p <- p +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = rotate_angle, hjust = 1)
        )
    }

    if (!is.null(custom_theme)) {
      p <- p + do.call(ggplot2::theme, custom_theme)
    }

    p <- p + ggplot2::theme(legend.position = legend_position)

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

  plot_out <- patchwork::wrap_plots(
    fig_list,
    ncol = ncol,
    guides = "collect"
  )
  plot_out & ggplot2::theme(legend.position = legend_position)
}

#' @export
autoplot.riskfactor <- autoplot.rating_table


resolve_autoplot_rating_table_args <- function(show_exposure_labels,
                                               decimal_mark,
                                               y_label,
                                               bar_fill,
                                               model_color,
                                               use_linetype,
                                               show_exposure_labels_supplied,
                                               decimal_mark_supplied,
                                               y_label_supplied,
                                               bar_fill_supplied,
                                               model_color_supplied,
                                               use_linetype_supplied,
                                               labels = NULL,
                                               dec.mark = NULL,
                                               ylab = NULL,
                                               fill = NULL,
                                               color = NULL,
                                               linetype = NULL) {
  if (!is.null(labels)) {
    if (show_exposure_labels_supplied) {
      stop("Use only one of `show_exposure_labels` and deprecated `labels`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "autoplot(labels)",
      "autoplot(show_exposure_labels)"
    )
    show_exposure_labels <- labels
  }
  if (!is.null(dec.mark)) {
    if (decimal_mark_supplied) {
      stop("Use only one of `decimal_mark` and deprecated `dec.mark`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "autoplot(dec.mark)",
      "autoplot(decimal_mark)"
    )
    decimal_mark <- dec.mark
  }
  if (!is.null(ylab)) {
    if (y_label_supplied) {
      stop("Use only one of `y_label` and deprecated `ylab`.", call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "autoplot(ylab)", "autoplot(y_label)")
    y_label <- ylab
  }
  if (!is.null(fill)) {
    if (bar_fill_supplied) {
      stop("Use only one of `bar_fill` and deprecated `fill`.", call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "autoplot(fill)", "autoplot(bar_fill)")
    bar_fill <- fill
  }
  if (!is.null(color)) {
    if (model_color_supplied) {
      stop("Use only one of `model_color` and deprecated `color`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "autoplot(color)",
      "autoplot(model_color)"
    )
    model_color <- color
  }
  if (!is.null(linetype)) {
    if (use_linetype_supplied) {
      stop("Use only one of `use_linetype` and deprecated `linetype`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "autoplot(linetype)",
      "autoplot(use_linetype)"
    )
    use_linetype <- linetype
  }

  list(
    show_exposure_labels = show_exposure_labels,
    decimal_mark = decimal_mark,
    y_label = y_label,
    bar_fill = bar_fill,
    model_color = model_color,
    use_linetype = use_linetype
  )
}
