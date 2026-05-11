#' Plot risk factor effects from `rating_table()` results
#'
#' @description
#' Create a ggplot visualisation of a `rating_table` object produced by
#' [rating_table()]. Estimates are plotted per risk factor, with optional
#' exposure bars. Observed portfolio experience can be added first with
#' [add_observed_experience()].
#'
#' When observed experience is attached, it is plotted as an additional line.
#' The scaling is controlled by [add_observed_experience()].
#'
#' @param object A `rating_table` object returned by [rating_table()].
#' @param risk_factors Character vector specifying which risk factors to plot.
#'   Defaults to all risk factors.
#' @param ncol Number of columns in the patchwork layout. Default is 1.
#' @param show_exposure_labels Logical; if `TRUE`, show exposure values as
#'   labels on the bars. Default is `TRUE`.
#' @param decimal_mark Character; decimal separator, either `","` (default) or
#'   `"."`.
#' @param y_label Character; label for the y-axis. Default is `"Relativity"`.
#' @param bar_fill Fill color for the exposure bars. If `NULL`, taken from the
#'   internal palette.
#' @param model_color Optional override for model line colors. If `NULL`, colors
#'   are taken from the internal discrete palette.
#' @param use_linetype Logical; if `TRUE`, use different line types for models.
#'   Default is `FALSE`.
#' @param rotate_angle Numeric value for angle of labels on the x-axis (degrees).
#' @param custom_theme List with customised theme options.
#' @param remove_underscores Logical; remove underscores from labels.
#' @param labels Deprecated alias for `show_exposure_labels`.
#' @param dec.mark Deprecated alias for `decimal_mark`.
#' @param ylab Deprecated alias for `y_label`.
#' @param fill Deprecated alias for `bar_fill`.
#' @param color Deprecated alias for `model_color`.
#' @param linetype Deprecated alias for `use_linetype`.
#' @param ... Additional arguments passed to ggplot2 layers.
#'
#' @return A `ggplot`/`patchwork` object.
#'
#' @import ggplot2
#' @importFrom patchwork wrap_plots
#' @export
autoplot.rating_table <- function(object,
                                  risk_factors = NULL,
                                  ncol = 1,
                                  show_exposure_labels = TRUE,
                                  decimal_mark = ",",
                                  y_label = "Relativity",
                                  bar_fill = NULL,
                                  model_color = NULL,
                                  use_linetype = FALSE,
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

    if (length(idx) == 0 || is.infinite(idx) || is.na(idx)) {
      return(NULL)
    }

    as.character(df_ref$level[idx[1]])
  }

  pal <- plot_palette()
  grid_theme <- plot_grid_theme()

  final_fill <- if (is.null(bar_fill)) pal$bg_bar else bar_fill
  observed <- object$observed_experience
  observed_label <- if (!is.null(observed)) observed$label else NULL
  observed_color <- if (!is.null(observed) && !is.null(observed$color)) {
    observed$color
  } else {
    pal$risk_premium
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
    experience <- observed$experience
    observed_metric <- observed$metric
    observed_scale <- observed$scale
    xvar_observed <- attr(experience, "xvar")
    if (length(xvar_observed) > 1) {
      xvar_observed <- xvar_observed[1]
    }

    if (!observed_metric %in% names(experience)) {
      stop("The observed metric is not found in the attached factor_analysis object.",
           call. = FALSE)
    }

    observed_df <- as.data.frame(experience)

    if (!xvar_observed %in% names(observed_df)) {
      stop("The attached factor_analysis object does not contain the expected x variable.",
           call. = FALSE)
    }

    names(observed_df)[names(observed_df) == xvar_observed] <- "level"

    if (!"risk_factor" %in% names(observed_df)) {
      observed_df$risk_factor <- xvar_observed
    }

    observed_df$level <- as.character(observed_df$level)

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
      ggplot2::labs(
        x = if (remove_underscores) gsub("_", " ", rf_i) else rf_i,
        y = y_label
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
