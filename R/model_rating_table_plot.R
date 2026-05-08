#' Plot risk factor effects from `rating_table()` results
#'
#' @description
#' Create a ggplot visualisation of a `rating_table` object produced by
#' [rating_table()]. Estimates are plotted per risk factor, with optional
#' exposure bars and, optionally, an additional univariate line.
#'
#' When `univariate_scale = "reference"`, the univariate line is scaled to the
#' model reference group. The reference group is determined as the level with
#' model coefficient equal to 1. If no level is exactly equal to 1, the level
#' with coefficient closest to 1 is used.
#'
#' @param object A `rating_table` object returned by [rating_table()].
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
#'   [factor_analysis()]. If supplied, the selected univariate statistic is
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
#' @aliases autoplot.rating_table
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

#' @export
autoplot.rating_table <- autoplot.riskfactor
