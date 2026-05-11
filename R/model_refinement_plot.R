# -----------------------------------------------------------------------------
# Plot selection helpers
# -----------------------------------------------------------------------------

.select_plot_step <- function(ref, variable = NULL, step = NULL) {
  .assert_refinement(ref)

  if (!is.null(step)) {
    if (!is.numeric(step) || length(step) != 1 || is.na(step)) {
      stop("'step' must be a single numeric index.", call. = FALSE)
    }

    step <- as.integer(step)

    if (step < 1 || step > length(ref$steps)) {
      stop("'step' is out of bounds.", call. = FALSE)
    }

    return(step)
  }

  if (!is.null(variable)) {
    matches <- which(vapply(
      ref$steps,
      function(s) identical(s$variable, variable),
      logical(1)
    ))

    if (length(matches) == 0) {
      stop("No refinement step found for variable: ", variable, call. = FALSE)
    }

    if (length(matches) > 1) {
      stop(
        "Multiple refinement steps found for variable: ",
        variable,
        ". Use `step =` instead.",
        call. = FALSE
      )
    }

    return(matches)
  }

  if (length(ref$steps) == 1) {
    return(1L)
  }

  stop(
    "Multiple refinement steps found. Use `variable =` or `step =` to select one.",
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# Preview refinement up to a selected step
# -----------------------------------------------------------------------------

preview_refinement <- function(ref, upto = length(ref$steps)) {
  .assert_refinement(ref)

  if (length(ref$steps) == 0) {
    stop("No refinement steps available.", call. = FALSE)
  }

  if (!is.numeric(upto) || length(upto) != 1 || is.na(upto)) {
    stop("'upto' must be a single numeric step index.", call. = FALSE)
  }

  upto <- as.integer(upto)

  if (upto < 1 || upto > length(ref$steps)) {
    stop("'upto' is out of bounds.", call. = FALSE)
  }

  state <- .make_exec_state(ref)

  for (i in seq_len(upto)) {
    state <- .apply_refinement_step(state, ref$steps[[i]])
  }

  list(
    state = state,
    step = ref$steps[[upto]]
  )
}


# -----------------------------------------------------------------------------
# autoplot for refinement workflow
# -----------------------------------------------------------------------------

#' Plot a model refinement step
#'
#' @description
#' Takes a `rating_refinement` object and plots one refinement step before
#' [refit()] is called. This is useful for checking whether manual tariff
#' restrictions, smoothing or expert-based relativities behave as intended
#' before they are used in a refined pricing model.
#'
#' For objects produced by `add_relativities()`, original levels that are split
#' into new levels are removed from the connected original line and from the
#' x-axis. Instead, the original level is shown as a horizontal blue segment
#' spanning all child categories, with the original level label centred above
#' the segment.
#'
#' @param object Object of class `rating_refinement`.
#' @param variable Optional character string specifying the risk factor to plot.
#'   If `NULL` (default), all available variables in the refinement object are
#'   shown. If specified, only the selected risk factor is plotted.
#'
#' @param step Optional integer specifying which refinement step to plot.
#'   This is mainly relevant when multiple refinement steps have been applied
#'   (e.g. multiple calls to `add_smoothing()`, `add_restriction()`, or
#'   `add_relativities()`).
#'
#'   - If `NULL` (default), the latest refinement step is shown.
#'   - If specified, the corresponding step in the refinement sequence is used.
#'
#'   This makes it possible to inspect intermediate refinement stages before
#'   calling `refit()`.
#' @param remove_underscores Logical; if `TRUE`, underscores are replaced by
#'   spaces in the x-axis label. Default is `FALSE`.
#' @param rotate_angle Optional numeric value for the angle of x-axis labels.
#' @param custom_theme Optional list passed to `ggplot2::theme()`.
#' @param ... Additional plotting arguments passed to ggplot2 geoms.
#'
#' @return A `ggplot2` object.
#'
#' @author Martin Haringa
#'
#' @import ggplot2
#' @importFrom dplyr left_join
#'
#' @export
autoplot.rating_refinement <- function(object,
                                       variable = NULL,
                                       step = NULL,
                                       remove_underscores = FALSE,
                                       rotate_angle = NULL,
                                       custom_theme = NULL,
                                       ...) {
  .assert_refinement(object)

  if (length(object$steps) == 0) {
    stop("No refinement steps available to plot.", call. = FALSE)
  }

  idx <- .select_plot_step(object, variable = variable, step = step)
  preview <- preview_refinement(object, upto = idx)

  selected_step <- preview$step
  state <- preview$state

  if (identical(selected_step$type, "smoothing")) {
    return(
      .plot_smoothing_step(
        state = state,
        step = selected_step,
        ...
      )
    )
  }

  if (selected_step$type %in% c("restriction", "relativities")) {
    return(
      .plot_restriction_step(
        state = state,
        step = selected_step,
        remove_underscores = remove_underscores,
        rotate_angle = rotate_angle,
        custom_theme = custom_theme,
        ...
      )
    )
  }

  stop("No plot available for this refinement step.", call. = FALSE)
}


# -----------------------------------------------------------------------------
# Internal plotting helpers
# -----------------------------------------------------------------------------

.plot_palette_ir <- function() {
  list(
    frequency        = "#2C7FB8",
    average_severity = "#41AB5D",
    risk_premium     = "#F28E2B",
    loss_ratio       = "#8C6BB1",
    average_premium  = "#2CB1A1",
    bg_bar           = "#E6E6E6"
  )
}

.plot_grid_theme_ir <- function() {
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.grid.major = ggplot2::element_line(color = "#F2F2F2", linewidth = 0.4),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border     = ggplot2::element_blank(),
    axis.text.y.right  = ggplot2::element_text(color = "#9E9E9E", size = 8),
    axis.title.y.right = ggplot2::element_text(color = "#9E9E9E", size = 9),
    axis.title.y       = ggplot2::element_text(size = 10),
    axis.line = ggplot2::element_line(colour = "grey55", linewidth = 0.3),
    axis.ticks = ggplot2::element_line(colour = "grey55", linewidth = 0.3)
  )
}


# -----------------------------------------------------------------------------
# Plot smoothing step
# -----------------------------------------------------------------------------

.plot_smoothing_step <- function(state, step, ...) {
  rf2 <- state$borders
  new <- state$new
  new_line <- state$new_line

  if (is.null(rf2) || is.null(new) || is.null(new_line)) {
    stop("No smoothing plot data found for this step.", call. = FALSE)
  }

  pal <- .plot_palette_ir()
  grid_theme <- .plot_grid_theme_ir()

  rf2_start_open   <- rf2[rf2$start_oc == "open", , drop = FALSE]
  rf2_start_closed <- rf2[rf2$start_oc == "closed", , drop = FALSE]
  rf2_end_open     <- rf2[rf2$end_oc == "open", , drop = FALSE]
  rf2_end_closed   <- rf2[rf2$end_oc == "closed", , drop = FALSE]

  new_start_open   <- new[new$start_oc == "open", , drop = FALSE]
  new_start_closed <- new[new$start_oc == "closed", , drop = FALSE]
  new_end_open     <- new[new$end_oc == "open", , drop = FALSE]
  new_end_closed   <- new[new$end_oc == "closed", , drop = FALSE]

  x_name <- names(new_line)[1]
  names(new_line)[names(new_line) == x_name] <- "col1"

  ggplot2::ggplot(data = rf2) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = start_,
        y = estimate,
        xend = end_,
        yend = estimate,
        color = "Model fit"
      ),
      linewidth = 0.8,
      ...
    ) +
    ggplot2::geom_segment(
      data = new,
      ggplot2::aes(
        x = breaks_min,
        y = yhat,
        xend = breaks_max,
        yend = yhat,
        color = "New cluster"
      ),
      linewidth = 0.8,
      ...
    ) +
    ggplot2::geom_line(
      data = new_line,
      ggplot2::aes(
        x = col1,
        y = yhat
      ),
      linewidth = 0.5,
      linetype = "dashed",
      color = "#4D4D4D",
      ...
    ) +
    ggplot2::geom_point(
      data = rf2_start_closed,
      ggplot2::aes(x = start_, y = estimate, color = "Model fit"),
      shape = 16,
      size = 2.2,
      ...
    ) +
    ggplot2::geom_point(
      data = rf2_end_closed,
      ggplot2::aes(x = end_, y = estimate, color = "Model fit"),
      shape = 16,
      size = 2.2,
      ...
    ) +
    ggplot2::geom_point(
      data = rf2_start_open,
      ggplot2::aes(x = start_, y = estimate, color = "Model fit"),
      shape = 21,
      fill = "white",
      stroke = 0.7,
      size = 2.2,
      ...
    ) +
    ggplot2::geom_point(
      data = rf2_end_open,
      ggplot2::aes(x = end_, y = estimate, color = "Model fit"),
      shape = 21,
      fill = "white",
      stroke = 0.7,
      size = 2.2,
      ...
    ) +
    ggplot2::geom_point(
      data = new_start_closed,
      ggplot2::aes(x = start_, y = yhat, color = "New cluster"),
      shape = 16,
      size = 2.2,
      ...
    ) +
    ggplot2::geom_point(
      data = new_end_closed,
      ggplot2::aes(x = end_, y = yhat, color = "New cluster"),
      shape = 16,
      size = 2.2,
      ...
    ) +
    ggplot2::geom_point(
      data = new_start_open,
      ggplot2::aes(x = start_, y = yhat, color = "New cluster"),
      shape = 21,
      fill = "white",
      stroke = 0.7,
      size = 2.2,
      ...
    ) +
    ggplot2::geom_point(
      data = new_end_open,
      ggplot2::aes(x = end_, y = yhat, color = "New cluster"),
      shape = 21,
      fill = "white",
      stroke = 0.7,
      size = 2.2,
      ...
    ) +
    ggplot2::labs(
      x = x_name,
      y = "Estimated relativity"
    ) +
    ggplot2::scale_colour_manual(
      name = NULL,
      values = c(
        "Model fit" = pal$frequency,
        "New cluster" = pal$risk_premium
      ),
      labels = c(
        "Model fit" = "Original fit",
        "New cluster" = "Clustered fit"
      )
    ) +
    ggplot2::theme_minimal() +
    grid_theme
}


# -----------------------------------------------------------------------------
# Plot restriction / relativities step
# -----------------------------------------------------------------------------

.plot_restriction_step <- function(state,
                                   step,
                                   remove_underscores = FALSE,
                                   rotate_angle = NULL,
                                   custom_theme = NULL,
                                   ...) {

  pal <- .plot_palette_ir()
  grid_theme <- .plot_grid_theme_ir()

  if (identical(step$type, "relativities")) {
    rel_df <- state$relativities_df
    rf <- as.data.frame(state$rating_factors)
    base_rf <- state$base_risk_factor
    display_rf <- state$display_risk_factor

    if (is.null(rel_df) || is.null(base_rf) || is.null(display_rf)) {
      stop("No relativities plot data found for this step.", call. = FALSE)
    }

    rel_df <- as.data.frame(rel_df)

    rf_base <- rf[rf$risk_factor == base_rf, c("level", "estimate"), drop = FALSE]
    names(rf_base)[2] <- "Coef"
    rf_base$type <- "Original fit"

    rel_plot <- rel_df[, c("new_level", "estimate"), drop = FALSE]
    names(rel_plot) <- c("level", "Coef")
    rel_plot$type <- "New relativities"

    replaced_levels <- unique(rel_df$level)

    rf_base_line <- rf_base[!rf_base$level %in% replaced_levels, , drop = FALSE]

    unsplit_levels <- setdiff(rf_base$level, unique(rel_df$level))
    if (length(unsplit_levels) > 0) {
      unsplit_plot <- rf_base[rf_base$level %in% unsplit_levels, c("level", "Coef"), drop = FALSE]
      unsplit_plot$type <- "New relativities"
      rel_plot <- rbind(rel_plot, unsplit_plot)
    }

    lvl_order <- unique(c(
      as.character(rf_base_line$level),
      as.character(rel_plot$level)
    ))

    x_lookup <- data.frame(
      level = lvl_order,
      x_num = seq_along(lvl_order),
      stringsAsFactors = FALSE
    )

    rf_base_line$level <- as.character(rf_base_line$level)
    rel_plot$level <- as.character(rel_plot$level)

    rf_base_line <- dplyr::left_join(rf_base_line, x_lookup, by = "level")
    rel_plot <- dplyr::left_join(rel_plot, x_lookup, by = "level")

    segments_list <- list()

    for (lvl in replaced_levels) {
      child_lvls <- as.character(rel_df$new_level[rel_df$level == lvl])
      child_lvls <- child_lvls[child_lvls %in% lvl_order]

      if (length(child_lvls) == 0) next

      child_pos <- x_lookup$x_num[match(child_lvls, x_lookup$level)]
      child_pos <- child_pos[!is.na(child_pos)]

      if (length(child_pos) == 0) next

      y_val <- rf_base$Coef[rf_base$level == lvl][1]

      segments_list[[as.character(lvl)]] <- data.frame(
        x_start = min(child_pos),
        x_end   = max(child_pos),
        y       = y_val,
        label   = as.character(lvl),
        stringsAsFactors = FALSE
      )
    }

    segments_df <- NULL
    if (length(segments_list) > 0) {
      segments_df <- do.call(rbind, segments_list)
    }

    x_lab <- display_rf
    if (remove_underscores) {
      x_lab <- gsub("_", " ", x_lab)
    }

    p <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = rf_base_line,
        ggplot2::aes(x = x_num, y = Coef, color = type, group = type),
        linewidth = 0.8,
        ...
      ) +
      ggplot2::geom_point(
        data = rf_base_line,
        ggplot2::aes(x = x_num, y = Coef, color = type),
        shape = 21,
        fill = "white",
        stroke = 0.7,
        size = 2.4,
        ...
      ) +
      ggplot2::geom_line(
        data = rel_plot,
        ggplot2::aes(x = x_num, y = Coef, color = type, group = type),
        linewidth = 0.8,
        ...
      ) +
      ggplot2::geom_point(
        data = rel_plot,
        ggplot2::aes(x = x_num, y = Coef, color = type),
        shape = 21,
        fill = "white",
        stroke = 0.7,
        size = 2.4,
        ...
      )

    if (!is.null(segments_df) && nrow(segments_df) > 0) {
      p <- p +
        ggplot2::geom_segment(
          data = segments_df,
          ggplot2::aes(
            x = x_start,
            xend = x_end,
            y = y,
            yend = y
          ),
          color = pal$frequency,
          linewidth = 0.9
        ) +
        ggplot2::geom_segment(
          data = segments_df,
          ggplot2::aes(
            x = x_start,
            xend = x_start,
            y = y,
            yend = y - 0.012
          ),
          color = pal$frequency,
          linewidth = 0.8
        ) +
        ggplot2::geom_segment(
          data = segments_df,
          ggplot2::aes(
            x = x_end,
            xend = x_end,
            y = y,
            yend = y - 0.012
          ),
          color = pal$frequency,
          linewidth = 0.8
        ) +
        ggplot2::geom_text(
          data = segments_df,
          ggplot2::aes(
            x = (x_start + x_end) / 2,
            y = y,
            label = label
          ),
          vjust = -0.7,
          color = pal$frequency,
          size = 3.2
        )
    }

    p <- p +
      ggplot2::scale_x_continuous(
        breaks = x_lookup$x_num,
        labels = x_lookup$level
      ) +
      ggplot2::scale_colour_manual(
        values = c(
          "Original fit" = pal$frequency,
          "New relativities" = pal$risk_premium
        ),
        name = NULL
      ) +
      ggplot2::labs(
        x = x_lab,
        y = "Relativity"
      ) +
      ggplot2::theme_minimal() +
      grid_theme

    if (!is.null(rotate_angle)) {
      p <- p +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = rotate_angle, hjust = 1)
        )
    }

    if (!is.null(custom_theme)) {
      p <- p + do.call(ggplot2::theme, custom_theme)
    }

    return(p)
  }

  if (identical(step$type, "restriction")) {
    naam_rst <- step$restrictions
    name <- names(naam_rst)[1]

    rf <- as.data.frame(state$rating_factors)
    naam_rf <- rf[rf$risk_factor == name, , drop = FALSE]
    naam_rf <- naam_rf[, c("level", "estimate"), drop = FALSE]

    names(naam_rst)[names(naam_rst) == name] <- "level"
    naam_rf <- matchColClasses(naam_rst, naam_rf)

    koppel <- dplyr::left_join(naam_rst, naam_rf, by = "level")

    restricted_name <- names(naam_rst)[2]
    unrestricted_name <- names(naam_rf)[2]

    koppel_restricted <- data.frame(
      level = koppel$level,
      type = "Adjusted fit",
      Coef = koppel[[restricted_name]],
      stringsAsFactors = FALSE
    )

    koppel_unrestricted <- data.frame(
      level = koppel$level,
      type = "Original fit",
      Coef = koppel[[unrestricted_name]],
      stringsAsFactors = FALSE
    )

    koppel_long <- rbind(koppel_unrestricted, koppel_restricted)
    lvl_order <- unique(koppel$level)

    x_lookup <- data.frame(
      level = lvl_order,
      x_num = seq_along(lvl_order),
      stringsAsFactors = FALSE
    )

    koppel_long <- dplyr::left_join(koppel_long, x_lookup, by = "level")

    x_lab <- name
    if (remove_underscores) {
      x_lab <- gsub("_", " ", x_lab)
    }

    p <- ggplot2::ggplot(
      data = koppel_long,
      ggplot2::aes(x = x_num, y = Coef, color = type, group = type)
    ) +
      ggplot2::geom_line(linewidth = 0.8, ...) +
      ggplot2::geom_point(
        shape = 21,
        fill = "white",
        stroke = 0.7,
        size = 2.4,
        ...
      ) +
      ggplot2::scale_x_continuous(
        breaks = x_lookup$x_num,
        labels = x_lookup$level
      ) +
      ggplot2::scale_colour_manual(
        values = c(
          "Original fit" = pal$frequency,
          "Adjusted fit" = pal$risk_premium
        ),
        name = NULL
      ) +
      ggplot2::labs(
        x = x_lab,
        y = "Relativity"
      ) +
      ggplot2::theme_minimal() +
      grid_theme

    if (!is.null(rotate_angle)) {
      p <- p +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = rotate_angle, hjust = 1)
        )
    }

    if (!is.null(custom_theme)) {
      p <- p + do.call(ggplot2::theme, custom_theme)
    }

    return(p)
  }

  stop("No restriction/relativities plot available for this step.", call. = FALSE)
}
