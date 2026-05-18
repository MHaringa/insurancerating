#' Exploratory severity diagnostics by category
#'
#' @description
#' Visualise individual claim amounts overall or per risk factor.
#'
#' Average claim amounts can be misleading because a small number of large
#' losses may dominate the mean. `plot_severity_distribution()` shows the full
#' claim amount distribution, usually on a log scale, together with mean and
#' median claim amount markers. If `risk_factor` is supplied, the distribution
#' is shown per level of that risk factor. If `risk_factor = NULL`, the function
#' shows the overall claim amount distribution. This makes heavy tails, clusters
#' of small claims, spread differences, extreme losses and distributional shape
#' visible in a way that average severity alone cannot.
#'
#' The function is intended for exploratory severity diagnostics in pricing
#' analysis, portfolio diagnostics, tariff notes, exploratory segmentation
#' analysis and severity model validation. It uses standard evaluation: pass
#' column names as character strings through `claim_amount` and `risk_factor`.
#'
#' If `threshold` is supplied, claims above the threshold are highlighted in
#' `"firebrick"` and a dotted threshold line is added. Claims at or below the
#' threshold remain light grey. Direct labels for the mean, median and optional
#' threshold are added with `ggrepel` when `show_labels = TRUE`; `ggrepel` is a
#' suggested package and is not imported as a hard dependency.
#'
#' @param data A `data.frame` with claim-level observations.
#' @param claim_amount Character string. Name of the claim amount column.
#' @param risk_factor Optional character string. Name of the risk factor used
#'   to split the severity distribution. If `NULL`, the overall claim amount
#'   distribution is shown.
#' @param xlab Optional character string. X-axis label. If `NULL`, a default is
#'   chosen from `claim_amount` and `orientation`.
#' @param ylab Optional character string. Y-axis label. If `NULL`, a default is
#'   chosen from `risk_factor`, `claim_amount` and `orientation`.
#' @param all_claims_label Character string used as the category label when
#'   `risk_factor = NULL`.
#' @param threshold Optional numeric scalar. If supplied, claims above this
#'   threshold are highlighted and a dotted threshold line is shown.
#' @param show_labels Logical. If `TRUE`, add direct labels for the mean,
#'   median and, when supplied, threshold. Requires the suggested package
#'   `ggrepel`.
#' @param mean_label Character string used for the direct mean marker label.
#'   Default is `"Mean"`.
#' @param median_label Character string used for the direct median marker label.
#'   Default is `"Median"`.
#' @param threshold_label Character string used for the optional threshold
#'   label.
#' @param top_n Positive whole number. Number of categories to keep after
#'   filtering and sorting.
#' @param min_claims Positive whole number. Categories with fewer than this
#'   number of claim observations are removed.
#' @param sort Character. Metric used to sort and select categories. One of
#'   `"median"`, `"mean"` or `"n_claims"`.
#' @param point_method Character. Point placement method. One of
#'   `"quasirandom"`, `"jitter"` or `"none"`.
#' @param distribution Character. Distribution layer. One of `"none"`,
#'   `"half_violin"` or `"violin"`. Default is `"none"`.
#' @param boxplot Logical. If `TRUE`, add a small centred boxplot. Default is
#'   `FALSE`.
#' @param boxplot_width Numeric scalar. Width of the optional boxplot. Smaller
#'   values keep the boxplot as a subtle summary layer behind the individual
#'   claim points.
#' @param mean Logical. If `TRUE`, add a marker for the average claim amount.
#' @param median Logical. If `TRUE`, add a marker for the median claim amount.
#' @param log_scale Logical. If `TRUE`, use a log10 scale for claim amounts.
#' @param orientation Character. `"horizontal"` places claim amount on the
#'   x-axis and categories on the y-axis. `"vertical"` reverses this.
#' @param point_alpha Numeric alpha for raw claim points.
#' @param point_size Numeric point size for raw claim points.
#' @param point_width Numeric spread for raw claim points.
#'
#' @return A ggplot object. The plot can be extended with regular ggplot2
#'   syntax, for example `+ ggplot2::labs(caption = "...")` or
#'   `+ ggplot2::theme(...)`.
#'
#' @author Martin Haringa
#'
#' @examples
#' x <- plot_severity_distribution(
#'   MTPL,
#'   claim_amount = "amount",
#'   risk_factor = "zip",
#'   top_n = 4,
#'   min_claims = 20,
#'   point_method = "jitter",
#'   show_labels = FALSE
#' )
#' print(x)
#'
#' x_threshold <- plot_severity_distribution(
#'   MTPL,
#'   claim_amount = "amount",
#'   risk_factor = NULL,
#'   threshold = 10000,
#'   min_claims = 20,
#'   point_method = "jitter",
#'   show_labels = FALSE
#' )
#'
#' @import ggplot2
#' @importFrom stats aggregate density median
#' @export
plot_severity_distribution <- function(data,
                                       claim_amount,
                                       risk_factor = NULL,
                                       xlab = NULL,
                                       ylab = NULL,
                                       all_claims_label = "All claims",
                                       threshold = NULL,
                                       show_labels = TRUE,
                                       mean_label = "Mean",
                                       median_label = "Median",
                                       threshold_label = "Threshold",
                                       top_n = 10,
                                       min_claims = 20,
                                       sort = c("median", "mean", "n_claims"),
                                       point_method = c("quasirandom", "jitter", "none"),
                                       distribution = c("none", "half_violin", "violin"),
                                       boxplot = FALSE,
                                       boxplot_width = 0.06,
                                       mean = TRUE,
                                       median = TRUE,
                                       log_scale = TRUE,
                                       orientation = c("horizontal", "vertical"),
                                       point_alpha = 0.16,
                                       point_size = 0.75,
                                       point_width = 0.15) {
  sort <- match.arg(sort)
  point_method <- match.arg(point_method)
  distribution <- match.arg(distribution)
  orientation <- match.arg(orientation)

  claim_amount_expr <- substitute(claim_amount)
  risk_factor_expr <- substitute(risk_factor)
  if (!is.character(claim_amount_expr) || length(claim_amount_expr) != 1L) {
    stop("`claim_amount` must be a single character string.", call. = FALSE)
  }
  if (!is.null(risk_factor_expr) &&
      (!is.character(risk_factor_expr) || length(risk_factor_expr) != 1L)) {
    stop("`risk_factor` must be NULL or a single character string.",
         call. = FALSE)
  }

  amount_nm <- claim_amount_expr
  risk_factor_nm <- if (is.null(risk_factor_expr)) NULL else risk_factor_expr

  validate_severity_distribution_args(
    data = data,
    claim_amount = amount_nm,
    risk_factor = risk_factor_nm,
    xlab = xlab,
    ylab = ylab,
    all_claims_label = all_claims_label,
    threshold = threshold,
    show_labels = show_labels,
    mean_label = mean_label,
    median_label = median_label,
    threshold_label = threshold_label,
    top_n = top_n,
    min_claims = min_claims,
    boxplot = boxplot,
    boxplot_width = boxplot_width,
    mean = mean,
    median = median,
    log_scale = log_scale,
    point_alpha = point_alpha,
    point_size = point_size,
    point_width = point_width
  )

  if (isTRUE(show_labels)) {
    rlang::check_installed(
      "ggrepel",
      reason = "to add direct labels to the severity distribution plot."
    )
  }

  plot_data <- data.frame(
    .category = if (is.null(risk_factor_nm)) {
      rep(all_claims_label, nrow(data))
    } else {
      as.character(data[[risk_factor_nm]])
    },
    .claim_amount = data[[amount_nm]],
    stringsAsFactors = FALSE
  )

  invalid <- is.na(plot_data$.category) |
    is.na(plot_data$.claim_amount) |
    !is.finite(plot_data$.claim_amount) |
    plot_data$.claim_amount <= 0

  if (any(invalid)) {
    warning(
      "Removed ", sum(invalid),
      " claim observation(s) with missing, non-finite or non-positive values.",
      call. = FALSE
    )
  }
  plot_data <- plot_data[!invalid, , drop = FALSE]

  if (nrow(plot_data) == 0) {
    stop("No valid claim observations remain after filtering.", call. = FALSE)
  }

  summary_data <- severity_distribution_summary(plot_data)
  summary_data <- summary_data[summary_data$n_claims >= min_claims, , drop = FALSE]

  if (nrow(summary_data) == 0) {
    stop("No categories have at least `min_claims` observations.", call. = FALSE)
  }

  summary_data <- summary_data[order(summary_data[[sort]], decreasing = TRUE), ,
                               drop = FALSE]
  selected_summary <- utils::head(summary_data, top_n)

  levels_plot <- rev(selected_summary$.category)
  plot_data <- plot_data[plot_data$.category %in% levels_plot, , drop = FALSE]
  plot_data$.category_id <- match(plot_data$.category, levels_plot)
  plot_data$.above_threshold <- if (!is.null(threshold)) {
    plot_data$.claim_amount > threshold
  } else {
    FALSE
  }

  plot_summary <- selected_summary[match(levels_plot, selected_summary$.category), ,
                                   drop = FALSE]
  plot_summary$.category_id <- seq_len(nrow(plot_summary))

  density_data <- if (!identical(distribution, "none")) {
    severity_distribution_density(
      plot_data = plot_data,
      levels_plot = levels_plot,
      distribution = distribution,
      log_scale = log_scale,
      width = 0.38
    )
  } else {
    data.frame(
      .category = character(),
      .amount = numeric(),
      .category_pos = numeric(),
      .group = character(),
      stringsAsFactors = FALSE
    )
  }

  p <- build_severity_distribution_plot(
    plot_data = plot_data,
    density_data = density_data,
    summary_data = plot_summary,
    risk_factor_nm = risk_factor_nm,
    amount_nm = amount_nm,
    xlab = xlab,
    ylab = ylab,
    point_method = point_method,
    distribution = distribution,
    boxplot = boxplot,
    boxplot_width = boxplot_width,
    mean = mean,
    median = median,
    threshold = threshold,
    show_labels = show_labels,
    mean_label = mean_label,
    median_label = median_label,
    threshold_label = threshold_label,
    log_scale = log_scale,
    orientation = orientation,
    point_alpha = point_alpha,
    point_size = point_size,
    point_width = point_width
  )

  attr(p, "severity_distribution_data") <- plot_data
  attr(p, "severity_distribution_settings") <- list(
    claim_amount = amount_nm,
    risk_factor = risk_factor_nm,
    xlab = xlab,
    ylab = ylab,
    all_claims_label = all_claims_label,
    threshold = threshold,
    show_labels = show_labels,
    mean_label = mean_label,
    median_label = median_label,
    threshold_label = threshold_label,
    top_n = top_n,
    min_claims = min_claims,
    sort = sort,
    point_method = point_method,
    distribution = distribution,
    boxplot = boxplot,
    boxplot_width = boxplot_width,
    mean = mean,
    median = median,
    log_scale = log_scale,
    orientation = orientation,
    point_alpha = point_alpha,
    point_size = point_size,
    point_width = point_width,
    category_summary = selected_summary
  )
  p
}

validate_severity_distribution_args <- function(data,
                                                claim_amount,
                                                risk_factor,
                                                xlab,
                                                ylab,
                                                all_claims_label,
                                                threshold,
                                                show_labels,
                                                mean_label,
                                                median_label,
                                                threshold_label,
                                                top_n,
                                                min_claims,
                                                boxplot,
                                                boxplot_width,
                                                mean,
                                                median,
                                                log_scale,
                                                point_alpha,
                                                point_size,
                                                point_width) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(claim_amount) || length(claim_amount) != 1L ||
      is.na(claim_amount)) {
    stop("`claim_amount` must be a single character string.", call. = FALSE)
  }
  if (!is.null(risk_factor) &&
      (!is.character(risk_factor) || length(risk_factor) != 1L ||
       is.na(risk_factor))) {
    stop("`risk_factor` must be NULL or a single character string.",
         call. = FALSE)
  }
  missing_cols <- setdiff(c(claim_amount, risk_factor), names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in `data`: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[claim_amount]])) {
    stop("`claim_amount` must refer to a numeric column.", call. = FALSE)
  }
  for (nm in c("xlab", "ylab")) {
    val <- get(nm)
    if (!is.null(val) &&
        (!is.character(val) || length(val) != 1L || is.na(val))) {
      stop("`", nm, "` must be NULL or a single character string.",
           call. = FALSE)
    }
  }
  if (!is.character(all_claims_label) || length(all_claims_label) != 1L ||
      is.na(all_claims_label)) {
    stop("`all_claims_label` must be a single character string.",
         call. = FALSE)
  }
  if (!is.null(threshold) &&
      (!is.numeric(threshold) || length(threshold) != 1L ||
       is.na(threshold) || !is.finite(threshold))) {
    stop("`threshold` must be NULL or a single finite numeric value.",
         call. = FALSE)
  }
  if (!is.null(threshold) && isTRUE(log_scale) && threshold <= 0) {
    stop("`threshold` must be larger than 0 when `log_scale = TRUE`.",
         call. = FALSE)
  }
  for (nm in c("show_labels", "boxplot", "mean", "median", "log_scale")) {
    val <- get(nm)
    if (!is.logical(val) || length(val) != 1L || is.na(val)) {
      stop("`", nm, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }
  for (nm in c("mean_label", "median_label", "threshold_label")) {
    val <- get(nm)
    if (!is.character(val) || length(val) != 1L || is.na(val)) {
      stop("`", nm, "` must be a single character string.", call. = FALSE)
    }
  }
  if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) ||
      top_n < 1 || top_n != floor(top_n)) {
    stop("`top_n` must be a positive whole number.", call. = FALSE)
  }
  if (!is.numeric(min_claims) || length(min_claims) != 1L ||
      is.na(min_claims) || min_claims < 1 || min_claims != floor(min_claims)) {
    stop("`min_claims` must be a positive whole number.", call. = FALSE)
  }
  for (nm in c("point_alpha", "point_size", "point_width", "boxplot_width")) {
    val <- get(nm)
    if (!is.numeric(val) || length(val) != 1L || !is.finite(val) ||
        val < 0) {
      stop("`", nm, "` must be a single non-negative number.", call. = FALSE)
    }
  }
  if (point_alpha > 1) {
    stop("`point_alpha` must be between 0 and 1.", call. = FALSE)
  }
  if (boxplot_width <= 0 || boxplot_width > 1) {
    stop("`boxplot_width` must be larger than 0 and at most 1.", call. = FALSE)
  }
}

severity_distribution_summary <- function(plot_data) {
  split_vals <- split(plot_data$.claim_amount, plot_data$.category)
  vals <- t(vapply(split_vals, function(x) {
    c(
      n_claims = length(x),
      mean = mean(x, na.rm = TRUE),
      median = stats::median(x, na.rm = TRUE)
    )
  }, FUN.VALUE = c(n_claims = 0, mean = 0, median = 0)))
  data.frame(
    .category = rownames(vals),
    n_claims = as.integer(vals[, "n_claims"]),
    mean = as.numeric(vals[, "mean"]),
    median = as.numeric(vals[, "median"]),
    stringsAsFactors = FALSE
  )
}

severity_distribution_density <- function(plot_data,
                                          levels_plot,
                                          distribution,
                                          log_scale,
                                          width = 0.38) {
  out <- lapply(seq_along(levels_plot), function(i) {
    vals <- plot_data$.claim_amount[plot_data$.category == levels_plot[i]]
    vals_density <- if (isTRUE(log_scale)) log10(vals) else vals
    vals_density <- vals_density[is.finite(vals_density)]

    if (length(unique(vals_density)) < 2) {
      return(NULL)
    }

    dens <- stats::density(vals_density, n = 128, na.rm = TRUE)
    dens_y <- dens$y / max(dens$y, na.rm = TRUE) * width
    amount <- if (isTRUE(log_scale)) 10^dens$x else dens$x

    if (identical(distribution, "half_violin")) {
      data.frame(
        .category = levels_plot[i],
        .amount = c(amount, rev(amount)),
        .category_pos = c(rep(i, length(amount)), rev(i + dens_y)),
        .group = paste0(levels_plot[i], "_half"),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        .category = levels_plot[i],
        .amount = c(amount, rev(amount)),
        .category_pos = c(i - dens_y, rev(i + dens_y)),
        .group = paste0(levels_plot[i], "_full"),
        stringsAsFactors = FALSE
      )
    }
  })

  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) {
    return(data.frame(
      .category = character(),
      .amount = numeric(),
      .category_pos = numeric(),
      .group = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, out)
}

build_severity_distribution_plot <- function(plot_data,
                                             density_data,
                                             summary_data,
                                             risk_factor_nm,
                                             amount_nm,
                                             xlab,
                                             ylab,
                                             point_method,
                                             distribution,
                                             boxplot,
                                             boxplot_width,
                                             mean,
                                             median,
                                             threshold,
                                             show_labels,
                                             mean_label,
                                             median_label,
                                             threshold_label,
                                             log_scale,
                                             orientation,
                                             point_alpha,
                                             point_size,
                                             point_width) {
  pal <- .plot_palette_ir()
  grid_theme <- .plot_grid_theme_ir()

  if (identical(orientation, "horizontal")) {
    p <- ggplot2::ggplot()

    if (!identical(distribution, "none") && nrow(density_data) > 0) {
      p <- p +
        ggplot2::geom_polygon(
          data = density_data,
          ggplot2::aes(
            x = .data[[".amount"]],
            y = .data[[".category_pos"]],
            group = .data[[".group"]]
          ),
          fill = pal$frequency,
          color = NA,
          alpha = 0.16
        )
    }

    if (!is.null(threshold)) {
      p <- p +
        ggplot2::geom_vline(
          xintercept = threshold,
          linetype = "dotted",
          color = "firebrick",
          linewidth = 0.4,
          alpha = 0.75
        )
    }

    if (isTRUE(boxplot)) {
      p <- p +
        ggplot2::geom_boxplot(
          data = plot_data,
          ggplot2::aes(
            x = .data[[".claim_amount"]],
            y = .data[[".category_id"]],
            group = .data[[".category_id"]]
          ),
          width = boxplot_width,
          outlier.shape = NA,
          fill = "white",
          alpha = 0.18,
          color = grDevices::adjustcolor("grey35", alpha.f = 0.50),
          linewidth = 0.22
        )
    }

    p <- add_severity_points(
      p, plot_data, point_method, point_alpha, point_size, point_width,
      orientation = "horizontal"
    )

    if (isTRUE(mean)) {
      p <- p +
        ggplot2::geom_point(
          data = summary_data,
          ggplot2::aes(
            x = .data[["mean"]],
            y = .data[[".category_id"]]
          ),
          shape = 21,
          size = 2.5,
          stroke = 0.4,
          color = pal$risk_premium,
          fill = pal$risk_premium,
          show.legend = FALSE
        )
    }

    if (isTRUE(median)) {
      p <- p +
        ggplot2::geom_point(
          data = summary_data,
          ggplot2::aes(
            x = .data[["median"]],
            y = .data[[".category_id"]]
          ),
          shape = 18,
          size = 3.5,
          color = pal$frequency,
          show.legend = FALSE
        )
    }

    p <- add_severity_direct_labels(
      p = p,
      summary_data = summary_data,
      threshold = threshold,
      show_labels = show_labels,
      mean = mean,
      median = median,
      mean_label = mean_label,
      median_label = median_label,
      threshold_label = threshold_label,
      orientation = "horizontal",
      pal = pal
    )

    p <- p +
      ggplot2::scale_y_continuous(
        breaks = summary_data$.category_id,
        labels = summary_data$.category,
        expand = ggplot2::expansion(mult = c(0.03, 0.10))
      ) +
      ggplot2::labs(
        x = if (is.null(xlab)) amount_nm else xlab,
        y = if (is.null(ylab)) {
          if (is.null(risk_factor_nm)) NULL else risk_factor_nm
        } else {
          ylab
        }
      )

    if (isTRUE(log_scale)) {
      p <- p + ggplot2::scale_x_log10()
    }
  } else {
    p <- ggplot2::ggplot()

    if (!identical(distribution, "none") && nrow(density_data) > 0) {
      p <- p +
        ggplot2::geom_polygon(
          data = density_data,
          ggplot2::aes(
            x = .data[[".category_pos"]],
            y = .data[[".amount"]],
            group = .data[[".group"]]
          ),
          fill = pal$frequency,
          color = NA,
          alpha = 0.16
        )
    }

    if (!is.null(threshold)) {
      p <- p +
        ggplot2::geom_hline(
          yintercept = threshold,
          linetype = "dotted",
          color = "firebrick",
          linewidth = 0.4,
          alpha = 0.75
        )
    }

    if (isTRUE(boxplot)) {
      p <- p +
        ggplot2::geom_boxplot(
          data = plot_data,
          ggplot2::aes(
            x = .data[[".category_id"]],
            y = .data[[".claim_amount"]],
            group = .data[[".category_id"]]
          ),
          width = boxplot_width,
          outlier.shape = NA,
          fill = "white",
          alpha = 0.18,
          color = grDevices::adjustcolor("grey35", alpha.f = 0.50),
          linewidth = 0.22
        )
    }

    p <- add_severity_points(
      p, plot_data, point_method, point_alpha, point_size, point_width,
      orientation = "vertical"
    )

    if (isTRUE(mean)) {
      p <- p +
        ggplot2::geom_point(
          data = summary_data,
          ggplot2::aes(
            x = .data[[".category_id"]],
            y = .data[["mean"]]
          ),
          shape = 21,
          size = 2.5,
          stroke = 0.4,
          color = pal$risk_premium,
          fill = pal$risk_premium,
          show.legend = FALSE
        )
    }

    if (isTRUE(median)) {
      p <- p +
        ggplot2::geom_point(
          data = summary_data,
          ggplot2::aes(
            x = .data[[".category_id"]],
            y = .data[["median"]]
          ),
          shape = 18,
          size = 3.5,
          color = pal$frequency,
          show.legend = FALSE
        )
    }

    p <- add_severity_direct_labels(
      p = p,
      summary_data = summary_data,
      threshold = threshold,
      show_labels = show_labels,
      mean = mean,
      median = median,
      mean_label = mean_label,
      median_label = median_label,
      threshold_label = threshold_label,
      orientation = "vertical",
      pal = pal
    )

    p <- p +
      ggplot2::scale_x_continuous(
        breaks = summary_data$.category_id,
        labels = summary_data$.category,
        expand = ggplot2::expansion(mult = c(0.03, 0.10))
      ) +
      ggplot2::labs(
        x = if (is.null(xlab)) {
          if (is.null(risk_factor_nm)) NULL else risk_factor_nm
        } else {
          xlab
        },
        y = if (is.null(ylab)) amount_nm else ylab
      )

    if (isTRUE(log_scale)) {
      p <- p + ggplot2::scale_y_log10()
    }
  }

  p +
    ggplot2::theme_minimal() +
    grid_theme +
    ggplot2::theme(
      legend.position = "none"
    )
}

add_severity_points <- function(p,
                                plot_data,
                                point_method,
                                point_alpha,
                                point_size,
                                point_width,
                                orientation) {
  if (identical(point_method, "none")) {
    return(p)
  }

  point_method_eff <- point_method
  if (identical(point_method_eff, "quasirandom") &&
      !requireNamespace("ggbeeswarm", quietly = TRUE)) {
    warning(
      "Package 'ggbeeswarm' is required for point_method = 'quasirandom'. ",
      "Falling back to jittered points.",
      call. = FALSE
    )
    point_method_eff <- "jitter"
  }

  regular <- plot_data[!plot_data$.above_threshold, , drop = FALSE]
  large <- plot_data[plot_data$.above_threshold, , drop = FALSE]

  add_one <- function(plot, data, color, alpha, size) {
    if (nrow(data) == 0) {
      return(plot)
    }

    data$.point_category_id <- data$.category_id

    if (identical(point_method_eff, "quasirandom")) {
      if (identical(orientation, "horizontal")) {
        return(
          plot + ggbeeswarm::geom_quasirandom(
            data = data,
            ggplot2::aes(
              x = .data[[".claim_amount"]],
              y = .data[[".point_category_id"]],
              group = .data[[".category_id"]]
            ),
            groupOnX = FALSE,
            orientation = "y",
            width = point_width,
            alpha = alpha,
            size = size,
            color = color,
            show.legend = FALSE
          )
        )
      }
      return(
        plot + ggbeeswarm::geom_quasirandom(
          data = data,
          ggplot2::aes(
            x = .data[[".point_category_id"]],
            y = .data[[".claim_amount"]],
            group = .data[[".category_id"]]
          ),
          groupOnX = TRUE,
          orientation = "x",
          width = point_width,
          alpha = alpha,
          size = size,
          color = color,
          show.legend = FALSE
        )
      )
    }

    if (identical(orientation, "horizontal")) {
      return(
        plot + ggplot2::geom_jitter(
          data = data,
          ggplot2::aes(
            x = .data[[".claim_amount"]],
            y = .data[[".point_category_id"]]
          ),
          height = point_width,
          width = 0,
          alpha = alpha,
          size = size,
          color = color,
          show.legend = FALSE
        )
      )
    }

    plot + ggplot2::geom_jitter(
      data = data,
      ggplot2::aes(
        x = .data[[".point_category_id"]],
        y = .data[[".claim_amount"]]
      ),
      width = point_width,
      height = 0,
      alpha = alpha,
      size = size,
      color = color,
      show.legend = FALSE
    )
  }

  p <- add_one(p, regular, color = "grey50", alpha = point_alpha,
               size = point_size)
  add_one(p, large, color = "firebrick", alpha = max(point_alpha, 0.45),
          size = point_size * 1.35)
}

add_severity_direct_labels <- function(p,
                                       summary_data,
                                       threshold,
                                       show_labels,
                                       mean,
                                       median,
                                       mean_label,
                                       median_label,
                                       threshold_label,
                                       orientation,
                                       pal) {
  if (!isTRUE(show_labels)) {
    return(p)
  }

  label_row <- summary_data[nrow(summary_data), , drop = FALSE]

  if (identical(orientation, "horizontal")) {
    if (isTRUE(mean)) {
      p <- p + ggrepel::geom_text_repel(
        data = label_row,
        ggplot2::aes(x = .data[["mean"]], y = .data[[".category_id"]]),
        label = mean_label,
        color = pal$risk_premium,
        size = 3,
        nudge_x = 0.10,
        nudge_y = 0.45,
        box.padding = 0.35,
        point.padding = 0.30,
        min.segment.length = 0,
        segment.color = "grey65",
        segment.size = 0.25,
        max.overlaps = Inf,
        show.legend = FALSE
      )
    }
    if (isTRUE(median)) {
      p <- p + ggrepel::geom_text_repel(
        data = label_row,
        ggplot2::aes(x = .data[["median"]], y = .data[[".category_id"]]),
        label = median_label,
        color = pal$frequency,
        size = 3,
        nudge_x = 0.10,
        nudge_y = -0.45,
        box.padding = 0.35,
        point.padding = 0.30,
        min.segment.length = 0,
        segment.color = "grey65",
        segment.size = 0.25,
        max.overlaps = Inf,
        show.legend = FALSE
      )
    }
    if (!is.null(threshold)) {
      p <- p + ggrepel::geom_text_repel(
        data = data.frame(
          .x = threshold,
          .y = max(summary_data$.category_id) + 0.30,
          stringsAsFactors = FALSE
        ),
        ggplot2::aes(x = .data[[".x"]], y = .data[[".y"]]),
        label = paste0(threshold_label, ": ", format(threshold, big.mark = ",")),
        color = "firebrick",
        size = 2.8,
        nudge_x = 0.08,
        nudge_y = 0,
        direction = "x",
        box.padding = 0.30,
        point.padding = 0.25,
        min.segment.length = 0,
        segment.color = "grey70",
        segment.size = 0.22,
        max.overlaps = Inf,
        show.legend = FALSE
      )
    }
    return(p)
  }

  if (isTRUE(mean)) {
    p <- p + ggrepel::geom_text_repel(
      data = label_row,
      ggplot2::aes(x = .data[[".category_id"]], y = .data[["mean"]]),
    label = mean_label,
    color = pal$risk_premium,
    size = 3,
    nudge_x = 0.45,
    nudge_y = 0.10,
    box.padding = 0.35,
    point.padding = 0.30,
    min.segment.length = 0,
    segment.color = "grey65",
    segment.size = 0.25,
    max.overlaps = Inf,
    show.legend = FALSE
  )
  }
  if (isTRUE(median)) {
    p <- p + ggrepel::geom_text_repel(
      data = label_row,
      ggplot2::aes(x = .data[[".category_id"]], y = .data[["median"]]),
    label = median_label,
    color = pal$frequency,
    size = 3,
    nudge_x = -0.45,
    nudge_y = 0.10,
    box.padding = 0.35,
    point.padding = 0.30,
    min.segment.length = 0,
    segment.color = "grey65",
    segment.size = 0.25,
    max.overlaps = Inf,
    show.legend = FALSE
  )
  }
  if (!is.null(threshold)) {
    p <- p + ggrepel::geom_text_repel(
      data = data.frame(
        .x = max(summary_data$.category_id) + 0.30,
        .y = threshold,
        stringsAsFactors = FALSE
      ),
      ggplot2::aes(x = .data[[".x"]], y = .data[[".y"]]),
      label = paste0(threshold_label, ": ", format(threshold, big.mark = ",")),
      color = "firebrick",
      size = 2.8,
      nudge_x = 0,
      nudge_y = 0.08,
      direction = "y",
      box.padding = 0.30,
      point.padding = 0.25,
      min.segment.length = 0,
      segment.color = "grey70",
      segment.size = 0.22,
      max.overlaps = Inf,
      show.legend = FALSE
    )
  }
  p
}
