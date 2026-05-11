#' Factor analysis for discrete risk factors
#'
#' @description
#' Performs a factor analysis for discrete risk factors in an insurance
#' portfolio. The following summary statistics are calculated:
#' \itemize{
#'   \item frequency = number of claims / exposure
#'   \item average severity = severity / number of claims
#'   \item risk premium = severity / exposure
#'   \item loss ratio = severity / premium
#'   \item average premium = premium / exposure
#' }
#'
#' @param data A `data.frame` with the insurance portfolio.
#' @param risk_factors Character vector: column(s) in `data` with the risk
#'   factor(s).
#' @param claim_amount Character, column in `data` with claim amounts
#'   (default = NULL).
#' @param claim_count Character, column in `data` with number of claims
#'   (default = NULL).
#' @param premium Character, column in `data` with premiums (default = NULL).
#' @param exposure Character, column in `data` with exposures (default = NULL).
#' @param group_by Character vector of column(s) in `data` to group by in
#'   addition to `risk_factors`.
#' @param df,x,severity,nclaims,by Deprecated argument names. Use `data`,
#'   `risk_factors`, `claim_amount`, `claim_count`, and `group_by` instead.
#'
#' @details
#' The function computes summary statistics for discrete risk factors.
#'
#' - **Frequency**: number of claims / exposure
#' - **Average severity**: severity / number of claims
#' - **Risk premium**: severity / exposure
#' - **Loss ratio**: severity / premium
#' - **Average premium**: premium / exposure
#'
#' If one or more input arguments are not specified, the related statistics are
#' omitted from the results.
#'
#' ## Migration from `univariate()`
#'
#' The function [univariate()] is deprecated as of version 0.8.0 and replaced by
#' [factor_analysis()]. In addition to the name change, the interface has also
#' changed:
#'
#' - `univariate()` used **non-standard evaluation (NSE)**, so column names could be
#'   passed unquoted (e.g. `x = area`).
#' - `factor_analysis()` uses **standard evaluation (SE)**, so column names must
#'   be passed as character strings (e.g. `x = "area"`).
#'
#' This makes the function easier to use in programmatic workflows.
#'
#' `univariate()` is still available for backward compatibility but will emit a
#' deprecation warning and will be removed in a future release.
#'
#' @return An object of class `"factor_analysis"` and `"univariate"` with
#' summary statistics.
#'
#' @author Martin Haringa
#'
#' @importFrom data.table data.table
#'
#' @examples
#' ## --- New usage (SE) ---
#' factor_analysis(MTPL2,
#'                 risk_factors = "area",
#'                 claim_amount = "amount",
#'                 claim_count = "nclaims",
#'                 exposure = "exposure",
#'                 premium = "premium")
#'
#' ## --- Deprecated usage (NSE) ---
#' univariate(MTPL2,
#'            x = area,
#'            severity = amount,
#'            nclaims = nclaims,
#'            exposure = exposure,
#'            premium = premium)
#'
#' @export
factor_analysis <- function(data = NULL, risk_factors = NULL,
                            claim_amount = NULL, claim_count = NULL,
                            exposure = NULL, premium = NULL, group_by = NULL,
                            df = NULL, x = NULL, severity = NULL,
                            nclaims = NULL, by = NULL) {
  args <- resolve_factor_analysis_args(
    data = data,
    risk_factors = risk_factors,
    claim_amount = claim_amount,
    claim_count = claim_count,
    group_by = group_by,
    df = df,
    x = x,
    severity = severity,
    nclaims = nclaims,
    by = by
  )

  data <- args$data
  risk_factors <- args$risk_factors
  claim_amount <- args$claim_amount
  claim_count <- args$claim_count
  group_by <- args$group_by

  validate_factor_analysis_args(
    data = data,
    risk_factors = risk_factors,
    claim_amount = claim_amount,
    claim_count = claim_count,
    exposure = exposure,
    premium = premium,
    group_by = group_by
  )

  .xvar <- risk_factors
  .xvar_out <- .xvar
  .by_out <- group_by

  if (length(.xvar) > 1) {
    group_by <- c(.xvar[-1], group_by)
    .xvar <- .xvar[1]
  }

  cols_ <- c(claim_amount, claim_count, exposure, premium)
  cols_ <- cols_[!is.null(cols_)]

  if (length(cols_) == 0) {
    stop("Define at least one column for claim_amount, claim_count, exposure or premium.",
         call. = FALSE)
  }

  BY <- c(.xvar, group_by)

  dt <- data.table::data.table(data)[, lapply(.SD, sum, na.rm = TRUE),
                                   by = BY, .SDcols = cols_]

  dt1 <- NULL
  if (!is.null(group_by)) {
    dt1 <- data.table::data.table(data)[, lapply(.SD, sum, na.rm = TRUE),
                                      by = .xvar, .SDcols = cols_]
  }

  dt  <- add_metrics(dt,  cols_, claim_count, exposure, claim_amount, premium)
  if (!is.null(group_by)) {
    dt1 <- add_metrics(dt1, cols_, claim_count, exposure, claim_amount, premium)
  }

  attr(dt, "xvar") <- .xvar_out
  attr(dt, "severity") <- claim_amount
  attr(dt, "claim_amount") <- claim_amount
  attr(dt, "nclaims") <- claim_count
  attr(dt, "claim_count") <- claim_count
  attr(dt, "exposure") <- exposure
  attr(dt, "premium") <- premium
  attr(dt, "by") <- .by_out
  attr(dt, "group_by") <- .by_out
  attr(dt, "dfby") <- as.data.frame(dt1)

  dt <- as.data.frame(dt)
  class(dt) <- c("factor_analysis", "univariate", "data.frame")
  return(dt)
}


resolve_factor_analysis_args <- function(data, risk_factors, claim_amount,
                                         claim_count, group_by, df, x,
                                         severity, nclaims, by) {
  if (!is.null(df)) {
    if (!is.null(data)) {
      stop("Use only one of `data` and deprecated `df`.", call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "factor_analysis(df)",
                              "factor_analysis(data)")
    data <- df
  }
  if (!is.null(x)) {
    if (!is.null(risk_factors)) {
      stop("Use only one of `risk_factors` and deprecated `x`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "factor_analysis(x)",
                              "factor_analysis(risk_factors)")
    risk_factors <- x
  }
  if (!is.null(severity)) {
    if (!is.null(claim_amount)) {
      stop("Use only one of `claim_amount` and deprecated `severity`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "factor_analysis(severity)",
                              "factor_analysis(claim_amount)")
    claim_amount <- severity
  }
  if (!is.null(nclaims)) {
    if (!is.null(claim_count)) {
      stop("Use only one of `claim_count` and deprecated `nclaims`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "factor_analysis(nclaims)",
                              "factor_analysis(claim_count)")
    claim_count <- nclaims
  }
  if (!is.null(by)) {
    if (!is.null(group_by)) {
      stop("Use only one of `group_by` and deprecated `by`.", call. = FALSE)
    }
    lifecycle::deprecate_warn("0.9.0", "factor_analysis(by)",
                              "factor_analysis(group_by)")
    group_by <- by
  }

  list(
    data = data,
    risk_factors = risk_factors,
    claim_amount = claim_amount,
    claim_count = claim_count,
    group_by = group_by
  )
}


validate_factor_analysis_args <- function(data, risk_factors,
                                          claim_amount = NULL,
                                          claim_count = NULL,
                                          exposure = NULL,
                                          premium = NULL,
                                          group_by = NULL) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(risk_factors) || length(risk_factors) == 0L ||
      anyNA(risk_factors)) {
    stop("`risk_factors` must be a non-empty character vector.", call. = FALSE)
  }

  optional_args <- list(
    claim_amount = claim_amount,
    claim_count = claim_count,
    exposure = exposure,
    premium = premium,
    group_by = group_by
  )

  for (nm in names(optional_args)) {
    value <- optional_args[[nm]]
    if (!is.null(value) && (!is.character(value) || anyNA(value))) {
      stop("`", nm, "` must be NULL or a character vector.", call. = FALSE)
    }
  }

  if (is.null(claim_amount) && is.null(claim_count) && is.null(exposure) &&
      is.null(premium)) {
    stop("You did not supply any of the required arguments.", call. = FALSE)
  }

  required_cols <- unique(c(risk_factors, claim_amount, claim_count, exposure,
                            premium, group_by))
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Column(s) not found in `data`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  metric_cols <- unique(c(claim_amount, claim_count, exposure, premium))
  non_numeric_cols <- metric_cols[!vapply(data[metric_cols], is.numeric, logical(1))]
  if (length(non_numeric_cols) > 0) {
    stop(
      "Metric column(s) must be numeric: ",
      paste(non_numeric_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Deprecated alias for `factor_analysis()`
#'
#' @description
#' `univariate()` is deprecated as of version 0.8.0. Use
#' [factor_analysis()] instead.
#'
#' @param df A `data.frame` with the insurance portfolio.
#' @param x Column name or expression with the risk factor.
#' @param severity Column name or expression with claim amounts.
#' @param nclaims Column name or expression with claim counts.
#' @param exposure Column name or expression with exposures.
#' @param premium Column name or expression with premiums.
#' @param by Optional grouping column name or expression.
#'
#' @return See [factor_analysis()].
#'
#' @export
#' @keywords internal
univariate <- function(df, x, severity = NULL, nclaims = NULL, exposure = NULL,
                       premium = NULL, by = NULL) {

  lifecycle::deprecate_warn("0.8.0", "univariate()", "factor_analysis()")

  nse_se_input <- function(arg) {
    expr <- substitute(arg)
    if (missing(arg) || is.null(expr) || deparse(expr) == "NULL") {
      return(NULL)
    }
    if (is.call(expr) && expr[[1]] == quote(vec_ext)) {
      return(as.character(eval.parent(expr[[2]])))
    }
    if (is.character(expr)) {
      return(arg)
    }
    if (is.call(expr) && (expr[[1]] == quote(list) ||
                          expr[[1]] == quote(c) ||
                          expr[[1]] == quote(.))) {
      return(as.character(expr[-1L]))
    }
    return(deparse(expr))
  }

  .xvar <- eval.parent(substitute(nse_se_input(x)))
  .severity <- eval.parent(substitute(nse_se_input(severity)))
  .nclaims <- eval.parent(substitute(nse_se_input(nclaims)))
  .exposure <- eval.parent(substitute(nse_se_input(exposure)))
  .premium <- eval.parent(substitute(nse_se_input(premium)))
  .by <- eval.parent(substitute(nse_se_input(by)))

  factor_analysis(
    df = df,
    x = .xvar,
    severity = .severity,
    nclaims = .nclaims,
    exposure = .exposure,
    premium = .premium,
    by = .by
  )
}



#' Automatically create a ggplot for objects obtained from factor analysis
#'
#' @description
#' Takes an object produced by [factor_analysis()] or [univariate()]
#' (deprecated NSE interface) and plots the available statistics.
#'
#' @param object A `factor_analysis` or `univariate` object produced by
#' [factor_analysis()] or [univariate()].
#' @param metrics Numeric or character vector specifying which metrics to plot
#'   (default is all available metrics). The numeric positions are:
#' \itemize{
#'   \item{1. Frequency (`nclaims / exposure`)}
#'   \item{2. Average severity (`severity / nclaims`)}
#'   \item{3. Risk premium (`severity / exposure`)}
#'   \item{4. Loss ratio (`severity / premium`)}
#'   \item{5. Average premium (`premium / exposure`)}
#'   \item{6. Exposure}
#'   \item{7. Severity}
#'   \item{8. Number of claims}
#'   \item{9. Premium}
#' }
#' Character values can be `"frequency"`, `"average_severity"`,
#' `"risk_premium"`, `"loss_ratio"`, `"average_premium"`, `"exposure"`,
#' `"claim_amount"`, `"claim_count"`, and `"premium"`.
#' @param ncol Number of columns in output (default = 1).
#' @param show_exposure Show exposure as background bars behind line plots
#'   (default = TRUE).
#' @param show_exposure_labels Show labels with the exposure bars
#'   (default = TRUE).
#' @param sort_by_exposure Sort risk factor levels into descending order by
#'   exposure (default = FALSE).
#' @param level_order Custom order for risk factor levels; character vector
#'   (default = NULL).
#' @param decimal_mark Decimal mark; defaults to `","`.
#' @param line_color Optional override for line/point color.
#'   If NULL (default), colors are taken from the internal palette.
#'   If specified, the chosen color is applied to all line-based plots.
#' @param bar_fill Optional override for background bar color.
#'   If NULL (default), the background color is taken from the internal palette.
#'   If specified, the chosen color is applied to all background bars.
#' @param label_width Width of labels on the x-axis (default = 10).
#' @param flip_bars Logical. If `TRUE`, flip cartesian coordinates for bar plots
#'   (metrics 6 to 9). This option does not affect the line-based plots for
#'   metrics 1 to 5.
#' @param show_total Show line for total if `by` is used (default = FALSE).
#' @param total_color Color for total line (default = `"black"`).
#' @param total_name Legend name for total line (default = NULL).
#' @param rotate_angle Numeric value for angle of labels on the x-axis (degrees).
#' @param custom_theme List with customized theme options.
#' @param remove_underscores Logical; remove underscores from labels
#'   (default = FALSE).
#' @param compact_x_axis Logical. When \code{TRUE} and \code{ncol == 1},
#'   x-axis components are removed from all plots except the last one.
#'   The following elements are suppressed:
#'   \itemize{
#'     \item \code{axis.title.x}
#'     \item \code{axis.text.x}
#'     \item \code{axis.ticks.x}
#'   }
#'   This prevents duplicated x-axes in vertically stacked patchwork plots.
#'   Defaults to \code{TRUE}.
#' @param show_plots Deprecated. Use `metrics` instead.
#' @param background Deprecated alias for `show_exposure`.
#' @param labels Deprecated alias for `show_exposure_labels`.
#' @param sort Deprecated alias for `sort_by_exposure`.
#' @param sort_manual Deprecated alias for `level_order`.
#' @param dec.mark Deprecated alias for `decimal_mark`.
#' @param color Deprecated alias for `line_color`.
#' @param color_bg Deprecated alias for `bar_fill`.
#' @param coord_flip Deprecated alias for `flip_bars`.
#' @param remove_x_elements Deprecated alias for `compact_x_axis`.
#' @param ... Other plotting parameters.
#'
#' @import patchwork
#' @import ggplot2
#'
#' @author Marc Haine, Martin Haringa
#'
#' @return A `ggplot2` object.
#'
#' @examples
#' ## --- New usage (SE, recommended) ---
#' x <- factor_analysis(MTPL2,
#'                      x = "area",
#'                      severity = "amount",
#'                      nclaims = "nclaims",
#'                      exposure = "exposure")
#' autoplot(x)
#'
#' ## --- Deprecated usage (NSE) ---
#' x_old <- univariate(MTPL2, x = area, severity = amount,
#'                     nclaims = nclaims, exposure = exposure)
#' autoplot(x_old)
#'
#' @export
autoplot.factor_analysis <- function(object,
                                     metrics = NULL,
                                     ncol = 1,
                                     show_exposure = TRUE,
                                     show_exposure_labels = TRUE,
                                     sort_by_exposure = FALSE,
                                     level_order = NULL,
                                     decimal_mark = ",",
                                     line_color = NULL,
                                     bar_fill = NULL,
                                     label_width = 50,
                                     flip_bars = FALSE,
                                     show_total = FALSE,
                                     total_color = NULL,
                                     total_name = NULL,
                                     rotate_angle = NULL,
                                     custom_theme = NULL,
                                     remove_underscores = FALSE,
                                     compact_x_axis = TRUE,
                                     show_plots = NULL,
                                     background = NULL,
                                     labels = NULL,
                                     sort = NULL,
                                     sort_manual = NULL,
                                     dec.mark = NULL,
                                     color = NULL,
                                     color_bg = NULL,
                                     coord_flip = NULL,
                                     remove_x_elements = NULL,
                                     ...) {

  if (!inherits(object, "factor_analysis") && !inherits(object, "univariate")) {
    stop("`object` must be a factor_analysis object.", call. = FALSE)
  }

  xvar     <- attr(object, "xvar")
  nclaims  <- attr(object, "nclaims")
  exposure <- attr(object, "exposure")
  severity <- attr(object, "severity")
  premium  <- attr(object, "premium")
  by       <- attr(object, "by")

  if (length(by) > 1) {
    stop(
      "`autoplot()` supports at most one `by` variable. ",
      "Create separate factor_analysis objects or call `autoplot()` with a single `by` variable.",
      call. = FALSE
    )
  }
  by <- as.character(by)

  args <- resolve_autoplot_factor_analysis_args(
    metrics = metrics,
    show_exposure = show_exposure,
    show_exposure_labels = show_exposure_labels,
    sort_by_exposure = sort_by_exposure,
    level_order = level_order,
    decimal_mark = decimal_mark,
    line_color = line_color,
    bar_fill = bar_fill,
    flip_bars = flip_bars,
    compact_x_axis = compact_x_axis,
    metrics_supplied = !missing(metrics),
    show_exposure_supplied = !missing(show_exposure),
    show_exposure_labels_supplied = !missing(show_exposure_labels),
    sort_by_exposure_supplied = !missing(sort_by_exposure),
    level_order_supplied = !missing(level_order),
    decimal_mark_supplied = !missing(decimal_mark),
    line_color_supplied = !missing(line_color),
    bar_fill_supplied = !missing(bar_fill),
    flip_bars_supplied = !missing(flip_bars),
    compact_x_axis_supplied = !missing(compact_x_axis),
    show_plots = show_plots,
    background = background,
    labels = labels,
    sort = sort,
    sort_manual = sort_manual,
    dec.mark = dec.mark,
    color = color,
    color_bg = color_bg,
    coord_flip = coord_flip,
    remove_x_elements = remove_x_elements
  )
  metrics <- args$metrics
  show_exposure <- args$show_exposure
  show_exposure_labels <- args$show_exposure_labels
  sort_by_exposure <- args$sort_by_exposure
  level_order <- args$level_order
  decimal_mark <- args$decimal_mark
  line_color <- args$line_color
  bar_fill <- args$bar_fill
  flip_bars <- args$flip_bars
  compact_x_axis <- args$compact_x_axis

  if (is.null(metrics)) {
    metrics <- 1:9
  }

  plots_allowed <- resolve_factor_analysis_metrics(metrics)

  if (length(xvar) > 1) {
    message(xvar, " has more than one element. Only the first is shown.")
    xvar <- xvar[1]
  }
  if (length(by) == 0) by <- "NULL"

  df   <- if (by == "NULL") object else attr(object, "dfby")
  dfby <- if (by == "NULL") attr(object, "dfby") else object

  if (!is.factor(df[[xvar]])) df[[xvar]] <- factor(df[[xvar]])
  if (by != "NULL" && !is.factor(dfby[[by]])) dfby[[by]] <- factor(dfby[[by]])

  if (is.null(total_color)) total_color <- "black"

  sep_mark <- separation_mark(decimal_mark)

  pal <- list(
    frequency        = "#2C7FB8",
    average_severity = "#41AB5D",
    risk_premium     = "#F28E2B",
    loss_ratio       = "#8C6BB1",
    average_premium  = "#2CB1A1",
    bg_bar           = "#E6E6E6"
  )

  final_bg <- if (!is.null(bar_fill)) bar_fill else pal$bg_bar

  resolve_line_color <- function(varname) {
    if (!is.null(line_color)) return(line_color)
    if (!is.null(pal[[varname]])) return(pal[[varname]])
    "#2C7FB8"
  }

  grid_theme <- ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.grid.major = ggplot2::element_line(color = "#F2F2F2", linewidth = 0.4),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border     = ggplot2::element_blank(),
    axis.text.y.right  = ggplot2::element_text(color = "#9E9E9E", size = 8),
    axis.title.y.right = ggplot2::element_text(color = "#9E9E9E", size = 9),
    axis.title.y = ggplot2::element_text(size = 10),
    axis.line.x = ggplot2::element_line(colour = "grey55", linewidth = 0.3),
    axis.line.y.left = ggplot2::element_line(colour = "grey55", linewidth = 0.3),
    axis.ticks = ggplot2::element_line(colour = "grey55", linewidth = 0.3),
    axis.ticks.y.right = ggplot2::element_line(colour = "grey75", linewidth = 0.25)
  )

  p_plots <- c(
    "frequency",
    "average_severity",
    "risk_premium",
    "loss_ratio",
    "average_premium",
    if (is.null(exposure) || exposure == "") NA_character_ else exposure,
    if (is.null(severity) || severity == "") NA_character_ else severity,
    if (is.null(nclaims)  || nclaims  == "") NA_character_ else nclaims,
    if (is.null(premium)  || premium  == "") NA_character_ else premium
  )

  valid_vars <- p_plots[plots_allowed]
  vars_exist <- !is.na(valid_vars) & valid_vars %in% names(df)
  create_plots <- plots_allowed[vars_exist]

  if (any(!vars_exist)) {
    missing_idx <- plots_allowed[!vars_exist]
    message("Ignoring plots ", paste(missing_idx, collapse = ", "),
            ": required column(s) not available in object")
  }

  if (length(create_plots) == 0) {
    stop("No valid plots could be created: required columns not found.",
         call. = FALSE)
  }

  plot_defs <- list(
    "1" = list(var = "frequency", lab = "Frequency", denom = exposure,
               fun = ggbarline),
    "2" = list(var = "average_severity", lab = "Average severity",
               denom = nclaims, fun = ggbarline),
    "3" = list(var = "risk_premium", lab = "Risk premium",
               denom = exposure, fun = ggbarline),
    "4" = list(var = "loss_ratio", lab = "Loss ratio",
               denom = premium, fun = ggbarline),
    "5" = list(var = "average_premium", lab = "Average premium",
               denom = exposure, fun = ggbarline),
    "6" = list(var = exposure, lab = exposure, denom = NULL, fun = ggbar),
    "7" = list(var = severity, lab = severity, denom = NULL, fun = ggbar),
    "8" = list(var = nclaims, lab = nclaims, denom = NULL, fun = ggbar),
    "9" = list(var = premium, lab = premium, denom = NULL, fun = ggbar)
  )

  if (isTRUE(sort_by_exposure) && is.null(level_order) &&
      !is.null(exposure) && exposure %in% names(df)) {
    exposure_order <- stats::aggregate(
      df[[exposure]],
      by = list(level = df[[xvar]]),
      FUN = sum,
      na.rm = TRUE
    )
    exposure_order <- exposure_order[order(exposure_order$x, decreasing = TRUE), ]
    level_order <- as.character(exposure_order$level)
  }

  plots <- list()
  for (i in create_plots) {
    def <- plot_defs[[as.character(i)]]

    if (identical(def$fun, ggbarline)) {
      this_line_color <- resolve_line_color(def$var)

      p <- def$fun(
        show_exposure, df, dfby, xvar,
        def$var, def$lab, def$denom,
        final_bg, this_line_color, sep_mark, by,
        show_exposure_labels, level_order, label_width,
        show_total, total_color,
        total_name, remove_underscores
      )
    } else {
      p <- def$fun(df, xvar, def$var, final_bg, sep_mark, flip_bars)
    }

    p <- p + grid_theme
    plots[[paste0("p", i)]] <- p
  }

  if (isTRUE(compact_x_axis) && isTRUE(ncol == 1) && length(plots) > 1) {
    strip_x <- function(p) {
      p + ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x  = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
    }

    last_idx <- length(plots)
    plots[-last_idx] <- lapply(plots[-last_idx], strip_x)
  }

  plot_out <- patchwork::wrap_plots(plots, ncol = ncol, guides = "collect")

  if (!is.null(rotate_angle)) {
    plot_out <- plot_out +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = rotate_angle,
                                                         hjust = 1))
  }

  plot_out
}


resolve_factor_analysis_metrics <- function(metrics) {
  if (is.numeric(metrics)) {
    metrics <- metrics[metrics %in% 1:9]
    return(as.integer(metrics))
  }

  if (!is.character(metrics)) {
    stop("`metrics` must be a numeric or character vector.", call. = FALSE)
  }

  metric_lookup <- c(
    frequency = 1L,
    average_severity = 2L,
    risk_premium = 3L,
    loss_ratio = 4L,
    average_premium = 5L,
    exposure = 6L,
    claim_amount = 7L,
    severity = 7L,
    claim_count = 8L,
    nclaims = 8L,
    premium = 9L
  )

  unknown <- setdiff(metrics, names(metric_lookup))
  if (length(unknown) > 0) {
    stop(
      "Unknown metric(s): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  unname(metric_lookup[metrics])
}


resolve_autoplot_factor_analysis_args <- function(metrics = NULL,
                                                  show_exposure = TRUE,
                                                  show_exposure_labels = TRUE,
                                                  sort_by_exposure = FALSE,
                                                  level_order = NULL,
                                                  decimal_mark = ",",
                                                  line_color = NULL,
                                                  bar_fill = NULL,
                                                  flip_bars = FALSE,
                                                  compact_x_axis = TRUE,
                                                  metrics_supplied = FALSE,
                                                  show_exposure_supplied = FALSE,
                                                  show_exposure_labels_supplied = FALSE,
                                                  sort_by_exposure_supplied = FALSE,
                                                  level_order_supplied = FALSE,
                                                  decimal_mark_supplied = FALSE,
                                                  line_color_supplied = FALSE,
                                                  bar_fill_supplied = FALSE,
                                                  flip_bars_supplied = FALSE,
                                                  compact_x_axis_supplied = FALSE,
                                                  show_plots = NULL,
                                                  background = NULL,
                                                  labels = NULL,
                                                  sort = NULL,
                                                  sort_manual = NULL,
                                                  dec.mark = NULL,
                                                  color = NULL,
                                                  color_bg = NULL,
                                                  coord_flip = NULL,
                                                  remove_x_elements = NULL) {
  if (!is.null(show_plots)) {
    if (metrics_supplied) {
      stop("Use only one of `metrics` and deprecated `show_plots`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "autoplot(show_plots)",
      "autoplot(metrics)"
    )
    metrics <- show_plots
  }
  if (!is.null(background)) {
    if (show_exposure_supplied) {
      stop("Use only one of `show_exposure` and deprecated `background`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "autoplot(background)",
      "autoplot(show_exposure)"
    )
    show_exposure <- background
  }
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
  if (!is.null(sort)) {
    if (sort_by_exposure_supplied) {
      stop("Use only one of `sort_by_exposure` and deprecated `sort`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "autoplot(sort)",
      "autoplot(sort_by_exposure)"
    )
    sort_by_exposure <- sort
  }
  if (!is.null(sort_manual)) {
    if (level_order_supplied) {
      stop("Use only one of `level_order` and deprecated `sort_manual`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "autoplot(sort_manual)",
      "autoplot(level_order)"
    )
    level_order <- sort_manual
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
  if (!is.null(color)) {
    if (line_color_supplied) {
      stop("Use only one of `line_color` and deprecated `color`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "autoplot(color)",
      "autoplot(line_color)"
    )
    line_color <- color
  }
  if (!is.null(color_bg)) {
    if (bar_fill_supplied) {
      stop("Use only one of `bar_fill` and deprecated `color_bg`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "autoplot(color_bg)",
      "autoplot(bar_fill)"
    )
    bar_fill <- color_bg
  }
  if (!is.null(coord_flip)) {
    if (flip_bars_supplied) {
      stop("Use only one of `flip_bars` and deprecated `coord_flip`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "autoplot(coord_flip)",
      "autoplot(flip_bars)"
    )
    flip_bars <- coord_flip
  }
  if (!is.null(remove_x_elements)) {
    if (compact_x_axis_supplied) {
      stop("Use only one of `compact_x_axis` and deprecated `remove_x_elements`.",
           call. = FALSE)
    }
    lifecycle::deprecate_warn(
      "0.9.0",
      "autoplot(remove_x_elements)",
      "autoplot(compact_x_axis)"
    )
    compact_x_axis <- remove_x_elements
  }

  list(
    metrics = metrics,
    show_exposure = show_exposure,
    show_exposure_labels = show_exposure_labels,
    sort_by_exposure = sort_by_exposure,
    level_order = level_order,
    decimal_mark = decimal_mark,
    line_color = line_color,
    bar_fill = bar_fill,
    flip_bars = flip_bars,
    compact_x_axis = compact_x_axis
  )
}


#' @export
autoplot.univariate <- function(object, ...) {
  autoplot.factor_analysis(object, ...)
}
