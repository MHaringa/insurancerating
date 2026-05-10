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
#' @param metrics Numeric vector specifying which metrics to plot (default is
#' `1:9`). There are nine available metrics:
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
#' @param show_plots Deprecated. Use `metrics` instead.
#' @param ncol Number of columns in output (default = 1).
#' @param background Show exposure as a background histogram (default = TRUE).
#' @param labels Show labels with the exposure (default = TRUE).
#' @param sort Sort (order) risk factor into descending order by exposure
#'   (default = FALSE).
#' @param sort_manual Sort risk factor into a custom order; character vector
#'   (default = NULL).
#' @param dec.mark Decimal mark; defaults to `","`.
#' @param color Optional override for line/point color.
#'   If NULL (default), colors are taken from the internal palette.
#'   If specified, the chosen color is applied to all line-based plots.
#' @param color_bg Optional override for background bar color.
#'   If NULL (default), the background color is taken from the internal palette.
#'   If specified, the chosen color is applied to all background bars.
#' @param label_width Width of labels on the x-axis (default = 10).
#' @param coord_flip Logical. If `TRUE`, flip cartesian coordinates for bar plots
#'   (metrics 6 to 9). This option does not affect the line-based plots for
#'   metrics 1 to 5.
#' @param show_total Show line for total if `by` is used (default = FALSE).
#' @param total_color Color for total line (default = `"black"`).
#' @param total_name Legend name for total line (default = NULL).
#' @param rotate_angle Numeric value for angle of labels on the x-axis (degrees).
#' @param custom_theme List with customized theme options.
#' @param remove_underscores Logical; remove underscores from labels
#'   (default = FALSE).
#' @param remove_x_elements Logical. When \code{TRUE} and \code{ncol == 1},
#'   x-axis components are removed from all plots except the last one.
#'   The following elements are suppressed:
#'   \itemize{
#'     \item \code{axis.title.x}
#'     \item \code{axis.text.x}
#'     \item \code{axis.ticks.x}
#'   }
#'   This prevents duplicated x-axes in vertically stacked patchwork plots.
#'   Defaults to \code{TRUE}.
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
                                     show_plots = NULL,
                                     ncol = 1,
                                     background = TRUE,
                                     labels = TRUE,
                                     sort = FALSE,
                                     sort_manual = NULL,
                                     dec.mark = ",",
                                     color = NULL,
                                     color_bg = NULL,
                                     label_width = 50,
                                     coord_flip = FALSE,
                                     show_total = FALSE,
                                     total_color = NULL,
                                     total_name = NULL,
                                     rotate_angle = NULL,
                                     custom_theme = NULL,
                                     remove_underscores = FALSE,
                                     remove_x_elements = TRUE,
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

  # backward compatibility
  if (!is.null(show_plots) && !is.null(metrics)) {
    stop("Use either `metrics` or `show_plots`, not both.", call. = FALSE)
  }

  if (!is.null(show_plots)) {
    lifecycle::deprecate_warn(
      when = "0.9.0",
      what = "autoplot(show_plots = )",
      with = "autoplot(metrics = )"
    )
    metrics <- show_plots
  }

  if (is.null(metrics)) {
    metrics <- 1:9
  }

  plots_allowed <- metrics[metrics %in% 1:9]

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

  sep_mark <- separation_mark(dec.mark)

  pal <- list(
    frequency        = "#2C7FB8",
    average_severity = "#41AB5D",
    risk_premium     = "#F28E2B",
    loss_ratio       = "#8C6BB1",
    average_premium  = "#2CB1A1",
    bg_bar           = "#E6E6E6"
  )

  final_bg <- if (!is.null(color_bg)) color_bg else pal$bg_bar

  resolve_line_color <- function(varname) {
    if (!is.null(color)) return(color)
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
    axis.title.y = ggplot2::element_text(size = 10)
  )

  color_bg <- pal$bg_bar

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

  plots <- list()
  for (i in create_plots) {
    def <- plot_defs[[as.character(i)]]

    if (identical(def$fun, ggbarline)) {
      this_line_color <- resolve_line_color(def$var)

      p <- def$fun(
        background, df, dfby, xvar,
        def$var, def$lab, def$denom,
        final_bg, this_line_color, sep_mark, by,
        labels, sort_manual, label_width,
        show_total, total_color,
        total_name, remove_underscores
      )
    } else {
      p <- def$fun(df, xvar, def$var, final_bg, sep_mark, coord_flip)
    }

    p <- p + grid_theme
    plots[[paste0("p", i)]] <- p
  }

  if (isTRUE(remove_x_elements) && isTRUE(ncol == 1) && length(plots) > 1) {
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


#' @export
autoplot.univariate <- function(object, ...) {
  autoplot.factor_analysis(object, ...)
}
