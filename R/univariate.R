#' Univariate analysis for discrete risk factors
#'
#' @description
#' Performs a univariate analysis for discrete risk factors in an insurance
#' portfolio. The following summary statistics are calculated:
#' \itemize{
#'   \item frequency = number of claims / exposure
#'   \item average severity = severity / number of claims
#'   \item risk premium = severity / exposure
#'   \item loss ratio = severity / premium
#'   \item average premium = premium / exposure
#' }
#'
#' @param df A `data.frame` with the insurance portfolio.
#' @param x Character vector: column(s) in `df` with the risk factor(s).
#' @param severity Character, column in `df` with claim amounts (default = NULL).
#' @param premium Character, column in `df` with premiums (default = NULL).
#' @param exposure Character, column in `df` with exposures (default = NULL).
#' @param nclaims Character, column in `df` with number of claims (default = NULL).
#' @param by Character vector of column(s) in `df` to group by in addition to `x`.
#'
#' @details
#' The function computes univariate statistics for discrete risk factors.
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
#' The function [univariate()] is deprecated as of version 0.7.6 and replaced by
#' [univariate_summary()]. In addition to the name change, the interface has also
#' changed:
#'
#' - `univariate()` used **non-standard evaluation (NSE)**, so column names could be
#'   passed unquoted (e.g. `x = area`).
#' - `univariate_summary()` uses **standard evaluation (SE)**, so column names must
#'   be passed as character strings (e.g. `x = "area"`).
#'
#' This makes the function easier to use in programmatic workflows.
#'
#' `univariate()` is still available for backward compatibility but will emit a
#' deprecation warning and will be removed in a future release.
#'
#'
#' @return A `data.table` of class `"univariate"` with summary statistics.
#'
#' @author Martin Haringa
#'
#' @importFrom data.table data.table
#'
#' @details
#' This function supersedes [univariate()], which relied on non-standard
#' evaluation (NSE). From version 0.7.6 onwards, you must supply column names
#' as **strings** (SE), which makes the function easier to use in programmatic
#' workflows.
#'
#' @examples
#' ## --- New usage (SE) ---
#' univariate_summary(MTPL2,
#'                    x = "area",
#'                    severity = "amount",
#'                    nclaims = "nclaims",
#'                    exposure = "exposure",
#'                    premium = "premium")
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
univariate_summary <- function(df, x, severity = NULL, nclaims = NULL,
                               exposure = NULL, premium = NULL, by = NULL) {

  .xvar <- x
  .xvar_out <- .xvar
  .by_out <- by

  if (is.null(severity) && is.null(nclaims) && is.null(exposure) &&
      is.null(premium)) {
    stop("You did not supply any of the required arguments.", call. = FALSE)
  }

  if (!all(.xvar %in% names(df))) {
    stop("Column(s) ", paste(.xvar, collapse = ", "), " not found in data.frame",
         call. = FALSE)
  }

  if (length(.xvar) > 1) {
    by <- c(.xvar[-1], by)
    .xvar <- .xvar[1]
  }

  # only keep the non-null arguments
  cols_ <- c(severity, nclaims, exposure, premium)
  cols_ <- cols_[!is.null(cols_)]

  if (length(cols_) == 0) {
    stop("Define at least one column for severity, nclaims, exposure or premium.",
         call. = FALSE)
  }

  BY <- c(.xvar, by)

  dt <- data.table::data.table(df)[, lapply(.SD, sum, na.rm = TRUE),
                                   by = BY, .SDcols = cols_]

  dt1 <- NULL
  if (!is.null(by)) {
    dt1 <- data.table::data.table(df)[, lapply(.SD, sum, na.rm = TRUE),
                                      by = .xvar, .SDcols = cols_]
  }

  dt  <- add_metrics(dt,  cols_, nclaims, exposure, severity, premium)
  if (!is.null(by)) {
    dt1 <- add_metrics(dt1, cols_, nclaims, exposure, severity, premium)
  }

  attr(dt, "xvar") <- .xvar_out
  attr(dt, "severity") <- severity
  attr(dt, "nclaims") <- nclaims
  attr(dt, "exposure") <- exposure
  attr(dt, "premium") <- premium
  attr(dt, "by") <- .by_out
  attr(dt, "dfby") <- as.data.frame(dt1)

  class(dt) <- c("univariate", class(df))
  return(dt)
}



#' @rdname univariate_summary
#' @description
#' [univariate()] is deprecated as of version 0.7.6.
#' Please use [univariate_summary()] instead with **standard evaluation** (SE),
#' i.e. column names as character strings.
#'
#' @export
univariate <- function(df, x, severity = NULL, nclaims = NULL, exposure = NULL,
                       premium = NULL, by = NULL) {

  lifecycle::deprecate_warn("0.7.6", "univariate()", "univariate_summary()")

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

  univariate_summary(df = df, x = .xvar, severity = .severity,
                     nclaims = .nclaims, exposure = .exposure,
                     premium = .premium, by = .by)
}



#' Automatically create a ggplot for objects obtained from univariate analysis
#'
#' @description
#' Takes an object produced by [univariate_summary()] (SE) or [univariate()]
#' (deprecated NSE interface) and plots the available statistics.
#'
#' @param object A `univariate` object produced by [univariate_summary()] or
#' [univariate()].
#' @param show_plots Numeric vector of plots to be shown (default is
#' `1:9`). There are nine available plots:
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
#' @param ncol Number of columns in output (default = 1).
#' @param background Show exposure as a background histogram (default = TRUE).
#' @param labels Show labels with the exposure (default = TRUE).
#' @param sort Sort (order) risk factor into descending order by exposure
#'   (default = FALSE).
#' @param sort_manual Sort risk factor into a custom order; character vector
#'   (default = NULL).
#' @param dec.mark Decimal mark; defaults to `","`.
#' @param color Color for points/lines (default = `"dodgerblue"`).
#' @param color_bg Color of background histogram (default = `"lightskyblue"`).
#' @param label_width Width of labels on the x-axis (default = 10).
#' @param coord_flip Flip cartesian coordinates (default = FALSE).
#' @param show_total Show line for total if `by` is used (default = FALSE).
#' @param total_color Color for total line (default = `"black"`).
#' @param total_name Legend name for total line (default = NULL).
#' @param rotate_angle Numeric value for angle of labels on the x-axis (degrees).
#' @param custom_theme List with customized theme options.
#' @param remove_underscores Logical; remove underscores from labels
#'   (default = FALSE).
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
#' x <- univariate_summary(MTPL2,
#'                         x = "area",
#'                         severity = "amount",
#'                         nclaims = "nclaims",
#'                         exposure = "exposure")
#' autoplot(x)
#'
#' ## --- Deprecated usage (NSE) ---
#' x_old <- univariate(MTPL2, x = area, severity = amount,
#'                     nclaims = nclaims, exposure = exposure)
#' autoplot(x_old)
#'
#' @export
autoplot.univariate <- function(object, show_plots = 1:9, ncol = 1,
                                background = TRUE, labels = TRUE,
                                sort = FALSE, sort_manual = NULL,
                                dec.mark = ",", color = "dodgerblue",
                                color_bg = "lightskyblue", label_width = 10,
                                coord_flip = FALSE, show_total = FALSE,
                                total_color = NULL, total_name = NULL,
                                rotate_angle = NULL,
                                custom_theme = NULL,
                                remove_underscores = FALSE, ...) {

  xvar     <- attr(object, "xvar")
  nclaims  <- attr(object, "nclaims")
  exposure <- attr(object, "exposure")
  severity <- attr(object, "severity")
  premium  <- attr(object, "premium")
  by       <- as.character(attr(object, "by"))

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

  # Build mapping (always 9 positions, missing values become NA)
  p_plots <- c(
    "frequency",        # 1
    "average_severity", # 2
    "risk_premium",     # 3
    "loss_ratio",       # 4
    "average_premium",  # 5
    if (is.null(exposure) || exposure == "") NA_character_ else exposure, # 6
    if (is.null(severity) || severity == "") NA_character_ else severity, # 7
    if (is.null(nclaims)  || nclaims  == "") NA_character_ else nclaims,  # 8
    if (is.null(premium)  || premium  == "") NA_character_ else premium   # 9
  )

  # Keep only allowed plot indices (1–9)
  plots_allowed <- show_plots[show_plots %in% 1:9]

  # Extract variable names for selected indices
  valid_vars <- p_plots[plots_allowed]

  # Check which variables actually exist in df (and are not NA)
  vars_exist <- !is.na(valid_vars) & valid_vars %in% names(df)

  # Final list of plots to create
  create_plots <- plots_allowed[vars_exist]

  # waarschuwing voor ontbrekende
  if (any(!vars_exist)) {
    missing_idx <- plots_allowed[!vars_exist]
    message("Ignoring plots ", paste(missing_idx, collapse = ", "),
            ": required column(s) not available in object")
  }

  if (length(create_plots) == 0) {
    stop("No valid plots could be created: required columns not found.",
         call. = FALSE)
  }

  # plot_defs (zoals in jouw code)
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
      plots[[paste0("p", i)]] <- def$fun(background, df, dfby, xvar,
                                         def$var, def$lab, def$denom,
                                         color_bg, color, sep_mark, by,
                                         labels, sort_manual, label_width,
                                         show_total, total_color,
                                         total_name, remove_underscores)
    } else {
      plots[[paste0("p", i)]] <- def$fun(df, xvar, def$var, color_bg,
                                         sep_mark, coord_flip)
    }
  }

  plot_out <- patchwork::wrap_plots(plots, ncol = ncol, guides = "collect")

  if (!is.null(rotate_angle)) {
    plot_out <- plot_out +
      ggplot2::theme(axis.text.x = element_text(angle = rotate_angle,
                                                vjust = 0.5, hjust = 1))
  }

  plot_out
}
