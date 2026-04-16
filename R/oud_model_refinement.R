#' #' Restrict coefficients in the model
#' #'
#' #' @description `r lifecycle::badge('experimental')`
#' #'  Add restrictions, like a bonus-malus structure, on the risk
#' #'  factors used in the model. `add_restriction()` must always be followed
#' #'  by `refit_glm()`.
#' #'
#' #' @author Martin Haringa
#' #'
#' #' @details Although restrictions could be applied either to the frequency or
#' #'   the severity model, it is more appropriate to impose the restrictions
#' #'   on the premium model. This can be achieved by calculating the pure
#' #'   premium for each record (i.e. expected number of claims times the expected
#' #'   claim amount), then fitting an "unrestricted" Gamma GLM to the pure
#' #'   premium,and then imposing the restrictions in a final "restricted" Gamma
#' #'   GLM.
#' #'
#' #' @param model object of class glm/restricted
#' #' @param restrictions data.frame with two columns containing restricted data.
#' #'   The first column, with the name of the risk factor as column name, must
#' #'   contain the levels of the risk factor. The second column must contain the
#' #'   restricted coefficients.
#' #'
#' #' @family update_glm
#' #' @family autoplot.restricted
#' #' @seealso [refit_glm()] for refitting the restricted model,
#' #' and [autoplot.restricted()].
#' #'
#' #' @return Object of class restricted.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Add restrictions to risk factors for region (zip) -------------------------
#' #'
#' #' # Fit frequency and severity model
#' #' library(dplyr)
#' #' freq <- glm(nclaims ~ bm + zip, offset = log(exposure), family = poisson(),
#' #'              data = MTPL)
#' #' sev <- glm(amount ~ bm + zip, weights = nclaims,
#' #'             family = Gamma(link = "log"),
#' #'             data = MTPL |> filter(amount > 0))
#' #'
#' #' # Add predictions for freq and sev to data, and calculate premium
#' #' premium_df <- MTPL |>
#' #'    add_prediction(freq, sev) |>
#' #'    mutate(premium = pred_nclaims_freq * pred_amount_sev)
#' #'
#' #' # Restrictions on risk factors for region (zip)
#' #' zip_df <- data.frame(zip = c(0,1,2,3), zip_rst = c(0.8, 0.9, 1, 1.2))
#' #'
#' #' # Fit unrestricted model
#' #' burn <- glm(premium ~ bm + zip, weights = exposure,
#' #'             family = Gamma(link = "log"), data = premium_df)
#' #'
#' #' # Fit restricted model
#' #' burn_rst <- burn |>
#' #'   add_restriction(restrictions = zip_df) |>
#' #'   refit_glm()
#' #'
#' #' # Show rating factors
#' #' rating_table(burn_rst)
#' #' }
#' #'
#' #' @export
#' add_restriction <- function(model, restrictions) {
#'
#'   if (inherits(model, "glm")) {
#'     fm <- formula(model)
#'     offset_term <- get_offset(model)
#'     fm_no_offset <- remove_offset_formula(fm)
#'     df_new <- model$data
#'     model_call <- model$call
#'     model_out <- model
#'
#'     rfdf <- rating_table(model, signif_stars = FALSE)$df
#'     colnames(rfdf)[3] <- c("estimate")
#'     rst_lst <- list(restrictions)
#'     names(rst_lst) <- names(restrictions[1])
#'     restricted_df <- restrict_df(restrictions)
#'     new_col_nm <- NULL
#'     old_col_nm <- NULL
#'     mgd_rst <- NULL
#'     mgd_smt <- NULL
#'   }
#'
#'   if (inherits(model, c("smooth", "restricted"))) {
#'     fm <- model$formula_restricted
#'     offset_term <- model$offset
#'     fm_no_offset <- model$formula_removed
#'     df_new <- model$data_restricted
#'     model_call <- model$model_call
#'     model_out <- model$model_out
#'
#'     rfdf <- model$rating_factors
#'     rst_lst <- model$restrictions_lst
#'     rst_lst[[names(restrictions)[1]]] <- restrictions
#'     restricted_df <- restrict_df(restrictions)
#'     new_col_nm <- model$new_col_nm
#'     old_col_nm <- model$old_col_nm
#'     mgd_rst <- model$mgd_rst
#'     mgd_smt <- model$mgd_smt
#'   }
#'
#'   if (inherits(model, "restricted")) {
#'     restricted_df <- rbind(model$rf_restricted_df, restricted_df)
#'   }
#'
#'   if (inherits(model, "smooth")) {
#'     restricted_df <- rbind(model$new_rf, restricted_df)
#'   }
#'
#'   fm_remove <- update_formula_remove(fm_no_offset, names(restrictions)[1])
#'   fm_add <- update_formula_add(offset_term, fm_remove, names(restrictions)[2])
#'   df_restricted <- add_restrictions_df(df_new, restrictions)
#'
#'   nrst <- unique(setdiff(names(restrictions), unique(rfdf$risk_factor)))
#'   orst <- unique(setdiff(names(restrictions), new_col_nm))
#'   mgd_rst <- append(mgd_rst, list(unique(c(orst, nrst))))
#'
#'   new_col_nm <- unique(append(new_col_nm,
#'                               setdiff(names(restrictions),
#'                                       unique(rfdf$risk_factor))))
#'   old_col_nm <- unique(append(old_col_nm, setdiff(names(restrictions),
#'                                                   new_col_nm)))
#'
#'   rt <- list(formula_restricted = fm_add[[1]],
#'              formula_removed = fm_remove,
#'              data_restricted = df_restricted,
#'              fm_no_offset = fm_no_offset,
#'              offset = fm_add[[2]],
#'              rating_factors = rfdf,
#'              restrictions_lst = rst_lst,
#'              rf_restricted_df = restricted_df,
#'              model_call = model_call,
#'              model_out = model_out,
#'              new_col_nm = new_col_nm,
#'              old_col_nm = old_col_nm,
#'              mgd_rst = mgd_rst,
#'              mgd_smt = mgd_smt)
#'   attr(rt, "class") <- "restricted"
#'   attr(rt, "has_smoothing") <- FALSE
#'   attr(rt, "last_smoothing_step") <- NULL
#'   invisible(rt)
#' }
#'
#' #' @rdname add_restriction
#' #' @export
#' restrict_coef <- function(model, restrictions) {
#'   lifecycle::deprecate_warn("0.8.0", "restrict_coef()", "add_restriction()")
#'   add_restriction(model, restrictions)
#' }
#'
#'
#'
#' #' Smooth coefficients in the model
#' #'
#' #' @description `r lifecycle::badge('experimental')`
#' #'  Apply smoothing on the risk factors used in the model. `add_smoothing()`
#' #'  must always be followed by `refit_glm()`.
#' #'
#' #' @author Martin Haringa
#' #'
#' #' @details Although smoothing could be applied either to the frequency or
#' #'   the severity model, it is more appropriate to impose the smoothing
#' #'   on the premium model. This can be achieved by calculating the pure
#' #'   premium for each record (i.e. expected number of claims times the expected
#' #'   claim amount), then fitting an "unrestricted" Gamma GLM to the pure
#' #'   premium, and then imposing the restrictions in a final "restricted"
#' #'   Gamma GLM.
#' #'
#' #' @param model object of class glm/smooth
#' #' @param x_cut column name with breaks/cut
#' #' @param x_org column name where x_cut is based on
#' #' @param degree order of polynomial
#' #' @param breaks numerical vector with new clusters for x
#' #' @param smoothing choose smoothing specification (all the shape
#' #' constrained smooth terms (SCOP-splines) are constructed using the B-splines
#' #' basis proposed by Eilers and Marx (1996) with a discrete penalty on the basis
#' #' coefficients:
#' #'  \itemize{
#' #'   \item{'spline' (default)}
#' #'   \item{'mpi': monotone increasing SCOP-splines}
#' #'   \item{'mpd': monotone decreasing SCOP-splines}
#' #'   \item{'cx': convex SCOP-splines}
#' #'   \item{'cv': concave SCOP-splines}
#' #'   \item{'micx': increasing and convex SCOP-splines}
#' #'   \item{'micv': increasing and concave SCOP-splines}
#' #'   \item{'mdcx': decreasing and convex SCOP-splines}
#' #'   \item{'mdcv': decreasing and concave SCOP-splines}
#' #'   \item{'gam': spline based smooth (thin plate regression spline)}
#' #' }
#' #' @param k number of basis functions be computed
#' #' @param weights weights used for smoothing, must be equal to the exposure
#' #' (defaults to NULL)
#' #'
#' #' @family update_glm
#' #' @family autoplot.smooth
#' #' @seealso [update_glm()] for refitting the smoothed model,
#' #' and [autoplot.smooth()].
#' #'
#' #' @return Object of class smooth
#' #'
#' #' @examples
#' #' \dontrun{
#' #' library(insurancerating)
#' #' library(dplyr)
#' #'
#' #' # Fit GAM for claim frequency
#' #' age_policyholder_frequency <- riskfactor_gam(data = MTPL,
#' #'                                              nclaims = "nclaims",
#' #'                                              x = "age_policyholder",
#' #'                                              exposure = "exposure")
#' #'
#' #' # Determine clusters
#' #' clusters_freq <- construct_tariff_classes(age_policyholder_frequency)
#' #'
#' #' # Add clusters to MTPL portfolio
#' #' dat <- MTPL |>
#' #'   mutate(age_policyholder_freq_cat = clusters_freq$tariff_classes) |>
#' #'   mutate(across(where(is.character), as.factor)) |>
#' #'   mutate(across(where(is.factor), ~biggest_reference(., exposure)))
#' #'
#' #' # Fit frequency and severity model
#' #' freq <- glm(nclaims ~ bm + age_policyholder_freq_cat, offset = log(exposure),
#' #'  family = poisson(), data = dat)
#' #' sev <- glm(amount ~ bm + zip, weights = nclaims,
#' #'  family = Gamma(link = "log"), data = dat |> filter(amount > 0))
#' #'
#' #' # Add predictions for freq and sev to data, and calculate premium
#' #' premium_df <- dat |>
#' #'   add_prediction(freq, sev) |>
#' #'   mutate(premium = pred_nclaims_freq * pred_amount_sev)
#' #'
#' #' # Fit unrestricted model
#' #' burn_unrestricted <- glm(premium ~ zip + bm + age_policyholder_freq_cat,
#' #'                          weights = exposure,
#' #'                          family = Gamma(link = "log"),
#' #'                          data = premium_df)
#' #'
#' #' # Impose smoothing and create figure
#' #' burn_unrestricted |>
#' #'   add_smoothing(x_cut = "age_policyholder_freq_cat",
#' #'                 x_org = "age_policyholder",
#' #'                 breaks = seq(18, 95, 5)) |>
#' #'   autoplot()
#' #'
#' #' # Impose smoothing and refit model
#' #' burn_restricted <- burn_unrestricted |>
#' #'   add_smoothing(x_cut = "age_policyholder_freq_cat",
#' #'                 x_org = "age_policyholder",
#' #'                 breaks = seq(18, 95, 5)) |>
#' #'   refit_glm()
#' #'
#' #' # Show new rating factors
#' #' rating_table(burn_restricted)
#' #' }
#' #'
#' #' @export
#' add_smoothing <- function(model, x_cut, x_org, degree = NULL, breaks = NULL,
#'                           smoothing = "spline", k = NULL, weights = NULL) {
#'
#'   if (is.null(breaks) || !is.numeric(breaks)) {
#'     stop("'breaks' must be a numerical vector", call. = FALSE)
#'   }
#'
#'   if (inherits(model, "glm")) {
#'     fm <- formula(model)
#'     offset_term <- get_offset(model)
#'     fm_no_offset <- remove_offset_formula(fm)
#'     df_new <- model$data
#'     model_call <- model$call
#'     model_out <- model
#'
#'     rfdf <- rating_table(model, signif_stars = FALSE)$df
#'     colnames(rfdf)[3] <- c("estimate")
#'     rst_lst <- NULL
#'     new_col_nm <- NULL
#'     old_col_nm <- NULL
#'     mgd_smt <- NULL
#'     mgd_rst <- NULL
#'   }
#'
#'   if (inherits(model, c("smooth", "restricted"))) {
#'     fm <- model$formula_restricted
#'     offset_term <- model$offset
#'     fm_no_offset <- model$formula_removed
#'     df_new <- model$data_restricted
#'     model_call <- model$model_call
#'     model_out <- model$model_out
#'
#'     rfdf <- model$rating_factors
#'     rst_lst <- model$restrictions_lst
#'     new_col_nm <- model$new_col_nm
#'     old_col_nm <- model$old_col_nm
#'     mgd_smt <- model$mgd_smt
#'     mgd_rst <- model$mgd_rst
#'   }
#'
#'   mgd_smt <- append(mgd_smt, list(c(paste0(x_org, "_smooth"),
#'                                     paste0(x_cut, "_smooth"))))
#'
#'   old_col_nm <- append(old_col_nm, paste0(x_org, "_smooth"))
#'   new_col_nm <- append(new_col_nm, paste0(x_cut, "_smooth"))
#'
#'   fm_remove <- update_formula_remove(fm_no_offset, x_cut)
#'   fm_add <- update_formula_add(offset_term, fm_remove, paste0(x_cut, "_smooth"))
#'
#'   borders_x_cut <- cut_borders_model(model, x_cut)
#'
#'   if (is.null(degree)) {
#'     degree <- nrow(borders_x_cut) - 1
#'   }
#'
#'   if (smoothing %in% c("mpi", "mpd", "cx", "cv", "micx", "micv", "mdcx",
#'                        "mdcv", "gam")) {
#'     if (is.null(weights)) {
#'       exposur0 <- rep(1, nrow(borders_x_cut))
#'     } else if (!weights %in% colnames(df_new)) {
#'       stop("weights column: ", deparse(substitute(weights)),
#'            " is not in the model data. Specify column with exposure.",
#'            call. = FALSE)
#'     } else {
#'       exposur0 <- aggregate(list(exposure = df_new[[weights]]),
#'                             by = list(x = df_new[[x_cut]]),
#'                             FUN = sum,
#'                             na.rm = TRUE,
#'                             na.action = NULL)[,2]
#'     }
#'   } else {
#'     exposur0 <- NULL
#'   }
#'
#'   fit_poly <- fit_polynomial(borders_x_cut, x_org, degree, breaks, smoothing,
#'                              k, exposur0)
#'   df_poly <- fit_poly[["new_poly_df"]]
#'   df_poly_line <- fit_poly[["poly_line"]]
#'   df_new_rf <- fit_poly[["new_rf"]]
#'
#'   if (inherits(model, "smooth")) {
#'     df_new_rf <- rbind(model$new_rf, df_new_rf)
#'   }
#'
#'   if (inherits(model, "restricted")) {
#'     df_new_rf <- rbind(model$rf_restricted_df, df_new_rf)
#'   }
#'
#'   df_smooth <- join_to_nearest(df_new, df_poly, x_org)
#'   names(df_smooth)[names(df_smooth) == "yhat"] <- paste0(x_cut, "_smooth")
#'
#'   st <- list(formula_restricted = fm_add[[1]],
#'              formula_removed = fm_remove,
#'              data_restricted = df_smooth,
#'              fm_no_offset = fm_no_offset,
#'              offset = fm_add[[2]],
#'              borders = borders_x_cut,
#'              new = df_poly,
#'              new_line = df_poly_line,
#'              model_call = model_call,
#'              rating_factors = as.data.frame(rfdf),
#'              restrictions_lst = rst_lst,
#'              new_rf = df_new_rf,
#'              degree = degree,
#'              model_out = model_out,
#'              new_col_nm = new_col_nm,
#'              old_col_nm = old_col_nm,
#'              mgd_rst = mgd_rst,
#'              mgd_smt = mgd_smt,
#'              smoothing = smoothing)
#'   attr(st, "class") <- "smooth"
#'   attr(st, "has_smoothing") <- TRUE
#'   attr(st, "last_smoothing_step") <- "add_smoothing"
#'   invisible(st)
#' }
#'
#' #' @rdname add_smoothing
#' #' @export
#' smooth_coef <- function(model, x_cut, x_org, degree = NULL, breaks = NULL,
#'                         smoothing = "spline", k = NULL, weights = NULL) {
#'   lifecycle::deprecate_warn("0.8.0", "smooth_coef()", "add_smoothing()")
#'   add_smoothing(model, x_cut, x_org, degree, breaks, smoothing, k, weights)
#' }
#'
#'
#' #' Print for object of class restricted
#' #'
#' #' @param x Object of class restricted
#' #' @param ... other plotting parameters to affect the output
#' #'
#' #' @noRd
#' #' @return Print object
#' #'
#' #' @author Martin Haringa
#' #'
#' #' @export
#' print.restricted <- function(x, ...) {
#'   cat("Formula: ")
#'   print(x$formula_restricted)
#' }
#'
#' #' Print for object of class smooth
#' #'
#' #' @param x Object of class smooth
#' #' @param ... other plotting parameters to affect the output
#' #'
#' #' @return Print object
#' #' @noRd
#' #'
#' #' @author Martin Haringa
#' #'
#' #' @export
#' print.smooth <- function(x, ...) {
#'   cat("Formula: ")
#'   print(x$formula_restricted)
#' }
#'
#'
#' #' Automatically create a ggplot for objects obtained from add_restriction()
#' #'
#' #' @description
#' #' `r lifecycle::badge("experimental")`
#' #' Takes an object produced by `add_restriction()` or `add_relativities()`
#' #' and creates a plot comparing the adjusted coefficients with the original
#' #' coefficients obtained from the model.
#' #'
#' #' For objects produced by `add_relativities()`, original levels that are split
#' #' into new levels are removed from the connected original line and from the
#' #' x-axis. Instead, the original level is shown as a horizontal blue segment
#' #' spanning all child categories, with the original level label centred above
#' #' the segment.
#' #'
#' #' @param object Object produced by `add_restriction()` or `add_relativities()`.
#' #' @param remove_underscores Logical; if `TRUE`, underscores are replaced by
#' #'   spaces in the x-axis label. Default is `FALSE`.
#' #' @param rotate_angle Optional numeric value for the angle of x-axis labels.
#' #' @param custom_theme Optional list passed to `ggplot2::theme()`.
#' #' @param ... Additional plotting arguments passed to ggplot2 geoms.
#' #'
#' #' @return A `ggplot2` object.
#' #'
#' #' @author Martin Haringa
#' #'
#' #' @import ggplot2
#' #' @importFrom dplyr left_join
#' #'
#' #' @export
#' autoplot.restricted <- function(object,
#'                                 remove_underscores = FALSE,
#'                                 rotate_angle = NULL,
#'                                 custom_theme = NULL,
#'                                 ...) {
#'
#'   plot_palette <- function() {
#'     list(
#'       frequency        = "#2C7FB8",
#'       average_severity = "#41AB5D",
#'       risk_premium     = "#F28E2B",
#'       loss_ratio       = "#8C6BB1",
#'       average_premium  = "#2CB1A1",
#'       bg_bar           = "#E6E6E6"
#'     )
#'   }
#'
#'   plot_grid_theme <- function() {
#'     ggplot2::theme(
#'       panel.background = ggplot2::element_rect(fill = "white", color = NA),
#'       panel.grid.major = ggplot2::element_line(color = "#F2F2F2", linewidth = 0.4),
#'       panel.grid.minor = ggplot2::element_blank(),
#'       panel.border     = ggplot2::element_blank(),
#'       axis.text.y.right  = ggplot2::element_text(color = "#9E9E9E", size = 8),
#'       axis.title.y.right = ggplot2::element_text(color = "#9E9E9E", size = 9),
#'       axis.title.y       = ggplot2::element_text(size = 10)
#'     )
#'   }
#'
#'   pal <- plot_palette()
#'   grid_theme <- plot_grid_theme()
#'
#'   # ---- Case 1: add_relativities() ------------------------------------------
#'   if (!is.null(object$relativities_df)) {
#'
#'     rel_df <- as.data.frame(object$relativities_df)
#'     rf <- as.data.frame(object$rating_factors)
#'     base_rf <- object$base_risk_factor
#'     display_rf <- object$display_risk_factor
#'
#'     rf_base <- rf[rf$risk_factor == base_rf, c("level", "estimate"), drop = FALSE]
#'     names(rf_base)[2] <- "Coef"
#'     rf_base$type <- "Original fit"
#'
#'     rel_plot <- rel_df[, c("new_level", "estimate"), drop = FALSE]
#'     names(rel_plot) <- c("level", "Coef")
#'     rel_plot$type <- "New relativities"
#'
#'     # original levels that were replaced by split levels
#'     replaced_levels <- unique(rel_df$level)
#'
#'     # original line should only use levels that were not replaced
#'     rf_base_line <- rf_base[!rf_base$level %in% replaced_levels, , drop = FALSE]
#'
#'     # add unsplit original levels to the new relativities so the orange line
#'     # remains complete over non-adjusted levels
#'     unsplit_levels <- setdiff(rf_base$level, unique(rel_df$level))
#'     if (length(unsplit_levels) > 0) {
#'       unsplit_plot <- rf_base[rf_base$level %in% unsplit_levels, c("level", "Coef"), drop = FALSE]
#'       unsplit_plot$type <- "New relativities"
#'       rel_plot <- rbind(rel_plot, unsplit_plot)
#'     }
#'
#'     # IMPORTANT:
#'     # x-axis order should NOT contain replaced parent levels like "0"
#'     lvl_order <- unique(c(
#'       as.character(rf_base_line$level),
#'       as.character(rel_plot$level)
#'     ))
#'
#'     # numeric x positions for reliable spanning segments
#'     x_lookup <- data.frame(
#'       level = lvl_order,
#'       x_num = seq_along(lvl_order),
#'       stringsAsFactors = FALSE
#'     )
#'
#'     rf_base_line$level <- as.character(rf_base_line$level)
#'     rel_plot$level <- as.character(rel_plot$level)
#'
#'     rf_base_line <- dplyr::left_join(rf_base_line, x_lookup, by = "level")
#'     rel_plot <- dplyr::left_join(rel_plot, x_lookup, by = "level")
#'
#'     # construct horizontal blue segments for split original levels
#'     segments_list <- list()
#'
#'     for (lvl in replaced_levels) {
#'       child_lvls <- as.character(rel_df$new_level[rel_df$level == lvl])
#'       child_lvls <- child_lvls[child_lvls %in% lvl_order]
#'
#'       if (length(child_lvls) == 0) next
#'
#'       child_pos <- x_lookup$x_num[match(child_lvls, x_lookup$level)]
#'       child_pos <- child_pos[!is.na(child_pos)]
#'
#'       if (length(child_pos) == 0) next
#'
#'       y_val <- rf_base$Coef[rf_base$level == lvl][1]
#'
#'       segments_list[[as.character(lvl)]] <- data.frame(
#'         x_start = min(child_pos),
#'         x_end   = max(child_pos),
#'         y       = y_val,
#'         label   = as.character(lvl),
#'         stringsAsFactors = FALSE
#'       )
#'     }
#'
#'     segments_df <- NULL
#'     if (length(segments_list) > 0) {
#'       segments_df <- do.call(rbind, segments_list)
#'     }
#'
#'     x_lab <- display_rf
#'     if (remove_underscores) {
#'       x_lab <- gsub("_", " ", x_lab)
#'     }
#'
#'     p <- ggplot2::ggplot() +
#'       ggplot2::geom_line(
#'         data = rf_base_line,
#'         ggplot2::aes(x = x_num, y = Coef, color = type, group = type),
#'         linewidth = 0.8,
#'         ...
#'       ) +
#'       ggplot2::geom_point(
#'         data = rf_base_line,
#'         ggplot2::aes(x = x_num, y = Coef, color = type),
#'         shape = 21,
#'         fill = "white",
#'         stroke = 0.7,
#'         size = 2.4,
#'         ...
#'       ) +
#'       ggplot2::geom_line(
#'         data = rel_plot,
#'         ggplot2::aes(x = x_num, y = Coef, color = type, group = type),
#'         linewidth = 0.8,
#'         ...
#'       ) +
#'       ggplot2::geom_point(
#'         data = rel_plot,
#'         ggplot2::aes(x = x_num, y = Coef, color = type),
#'         shape = 21,
#'         fill = "white",
#'         stroke = 0.7,
#'         size = 2.4,
#'         ...
#'       )
#'
#'     if (!is.null(segments_df) && nrow(segments_df) > 0) {
#'       p <- p +
#'         ggplot2::geom_segment(
#'           data = segments_df,
#'           ggplot2::aes(
#'             x = x_start,
#'             xend = x_end,
#'             y = y,
#'             yend = y
#'           ),
#'           color = pal$frequency,
#'           linewidth = 0.9
#'         ) +
#'         ggplot2::geom_segment(
#'           data = segments_df,
#'           ggplot2::aes(
#'             x = x_start,
#'             xend = x_start,
#'             y = y,
#'             yend = y - 0.012
#'           ),
#'           color = pal$frequency,
#'           linewidth = 0.8
#'         ) +
#'         ggplot2::geom_segment(
#'           data = segments_df,
#'           ggplot2::aes(
#'             x = x_end,
#'             xend = x_end,
#'             y = y,
#'             yend = y - 0.012
#'           ),
#'           color = pal$frequency,
#'           linewidth = 0.8
#'         ) +
#'         ggplot2::geom_text(
#'           data = segments_df,
#'           ggplot2::aes(
#'             x = (x_start + x_end) / 2,
#'             y = y,
#'             label = label
#'           ),
#'           vjust = -0.7,
#'           color = pal$frequency,
#'           size = 3.2
#'         )
#'     }
#'
#'     p <- p +
#'       ggplot2::scale_x_continuous(
#'         breaks = x_lookup$x_num,
#'         labels = x_lookup$level
#'       ) +
#'       ggplot2::scale_colour_manual(
#'         values = c(
#'           "Original fit" = pal$frequency,
#'           "New relativities" = pal$risk_premium
#'         ),
#'         name = NULL
#'       ) +
#'       ggplot2::labs(
#'         x = x_lab,
#'         y = "Relativity"
#'       ) +
#'       ggplot2::theme_minimal() +
#'       grid_theme
#'
#'     if (!is.null(rotate_angle)) {
#'       p <- p +
#'         ggplot2::theme(
#'           axis.text.x = ggplot2::element_text(angle = rotate_angle, hjust = 1)
#'         )
#'     }
#'
#'     if (!is.null(custom_theme)) {
#'       p <- p + do.call(ggplot2::theme, custom_theme)
#'     }
#'
#'     return(p)
#'   }
#'
#'   # ---- Case 2: add_restriction() -------------------------------------------
#'   names_rf <- names(object$restrictions_lst)
#'   name <- names_rf[length(names_rf)]
#'   naam_rst <- object$restrictions_lst[[name]]
#'
#'   rf <- object$rating_factors
#'   naam_rf <- rf[rf$risk_factor == name, , drop = FALSE]
#'   naam_rf <- naam_rf[, 2:3, drop = FALSE]
#'   names(naam_rst)[names(naam_rst) == name] <- "level"
#'
#'   naam_rf <- matchColClasses(naam_rst, naam_rf)
#'
#'   koppel <- dplyr::left_join(naam_rst, naam_rf, by = "level")
#'
#'   restricted_name <- names(naam_rst)[2]
#'   unrestricted_name <- names(naam_rf)[2]
#'
#'   koppel_restricted <- data.frame(
#'     level = koppel$level,
#'     type = "Adjusted fit",
#'     Coef = koppel[[restricted_name]],
#'     stringsAsFactors = FALSE
#'   )
#'
#'   koppel_unrestricted <- data.frame(
#'     level = koppel$level,
#'     type = "Original fit",
#'     Coef = koppel[[unrestricted_name]],
#'     stringsAsFactors = FALSE
#'   )
#'
#'   koppel_long <- rbind(koppel_unrestricted, koppel_restricted)
#'   lvl_order <- unique(koppel$level)
#'
#'   x_lookup <- data.frame(
#'     level = lvl_order,
#'     x_num = seq_along(lvl_order),
#'     stringsAsFactors = FALSE
#'   )
#'
#'   koppel_long <- dplyr::left_join(koppel_long, x_lookup, by = "level")
#'
#'   x_lab <- name
#'   if (remove_underscores) {
#'     x_lab <- gsub("_", " ", x_lab)
#'   }
#'
#'   p <- ggplot2::ggplot(
#'     data = koppel_long,
#'     ggplot2::aes(x = x_num, y = Coef, color = type, group = type)
#'   ) +
#'     ggplot2::geom_line(linewidth = 0.8, ...) +
#'     ggplot2::geom_point(
#'       shape = 21,
#'       fill = "white",
#'       stroke = 0.7,
#'       size = 2.4,
#'       ...
#'     ) +
#'     ggplot2::scale_x_continuous(
#'       breaks = x_lookup$x_num,
#'       labels = x_lookup$level
#'     ) +
#'     ggplot2::scale_colour_manual(
#'       values = c(
#'         "Original fit" = pal$frequency,
#'         "Adjusted fit" = pal$risk_premium
#'       ),
#'       name = NULL
#'     ) +
#'     ggplot2::labs(
#'       x = x_lab,
#'       y = "Relativity"
#'     ) +
#'     ggplot2::theme_minimal() +
#'     grid_theme
#'
#'   if (!is.null(rotate_angle)) {
#'     p <- p +
#'       ggplot2::theme(
#'         axis.text.x = ggplot2::element_text(angle = rotate_angle, hjust = 1)
#'       )
#'   }
#'
#'   if (!is.null(custom_theme)) {
#'     p <- p + do.call(ggplot2::theme, custom_theme)
#'   }
#'
#'   p
#' }
#'
#'
#' #' Automatically create a ggplot for objects obtained from add_smoothing()
#' #'
#' #' @description
#' #' `r lifecycle::badge("experimental")`
#' #' Takes an object produced by `add_smoothing()` and creates a plot comparing
#' #' the original model fit, the new clustered values, and the resulting smooth.
#' #'
#' #' @param object Object produced by `add_smoothing()`.
#' #' @param ... Additional plotting arguments passed to ggplot2 geoms.
#' #'
#' #' @return A `ggplot2` object.
#' #'
#' #' @author Martin Haringa
#' #'
#' #' @import ggplot2
#' #' @importFrom scales ordinal
#' #'
#' #' @export
#' autoplot.smooth <- function(object, ...) {
#'
#'   rf2 <- object$borders
#'   new <- object$new
#'   new_line <- object$new_line
#'   degree <- scales::ordinal(object$degree)
#'   smoothing <- object$smoothing
#'
#'   if (smoothing == "spline") {
#'     degree_name <- paste0(degree, " order polynomial")
#'   } else {
#'     degree_name <- toupper(smoothing)
#'   }
#'
#'   # internal style helpers ---------------------------------------------------
#'   plot_palette <- function() {
#'     list(
#'       frequency        = "#2C7FB8",
#'       average_severity = "#41AB5D",
#'       risk_premium     = "#F28E2B",
#'       loss_ratio       = "#8C6BB1",
#'       average_premium  = "#2CB1A1",
#'       bg_bar           = "#E6E6E6"
#'     )
#'   }
#'
#'   plot_grid_theme <- function() {
#'     ggplot2::theme(
#'       panel.background = ggplot2::element_rect(fill = "white", color = NA),
#'       panel.grid.major = ggplot2::element_line(color = "#F2F2F2", linewidth = 0.4),
#'       panel.grid.minor = ggplot2::element_blank(),
#'       panel.border     = ggplot2::element_blank(),
#'       axis.text.y.right  = ggplot2::element_text(color = "#9E9E9E", size = 8),
#'       axis.title.y.right = ggplot2::element_text(color = "#9E9E9E", size = 9),
#'       axis.title.y       = ggplot2::element_text(size = 10)
#'     )
#'   }
#'
#'   pal <- plot_palette()
#'   grid_theme <- plot_grid_theme()
#'
#'   rf2_start_open   <- rf2[rf2$start_oc == "open", , drop = FALSE]
#'   rf2_start_closed <- rf2[rf2$start_oc == "closed", , drop = FALSE]
#'   rf2_end_open     <- rf2[rf2$end_oc == "open", , drop = FALSE]
#'   rf2_end_closed   <- rf2[rf2$end_oc == "closed", , drop = FALSE]
#'
#'   new_start_open   <- new[new$start_oc == "open", , drop = FALSE]
#'   new_start_closed <- new[new$start_oc == "closed", , drop = FALSE]
#'   new_end_open     <- new[new$end_oc == "open", , drop = FALSE]
#'   new_end_closed   <- new[new$end_oc == "closed", , drop = FALSE]
#'
#'   x_name <- names(new_line)[1]
#'   names(new_line)[names(new_line) == x_name] <- "col1"
#'
#'   ggplot2::ggplot(data = rf2) +
#'     ggplot2::geom_segment(
#'       ggplot2::aes(
#'         x = start_,
#'         y = estimate,
#'         xend = end_,
#'         yend = estimate,
#'         color = "Model fit"
#'       ),
#'       linewidth = 0.8,
#'       ...
#'     ) +
#'     ggplot2::geom_segment(
#'       data = new,
#'       ggplot2::aes(
#'         x = breaks_min,
#'         y = yhat,
#'         xend = breaks_max,
#'         yend = yhat,
#'         color = "New cluster"
#'       ),
#'       linewidth = 0.8,
#'       ...
#'     ) +
#'     ggplot2::geom_line(
#'       data = new_line,
#'       ggplot2::aes(
#'         x = col1,
#'         y = yhat
#'       ),
#'       linewidth = 0.5,
#'       linetype = "dashed", # of "dashed"
#'       color = "#4D4D4D",
#'       ...
#'     ) +
#'
#'     # model fit points
#'     ggplot2::geom_point(
#'       data = rf2_start_closed,
#'       ggplot2::aes(x = start_, y = estimate, color = "Model fit"),
#'       shape = 16,
#'       size = 2.2,
#'       ...
#'     ) +
#'     ggplot2::geom_point(
#'       data = rf2_end_closed,
#'       ggplot2::aes(x = end_, y = estimate, color = "Model fit"),
#'       shape = 16,
#'       size = 2.2,
#'       ...
#'     ) +
#'     ggplot2::geom_point(
#'       data = rf2_start_open,
#'       ggplot2::aes(x = start_, y = estimate, color = "Model fit"),
#'       shape = 21,
#'       fill = "white",
#'       stroke = 0.7,
#'       size = 2.2,
#'       ...
#'     ) +
#'     ggplot2::geom_point(
#'       data = rf2_end_open,
#'       ggplot2::aes(x = end_, y = estimate, color = "Model fit"),
#'       shape = 21,
#'       fill = "white",
#'       stroke = 0.7,
#'       size = 2.2,
#'       ...
#'     ) +
#'
#'     # new cluster points
#'     ggplot2::geom_point(
#'       data = new_start_closed,
#'       ggplot2::aes(x = start_, y = yhat, color = "New cluster"),
#'       shape = 16,
#'       size = 2.2,
#'       ...
#'     ) +
#'     ggplot2::geom_point(
#'       data = new_end_closed,
#'       ggplot2::aes(x = end_, y = yhat, color = "New cluster"),
#'       shape = 16,
#'       size = 2.2,
#'       ...
#'     ) +
#'     ggplot2::geom_point(
#'       data = new_start_open,
#'       ggplot2::aes(x = start_, y = yhat, color = "New cluster"),
#'       shape = 21,
#'       fill = "white",
#'       stroke = 0.7,
#'       size = 2.2,
#'       ...
#'     ) +
#'     ggplot2::geom_point(
#'       data = new_end_open,
#'       ggplot2::aes(x = end_, y = yhat, color = "New cluster"),
#'       shape = 21,
#'       fill = "white",
#'       stroke = 0.7,
#'       size = 2.2,
#'       ...
#'     ) +
#'
#'     ggplot2::labs(
#'       x = x_name,
#'       y = "Estimated relativity"
#'     ) +
#'     ggplot2::scale_colour_manual(
#'       name = NULL,
#'       values = c(
#'         "Model fit" = pal$frequency,
#'         "New cluster" = pal$risk_premium
#'       ),
#'       labels = c(
#'         "Model fit" = "Original fit",
#'         "New cluster" = "Clustered fit"
#'       )
#'     ) +
#'     ggplot2::theme_minimal() +
#'     grid_theme
#' }
#'
#'
#' #' Refit a GLM model
#' #'
#' #' @description `r lifecycle::badge('stable')`
#' #'  This is the new version of `refit_glm()`. It refits the GLM with
#' #'  any restrictions or smoothings that were previously added.
#' #'
#' #' @param x Object of class restricted or of class smooth
#' #' @param intercept_only Logical. Default is \code{FALSE}. If \code{TRUE}, only
#' #' the intercept is updated, ensuring that the changes have no impact on the
#' #' other variables.
#' #' @param ... Other arguments
#' #'
#' #' @author Martin Haringa
#' #'
#' #' @importFrom stats glm
#' #' @importFrom stats terms.formula
#' #' @importFrom utils modifyList
#' #'
#' #' @return Object of class GLM
#' #'
#' #' @export
#' refit_glm <- function(x, intercept_only = FALSE, ...) {
#'
#'   if (!inherits(x, c("restricted", "smooth"))) {
#'     stop("Input must be of class 'restricted' or 'smooth'.", call. = FALSE)
#'   }
#'
#'   if (isTRUE(intercept_only)) {
#'     andere <- attr(stats::terms.formula(x$formula_removed), "term.labels")
#'     if (length(andere) > 0) {
#'       tot_rf <- x$rating_factors
#'       df <- tot_rf[tot_rf$risk_factor %in% andere,]
#'       rf_mult <- names(table(df$risk_factor)[table(df$risk_factor) > 1])
#'       rf_single <- names(table(df$risk_factor)[table(df$risk_factor) == 1])
#'       if (length(rf_mult) > 0) {
#'         df1 <- df[df$risk_factor %in% rf_mult,]
#'         mult_lst <- split(df1, df1$risk_factor)
#'
#'         for (i in seq_along(mult_lst)) {
#'           risk_factor_name <- unique(mult_lst[[i]]$risk_factor)
#'           names(mult_lst[[i]])[names(mult_lst[[i]]) == "level"] <- risk_factor_name
#'           names(mult_lst[[i]])[names(mult_lst[[i]]) == "estimate"] <- paste0(
#'             risk_factor_name, "_rst99"
#'           )
#'           mult_lst[[i]]$risk_factor <- NULL
#'           x <- add_restriction(x, mult_lst[[i]])
#'         }
#'       }
#'
#'       if (length(rf_single) > 0) {
#'
#'         df2 <- df[df$risk_factor %in% rf_single,]
#'         sng_lst <- split(df2, df2$risk_factor)
#'
#'         for (i in seq_along(sng_lst)) {
#'           formula_removed <- x$formula_removed
#'           rf_name <- unique(sng_lst[[i]]$risk_factor)
#'           rf_est <- unique(sng_lst[[i]]$estimate)
#'           formula_removed <- update(formula_removed, paste("~ . -", rf_name))
#'           add_offset <- paste0(rf_name, " * log(", rf_est, ")")
#'           newoffset <- paste0(x$offset, " + ", add_offset)
#'           newoffsetterm <- paste0("offset(", newoffset, ")")
#'           formula_restricted <- update(formula_removed, paste("~ . + ",
#'                                                               newoffsetterm))
#'           x$offset <- newoffset
#'           x$formula_restricted <- formula_restricted
#'           x$formula_removed <- formula_removed
#'         }
#'       }
#'     }
#'   }
#'
#'   lst_call <- as.list(x$model_call)
#'
#'   lst <- list(formula = x$formula_restricted,
#'               data = x$data_restricted,
#'               offset = NULL) # offset is already in formula with +
#'
#'   y <- eval(as.call(modifyList(lst_call, lst)))
#'
#'   y$call$formula <- lst$formula
#'   y$call$data <- quote(df_new)
#'
#'   offweights <- NULL
#'   if (!is.null(lst_call$weights)) {
#'     offweights <- append(offweights, as.character(lst_call$weights))
#'   }
#'
#'   if (!is.null(lst_call$offset)) {
#'     offweights <- append(offweights, as.character(lst_call$offset)[2])
#'   }
#'
#'   if (inherits(x, "smooth")) {
#'     attr(y, "new_rf") <- x[["new_rf"]]
#'     attr(y, "class") <- append(class(y), "refitsmooth")
#'   }
#'
#'   if (inherits(x, "restricted")) {
#'     attr(y, "new_rf_rst") <- x[["rf_restricted_df"]]
#'     attr(y, "class") <- append(class(y), "refitrestricted")
#'   }
#'
#'   rf <- x$rating_factors
#'   rf2 <- unique(rf$risk_factor[rf$risk_factor != "(Intercept)"])
#'
#'   # Continuous variables
#'   rf_single <- names(which(table(rf$risk_factor) == 1))
#'   rf_single <- setdiff(rf_single, "(Intercept)")
#'   rf_single_rows <- rf[rf$risk_factor %in% rf_single, ]
#'
#'   attr(y, "new_col_nm") <- x$new_col_nm
#'   attr(y, "old_col_nm") <- x$old_col_nm
#'   attr(y, "rf") <- rf2
#'   attr(y, "mgd_smt") <- x$mgd_smt
#'   attr(y, "mgd_rst") <- x$mgd_rst
#'   attr(y, "offweights") <- offweights
#'   attr(y, "continuous_factors") <- rf_single_rows
#'   attr(y, "intercept_only") <- isTRUE(intercept_only)
#'   y
#' }
#'
#' #' @rdname refit_glm
#' #' @export
#' #' @description `r lifecycle::badge('deprecated')`
#' update_glm <- function(x, intercept_only = FALSE, ...) {
#'   lifecycle::deprecate_warn("0.8.0", "update_glm()", "refit_glm()")
#'   refit_glm(x, intercept_only = intercept_only, ...)
#' }
#'
#' #' Update an existing smoothing curve
#' #'
#' #' @description
#' #' `update_smoothing()` modifies the most recent smoothing created with
#' #' [`add_smoothing()`] on a `"smooth"` or `"restricted"` object.
#' #'
#' #' It retrieves the original continuous factor and the corresponding tariff
#' #' factor (i.e. `x_org` and `x_cut`) from the model object itself, so it must be
#' #' called *after* at least one call to [`add_smoothing()`].
#' #'
#' #' @param model A model object of class `"smooth"` or `"restricted"`, typically
#' #'   returned by [`add_smoothing()`] (possibly combined with
#' #'   [`add_restriction()`]).
#' #' @param x1,x2 Numeric. Start and end of the interval over which the smoothing
#' #'   should be modified. Must satisfy `x1 < x2`.
#' #' @param overwrite_y1,overwrite_y2 Optional numeric. Overrides for the smoothed
#' #'   values at `x1` and `x2`. If `NULL`, existing smoothed values are used.
#' #' @param knots_x,knots_y Optional numeric vectors of equal length specifying
#' #'   intermediate knot points through which the modified curve should pass.
#' #' @param allow_extrapolation Logical. If `TRUE`, the smoothing curve may be
#' #'   extended beyond its original range. Setting `x1` or `x2` outside the
#' #'   original range is only allowed when `allow_extrapolation = TRUE`; this
#' #'   explicit flag acts as a safeguard against unintended extrapolation in
#' #'   pricing models.
#' #' @param extrapolation_break_size Numeric scalar (> 0) or `NULL`. Width of
#' #'   synthetic break intervals used to discretize the extrapolated part of the
#' #'   smoothing curve. If `NULL`, it defaults to the median width of the existing
#' #'   tariff breaks (scale-aware default).
#' #'
#' #' @author Rik Dommerholt, Martin Haringa
#' #'
#' #' @return
#' #' A modified object of class `"smooth"` with an updated smoothing curve for the
#' #' most recently smoothed risk factor.
#' #'
#' #' @export
#' update_smoothing <- function(model,
#'                              x1, x2,
#'                              overwrite_y1 = NULL, overwrite_y2 = NULL,
#'                              knots_x = NULL, knots_y = NULL,
#'                              allow_extrapolation = FALSE,
#'                              extrapolation_break_size = NULL) {
#'
#'   has_smoothing <- isTRUE(attr(model, "has_smoothing"))
#'
#'   if (!has_smoothing) {
#'     stop(
#'       "update_smoothing() must be preceded by add_smoothing() or update_smoothing().\n",
#'       "Smoothing context was not found (possibly broken by add_restriction() or refit_glm()).",
#'       call. = FALSE
#'     )
#'   }
#'
#'   if (is.null(knots_x)) knots_x <- numeric()
#'   if (is.null(knots_y)) knots_y <- numeric()
#'
#'   if (length(knots_x) != length(knots_y)) {
#'     stop("Lengths of 'knots_x' and 'knots_y' must be equal.", call. = FALSE)
#'   }
#'
#'   middle_x <- knots_x
#'   middle_y <- knots_y
#'
#'   if (!inherits(model, c("smooth", "restricted"))) {
#'     stop(
#'       sprintf(
#'         "Input must be of class 'smooth' or 'restricted', not '%s'. Precede this function with add_smoothing().",
#'         paste(class(model), collapse = ", ")
#'       ),
#'       call. = FALSE
#'     )
#'   }
#'
#'   # -- read x_org / x_cut from last smoothing metadata
#'   mgd_smt <- model$mgd_smt
#'   if (is.null(mgd_smt) || length(mgd_smt) == 0) {
#'     stop("No smoothing metadata found in 'model$mgd_smt'. Call add_smoothing() before update_smoothing().",
#'          call. = FALSE)
#'   }
#'
#'   last_pair <- mgd_smt[[length(mgd_smt)]]
#'   if (length(last_pair) != 2L) {
#'     stop("Last element of 'model$mgd_smt' is malformed. Expected length 2.", call. = FALSE)
#'   }
#'
#'   x_org <- sub("_smooth$", "", last_pair[1])
#'   x_cut <- sub("_smooth$", "", last_pair[2])
#'
#'   # pull required fields
#'   fm           <- model$formula_restricted
#'   offset_term  <- model$offset
#'   fm_no_offset <- model$formula_removed
#'   df_new       <- model$data_restricted
#'   model_call   <- model$model_call
#'   model_out    <- model$model_out
#'   rfdf         <- model$rating_factors
#'   rst_lst      <- model$restrictions_lst
#'   new_col_nm   <- model$new_col_nm
#'   old_col_nm   <- model$old_col_nm
#'   mgd_rst      <- model$mgd_rst
#'   new          <- model$new
#'   degree       <- model$degree
#'   smoothing    <- model$smoothing
#'
#'   borders_x_cut <- cut_borders_model(model, x_cut)
#'
#'   # ---- NEW: scale-aware default for extrapolation_break_size
#'   if (is.null(extrapolation_break_size)) {
#'     extrapolation_break_size <- default_extrapolation_break_size(new,
#'                                                                  factor = 1)
#'   } else {
#'     if (!is.numeric(extrapolation_break_size) ||
#'         length(extrapolation_break_size) != 1 ||
#'         !is.finite(extrapolation_break_size) ||
#'         extrapolation_break_size <= 0) {
#'       stop("'extrapolation_break_size' must be a single positive numeric value or NULL.",
#'            call. = FALSE)
#'     }
#'   }
#'
#'   fit_poly <- change_xy(
#'     borders_model           = new,
#'     x_org                   = x_org,
#'     x1                      = x1,
#'     x2                      = x2,
#'     overwrite_y1            = overwrite_y1,
#'     overwrite_y2            = overwrite_y2,
#'     middle_x                = middle_x,
#'     middle_y                = middle_y,
#'     allow_extrapolation     = allow_extrapolation,
#'     extrapolation_break_size= extrapolation_break_size
#'   )
#'
#'   df_poly      <- fit_poly[["new_poly_df"]]
#'   df_poly_line <- fit_poly[["poly_line"]]
#'   df_new_rf    <- fit_poly[["new_rf"]]
#'
#'   if (inherits(model, "smooth")) {
#'     keep <- model$new_rf$risk_factor != paste0(x_org, "_smooth")
#'     model$new_rf <- model$new_rf[keep, , drop = FALSE]
#'     df_new_rf <- rbind(model$new_rf, df_new_rf)
#'   }
#'
#'
#'   if (inherits(model, "restricted")) {
#'     df_new_rf <- rbind(model$rf_restricted_df, df_new_rf)
#'   }
#'
#'   double_variable <- paste0(x_cut, "_smooth")
#'
#'   cn <- colnames(df_new)
#'
#'   drop <- cn %in% double_variable |
#'     startsWith(cn, "i.") |
#'     grepl("avg_", cn, fixed = TRUE) |
#'     grepl("breaks_max", cn, fixed = TRUE) |
#'     grepl("breaks_min", cn, fixed = TRUE) |
#'     grepl("end_", cn, fixed = TRUE) |
#'     grepl("end_oc", cn, fixed = TRUE) |
#'     endsWith(cn, "_smooth") |
#'     grepl("risk_factor", cn, fixed = TRUE) |
#'     grepl("start_", cn, fixed = TRUE) |
#'     grepl("start_oc", cn, fixed = TRUE)
#'
#'   df_new <- df_new[, !drop, drop = FALSE]
#'
#'   df_smooth <- join_to_nearest(df_new, df_poly, x_org)
#'   names(df_smooth)[names(df_smooth) == "yhat"] <- paste0(x_cut, "_smooth")
#'
#'   st <- list(
#'     formula_restricted = fm,
#'     formula_removed    = fm_no_offset,
#'     data_restricted    = df_smooth,
#'     fm_no_offset       = fm_no_offset,
#'     offset             = offset_term,
#'     borders            = borders_x_cut,
#'     new                = df_poly,
#'     new_line           = df_poly_line,
#'     model_call         = model_call,
#'     rating_factors     = as.data.frame(rfdf),
#'     restrictions_lst   = rst_lst,
#'     new_rf             = df_new_rf,
#'     degree             = degree,
#'     model_out          = model_out,
#'     new_col_nm         = new_col_nm,
#'     old_col_nm         = old_col_nm,
#'     mgd_rst            = mgd_rst,
#'     mgd_smt            = model$mgd_smt,
#'     smoothing          = smoothing
#'   )
#'
#'   attr(st, "class") <- "smooth"
#'
#'   # markeer dat smoothing is geüpdatet
#'   attr(st, "has_smoothing") <- TRUE
#'   attr(st, "last_smoothing_step") <- "update_smoothing"
#'
#'   invisible(st)
#' }
#'
#'
#'
#'
#' #' Add expert-based relativities to the model
#' #'
#' #' @description `r lifecycle::badge('experimental')`
#' #' Add expert-based relativities to one or more levels of a risk factor by
#' #' splitting these levels into more granular sublevels. `add_relativities()`
#' #' must always be followed by `refit_glm()`.
#' #'
#' #' @author Martin Haringa
#' #'
#' #' @details
#' #' This function is useful when the model is estimated on a relatively broad
#' #' risk factor level, while expert judgement indicates that meaningful
#' #' differences exist within that level.
#' #'
#' #' A typical use case occurs when a subgroup has insufficient exposure or too
#' #' little claims experience to estimate a stable coefficient directly in the GLM.
#' #' In that case, the GLM can first be estimated on a broader level, and then
#' #' refined afterwards by splitting that level into more granular subgroups.
#' #'
#' #' For example, a GLM may estimate a single coefficient for the business
#' #' activity group `"industry"` based on the full industry portfolio. In practice,
#' #' however, it may still be desirable to distinguish between `"heavy_industry"`
#' #' and `"light_industry"` based on expert judgement. The user can then specify
#' #' relativities such as 1.1 for heavy industry and 1.0 for light industry.
#' #'
#' #' If `normalize = TRUE`, these relativities are rescaled such that their
#' #' exposure-weighted average remains equal to 1 within the original level.
#' #' As a result, the weighted average of the refined subgroup coefficients
#' #' remains equal to the original GLM coefficient for the full group. This makes
#' #' it possible to follow the model output while applying a transparent and
#' #' explainable adjustment to improve pricing differentiation.
#' #'
#' #' This is especially useful when the portfolio contains only a small amount of
#' #' exposure for a higher-risk subgroup, for example heavy industry with little
#' #' or no observed claims, but where business knowledge still supports a higher
#' #' premium than for lower-risk subgroups.
#' #'
#' #' This method preserves the original model structure while allowing controlled,
#' #' transparent adjustments to pricing based on expert insight.
#' #'
#' #' @param model object of class glm/restricted/smooth
#' #' @param risk_factor Character string. Name of the existing risk factor in the
#' #'   model that should be refined.
#' #' @param risk_factor_split Character string. Column name in the underlying
#' #'   portfolio data containing the more granular split of `risk_factor`.
#' #' @param relativities Named list of data.frames. Each list name must correspond
#' #'   to an existing level of `risk_factor` to be split. Each data.frame must
#' #'   contain the columns:
#' #'   \describe{
#' #'     \item{new_level}{Character. Name of the new sublevel.}
#' #'     \item{relativity}{Numeric. Relativity relative to the original model
#' #'     factor of the corresponding level.}
#' #'   }
#' #' @param exposure Character string. Column name in the portfolio data
#' #'   containing exposure weights used for optional normalization.
#' #' @param normalize Logical. If `TRUE` (default), relativities are normalized
#' #'   such that the exposure-weighted average equals 1 within each split level.
#' #'
#' #' @family update_glm
#' #' @seealso [refit_glm()] for refitting the model after adding relativities.
#' #'
#' #' @return Object of class restricted.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' relativities_activity <- relativities_list(
#' #' split_level("construction",
#' #'            c("residential_construction",
#' #'              "commercial_construction",
#' #'              "civil_engineering"),
#' #'            c(1.00, 1.10, 1.25))
#' #' )
#' #'
#' #' burn_rel <- burn_unrestricted |>
#' #'   add_relativities(
#' #'     risk_factor = "business_activity",
#' #'     risk_factor_split = "business_activity_split",
#' #'     relativities = relativities_activity,
#' #'     exposure = "exposure",
#' #'     normalize = TRUE
#' #'   ) |>
#' #'   refit_glm()
#' #'
#' #' rating_table(burn_rel)
#' #' }
#' #'
#' #' @export
#' add_relativities <- function(model,
#'                              risk_factor,
#'                              risk_factor_split,
#'                              relativities,
#'                              exposure,
#'                              normalize = TRUE) {
#'
#'   if (!inherits(model, c("glm", "restricted", "smooth"))) {
#'     stop("'model' must be of class glm, restricted or smooth.", call. = FALSE)
#'   }
#'
#'   if (!is.character(risk_factor) || length(risk_factor) != 1) {
#'     stop("'risk_factor' must be a single character string.", call. = FALSE)
#'   }
#'
#'   if (!is.character(risk_factor_split) || length(risk_factor_split) != 1) {
#'     stop("'risk_factor_split' must be a single character string.", call. = FALSE)
#'   }
#'
#'   if (!is.character(exposure) || length(exposure) != 1) {
#'     stop("'exposure' must be a single character string.", call. = FALSE)
#'   }
#'
#'   if (!is.logical(normalize) || length(normalize) != 1) {
#'     stop("'normalize' must be TRUE or FALSE.", call. = FALSE)
#'   }
#'
#'   if (!is.list(relativities) || is.null(names(relativities)) ||
#'       any(names(relativities) == "")) {
#'     stop(
#'       "'relativities' must be a named list of data.frames, with list names ",
#'       "corresponding to levels of 'risk_factor'.",
#'       call. = FALSE
#'     )
#'   }
#'
#'   .check_relativities_list(relativities)
#'
#'   if (inherits(model, "glm")) {
#'     fm <- formula(model)
#'     offset_term <- get_offset(model)
#'     fm_no_offset <- remove_offset_formula(fm)
#'     df_new <- model$data
#'     model_call <- model$call
#'     model_out <- model
#'
#'     rfdf <- rating_table(model, signif_stars = FALSE)$df
#'     colnames(rfdf)[3] <- "estimate"
#'     rst_lst <- NULL
#'     new_col_nm <- NULL
#'     old_col_nm <- NULL
#'     mgd_rst <- NULL
#'     mgd_smt <- NULL
#'   }
#'
#'   if (inherits(model, c("smooth", "restricted"))) {
#'     fm <- model$formula_restricted
#'     offset_term <- model$offset
#'     fm_no_offset <- model$formula_removed
#'     df_new <- model$data_restricted
#'     model_call <- model$model_call
#'     model_out <- model$model_out
#'
#'     rfdf <- model$rating_factors
#'     rst_lst <- model$restrictions_lst
#'     new_col_nm <- model$new_col_nm
#'     old_col_nm <- model$old_col_nm
#'     mgd_rst <- model$mgd_rst
#'     mgd_smt <- model$mgd_smt
#'   }
#'
#'   if (!risk_factor %in% names(df_new)) {
#'     stop("risk_factor column: ", risk_factor, " is not in the model data.",
#'          call. = FALSE)
#'   }
#'
#'   if (!risk_factor_split %in% names(df_new)) {
#'     stop("risk_factor_split column: ", risk_factor_split,
#'          " is not in the model data.", call. = FALSE)
#'   }
#'
#'   if (!exposure %in% names(df_new)) {
#'     stop("exposure column: ", exposure, " is not in the model data.",
#'          call. = FALSE)
#'   }
#'
#'   if (!risk_factor %in% unique(rfdf$risk_factor)) {
#'     stop("'", risk_factor, "' is not present as a risk factor in the model.",
#'          call. = FALSE)
#'   }
#'
#'   rel_levels <- names(relativities)
#'   model_levels <- rfdf$level[rfdf$risk_factor == risk_factor]
#'
#'   missing_levels <- setdiff(rel_levels, model_levels)
#'   if (length(missing_levels) > 0) {
#'     stop(
#'       "The following levels in 'relativities' are not present in risk_factor '",
#'       risk_factor, "': ", paste(missing_levels, collapse = ", "),
#'       call. = FALSE
#'     )
#'   }
#'
#'   rel_df <- .build_relativities_df(relativities)
#'
#'   exposure_df <- stats::aggregate(
#'     df_new[[exposure]],
#'     by = list(
#'       level = df_new[[risk_factor]],
#'       new_level = df_new[[risk_factor_split]]
#'     ),
#'     FUN = sum,
#'     na.rm = TRUE
#'   )
#'   names(exposure_df)[3] <- "exposure"
#'
#'   rel_df <- merge(
#'     rel_df,
#'     exposure_df,
#'     by = c("level", "new_level"),
#'     all.x = TRUE,
#'     sort = FALSE
#'   )
#'
#'   if (any(is.na(rel_df$exposure))) {
#'     miss <- unique(rel_df$new_level[is.na(rel_df$exposure)])
#'     stop(
#'       "No matching exposure found in model data for the following new levels in '",
#'       risk_factor_split, "': ", paste(miss, collapse = ", "),
#'       call. = FALSE
#'     )
#'   }
#'
#'   if (normalize) {
#'     rel_df <- .normalize_relativities(rel_df)
#'   } else {
#'     rel_df$relativity_final <- rel_df$relativity
#'   }
#'
#'   base_df <- rfdf[rfdf$risk_factor == risk_factor, c("level", "estimate")]
#'   names(base_df)[2] <- "estimate_base"
#'
#'   rel_df <- merge(
#'     rel_df,
#'     base_df,
#'     by = "level",
#'     all.x = TRUE,
#'     sort = FALSE
#'   )
#'
#'   rel_df$estimate <- rel_df$estimate_base * rel_df$relativity_final
#'
#'   new_rf_name <- paste0(risk_factor, "_rel")
#'   display_rf_name <- risk_factor_split
#'
#'   map_unsplit <- rfdf[rfdf$risk_factor == risk_factor, c("level", "estimate")]
#'   names(map_unsplit) <- c(risk_factor, "estimate_base")
#'
#'   map_split <- rel_df[, c("level", "new_level", "estimate")]
#'   names(map_split)[names(map_split) == "new_level"] <- risk_factor_split
#'
#'   df_restricted <- df_new
#'   df_restricted$row_id__tmp <- seq_len(nrow(df_restricted))
#'
#'   df_restricted <- merge(
#'     df_restricted,
#'     map_unsplit,
#'     by = risk_factor,
#'     all.x = TRUE,
#'     sort = FALSE
#'   )
#'
#'   df_restricted <- merge(
#'     df_restricted,
#'     map_split,
#'     by.x = c(risk_factor, risk_factor_split),
#'     by.y = c("level", risk_factor_split),
#'     all.x = TRUE,
#'     sort = FALSE
#'   )
#'
#'   df_restricted[[new_rf_name]] <- ifelse(
#'     !is.na(df_restricted$estimate),
#'     df_restricted$estimate,
#'     df_restricted$estimate_base
#'   )
#'
#'   df_restricted <- df_restricted[order(df_restricted$row_id__tmp), ]
#'   rownames(df_restricted) <- NULL
#'
#'   df_restricted$estimate <- NULL
#'   df_restricted$estimate_base <- NULL
#'   df_restricted$row_id__tmp <- NULL
#'
#'   unsplit_levels <- setdiff(
#'     unique(as.character(df_restricted[[risk_factor]])),
#'     names(relativities)
#'   )
#'
#'   if (length(unsplit_levels) > 0) {
#'     unsplit_df <- data.frame(
#'       level = unsplit_levels,
#'       yhat = map_unsplit$estimate_base[
#'         match(unsplit_levels, map_unsplit[[risk_factor]])
#'       ],
#'       risk_factor = rep(display_rf_name, length(unsplit_levels)),
#'       stringsAsFactors = FALSE
#'     )
#'   } else {
#'     unsplit_df <- data.frame(
#'       level = character(0),
#'       yhat = numeric(0),
#'       risk_factor = character(0),
#'       stringsAsFactors = FALSE
#'     )
#'   }
#'
#'   split_df_display <- rel_df[, c("new_level", "estimate")]
#'   names(split_df_display) <- c("level", "yhat")
#'   split_df_display$risk_factor <- rep(display_rf_name, nrow(split_df_display))
#'   split_df_display$level <- as.character(split_df_display$level)
#'
#'   restricted_df_new <- rbind(
#'     unsplit_df[, c("level", "yhat", "risk_factor")],
#'     split_df_display[, c("level", "yhat", "risk_factor")]
#'   )
#'
#'   restricted_df_new <- unique(restricted_df_new)
#'   rownames(restricted_df_new) <- NULL
#'
#'   if (inherits(model, "restricted")) {
#'     restricted_df <- rbind(model$rf_restricted_df, restricted_df_new)
#'   } else if (inherits(model, "smooth")) {
#'     restricted_df <- rbind(model$new_rf, restricted_df_new)
#'   } else {
#'     restricted_df <- restricted_df_new
#'   }
#'
#'   restricted_df <- unique(restricted_df)
#'   rownames(restricted_df) <- NULL
#'
#'   fm_remove <- update_formula_remove(fm_no_offset, risk_factor)
#'   fm_add <- update_formula_add(offset_term, fm_remove, new_rf_name)
#'
#'   rst_lst <- append(rst_lst, list(relativities))
#'   names(rst_lst)[length(rst_lst)] <- new_rf_name
#'
#'   mgd_rst <- append(mgd_rst, list(c(risk_factor, new_rf_name)))
#'   new_col_nm <- unique(append(new_col_nm, c(new_rf_name, display_rf_name)))
#'   old_col_nm <- unique(append(old_col_nm, risk_factor))
#'
#'   rt <- list(
#'     formula_restricted = fm_add[[1]],
#'     formula_removed = fm_remove,
#'     data_restricted = df_restricted,
#'     fm_no_offset = fm_no_offset,
#'     offset = fm_add[[2]],
#'     rating_factors = rfdf,
#'     restrictions_lst = rst_lst,
#'     rf_restricted_df = restricted_df,
#'     model_call = model_call,
#'     model_out = model_out,
#'     new_col_nm = new_col_nm,
#'     old_col_nm = old_col_nm,
#'     mgd_rst = mgd_rst,
#'     mgd_smt = mgd_smt,
#'     relativities_df = rel_df,
#'     normalize = normalize,
#'     exposure = exposure,
#'     base_risk_factor = risk_factor,
#'     risk_factor_split = risk_factor_split,
#'     display_risk_factor = display_rf_name,
#'     model_risk_factor = new_rf_name
#'   )
#'
#'   attr(rt, "class") <- "restricted"
#'   attr(rt, "has_smoothing") <- FALSE
#'   attr(rt, "last_smoothing_step") <- NULL
#'
#'   invisible(rt)
#' }
#'
#'
