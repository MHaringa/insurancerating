#' Restrict coefficients in the model
#'
#' @description `r lifecycle::badge('experimental')`
#'  Add restrictions, like a bonus-malus structure, on the risk
#'  factors used in the model. `restrict_coef()` must always be followed
#'  by `update_glm()`.
#'
#' @author Martin Haringa
#'
#' @details Although restrictions could be applied either to the frequency or
#'   the severity model, it is more appropriate to impose the restrictions
#'   on the premium model. This can be achieved by calculating the pure
#'   premium for each record (i.e. expected number of claims times the expected
#'   claim amount), then fitting an "unrestricted" Gamma GLM to the pure
#'   premium,and then imposing the restrictions in a final "restricted" Gamma
#'   GLM.
#'
#' @param model object of class glm/restricted
#' @param restrictions data.frame with two columns containing restricted data.
#'   The first column, with the name of the risk factor as column name, must
#'   contain the levels of the risk factor. The second column must contain the
#'   restricted coefficients.
#'
#' @family update_glm
#' @family autoplot.restricted
#' @seealso [update_glm()] for refitting the restricted model,
#' and [autoplot.restricted()].
#'
#' @return Object of class restricted.
#'
#' @examples
#' \dontrun{
#' # Add restrictions to risk factors for region (zip) -------------------------
#'
#' # Fit frequency and severity model
#' library(dplyr)
#' freq <- glm(nclaims ~ bm + zip, offset = log(exposure), family = poisson(),
#'              data = MTPL)
#' sev <- glm(amount ~ bm + zip, weights = nclaims,
#'             family = Gamma(link = "log"),
#'             data = MTPL %>% filter(amount > 0))
#'
#' # Add predictions for freq and sev to data, and calculate premium
#' premium_df <- MTPL %>%
#'    add_prediction(freq, sev) %>%
#'    mutate(premium = pred_nclaims_freq * pred_amount_sev)
#'
#' # Restrictions on risk factors for region (zip)
#' zip_df <- data.frame(zip = c(0,1,2,3), zip_rst = c(0.8, 0.9, 1, 1.2))
#'
#' # Fit unrestricted model
#' burn <- glm(premium ~ bm + zip, weights = exposure,
#'             family = Gamma(link = "log"), data = premium_df)
#'
#' # Fit restricted model
#' burn_rst <- burn %>%
#'   restrict_coef(., zip_df) %>%
#'   update_glm()
#'
#' # Show rating factors
#' rating_factors(burn_rst)
#' }
#'
#' @export
restrict_coef <- function(model, restrictions) {

  if (inherits(model, "glm")) {
    fm <- formula(model)
    offset_term <- get_offset(model)
    fm_no_offset <- remove_offset_formula(fm)
    df_new <- model$data
    model_call <- model$call
    model_out <- model

    rfdf <- rating_factors(model, signif_stars = FALSE)$df
    colnames(rfdf)[3] <- c("estimate")
    rst_lst <- list(restrictions)
    names(rst_lst) <- names(restrictions[1])
    restricted_df <- restrict_df(restrictions)
    new_col_nm <- NULL
    old_col_nm <- NULL
    mgd_rst <- NULL
    mgd_smt <- NULL
  }

  if (inherits(model, c("smooth", "restricted"))) {
    fm <- model$formula_restricted
    offset_term <- model$offset
    fm_no_offset <- model$formula_removed
    df_new <- model$data_restricted
    model_call <- model$model_call
    model_out <- model$model_out

    rfdf <- model$rating_factors
    rst_lst <- model$restrictions_lst
    rst_lst[[names(restrictions)[1]]] <- restrictions
    restricted_df <- restrict_df(restrictions)
    new_col_nm <- model$new_col_nm
    old_col_nm <- model$old_col_nm
    mgd_rst <- model$mgd_rst
    mgd_smt <- model$mgd_smt
  }

  if (inherits(model, "restricted")) {
    restricted_df <- rbind(model$rf_restricted_df, restricted_df)
  }

  if (inherits(model, "smooth")) {
    restricted_df <- rbind(model$new_rf, restricted_df)
  }

  fm_remove <- update_formula_remove(fm_no_offset, names(restrictions)[1])
  fm_add <- update_formula_add(offset_term, fm_remove, names(restrictions)[2])
  df_restricted <- add_restrictions_df(df_new, restrictions)

  nrst <- unique(setdiff(names(restrictions), unique(rfdf$risk_factor)))
  orst <- unique(setdiff(names(restrictions), new_col_nm))
  mgd_rst <- append(mgd_rst, list(unique(c(orst, nrst))))

  new_col_nm <- unique(append(new_col_nm,
                              setdiff(names(restrictions),
                                      unique(rfdf$risk_factor))))
  old_col_nm <- unique(append(old_col_nm, setdiff(names(restrictions),
                                                  new_col_nm)))

  rt <- list(formula_restricted = fm_add[[1]],
             formula_removed = fm_remove,
             data_restricted = df_restricted,
             fm_no_offset = fm_no_offset,
             offset = fm_add[[2]],
             rating_factors = rfdf,
             restrictions_lst = rst_lst,
             rf_restricted_df = restricted_df,
             model_call = model_call,
             model_out = model_out,
             new_col_nm = new_col_nm,
             old_col_nm = old_col_nm,
             mgd_rst = mgd_rst,
             mgd_smt = mgd_smt)
  attr(rt, "class") <- "restricted"
  invisible(rt)
}



#' Smooth coefficients in the model
#'
#' @description `r lifecycle::badge('experimental')`
#'  Apply smoothing on the risk factors used in the model. `smooth_coef()`
#'  must always be followed by `update_glm()`.
#'
#' @author Martin Haringa
#'
#' @details Although smoothing could be applied either to the frequency or
#'   the severity model, it is more appropriate to impose the smoothing
#'   on the premium model. This can be achieved by calculating the pure
#'   premium for each record (i.e. expected number of claims times the expected
#'   claim amount), then fitting an "unrestricted" Gamma GLM to the pure
#'   premium, and then imposing the restrictions in a final "restricted"
#'   Gamma GLM.
#'
#' @param model object of class glm/smooth
#' @param x_cut column name with breaks/cut
#' @param x_org column name where x_cut is based on
#' @param degree order of polynomial
#' @param breaks numerical vector with new clusters for x
#'
#' @family update_glm
#' @family autoplot.smooth
#' @seealso [update_glm()] for refitting the smoothed model,
#' and [autoplot.smooth()].
#'
#' @return Object of class smooth
#'
#' @examples
#' \dontrun{
#' library(insurancerating)
#' library(dplyr)
#'
#' # Fit GAM for claim frequency
#' age_policyholder_frequency <- fit_gam(data = MTPL,
#'                                       nclaims = nclaims,
#'                                       x = age_policyholder,
#'                                       exposure = exposure)
#'
#' # Determine clusters
#' clusters_freq <- construct_tariff_classes(age_policyholder_frequency)
#'
#' # Add clusters to MTPL portfolio
#' dat <- MTPL %>%
#'   mutate(age_policyholder_freq_cat = clusters_freq$tariff_classes) %>%
#'   mutate(across(where(is.character), as.factor)) %>%
#'   mutate(across(where(is.factor), ~biggest_reference(., exposure)))
#'
#' # Fit frequency and severity model
#' freq <- glm(nclaims ~ bm + age_policyholder_freq_cat, offset = log(exposure),
#'  family = poisson(), data = dat)
#' sev <- glm(amount ~ bm + zip, weights = nclaims,
#'  family = Gamma(link = "log"), data = dat %>% filter(amount > 0))
#'
#' # Add predictions for freq and sev to data, and calculate premium
#' premium_df <- dat %>%
#'   add_prediction(freq, sev) %>%
#'   mutate(premium = pred_nclaims_freq * pred_amount_sev)
#'
#' # Fit unrestricted model
#' burn_unrestricted <- glm(premium ~ zip + bm + age_policyholder_freq_cat,
#'                          weights = exposure,
#'                          family = Gamma(link = "log"),
#'                          data = premium_df)
#'
#' # Impose smoothing and create figure
#' burn_unrestricted %>%
#'   smooth_coef(x_cut = "age_policyholder_freq_cat",
#'               x_org = "age_policyholder",
#'               breaks = seq(18, 95, 5)) %>%
#'   autoplot()
#'
#' # Impose smoothing and refit model
#' burn_restricted <- burn_unrestricted %>%
#'   smooth_coef(x_cut = "age_policyholder_freq_cat",
#'               x_org = "age_policyholder",
#'               breaks = seq(18, 95, 5)) %>%
#'   update_glm()
#'
#' # Show new rating factors
#' rating_factors(burn_restricted)
#' }
#'
#' @export
smooth_coef <- function(model, x_cut, x_org, degree = NULL, breaks = NULL) {

  if (is.null(breaks) || !is.numeric(breaks)) {
    stop("'breaks' must be a numerical vector", call. = FALSE)
  }

  if (inherits(model, "glm")) {
    fm <- formula(model)
    offset_term <- get_offset(model)
    fm_no_offset <- remove_offset_formula(fm)
    df_new <- model$data
    model_call <- model$call
    model_out <- model

    rfdf <- rating_factors(model, signif_stars = FALSE)$df
    colnames(rfdf)[3] <- c("estimate")
    rst_lst <- NULL
    new_col_nm <- NULL
    old_col_nm <- NULL
    mgd_smt <- NULL
    mgd_rst <- NULL
  }

  if (inherits(model, c("smooth", "restricted"))) {
    fm <- model$formula_restricted
    offset_term <- model$offset
    fm_no_offset <- model$formula_removed
    df_new <- model$data_restricted
    model_call <- model$model_call
    model_out <- model$model_out

    rfdf <- model$rating_factors
    rst_lst <- model$restrictions_lst
    new_col_nm <- model$new_col_nm
    old_col_nm <- model$old_col_nm
    mgd_smt <- model$mgd_smt
    mgd_rst <- model$mgd_rst
  }

  mgd_smt <- append(mgd_smt, list(c(paste0(x_org, "_smooth"),
                                    paste0(x_cut, "_smooth"))))

  old_col_nm <- append(old_col_nm, paste0(x_org, "_smooth"))
  new_col_nm <- append(new_col_nm, paste0(x_cut, "_smooth"))

  fm_remove <- update_formula_remove(fm_no_offset, x_cut)
  fm_add <- update_formula_add(offset_term, fm_remove, paste0(x_cut, "_smooth"))

  borders_x_cut <- cut_borders_model(model, x_cut)

  if (is.null(degree)) {
    degree <- nrow(borders_x_cut) - 1
  }

  fit_poly <- fit_polynomial(borders_x_cut, x_org, degree, breaks)
  df_poly <- fit_poly[["new_poly_df"]]
  df_poly_line <- fit_poly[["poly_line"]]
  df_new_rf <- fit_poly[["new_rf"]]

  if (inherits(model, "smooth")) {
    df_new_rf <- rbind(model$new_rf, df_new_rf)
  }

  if (inherits(model, "restricted")) {
    df_new_rf <- rbind(model$rf_restricted_df, df_new_rf)
  }

  df_smooth <- join_to_nearest(df_new, df_poly, x_org)
  names(df_smooth)[names(df_smooth) == "yhat"] <- paste0(x_cut, "_smooth")

  st <- list(formula_restricted = fm_add[[1]],
             formula_removed = fm_remove,
             data_restricted = df_smooth,
             fm_no_offset = fm_no_offset,
             offset = fm_add[[2]],
             borders = borders_x_cut,
             new = df_poly,
             new_line = df_poly_line,
             model_call = model_call,
             rating_factors = rfdf,
             restrictions_lst = rst_lst,
             new_rf = df_new_rf,
             degree = degree,
             model_out = model_out,
             new_col_nm = new_col_nm,
             old_col_nm = old_col_nm,
             mgd_rst = mgd_rst,
             mgd_smt = mgd_smt)
  attr(st, "class") <- "smooth"
  invisible(st)
}



#' Print for object of class restricted
#'
#' @param x Object of class restricted
#' @param ... other plotting parameters to affect the output
#'
#' @noRd
#' @return Print object
#'
#' @author Martin Haringa
#'
#' @export
print.restricted <- function(x, ...) {
  cat("Formula: ")
  print(x$formula_restricted)
}

#' Print for object of class smooth
#'
#' @param x Object of class smooth
#' @param ... other plotting parameters to affect the output
#'
#' @return Print object
#' @noRd
#'
#' @author Martin Haringa
#'
#' @export
print.smooth <- function(x, ...) {
  cat("Formula: ")
  print(x$formula_restricted)
}

#' Automatically create a ggplot for objects obtained from restrict_coef()
#'
#' @description `r lifecycle::badge('experimental')`
#'  Takes an object produced by `restrict_coef()`, and produces
#'  a line plot with a comparison between the restricted coefficients and
#'  estimated coefficients obtained from the model.
#'
#' @param object object produced by `restrict_coef()`
#' @param ... other plotting parameters to affect the plot
#'
#' @author Martin Haringa
#'
#' @importFrom dplyr left_join
#' @importFrom data.table melt
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#' @import ggplot2
#'
#' @return Object of class ggplot2
#'
#' @examples
#' freq <- glm(nclaims ~ bm + zip, weights = power, family = poisson(),
#'  data = MTPL)
#' zip_df <- data.frame(zip = c(0,1,2,3), zip_rst = c(0.8, 0.9, 1, 1.2))
#' freq %>%
#'   restrict_coef(., zip_df) %>%
#'   autoplot()
#'
#' @export
autoplot.restricted <- function(object, ...) {

  names_rf <- names(object$restrictions_lst)
  name <- names_rf[length(names_rf)]
  naam_rst <- object$restrictions_lst[[name]]

  rf <- object$rating_factors
  naam_rf <- rf[rf$risk_factor == name, ]
  naam_rf <- naam_rf[, 2:3]
  names(naam_rst)[names(naam_rst) == name] <- "level"

  naam_rf <- matchColClasses(naam_rst, naam_rf)

  koppel <- dplyr::left_join(naam_rst, naam_rf, by = "level")
  meas_vars <- c(names(naam_rst)[2], names(rf)[3])

  koppel_dt <- data.table::setDT(koppel)
  koppel_ldt <- data.table::melt(koppel_dt,
                                 id.vars = names(koppel_dt)[!names(
                                   koppel_dt) %in% meas_vars],
                                 measure.vars = meas_vars,
                                 variable.name = "type",
                                 value.name = "Coef")
  koppel <- data.table::setDF(koppel_ldt)
  koppel$level <- as.factor(koppel$level)
  koppel$type <- as.character(koppel$type)

  koppel$type[koppel$type == names(naam_rst)[2]] <- "restricted"
  koppel$type[koppel$type == names(rf)[3]] <- "unrestricted"

  ggplot2::ggplot(data = koppel, aes(x = level,
                                     y = Coef,
                                     color = type, group = type)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = name, color = NULL)
}

#' Automatically create a ggplot for objects obtained from smooth_coef()
#'
#' @description `r lifecycle::badge('experimental')`
#'  Takes an object produced by `smooth_coef()`, and produces
#'  a plot with a comparison between the smoothed coefficients and
#'  estimated coefficients obtained from the model.
#'
#' @param object object produced by `smooth_coef()`
#' @param ... other plotting parameters to affect the plot
#'
#' @author Martin Haringa
#'
#' @importFrom dplyr left_join
#' @import ggplot2
#' @importFrom scales ordinal
#'
#' @return Object of class ggplot2
#'
#' @export
autoplot.smooth <- function(object, ...) {
  rf2 <- object$borders
  new <- object$new
  new_line <- object$new_line
  degree <- scales::ordinal(object$degree)
  degree_name <- paste0(degree, " order polynomial")

  rf2_start_open <- rf2[rf2$start_oc == "open", ]
  rf2_start_closed <- rf2[rf2$start_oc == "closed", ]
  rf2_end_open <- rf2[rf2$end_oc == "open", ]
  rf2_end_closed <- rf2[rf2$end_oc == "closed", ]

  new_start_open <- new[new$start_oc == "open", ]
  new_start_closed <- new[new$start_oc == "closed", ]
  new_end_open <- new[new$end_oc == "open", ]
  new_end_closed <- new[new$end_oc == "closed", ]

  x_name <- names(new_line)[1]
  names(new_line)[names(new_line) == x_name] <- "col1"

  ggplot2::ggplot(data = rf2) +
    ggplot2::geom_segment(ggplot2::aes(x = start_, y = estimate, xend = end_,
                                       yend = estimate, color = "Model fit"),
                          group = 1) +
    ggplot2::geom_segment(data = new, ggplot2::aes(x = breaks_min, y = yhat,
                                                   xend = breaks_max,
                                                   yend = yhat,
                                                   color = "New cluster"),
                          group = 2) +
    ggplot2::geom_point(data = rf2_start_closed, ggplot2::aes(x = start_,
                                                              y = estimate),
                        color = "dodgerblue") +
    ggplot2::geom_point(data = rf2_end_closed, ggplot2::aes(x = end_,
                                                            y = estimate),
                        color = "dodgerblue") +
    ggplot2::geom_point(data = rf2_start_open, ggplot2::aes(x = start_,
                                                            y = estimate),
                        color = "dodgerblue", shape = 21, fill = "white") +
    ggplot2::geom_point(data = rf2_end_open, ggplot2::aes(x = end_,
                                                          y = estimate),
                        color = "dodgerblue", shape = 21, fill = "white") +
    ggplot2::geom_point(data = new_start_closed, ggplot2::aes(x = start_,
                                                              y = yhat),
                        color = "red") +
    ggplot2::geom_point(data = new_end_closed, ggplot2::aes(x = end_,
                                                            y = yhat),
                        color = "red") +
    ggplot2::geom_point(data = new_start_open, ggplot2::aes(x = start_,
                                                            y = yhat),
                        color = "red", shape = 21, fill = "white") +
    ggplot2::geom_point(data = new_end_open, ggplot2::aes(x = end_,
                                                          y = yhat),
                        color = "red", shape = 21, fill = "white") +
    ggplot2::labs(x = x_name, y = "Estimated coefficient") +
    ggplot2::geom_line(data = new_line, ggplot2::aes(x = col1, y = yhat,
                                                     color = "Smooth"),
                       group = 3) +
    ggplot2::scale_colour_manual(name = "Risk factor",
                                 values = c("Model fit" = "dodgerblue",
                                            "New cluster" = "red",
                                            "Smooth" = "black"),
                                 labels = c("Model fit", "New cluster",
                                            degree_name)) +
    ggplot2::theme_minimal()
}


#' Refitting Generalized Linear Models
#'
#' @description `r lifecycle::badge('experimental')`
#'  `update_glm()` is used to refit generalized linear models, and must be
#'  preceded by `restrict_coef()`.
#'
#' @param x Object of class restricted or of class smooth
#'
#' @author Martin Haringa
#'
#' @importFrom stats glm
#' @importFrom utils modifyList
#'
#' @return Object of class GLM
#'
#' @export
update_glm <- function(x) {

  if (!inherits(x, c("restricted", "smooth"))) {
    stop("Input must be of class restricted or of class smooth", call. = FALSE)
  }

  lst_call <- as.list(x$model_call)
  lst <- list(formula = x$formula_restricted, data = x$data_restricted,
              offset = NULL)
  y <- eval(as.call(modifyList(lst_call, lst)))
  y$call$formula <- lst$formula
  y$call$data <- quote(df_new)

  offweights <- NULL
  if (!is.null(lst_call$weights)) {
    offweights <- append(offweights, as.character(lst_call$weights))
  }

  if (!is.null(lst_call$offset)) {
    offweights <- append(offweights, as.character(lst_call$offset)[2])
  }

  if (inherits(x, "smooth")) {
    attr(y, "new_rf") <- x[["new_rf"]]
    attr(y, "class") <- append(class(y), "refitsmooth")
  }

  if (inherits(x, "restricted")) {
    attr(y, "new_rf_rst") <- x[["rf_restricted_df"]]
    attr(y, "class") <- append(class(y), "refitrestricted")
  }

  rf <- x$rating_factors
  rf2 <- unique(rf$risk_factor[rf$risk_factor != "(Intercept)"])

  attr(y, "new_col_nm") <- x$new_col_nm
  attr(y, "old_col_nm") <- x$old_col_nm
  attr(y, "rf") <- rf2
  attr(y, "mgd_smt") <- x$mgd_smt
  attr(y, "mgd_rst") <- x$mgd_rst
  attr(y, "offweights") <- offweights
  y
}
