#' Include reference group in regression output
#'
#' @description Extract coefficients in terms of the original levels of the
#'   coefficients rather than the coded variables.Use rating_factors() to
#'   compare the output obtained from two or more glm objects.
#'
#' @param model a single glm object produced by `glm()`
#' @param model_data data.frame used to create glm object, this should only
#'   be specified in case the exposure is desired in the output, default
#'   value is NULL
#' @param exposure the name of the exposure column in `model_data`,
#'   default value is NULL
#' @param colname the name of the output column, default value is "estimate"
#' @param exponentiate logical indicating whether or not to exponentiate
#'   the coefficient estimates. Defaults to TRUE.
#' @param round_exposure number of digits for exposure (default to 0)
#'
#' @author Martin Haringa
#'
#' @importFrom data.table data.table
#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom stats terms
#' @importFrom utils stack
#'
#' @examples
#' MTPL2a <- MTPL2
#' MTPL2a$area <- as.factor(MTPL2a$area)
#' x <- glm(nclaims ~ area, offset = log(exposure), family = poisson(), data = MTPL2a)
#' rating_factors1(x)
#'
#' @export
rating_factors1 <- function(model, model_data = NULL, exposure = NULL,
                            colname = "estimate", exponentiate = TRUE,
                            round_exposure = 0){

  xl <- model$xlevels
  model_nm <- deparse(substitute(model))

  if( inherits(model, c("restricted", "smooth")) ) {
    stop("Input must be of class glm. Use refit_glm() first.", call. = FALSE)
  }

  if( !inherits(model, c("glm", "refitsmooth", "refitrestricted")) ) {
    stop("Input must be of class glm.", call. = FALSE)
  }

  if (inherits(model, "refitsmooth")){
    smooth_rf <- attr(model, "new_rf")
    smooth_rf$ind <- as.character(smooth_rf$risk_factor)
    smooth_rf$values <- as.character(smooth_rf$level)
    smooth_rf$ind_values <- paste0(smooth_rf$ind, smooth_rf$values)
    smooth_rf2 <- smooth_rf[, c("ind", "values", "ind_values")]
  }

  if (inherits(model, "refitrestricted")){
    rst_rf <- attr(model, "new_rf_rst")
    rst_rf$ind <- as.character(rst_rf$risk_factor)
    rst_rf$values <- as.character(rst_rf$level)
    rst_rf$ind_values <- paste0(rst_rf$ind, rst_rf$values)
    rst_rf2 <- rst_rf[, c("ind", "values", "ind_values")]
  }

  xl_names <- NULL

  if ( length(xl) > 0) {
    xl_names <- names(xl)
    xl_df <- stack(xl)
    xl_df$ind <- as.character(xl_df$ind)
    xl_df$values <- as.character(xl_df$values)
    xl_df$ind_values <- paste0(xl_df$ind, xl_df$values)
  }

  if ( is.null(xl) ){
    xl_df <- data.frame(ind = character(),
                        values = character(),
                        ind_values = character(),
                        stringsAsFactors = FALSE)
  }


  if (inherits(model, "refitsmooth")) {

    if ( length(xl) > 0){
      xl_df <- rbind(xl_df, smooth_rf2)
    }

    if ( !length(xl) ){
      xl_df <- smooth_rf2
    }

    xl_names <- c(xl_names, unique(smooth_rf$ind))
  }

  if (inherits(model, "refitrestricted")) {

    if ( length(xl) > 0){
      xl_df <- rbind(xl_df, rst_rf2)
    }

    if ( !length(xl) ){
      xl_df <- rst_rf2
    }

    xl_names <- c(xl_names, unique(rst_rf$ind))
  }

  names(xl_df)[names(xl_df) == "values"] <- "level"
  names(xl_df)[names(xl_df) == "ind"] <- "risk_factor"

  exposure <- deparse(substitute(exposure))
  model_data_name <- deparse(substitute(model_data))

  xl_names_in <- xl_names[which(xl_names %in% names(model_data))]
  xl_names_out <- setdiff(xl_names, xl_names_in)

  # Determine exposure per level
  if ( !is.null( model_data ) & exposure != "NULL" ){

    if ( length( xl_names_in ) > 0){

      model_data <- as.data.frame(model_data)

      if ( !exposure %in% names(model_data) ) {
        stop( exposure, " is unknown in ", model_data_name, call. = FALSE)
      }

      if ( !is.numeric(model_data[[exposure]] )) {
        stop( exposure, " should be numeric", call. = FALSE)
      }

      if ( length( xl_names_out ) > 0){
        message(paste0(xl_names_out, collapse = ", "), " not in ", model_data_name)
      }

      xl_names_in <- xl_names_in[which(xl_names_in %in% names(model_data))]

      exp_fn <- function(var1){
        x <- model_data[!is.na(model_data[[var1]]),]
        x <- data.table::data.table(x)[, lapply(.SD, sum, na.rm = TRUE),
                                       by = var1, .SDcols = exposure]
        names(x)[1] <- c("level")
        x$risk_factor <- var1
        return(x)
      }

      listexp <- lapply(xl_names_in, exp_fn)
      dfexp <- do.call(rbind, listexp)
      dfexp$level <- as.character(dfexp$level)
      xl_df <- dplyr::left_join(xl_df, dfexp, by = c("level", "risk_factor"))
    } else{
      message(paste0(xl_names_out, collapse = ", "), " not in ", model_data_name)
    }
  }

  coef <- coefficients(model)
  vals <- stack(coef)

  vals$pvalues <- as.numeric(coef(summary(model))[,4])
  vals$ind <- as.character(vals$ind)

  if ( inherits(model, "refitsmooth")){
    smooth_coef <- smooth_rf[, c("yhat", "ind_values")]
    colnames(smooth_coef)[1] <- "values"
    colnames(smooth_coef)[2] <- "ind"
    smooth_coef$pvalues <- NA
    smooth_coef$values <- log(smooth_coef$values)
    vals <- rbind(vals, smooth_coef)
  }

  if ( inherits(model, "refitrestricted")){
    rst_coef <- rst_rf[, c("yhat", "ind_values")]
    colnames(rst_coef)[1] <- "values"
    colnames(rst_coef)[2] <- "ind"
    rst_coef$pvalues <- NA
    rst_coef$values <- log(rst_coef$values)
    vals <- rbind(vals, rst_coef)
  }

  uit <- dplyr::full_join(xl_df, vals, by = c("ind_values" = "ind"))
  uit$values <- ifelse(is.na( uit$values ), 0, uit$values)

  Terms <- terms(model)
  int <- attr(Terms, "intercept")

  uit$level <- ifelse( int == 1 & uit$ind_values == "(Intercept)",
                       "(Intercept)", uit$level)
  uit$risk_factor <- ifelse( int == 1 & uit$ind_values == "(Intercept)",
                             "(Intercept)", uit$risk_factor)
  uit$level <- ifelse( is.na(uit$level) & is.na(uit$risk_factor),
                       uit$ind_values, uit$level)
  uit$risk_factor <- ifelse( is.na(uit$risk_factor), uit$ind_values,
                             uit$risk_factor)

  if ( isTRUE( exponentiate )) {
    uit$values <- exp(uit$values)
  }

  if ( int == 1){
    int_row <- uit[uit$risk_factor == "(Intercept)", ]
    uit1 <- uit[uit$risk_factor != "(Intercept)", ]
    uit <- rbind(int_row, uit1)
  }
  row.names(uit) <- NULL
  if ( !is.null( model_data ) & exposure != "NULL" & length( xl_names_in ) > 0){
    uit <- uit[, c("risk_factor", "level", "values", exposure, "pvalues")]
    uit[[exposure]] <- round(uit[[exposure]], round_exposure)
  } else {
    uit <- uit[, c("risk_factor", "level", "values", "pvalues")]
  }
  names(uit)[names(uit) == "values"] <- colname

  uit$pvalues <- sapply(uit$pvalues, function(x) make_stars(x))
  return(uit)
}


#' Include reference group in regression output
#'
#' @description Extract coefficients in terms of the original levels of the
#'   coefficients rather than the coded variables.
#'
#' @param ... glm object(s) produced by `glm()`
#' @param model_data data.frame used to create glm object(s), this should only
#'   be specified in case the exposure is desired in the output, default value is NULL
#' @param exposure column in `model_data` with exposure, default value is NULL
#' @param exponentiate logical indicating whether or not to exponentiate the
#'   coefficient estimates. Defaults to TRUE.
#' @param signif_stars show significance stars for p-values (defaults to TRUE)
#' @param round_exposure number of digits for exposure (defaults to 0)
#'
#' @details A fitted linear model has coefficients for the contrasts of the
#'   factor terms, usually one less in number than the number of levels. This
#'   function re-expresses the coefficients in the original coding. This
#'   function is adopted from dummy.coef(). Our adoption prints a data.frame as output.
#'
#' @return data.frame
#'
#' @importFrom dplyr full_join
#' @importFrom utils stack
#' @importFrom stats coefficients
#'
#' @author Martin Haringa
#'
#' @examples
#' library(dplyr)
#' df <- MTPL2 %>%
#'   mutate(across(c(area), as.factor)) %>%
#'   mutate(across(c(area), ~biggest_reference(., exposure)))
#'
#' mod1 <- glm(nclaims ~ area + premium, offset = log(exposure), family = poisson(), data = df)
#' mod2 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(), data = df)
#'
#' rating_factors(mod1, mod2, model_data = df, exposure = exposure)
#'
#' @export
rating_factors <- function(..., model_data = NULL, exposure = NULL,
                           exponentiate = TRUE, signif_stars = TRUE,
                           round_exposure = 0){

  model_data_nm <- deparse(substitute(model_data))
  exposure_nm <- deparse(substitute(exposure))

  modvars <- substitute(list(...))[-1]
  cols <- unique(sapply(modvars, deparse))

  rf_list <- list()
  for (i in 1:length(cols)){
    df <- eval.parent(substitute(rating_factors1(eval(parse( text = cols[i])),
                                                 model_data, exposure,
                                                 exponentiate = exponentiate,
                                                 round_exposure = round_exposure)))
    names(df)[names(df) == "estimate"] <- paste0("est_", cols[i])
    names(df)[names(df) == "pvalues"] <- paste0("signif_", cols[i])
    rf_list[[paste0("m_", i)]] <- df
  }

  if( model_data_nm != "NULL" & exposure_nm != "NULL" ) {
    rf_fj <- Reduce(function(dtf1, dtf2) dplyr::full_join(dtf1, dtf2, by = c("risk_factor", "level", exposure_nm)), rf_list)
    rf_fj <- rf_fj[,c("risk_factor", "level", paste0("est_", cols), paste0("signif_", cols), exposure_nm)]
  } else {
    rf_fj <- Reduce(function(dtf1, dtf2) dplyr::full_join(dtf1, dtf2, by = c("risk_factor", "level")), rf_list)
    rf_fj <- rf_fj[,c("risk_factor", "level", paste0("est_", cols), paste0("signif_", cols))]
  }

  if ( !isTRUE(signif_stars) ){
    rf_fj <- rf_fj[ , -which(names(rf_fj) %in% paste0("signif_", cols))]
  }

  if ( !is.null(model_data)){
    lst_order <- lapply(names(model_data), function(x) { attributes(model_data[[x]])$xoriginal })
    names(lst_order) <- names(model_data)
    lst_order <- lst_order[lengths(lst_order) != 0]

    if (length(lst_order) > 0){
      df_order <- stack(lst_order)
      names(df_order) <- c("level", "risk_factor")
      df_order <- df_order[, 2:1]
      df_order <- df_order[df_order$risk_factor %in% unique(rf_fj$risk_factor), ]
      rf_fj$risk_factor <- as.character(rf_fj$risk_factor)
      df_order$risk_factor <- as.character(df_order$risk_factor)
      uit <- dplyr::full_join(df_order, rf_fj, by = c("risk_factor", "level"))
      rf_fj <- uit[order(match(uit$risk_factor, rf_fj$risk_factor)),]
      rownames(rf_fj) <- NULL
    }
  }

  rf_fj_stars <- NULL
  signif_levels <- NULL

  if ( isTRUE(signif_stars) ) {
    signif_levels <- "Significance levels: *** p < 0.001; ** p < 0.01;  * p < 0.05; . p < 0.1"
    rf_fj_stars <- rf_fj
    for (i in 1:length(cols)){
      pvalues_num <- round(rf_fj_stars[[paste0("est_", cols[i])]], 6)
      pvalues_char <- format(pvalues_num, digits = 6, nsmall = 2)
      stars_char <- rf_fj_stars[[paste0("signif_", cols[i])]]
      stars_char[is.na(stars_char)] <- ""
      rf_fj_stars[[paste0("est_", cols[i])]] <- format(paste0(pvalues_char, " ",
                                                              stars_char), justify = "left")
    }
    rf_fj_stars <- rf_fj_stars[ , -which(names(rf_fj_stars) %in% paste0("signif_", cols))]
  }

  return(structure(list(df = rf_fj,
                        df_stars = rf_fj_stars,
                        models = cols,
                        exposure = exposure_nm,
                        model_data = model_data_nm,
                        expon = exponentiate,
                        signif_stars = signif_stars,
                        signif_levels = signif_levels),
                   class = "riskfactor"))
}

#' @export
print.riskfactor <- function(x, ...) {

  if ( isTRUE( x$signif_stars ) ){
    x1 <- x$signif_levels
    x1[!is.na(x1)] <- paste0("\033[34m", x1[!is.na(x1)], "\033[39m")
    cat(x1)
    print(x$df_stars)
  } else{
    print(x$df)
  }
}


#' @export
as.data.frame.riskfactor <- function(x, ...) {

  if ( isTRUE( x$signif_stars ) ){
    df <- x$df_stars
  } else{
    df <- x$df
  }

  return(as.data.frame(df))
}

#' Automatically create a ggplot for objects obtained from rating_factors()
#'
#' @description Takes an object produced by `univariate()`, and plots the
#'   available input.
#'
#' @param object riskfactor object produced by `rating_factors()`
#' @param risk_factors character vector to define which factors are included.
#'   Defaults to all risk factors.
#' @param ncol number of columns in output (default is 1)
#' @param labels show labels with the exposure (default is TRUE)
#' @param dec.mark control the format of the decimal point, as well as the mark
#'   between intervals before the decimal point, choose either "," (default) or "."
#' @param ylab modify label for the y-axis
#' @param fill color to fill histogram
#' @param color color to plot line colors of histogram (default is "skyblue")
#' @param linetype use different linetypes (default is FALSE)
#' @param ... other plotting parameters to affect the plot
#'
#' @author Martin Haringa
#'
#' @return a ggplot2 object
#' @export
#'
#' @import ggplot2
#' @importFrom patchwork wrap_plots
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#'
#' @examples
#' library(dplyr)
#' df <- MTPL2 %>%
#'   mutate(across(c(area), as.factor)) %>%
#'   mutate(across(c(area), ~biggest_reference(., exposure)))
#'
#' mod1 <- glm(nclaims ~ area + premium, offset = log(exposure), family = poisson(), data = df)
#' mod2 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(), data = df)
#'
#' x <- rating_factors(mod1, mod2, model_data = df, exposure = exposure)
#' autoplot(x)
#'
autoplot.riskfactor <- function(object, risk_factors = NULL, ncol = 1,
                                labels = TRUE, dec.mark = ",",
                                ylab = "rate", fill = NULL, color = NULL,
                                linetype = FALSE, ...){

  if ( !inherits(object, "riskfactor")){
    stop("autoplot.riskfactor requires a riskfactor object, use object = object", call. = FALSE)
  }

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("patchwork is needed for this function to work. Install it via install.packages(\"patchwork\")", call. = FALSE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }

  df <- object$df
  models <- object$models
  models_nm <- paste0("est_", models)
  exposure_nm <- object$exposure
  expon <- object$expon

  df <- df[df$risk_factor != df$level, ]

  df_long <- tidyr::pivot_longer(df, cols = tidyselect::all_of(models_nm), names_to = "model", values_to = "est")
  df_long$model <- gsub("^est_", "", df_long$model)

  if ( !isTRUE(expon) ){
    df_long$est = exp(df_long$est)
  }


  if ( dec.mark == "," ){
    sep_fn <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  } else{
    sep_fn <- function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)
  }

  if ( is.null(risk_factors) ){
    rf_names <- unique(df$risk_factor)
  } else if ( all(risk_factors %in% df$risk_factor)) {
    rf_names <- risk_factors
  } else{
    rf_diff <- setdiff(risk_factors, unique(df$risk_factor))
    stop(paste(rf_diff, collapse = ", "), " unknown risk_factor(s)", call. = FALSE)
  }


  fig_list <- list()
  for ( i in 1:length(rf_names)){

    df1 <- df_long[df_long$risk_factor == rf_names[i],]
    df1[["level"]] <- factor(df1[["level"]], levels = unique(df1[["level"]]))

    if (exposure_nm != "NULL") {
      df1$s_axis_scale <- df1[[exposure_nm]] / max(df1[[exposure_nm]],
                                                   na.rm = TRUE) * max(df1[["est"]],
                                                                       na.rm = TRUE)
      df1$y_print <- round(df1[[exposure_nm]], 0)
      df1 <- df1[!is.na(df1$est),]
      df1_bar <- unique(df1[, c("risk_factor", "level", exposure_nm, "s_axis_scale", "y_print")])
    }

    if ( is.null(fill) & is.null(color) ){
      color <- "lightskyblue"
      fill <- lighten_color(color)[2]
    }

    if ( is.null(fill) & !is.null(color) ){
      color <- color
      fill <- lighten_color(color)[2]
    }

    if ( !is.null(fill) & is.null(color) ){
      fill <- fill
      color <- darken_color(fill)[3]
    }

    fig_list[[paste0("p", i)]] <- ggplot2::ggplot(data = df1) +
      ggplot2::theme_minimal() +
      { if (exposure_nm == "NULL")  ggplot2::scale_y_continuous(labels = sep_fn,
                                                                limits = c(0, NA),
                                                                expand = expansion(mult = c(0, 0.02))) } +
      { if (exposure_nm != "NULL") ggplot2::geom_bar(data = df1_bar,
                                                     aes(x = .data[["level"]],
                                                         y = .data[["s_axis_scale"]]),
                                                     stat = "identity",
                                                     color = color,
                                                     fill = fill,
                                                     alpha = 1) } +
      { if (exposure_nm != "NULL") ggplot2::scale_y_continuous(
        labels = sep_fn,
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.02)),
        sec.axis = sec_axis(~ . * max(df1_bar[[exposure_nm]]) / max(df1[["est"]]),
                            name = exposure_nm,
                            labels = sep_fn)) } +
      ggplot2::geom_point(aes(x = .data[["level"]],
                              y = .data[["est"]],
                              group = .data[["model"]],
                              color = .data[["model"]])) +
      { if ( length(models) == 1) theme(legend.position = "none") } +
      { if (isTRUE(linetype)) ggplot2::geom_line(aes(x = .data[["level"]],
                                                     y = .data[["est"]],
                                                     group = .data[["model"]],
                                                     color = .data[["model"]],
                                                     linetype = .data[["model"]])) } +
      { if (!isTRUE(linetype)) ggplot2::geom_line(aes(x = .data[["level"]],
                                                      y = .data[["est"]],
                                                      group = .data[["model"]],
                                                      color = .data[["model"]])) } +
      { if ( isTRUE(labels) & exposure_nm != "NULL") ggplot2::geom_text(
        aes(x = .data[["level"]],
            y = .data[["s_axis_scale"]],
            label = sep_fn(.data[["y_print"]])),
        vjust = "inward",
        size = 3) } +
      ggplot2::labs(x = rf_names[i],
                    y = ylab) +
      ggplot2::theme(legend.title = element_blank())
  }

  pl <- patchwork::wrap_plots(fig_list, ncol = ncol)
  return(pl)
}







