#' Include reference group in regression output
#'
#' @description This extracts coefficients in terms of the original levels of the coefficients rather than the coded variables.

#' @param model glm object produced by \code{glm()}
#' @param model_data data.frame used to create glm object
#' @param exposure column in \code{model_data} with exposure
#' @param colname name of column with estimates. Defaults to "estimate".
#' @param exponentiate Logical indicating whether or not to exponentiate the the coefficient estimates. Defaults to TRUE.
#'
#'
#' @importFrom data.table data.table
#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom stats terms
#' @importFrom utils stack
#'
#' @export
rating_factors1 <- function(model, model_data = NULL, exposure = NULL, colname = "estimate", exponentiate = TRUE){

  xl <- model$xlevels
  model_nm <- deparse(substitute(model))

  if(!length(xl)){ # no factors in model
    stop(paste0("no factors in model"))
  }

  xl_names <- names(xl)
  xl_df <- stack(xl)
  xl_df$ind <- as.character(xl_df$ind)
  xl_df$values <- as.character(xl_df$values)
  xl_df$ind_values <- paste0(xl_df$ind, xl_df$values)
  names(xl_df)[names(xl_df) == "values"] <- "level"
  names(xl_df)[names(xl_df) == "ind"] <- "risk_factor"

  exposure <- deparse(substitute(exposure))
  model_data_name <- deparse(substitute(model_data))

  xl_names_in <- xl_names[which(xl_names %in% names(model_data))]
  xl_names_out <- setdiff(xl_names, xl_names_in)


  if ( !is.null( model_data ) & exposure != "NULL" ){

    if ( length( xl_names_in ) > 0){

      model_data <- as.data.frame(model_data)

      if ( !exposure %in% names(model_data) ) {
        stop( exposure, " is unknown in ", model_data_name )
      }

      if ( !is.numeric(model_data[[exposure]] )) {
        stop( exposure, " should be numeric" )
      }

      if ( length( xl_names_out ) > 0){
        message(paste0(xl_names_out, collapse = ", "), " not in ", model_data_name)
      }

      xl_names_in <- xl_names_in[which(xl_names_in %in% names(model_data))]

      exp_fn <- function(var1){
        x <- model_data[!is.na(model_data[[var1]]),]
        x <- data.table::data.table(x)[, lapply(.SD, sum, na.rm = TRUE), by = var1, .SDcols = exposure]
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

  coef <- model$coefficients
  vals <- stack(coef)
  vals$ind <- as.character(vals$ind)

  uit <- dplyr::full_join(xl_df, vals, by = c("ind_values" = "ind"))
  uit$values <- ifelse(is.na( uit$values ), 0, uit$values)

  Terms <- terms(model)
  int <- attr(Terms, "intercept")

  uit$level <- ifelse( int == 1 & uit$ind_values == "(Intercept)", "(Intercept)", uit$level)
  uit$risk_factor <- ifelse( int == 1 & uit$ind_values == "(Intercept)", "(Intercept)", uit$risk_factor)
  uit$level <- ifelse( is.na(uit$level) & is.na(uit$risk_factor), uit$ind_values, uit$level)
  uit$risk_factor <- ifelse( is.na(uit$risk_factor), uit$ind_values, uit$risk_factor)

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
    uit <- uit[, c("risk_factor", "level", "values", exposure)]
  } else {
    uit <- uit[, c("risk_factor", "level", "values")]
  }
  names(uit)[names(uit) == "values"] <- colname
  return(uit)
}


#' Include reference group in regression output
#'
#' @description This extracts coefficients in terms of the original levels of the coefficients rather than the coded variables.
#'
#' @param ... glm object(s) produced by \code{glm()}
#' @param model_data data.frame used to create glm object(s)
#' @param exposure column in \code{model_data} with exposure
#' @param exponentiate Logical indicating whether or not to exponentiate the the coefficient estimates. Defaults to TRUE.
#'
#' @details A fitted linear model has coefficients for the contrasts of the factor terms, usually one less in number than the number of levels.
#' This function re-expresses the coefficients in the original coding. This function is adopted from dummy.coef(). Our adoption prints a data.frame as output.
#'
#' @return data.frame
#'
#' @importFrom dplyr full_join
#' @importFrom utils stack
#'
#' @exportClass riskfactor
#'
#' @author Martin Haringa
#'
#' @examples
#' library(dplyr)
#' df <- MTPL2 %>%
#'     mutate_at(vars(area), as.factor) %>%
#'     mutate_at(vars(area), ~biggest_reference(., exposure))
#'
#' mod1 <- glm(nclaims ~ area + premium, offset = log(exposure), family = poisson(), data = df)
#' mod2 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(), data = df)
#'
#' rating_factors(mod1, mod2, model_data = df, exposure = exposure)
#'
#' @export
rating_factors <- function(..., model_data = NULL, exposure = NULL, exponentiate = TRUE){

  model_data_nm <- deparse(substitute(model_data))
  exposure_nm <- deparse(substitute(exposure))

  modvars <- substitute(list(...))[-1]
  cols <- unique(sapply(modvars, deparse))

  rf_list <- list()
  for (i in 1:length(cols)){
    df <- eval.parent(substitute(rating_factors1(eval(parse( text = cols[i])), model_data, exposure, exponentiate = exponentiate)))
    names(df)[names(df) == "estimate"] <- paste0("est_", cols[i])
    rf_list[[paste0("m_", i)]] <- df
  }

  if( model_data_nm != "NULL" & exposure_nm != "NULL" ) {
    rf_fj <- Reduce(function(dtf1,dtf2) dplyr::full_join(dtf1,dtf2, by = c("risk_factor", "level", exposure_nm)), rf_list)
    rf_fj <- rf_fj[,c("risk_factor", "level", paste0("est_", cols), exposure_nm)]
  } else {
    rf_fj <- Reduce(function(dtf1,dtf2) dplyr::full_join(dtf1,dtf2, by = c("risk_factor", "level")), rf_list)
    rf_fj <- rf_fj[,c("risk_factor", "level", paste0("est_", cols))]
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

  return(structure(list(df = rf_fj,
                        models = cols,
                        exposure = exposure_nm,
                        model_data = model_data_nm,
                        expon = exponentiate),
                   class = "riskfactor"))
}

#' @export
print.riskfactor <- function(x, ...) {
  print(x$df)
}

#' @export
as.data.frame.riskfactor <- function(x, ...) {
  df <- x$df
  return(as.data.frame(df))
}






