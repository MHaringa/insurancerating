#' Get offset from model object
#'
#' @param model Must be of class glm
#'
#' @return Character string with offset term
#'
#' @keywords internal
get_offset <- function(model) {

  if( !inherits(model, "lm") ) {
    stop("Input must be of class (g)lm", call. = FALSE)
  }

  nm1 <- names(attributes(model$terms)$dataClasses)
  n_offsets <- sum(lengths(regmatches(nm1, gregexpr("offset", nm1))))

  if ( n_offsets > 1 ) {
    stop("Length of offset-terms must be equal to 1", call. = FALSE)
  }

  if( '(offset)' %in% nm1 ) {
    deparse(as.list(model$call)$offset)
  } else {
    out <- sub("offset\\((.*)\\)$", "\\1", grep('offset', nm1, value = TRUE))
    if ( identical(out, character(0)) ) {
      out <- NULL
    }
    out
  }
}


#' Remove offset term from formula
#'
#' @param formula Formula of class formula
#'
#' @keywords internal
remove_offset_formula <- function(formula) {

  if( !inherits(formula, "formula") ) {
    stop("Input must be of class formula", call. = FALSE)
  }

  proc <- function(x) {
    if (length(x) == 1) return(x)
    if (x[[1]] == as.name("offset")) return(x[[1]])
    replace(x, -1, lapply(x[-1], proc))
  }

  update(proc(formula), . ~ . - offset)
}


#' Remove risk factor from formula
#'
#' @param fm Formula of class formula
#' @param remove_term Risk factor to remove
#'
#' @keywords internal
update_formula_remove <- function(fm, remove_term){

  if( !inherits(fm, "formula") ) {
    stop("Input must be of class formula.", call. = FALSE)
  }

  if ( !is.character(remove_term) ){
    stop("Column must be a character.", call. = FALSE)
  }

  fm_new <- update(fm, paste("~ . -", remove_term))

  if ( fm == fm_new ) {
    warning("Column must be in model.\n")
  }

  fm_new
}

#' Create new offset-term and new formula
#'
#' @param offset_term String obtained from get_offset()
#' @param fm_no_offset Obtained from remove_offset_formula()
#' @param add_term Name of restricted risk factor to add
#'
#' @keywords internal
update_formula_add <- function(offset_term, fm_no_offset, add_term){

  if( !inherits(fm_no_offset, "formula") ) {
    stop("Input must be of class formula", call. = FALSE)
  }

  new_offset <- paste0("log(", add_term, ")", " + ", offset_term)
  new_offset_term <- paste0("offset(", new_offset, ")")
  new_fm <- update(fm_no_offset, paste("~ . +", new_offset_term))
  list(new_fm, new_offset)
}


#' Join restricted data to model data
#'
#' @param model_data data.frame with original model data
#' @param restrictions_df data.frame with two columns, column 1 must include
#'   the levels of the risk factor, and column 2 must include the new restricted
#'   coefficients.
#'
#' @importFrom dplyr left_join
#'
#' @keywords internal
add_restrictions_df <- function(model_data, restrictions_df){

  rcol1 <- names(restrictions_df)[1]
  rcol2 <- names(restrictions_df)[2]

  if ( !rcol1 %in% names(model_data) ) {
    stop("Can't find column '", rcol1, "' in model.", call. = FALSE )
  }

  if ( ncol(restrictions_df) != 2){
    stop("Number of columns must be equal to 2.", call. = FALSE)
  }

  if ( length(unique(restrictions_df[,1])) != nrow(restrictions_df) ) {
    stop(rcol1, "in restricted data must have unique values.", call. = FALSE)
  }

  if ( rcol2 %in% names(model_data) ) {
    stop("Column '", rcol2,
         "' in restricted data must be different from existing columns.",
         call. = FALSE)
  }

  if ( is.factor(model_data[[rcol1]]) ){
    restrictions_df[[rcol1]] <- as.factor(restrictions_df[[rcol1]])
  }

  model_df_restrictions <- dplyr::left_join(model_data, restrictions_df,
                                            by = rcol1)

  if ( anyNA(model_df_restrictions[[rcol2]]) ) {
    warning("Can't match all existing factor levels to new levels.\n")
  }

  model_df_restrictions
}


#' Restrict coefficients
#'
#' @description Restrict coefficients
#'
#' @param model object of class glm/restricted
#' @param data data.frame with restricted data
#'
#' @return Object of class restricted
#'
#' @export
restrict_coef <- function(model, data){
  if ( inherits(model, "glm") ){
    fm <- formula(model)
    offset_term <- get_offset(model)
    fm_no_offset <- remove_offset_formula(fm)
    df_new <- model$data
    model_family <- model$call$family
    rfdf <- rating_factors(model)$df
    rst_lst <- list(data)
    names(rst_lst) <- names(data[1])
  }

  if ( inherits(model, "restricted") ){
    fm <- model$formula_restricted
    offset_term <- model$offset
    fm_no_offset <- model$formula_removed
    df_new <- model$data_restricted
    model_family <- model$model_family
    rfdf <- model$rating_factors
    rst_lst <- model$restrictions_lst
    rst_lst[[names(data)[1]]] <- data
  }

  fm_remove <- update_formula_remove(fm_no_offset, names(data)[1])
  fm_add <- update_formula_add(offset_term, fm_remove, names(data)[2])
  df_restricted <- add_restrictions_df(df_new, data)

  rt <- list(formula_restricted = fm_add[[1]],
             formula_removed = fm_remove,
             data_restricted = df_restricted,
             fm_no_offset = fm_no_offset,
             offset = fm_add[[2]],
             model_family = model_family,
             rating_factors = rfdf,
             restrictions_lst = rst_lst)
  attr(rt, "class") <- "restricted"
  invisible(rt)
}


#' Refit GLM
#'
#' @description Refit GLM model
#'
#' @param x Object of class restricted
#'
#' @importFrom stats glm
#'
#' @return Object of class GLM
#'
#' @export
refit_glm.restricted <- function(x){

  if( !inherits(fm_no_offset, "restricted") ) {
    stop("Input must be of class restricted", call. = FALSE)
  }

  df <- x$data_restricted
  fm_plus <- x$formula_restricted
  family <- x$model_family
  x <- stats::glm(fm_plus, family = family, data = df)
  x$call$formula <- fm_plus
  x$call$family <- family
  x
}


#' Print for object of class restricted
#'
#' @param x Object of class restricted
#' @param ... other plotting parameters to affect the output
#'
#' @return Print object
#'
#' @export
print.restricted <- function(x, ...){
  cat("Formula: ")
  x$formula_restricted
}


#' Automatically create a ggplot for objects obtained from restrict_coef()
#'
#' @description Takes an object produced by \code{restrict_coef()}, and produces
#'   a line plot with a comparison between the restricted coefficients and
#'   estimated coefficents obtained from the model.
#'
#' @param object check_residuals object produced by \code{restrict_coef()}
#' @param name name of risk factor to show (defaults to NULL)
#' @param ... other plotting parameters to affect the plot
#'
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#'
#' @return Object of class ggplot2
#'
#' @export
autoplot.restricted <- function(object, name = NULL, ...){

  if ( length(object$restrictions_lst) > 1 & is.null(name) ) {
    stop("Name of risk factor must be specified.", call. = FALSE)
  }

  if ( length(name) > 1 ) {
    stop("Name must be of length 1.", call. = FALSE)
  }

  if ( is.null(name) ) {
    name <- names(x$restrictions_lst)[1]
  }

  naam_rst <- object$restrictions_lst[[name]]
  rf <- object$rating_factors
  naam_rf <- rf[rf$risk_factor == name,]
  naam_rf <- naam_rf[,2:3]
  names(naam_rst)[names(naam_rst) == name] <- "level"
  koppel <- dplyr::left_join(naam_rst, naam_rf, by = "level")

  koppel <- tidyr::pivot_longer(koppel,
                                cols = c(names(naam_rst)[2], names(rf)[3]),
                                names_to = "type",
                                values_to = "Coef")
  koppel$level <- as.factor(koppel$level)

  ggplot2::ggplot(data = koppel,
                  aes(x = level, y = Coef, color = type, group = type)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = name, color = NULL)
}




