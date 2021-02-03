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

  if (is.null(offset_term)){
    new_offset <- paste0("log(", add_term, ")")
  }

  if (!is.null(offset_term)){
    new_offset <- paste0("log(", add_term, ")", " + ", offset_term)
  }

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


#' Restrict coefficients in the model
#'
#' @description Add restrictions, like a bonus-malus structure, on the risk
#' factors used in the model. \code{restrict_coef()} must always be followed
#' by \code{refit_glm()}.
#'
#' @author Martin Haringa
#'
#' @details Although restrictions could be applied either to the frequency or
#'   the severity model, it is more appropriate to impose the restrictions
#'   on the premium model. This can be achieved by calculating the pure
#'   premium for each record (i.e. expected number of claims times the expected
#'   claim amount), then fitting an "unrestricted" Gamma GLM to the pure premium,
#'   and then imposing the restrictions in a final "restricted" Gamma GLM.
#'
#' @param model object of class glm/restricted
#' @param restrictions data.frame with two columns containing restricted data. The first
#'   column, with the name of the risk factor as column name, must contain the
#'   levels of the risk factor. The second column must contain the restricted
#'   coefficients.
#'
#' @return Object of class restricted.
#'
#' @export
restrict_coef <- function(model, restrictions){
  if ( inherits(model, "glm") ){
    fm <- formula(model)
    offset_term <- get_offset(model)
    fm_no_offset <- remove_offset_formula(fm)
    df_new <- model$data
    model_call <- model$call
    rfdf <- rating_factors(model)$df
    rst_lst <- list(restrictions)
    names(rst_lst) <- names(restrictions[1])
  }

  if ( inherits(model, "restricted") ){
    fm <- model$formula_restricted
    offset_term <- model$offset
    fm_no_offset <- model$formula_removed
    df_new <- model$data_restricted
    model_call <- model$model_call
    rfdf <- model$rating_factors
    rst_lst <- model$restrictions_lst
    rst_lst[[names(restrictions)[1]]] <- restrictions
  }

  fm_remove <- update_formula_remove(fm_no_offset, names(restrictions)[1])
  fm_add <- update_formula_add(offset_term, fm_remove, names(restrictions)[2])
  df_restricted <- add_restrictions_df(df_new, restrictions)

  rt <- list(formula_restricted = fm_add[[1]],
             formula_removed = fm_remove,
             data_restricted = df_restricted,
             fm_no_offset = fm_no_offset,
             offset = fm_add[[2]],
             rating_factors = rfdf,
             restrictions_lst = rst_lst,
             model_call = model_call)
  attr(rt, "class") <- "restricted"
  invisible(rt)
}


#' Refitting Generalized Linear Models
#'
#' @description \code{refit_glm()} is used to refit generalized linear models,
#'   and must be preceded by \code{restrict_coef()}.
#'
#' @param x Object of class restricted
#'
#' @author Martin Haringa
#'
#' @importFrom stats glm
#'
#' @return Object of class GLM
#'
#' @examples
#' \dontrun{
#' restricted_df <- data.frame(gear = c(3,4,5), gear_coef = c(.9,1,1.1))
#' mod1 <- glm(cyl ~ am + gear, offset = log(carb), family = "poisson", data = mtcars)
#' mod1 %>%
#'     restrict_coef(., restricted_df) %>%
#'     refit_glm(.)
#' }
#'
#' @export
refit_glm <- function(x){

  if( !inherits(x, "restricted") ) {
    stop("Input must be of class restricted", call. = FALSE)
  }

  lst_call <- as.list(x$model_call)
  lst <- list(formula = x$formula_restricted, data = x$data_restricted)
  y <- eval(as.call(modifyList(lst_call, lst)))
  y$call$formula <- lst$formula
  y$call$data <- quote(df_new)
  y
}


#' Print for object of class restricted
#'
#' @param x Object of class restricted
#' @param ... other plotting parameters to affect the output
#'
#' @return Print object
#'
#' @author Martin Haringa
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
#' @author Martin Haringa
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

  ggplot2::ggplot(data = koppel, aes(x = level,
                                     y = Coef,
                                     color = type, group = type)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = name, color = NULL)
}




