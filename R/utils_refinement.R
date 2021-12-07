#' Get offset from model object
#'
#' @noRd
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
#' @noRd
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
#' @noRd
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
    warning("Column '", remove_term, "' must be in model call.\n")
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

#' @keywords internal
cut_borders_df <- function(df, col){
  if (!col %in% names(df)) stop("Column name must be available in data",
                                call. = FALSE)
  df_vec <- df[[col]]

  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  suppressWarnings({
    df$start_oc <- ifelse(gsub(pattern,"\\1", df_vec) == "(", "open",
                          ifelse(gsub(pattern,"\\1", df_vec) == "[",
                                 "closed", NA))
    df$end_oc <- ifelse(gsub(pattern,"\\4", df_vec) == ")", "open",
                        ifelse(gsub(pattern,"\\4", df_vec) == "]",
                               "closed", NA))
    df$start_  <- as.numeric(gsub(pattern,"\\2", df_vec))
    df$end_ <- as.numeric(gsub(pattern,"\\3", df_vec))
  })
  df$avg_ <- rowMeans(df[, c('start_', 'end_')], na.rm = TRUE)

  return(df)
}

#' @keywords internal
cut_borders_model <- function(model, x_cut){

  if ( inherits(model, "glm") ){
    rf <- rating_factors1(model)
  }

  if ( inherits(model, "smooth") ){
    rf <- rating_factors1(model$model_out)
  }

  if ( inherits(model, "restricted")){
    rf <- rating_factors1(model$model_out)
  }

  rf_xcut <- rf[rf$risk_factor == x_cut, ]
  cut_borders_df(rf_xcut, "level")
}

#' @noRd
#'
#' @importFrom stats lm
#'
#' @keywords internal
fit_polynomial <- function(borders_model, x_org, degree = NULL, breaks = NULL){

  if ( is.null(breaks) ){
    breaks <- seq(min(borders_model$start_), max(borders_model$end_),
                  length.out = nrow(borders_model))
  }

  # Take halfway points of breaks to fit polynomial
  breaks_min <- breaks[-length(breaks)]
  breaks_max <- breaks[-1]
  breaks_mid <- (breaks + c(breaks[-1], NA)) / 2
  breaks_mid <- breaks_mid[!is.na(breaks_mid)]

  unique_borders <- unique(c(breaks_min, breaks_max))
  levels_borders <- levels(cut(breaks_min, breaks = unique_borders,
                               include.lowest = TRUE, dig.lab = 9))

  lm_poly <- lm(estimate ~ poly(avg_, degree = degree), data = borders_model)
  new_poly_df <- data.frame(avg_ = breaks_mid)
  poly_line <- data.frame(avg_ = breaks)
  poly_line$yhat <- as.numeric(predict(lm_poly, poly_line))

  new_poly_df$yhat <- as.numeric(predict(lm_poly, new_poly_df))
  new_poly_df$breaks_min <- breaks_min
  new_poly_df$breaks_max <- breaks_max
  new_poly_df$cuts <- levels_borders
  new_poly_df$risk_factor <- paste0(x_org, "_smooth")
  colnames(new_poly_df)[1] <- x_org

  # new_colname_cat <- paste0(x_org, "_cat_new")
  new_colname_cat <- paste0(x_org, "_smooth")
  colnames(new_poly_df)[5] <- new_colname_cat
  new_poly_df <- cut_borders_df(new_poly_df, new_colname_cat)
  colnames(poly_line)[1] <- x_org

  new_rf <- new_poly_df[, c("risk_factor", new_colname_cat, "yhat")]
  colnames(new_rf)[2] <- "level"

  list(new_poly_df = new_poly_df, poly_line = poly_line, new_rf = new_rf)
}



#' Join new data to nearest data point
#'
#' @noRd
#'
#' @param dat data.frame with model data
#' @param reference data.frame with polynomial fit
#' @param x character string to join on
#'
#' @importFrom data.table data.table
#'
#' @keywords internal
join_to_nearest <- function(dat, reference, x){
  reference <- data.table::data.table(reference)
  dat <- data.table::data.table(dat)
  reference_nm <- setdiff(names(reference), x)
  dat <- dat[, c(x) := as.numeric(get(x))]
  join <- reference[dat, roll = "nearest", on = x][is.na(get(x)),
                                                   c(reference_nm) := NA]
  as.data.frame(join)
}



#' Join restricted data to model data
#'
#' @noRd
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
    stop("Can't find column '", rcol1, "' in model call.", call. = FALSE )
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


#' Create one data.frame from multiple restriction data.frames
#'
#' @noRd
#'
#' @param restricted_df data.frame with restrictions
#'
#' @keywords internal
restrict_df <- function(restricted_df){
  restricted_df$risk_factor <- colnames(restricted_df)[2]
  colnames(restricted_df)[2] <- "yhat"
  colnames(restricted_df)[1] <- "level"
  restricted_df$level <- as.character(restricted_df$level)
  restricted_df
}

