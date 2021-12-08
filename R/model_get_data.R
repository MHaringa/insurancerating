#' Get model data
#'
#' @description `r lifecycle::badge('experimental')` `construct_model_points()`
#' is used to construct model points from refitted generalized linear models,
#' and must be preceded by `update_glm()`.
#'
#' @param x Object of class refitsmooth or of class refitrestricted
#'
#' @author Martin Haringa
#'
#' @return data.frame
#'
#' @export
model_data <- function(x){

  if( !inherits(x, c("refitsmooth", "refitrestricted", "glm")) ) {
    stop("Input must be of class refitsmooth, glm or of class refitrestricted",
         call. = FALSE)
  }

  if ( inherits(x, "glm") ){
    out <- x$data
    rf <- rating_factors1(x)
    rf2_nm <- unique(rf$risk_factor[rf$risk_factor != "(Intercept)"])
    attr(out, "rf") <- rf2_nm
  }

  if( inherits(x, c("refitsmooth", "refitrestricted")) ) {
    xdf <- x$data
    out <- xdf[!names(xdf) %in% c("breaks_min", "breaks_max",
                                  "start_oc", "end_oc",
                                  "start_", "end_",
                                  "avg_", "risk_factor")]

    attr(out, "new_nm") <- attr(x, "new_col_nm")
    attr(out, "old_nm") <- attr(x, "old_col_nm")
    attr(out, "rf") <- attr(x, "rf")
  }

  attr(out, "class") <- append(class(out), "model_data")
  return(out)
}


#' Construct model points from Generalized Linear Model
#'
#' @description `r lifecycle::badge('experimental')` `construct_model_points()`
#' is used to construct model points from refitted generalized linear models,
#' and must be preceded by `update_glm()`.
#'
#' @param x Object of class refitsmooth or of class refitrestricted
#' @param expand expand true or false
#' @param drop_na drop na values
#' @param exposure column with exposure
#' @param exposure_by split column exposure by year column e.g.
#'
#' @author Martin Haringa
#'
#' @importFrom stats na.omit
#'
#' @return data.frame
#'
#' @export
construct_model_points <- function(x, expand = FALSE, drop_na = FALSE,
                                   exposure = NULL, exposure_by = NULL){

  if( !inherits(x, c("model_data")) ) {
    stop("Input must be of class model_data, use model_data() to create data",
         call. = FALSE)
  }

  premium_nm <- attr(x, "rf")
  premium_df <- x[, premium_nm, drop = FALSE]
  premium_df <- unique(premium_df)

  old_nm <- attr(x, "old_nm")
  new_nm <- attr(x, "new_nm")
  df <- data.frame(old_nm, new_nm)
  xdf <- x
  refinement_nm <- lapply(split(df, seq_len(nrow(df))), as.character)
  refinement_df <- lapply(refinement_nm, function(x) xdf[, x, drop = FALSE])
  refinement_df <- lapply(refinement_df, unique)

  if ( isTRUE(drop_na) ){
    premium_vec <- sapply(premium_df, function(x) unique(na.omit(x)))
  }

  if ( !isTRUE(drop_na) ){
    premium_vec <- sapply(premium_df, unique)
  }

  premium_complete <- Reduce(function(...) merge(..., by = NULL), premium_vec)
  names(premium_complete) <- names(premium_df)

  premium_refinement_lst <- c(list(premium_complete), refinement_df)
  premium_join <- Reduce(function(...) merge(..., all.x = TRUE),
                         premium_refinement_lst)
  premium_join
}

