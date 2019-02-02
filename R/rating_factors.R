#' Include reference group in regression output
#'
#' @description This extracts coefficients in terms of the original levels of the coefficients rather than the coded variables.
#'
#' @param model a (generalized) linear model fit
#' @param colname name of column with estimates. Defaults to "estimate".
#' @param exponentiate Logical indicating whether or not to exponentiate the the coefficient estimates. Defaults to TRUE.
#'
#' @return data.frame
#' @export rating_factors
#'
#' @details This function is adopted from the dummy.coef{stats} function. Our adoption prints a data.frame as output.
#'
#' @author Martin Haringa
#'
#' @examples g1 <- glm(nclaims ~ age_policyholder, family = "poisson", data = MTPL)
#' rating_factors(g1)
remove_duplicates <- function(x){
  d <- unlist(strsplit(x, split = "\\."))
  paste(unique(d), collapse = ' ')
}

rating_factors <- function(model, colname = "estimate", exponentiate = TRUE){

  coefs <- dummy.coef(model)
  coefs_df <- data.frame(unlist(coefs))
  coefs_df$term <- row.names(coefs_df)
  row.names(coefs_df) <- NULL
  colnames(coefs_df)[1] <- colname

  if(isTRUE(exponentiate)){
    coefs_df[,1] <- exp(coefs_df[,1])
  }

  term_list <- lapply(coefs_df$term, remove_duplicates)
  coefs_df$term <- as.vector(do.call(rbind, term_list))

  return(coefs_df[,2:1])
}
