#' Include reference group in regression output
#'
#' @description This extracts coefficients in terms of the original levels of the coefficients rather than the coded variables.
#'
#' @param model a (generalized) linear model fit
#' @param colname name of column with estimates. Defaults to "estimate".
#' @param exponentiate Logical indicating whether or not to exponentiate the the coefficient estimates. Defaults to TRUE.
#'
#' @return data.frame
#'
#' @importFrom stats dummy.coef
#' @import stringr
#'
#' @details This function is adopted from the dummy.coef{stats} function. Our adoption prints a data.frame as output. Categorical variables
#'     should be changed to factors in the data.frame used to fit the (generalized) linear model.
#'
#' @author Martin Haringa
#'
#' @examples g1 <- glm(nclaims ~ age_policyholder, family = "poisson", data = MTPL)
#' rating_factors(g1)
#' @export rating_factors
rating_factors <- function(model, colname = "estimate", exponentiate = TRUE){

  coefs <- dummy.coef(model)
  coefs_df <- data.frame(unlist(coefs))
  coefs_df$term <- row.names(coefs_df)
  row.names(coefs_df) <- NULL
  colnames(coefs_df)[1] <- colname

  if(isTRUE(exponentiate)){
    coefs_df[,1] <- exp(coefs_df[,1])
  }

  names_coef <- paste(names(coefs), collapse = "|")
  clusters <- stringr::str_remove(coefs_df$term, names_coef)
  coefs_df$cluster <- gsub('^\\.|\\.$|\\(\\)\\.', '', clusters)
  coefs_df$term <- stringr::str_extract(coefs_df$term, names_coef)

  return(coefs_df[,c(2,3,1)])
}




