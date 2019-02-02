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
#'
#' @details This function is adopted from the dummy.coef{stats} function. Our adoption prints a data.frame as output.
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

  term_list <- lapply(coefs_df$term, function(x) paste(unique(unlist(strsplit(x, split = "\\."))), collapse = ' '))
  coefs_df$term <- as.vector(do.call(rbind, term_list))

  return(coefs_df[,2:1])
}
