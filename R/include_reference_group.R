#' Include reference group in regression output
#'
#' @param df data frame
#' @param model Regression model
#'
#' @return Data.frame
#' @export include_df
#'
#' @author Martin Haringa
#'
#' @examples
include_df <- function(df, model){

  varnamen <- all.vars(formula(model)[[3]])

  naam <- deparse(substitute(model))

  tidy_ci <- model %>%
    broom::tidy(., exponentiate = TRUE, quick = TRUE) %>%
    bind_cols(., broom::confint_tidy(model, func = stats::confint.default))

  uit <- df %>%
    dplyr::select(varnamen) %>%
    tidyr::gather(factor, klasse) %>%
    dplyr::distinct(.) %>%
    tidyr::unite(col = "term", factor:klasse, sep = "", remove = FALSE) %>%
    dplyr::full_join(tidy_ci, .) %>%
    dplyr::arrange(!is.na(factor), factor, nchar(klasse), klasse) %>%
    dplyr::select(factor, klasse, estimate, conf.low, conf.high) %>%
    dplyr::mutate(factor = replace(factor, is.na(factor), "(intercept)")) %>%
    dplyr::mutate(estimate = replace(estimate, is.na(estimate), 1)) %>%
    dplyr::rename_at(c("estimate", "conf.low", "conf.high"), .funs = funs(paste(naam, ., sep = "_")))

  return(uit)
}


