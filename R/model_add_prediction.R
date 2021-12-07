#' Add predictions to a data frame
#'
#' @description Add model predictions and confidence bounds to a data frame.
#'
#' @param data 	a data frame of new data.
#' @param ... one or more objects of class `glm`.
#' @param var the name of the output column(s), defaults to NULL
#' @param alpha a real number between 0 and 1. Controls the confidence level of
#' the interval estimates (defaults to 0.10, representing 90 percent confidence
#' interval).
#' @param conf_int determines whether confidence intervals will be shown.
#' Defaults to `conf_int = FALSE`.
#'
#' @importFrom ciTools add_ci
#'
#' @return data.frame
#'
#' @examples
#' mod1 <- glm(nclaims ~ age_policyholder, data = MTPL,
#'     offset = log(exposure), family = poisson())
#' add_prediction(MTPL, mod1)
#'
#' # Include confidence bounds
#' add_prediction(MTPL, mod1, conf_int = TRUE)
#'
#' @export
add_prediction <- function(data, ..., var = NULL, conf_int = FALSE,
                           alpha = 0.1) {

  objects <- list(...)
  object_names <- match.call(expand.dots = FALSE)$`...`

  if ( !is.null(var) & length(var) != length(object_names)){
    stop("Character vector 'var' should have the same length as number of
         objects", call. = FALSE)
  }

  listdf <- list()

  for ( i in seq_len(length(object_names))) {
    object <- objects[[i]]
    object_name <- object_names[i]
    addcol <- as.numeric(stats::predict(object, data, type = "response"))

    response_nm <- as.character(attributes(object$terms)$variables[[2]])
    if ( is.null(var) ){
      var_nm <- paste0("pred_", response_nm, "_" , object_name)
    } else {
      var_nm <- var[i]
    }

    df <- data.frame(addcol)
    names(df) <- var_nm

    if ( isTRUE(conf_int) ) {
       ucb <- paste0(var_nm, "_ucb")
       lcb <- paste0(var_nm, "_lcb")
       suppressWarnings({
         lcbucb <- ciTools::add_ci(data, object, names = c("lcb", "ucb"),
                                   alpha = alpha)
       })
       df[[lcb]] <- lcbucb$lcb
       df[[ucb]] <- lcbucb$ucb
    }

    listdf[[i]] <- df
  }

  cbind(data, as.data.frame(listdf))
}





