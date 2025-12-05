#' Add Model Predictions to a Data Frame
#'
#' @description
#' Adds predictions (and optionally confidence intervals) from one or more
#' `glm` models to a data frame.
#'
#' @param data A `data.frame` containing the new data for which predictions
#'   should be generated.
#' @param ... One or more fitted model objects of class `"glm"`.
#' @param var Optional character vector giving names for the new prediction
#'   columns. Must have the same length as the number of models supplied. If
#'   `NULL` (default), names are generated automatically.
#' @param conf_int Logical. If `TRUE`, add confidence intervals for predictions.
#'   Default is `FALSE`.
#' @param alpha Numeric between 0 and 1. Controls the confidence level for
#'   interval estimates. Default is `0.10`, corresponding to a 90% confidence
#'   interval.
#'
#' @return
#' A `data.frame` containing the original data along with additional columns for
#' model predictions (and confidence intervals if requested).
#'
#' @importFrom ciTools add_ci
#'
#' @author Martin Haringa
#'
#' @examples
#' mod1 <- glm(nclaims ~ age_policyholder,
#'             data = MTPL,
#'             offset = log(exposure),
#'             family = poisson())
#'
#' # Add predicted values
#' mtpl_pred <- add_prediction(MTPL, mod1)
#'
#' # Add predicted values with confidence bounds
#' mtpl_pred_ci <- add_prediction(MTPL, mod1, conf_int = TRUE)
#'
#' @export
add_prediction <- function(data, ..., var = NULL, conf_int = FALSE,
                           alpha = 0.1) {

  objects <- list(...)
  object_names <- as.character(match.call(expand.dots = FALSE)$`...`)

  if (length(objects) == 0) {
    stop("Please provide at least one 'glm' object.", call. = FALSE)
  }

  if (!all(sapply(objects, inherits, what = "glm"))) {
    stop("All objects must be of class 'glm'.", call. = FALSE)
  }

  if (!is.null(var) && length(var) != length(objects)) {
    stop("Argument 'var' must have the same length as the number of models.",
         call. = FALSE)
  }

  listdf <- vector("list", length(objects))


  for (i in seq_along(objects)) {
    object <- objects[[i]]
    object_name <- object_names[i]

    # predictions
    addcol <- as.numeric(stats::predict(object, data, type = "response"))
    response_nm <- all.vars(formula(object))[1]

    if (is.null(var)) {
      var_nm <- paste0("pred_", response_nm, "_", object_name)
    } else {
      var_nm <- var[i]
    }

    df <- data.frame(addcol)
    names(df) <- var_nm

    if (isTRUE(conf_int)) {
      lcb <- paste0(var_nm, "_lcb")
      ucb <- paste0(var_nm, "_ucb")

      suppressWarnings({
        ci_df <- ciTools::add_ci(data, object, names = c("lcb", "ucb"),
                                 alpha = alpha)
      })

      df[[lcb]] <- ci_df$lcb
      df[[ucb]] <- ci_df$ucb
    }

    listdf[[i]] <- df
  }

  cbind(data, do.call(cbind, listdf))
}
