#' Add Model Predictions to a Data Frame
#'
#' @description
#' Adds predictions (and optionally confidence intervals) from one or more
#' `glm` models to a data frame.
#'
#' @param data A `data.frame` containing the new data for which predictions
#'   should be generated.
#' @param ... One or more fitted model objects of class `"glm"`.
#' @param predictions Optional character vector giving names for the new
#'   prediction columns. Must have the same length as the number of models
#'   supplied. If `NULL` (default), names are generated automatically using
#'   `prefix`, the model response, and the model object name.
#' @param prefix Character. Prefix used for automatically generated prediction
#'   column names. Default is `"pred"`.
#' @param confidence Logical. If `TRUE`, add confidence intervals for
#'   predictions. Default is `FALSE`.
#' @param interval_names Character vector of length two. Names appended to the
#'   prediction column name for lower and upper confidence interval bounds.
#'   Default is `c("lower", "upper")`.
#' @param alpha Numeric between 0 and 1. Controls the miscoverage level for
#'   interval estimates. Default is `0.10`, corresponding to a 90% confidence
#'   interval.
#' @param var Deprecated. Use `predictions` instead.
#' @param conf_int Deprecated. Use `confidence` instead.
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
#' mtpl_pred_ci <- add_prediction(MTPL, mod1, confidence = TRUE)
#'
#' @export
add_prediction <- function(data, ..., predictions = NULL, prefix = "pred",
                           confidence = FALSE,
                           interval_names = c("lower", "upper"),
                           alpha = 0.1, var = NULL, conf_int = NULL) {

  objects <- list(...)
  object_names <- as.character(match.call(expand.dots = FALSE)$`...`)

  if (length(objects) == 0) {
    stop("Please provide at least one 'glm' object.", call. = FALSE)
  }

  if (!all(sapply(objects, inherits, what = "glm"))) {
    stop("All objects must be of class 'glm'.", call. = FALSE)
  }

  if (!is.null(var)) {
    lifecycle::deprecate_warn(
      "0.8.0",
      "add_prediction(var = )",
      "add_prediction(predictions = )"
    )
    if (!is.null(predictions)) {
      stop("Use either `predictions` or deprecated `var`, not both.",
           call. = FALSE)
    }
    predictions <- var
  }

  if (!is.null(conf_int)) {
    lifecycle::deprecate_warn(
      "0.8.0",
      "add_prediction(conf_int = )",
      "add_prediction(confidence = )"
    )
    confidence <- conf_int
  }

  validate_add_prediction_args(
    data = data,
    objects = objects,
    object_names = object_names,
    predictions = predictions,
    prefix = prefix,
    confidence = confidence,
    interval_names = interval_names,
    alpha = alpha
  )

  prediction_names <- make_prediction_names(
    objects = objects,
    object_names = object_names,
    predictions = predictions,
    prefix = prefix
  )

  added_names <- prediction_names
  if (isTRUE(confidence)) {
    added_names <- as.vector(rbind(
      prediction_names,
      paste0(prediction_names, "_", interval_names[1]),
      paste0(prediction_names, "_", interval_names[2])
    ))
  }

  if (anyDuplicated(added_names)) {
    stop("Prediction column names must be unique.", call. = FALSE)
  }
  if (any(added_names %in% names(data))) {
    stop(
      "Prediction column names must not already exist in `data`: ",
      paste(intersect(added_names, names(data)), collapse = ", "),
      call. = FALSE
    )
  }

  listdf <- vector("list", length(objects))


  for (i in seq_along(objects)) {
    object <- objects[[i]]

    # predictions
    addcol <- as.numeric(stats::predict(object, data, type = "response"))
    var_nm <- prediction_names[i]

    df <- data.frame(addcol)
    names(df) <- var_nm

    if (isTRUE(confidence)) {
      lower <- paste0(var_nm, "_", interval_names[1])
      upper <- paste0(var_nm, "_", interval_names[2])

      suppressWarnings({
        ci_df <- ciTools::add_ci(data, object, names = c("lower", "upper"),
                                 alpha = alpha)
      })

      df[[lower]] <- ci_df$lower
      df[[upper]] <- ci_df$upper
    }

    listdf[[i]] <- df
  }

  cbind(data, do.call(cbind, listdf))
}

validate_add_prediction_args <- function(data, objects, object_names,
                                         predictions, prefix, confidence,
                                         interval_names, alpha) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.null(predictions) &&
      (!is.character(predictions) || length(predictions) != length(objects) ||
       anyNA(predictions) || any(predictions == ""))) {
    stop(
      "`predictions` must be a character vector with one name per model.",
      call. = FALSE
    )
  }
  if (!is.character(prefix) || length(prefix) != 1L ||
      is.na(prefix) || prefix == "") {
    stop("`prefix` must be a single non-empty character string.",
         call. = FALSE)
  }
  if (!is.logical(confidence) || length(confidence) != 1L ||
      is.na(confidence)) {
    stop("`confidence` must be either TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(interval_names) || length(interval_names) != 2L ||
      anyNA(interval_names) || any(interval_names == "")) {
    stop("`interval_names` must be a character vector of length two.",
         call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L ||
      is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number between 0 and 1.", call. = FALSE)
  }
  if (anyDuplicated(object_names) && is.null(predictions)) {
    stop(
      "Automatic prediction names are duplicated. Use `predictions` to provide ",
      "unique names.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

make_prediction_names <- function(objects, object_names, predictions, prefix) {
  if (!is.null(predictions)) {
    return(predictions)
  }

  vapply(
    seq_along(objects),
    function(i) {
      object_name <- object_names[i]
      if (!grepl("^[[:alpha:].][[:alnum:]_.]*$", object_name)) {
        stop(
          "Automatic prediction names require models supplied as named objects. ",
          "Use `predictions` when passing inline model expressions.",
          call. = FALSE
        )
      }
      response_nm <- all.vars(stats::formula(objects[[i]]))[1]
      paste(prefix, response_nm, object_name, sep = "_")
    },
    character(1)
  )
}
