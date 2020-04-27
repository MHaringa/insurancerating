#' @keywords internal
create_residuals <- function(simulatedResponse, observedResponse, fittedPredictedResponse = NULL, seed = 123){

  out = list()
  out$simulatedResponse = simulatedResponse
  out$refit = F
  out$observedResponse = observedResponse
  out$nObs = length(observedResponse)
  out$nSim = ncol(simulatedResponse)
  out$scaledResiduals = get_quantiles(simulations = simulatedResponse,
                                     observed = observedResponse, n = out$nObs, nSim = out$nSim, seed = seed)

  if(is.null(fittedPredictedResponse)){
    message("No fitted predicted response provided, using the mean of the simulations")
    fittedPredictedResponse = apply(simulatedResponse, 1, mean)
  }
  out$fittedPredictedResponse = fittedPredictedResponse
  class(out) = "pois_residuals"
  return(out)

}

#' @keywords internal
residuals.ecdf <- function (x){
  x <- sort(x)
  n <- length(x)
  if (n < 1)
    stop(paste("Length vector < 1", x))
  vals <- unique(x)
  rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/ (n + 1),
                    method = "linear", yleft = 0, yright = 1, ties = "ordered")
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}

#' Quantile calculations
#'
#' @keywords internal
get_quantiles <- function(simulations, observed, n, nSim, seed){

  scaledResiduals = rep(NA, n)
  for (i in 1:n){
      scaledResiduals[i] <- residuals.ecdf(simulations[i,] + runif(nSim, -0.5, 0.5))(observed[i] + runif(1, -0.5, 0.5))
  }
  return(scaledResiduals)
}

#' @keywords internal
simulate_poisson_glm <- function(model, n){
  prediction <- predict(model, type = "response")
  n_obs = length(prediction)
  sim = matrix(nrow = n_obs, ncol = n)
  for(i in 1:n) sim[,i] = rpois(n_obs, prediction)
  return(sim)
}

#' @keywords internal
.retrieve_data <- function(x) {
  # retrieve model
  obj_name <- attr(x, "object_name", exact = TRUE)
  dat <- NULL

  if (!is.null(obj_name)) {
    # first try, parent frame
    dat <- tryCatch({
      get(obj_name, envir = parent.frame())
    },
    error = function(e) { NULL }
    )

    if (is.null(dat)) {
      # second try, global env
      dat <- tryCatch({
        get(obj_name, envir = globalenv())
      },
      error = function(e) { NULL }
      )

    }
  }

  if (is.null(dat)) {
    dat <- attr(x, "data", exact = TRUE)
  }


  if (is.null(dat)) {
    stop("Failed at retrieving data :( Please provide original model or data through the `data` argument", call. = FALSE)
  }

  dat
}





