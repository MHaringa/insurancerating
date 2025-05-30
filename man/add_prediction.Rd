% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_add_prediction.R
\name{add_prediction}
\alias{add_prediction}
\title{Add predictions to a data frame}
\usage{
add_prediction(data, ..., var = NULL, conf_int = FALSE, alpha = 0.1)
}
\arguments{
\item{data}{a data frame of new data.}

\item{...}{one or more objects of class \code{glm}.}

\item{var}{the name of the output column(s), defaults to NULL}

\item{conf_int}{determines whether confidence intervals will be shown.
Defaults to \code{conf_int = FALSE}.}

\item{alpha}{a real number between 0 and 1. Controls the confidence level of
the interval estimates (defaults to 0.10, representing 90 percent confidence
interval).}
}
\value{
data.frame
}
\description{
Add model predictions and confidence bounds to a data frame.
}
\examples{
mod1 <- glm(nclaims ~ age_policyholder, data = MTPL,
    offset = log(exposure), family = poisson())
mtpl_pred <- add_prediction(MTPL, mod1)

# Include confidence bounds
mtpl_pred_ci <- add_prediction(MTPL, mod1, conf_int = TRUE)

}
