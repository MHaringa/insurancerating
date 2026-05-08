# Add Model Predictions to a Data Frame

Adds predictions (and optionally confidence intervals) from one or more
`glm` models to a data frame.

## Usage

``` r
add_prediction(
  data,
  ...,
  predictions = NULL,
  prefix = "pred",
  confidence = FALSE,
  interval_names = c("lower", "upper"),
  alpha = 0.1,
  var = NULL,
  conf_int = NULL
)
```

## Arguments

- data:

  A `data.frame` containing the new data for which predictions should be
  generated.

- ...:

  One or more fitted model objects of class `"glm"`.

- predictions:

  Optional character vector giving names for the new prediction columns.
  Must have the same length as the number of models supplied. If `NULL`
  (default), names are generated automatically using `prefix`, the model
  response, and the model object name.

- prefix:

  Character. Prefix used for automatically generated prediction column
  names. Default is `"pred"`.

- confidence:

  Logical. If `TRUE`, add confidence intervals for predictions. Default
  is `FALSE`.

- interval_names:

  Character vector of length two. Names appended to the prediction
  column name for lower and upper confidence interval bounds. Default is
  `c("lower", "upper")`.

- alpha:

  Numeric between 0 and 1. Controls the miscoverage level for interval
  estimates. Default is `0.10`, corresponding to a 90% confidence
  interval.

- var:

  Deprecated. Use `predictions` instead.

- conf_int:

  Deprecated. Use `confidence` instead.

## Value

A `data.frame` containing the original data along with additional
columns for model predictions (and confidence intervals if requested).

## Author

Martin Haringa

## Examples

``` r
mod1 <- glm(nclaims ~ age_policyholder,
            data = MTPL,
            offset = log(exposure),
            family = poisson())

# Add predicted values
mtpl_pred <- add_prediction(MTPL, mod1)

# Add predicted values with confidence bounds
mtpl_pred_ci <- add_prediction(MTPL, mod1, confidence = TRUE)
```
