# Add Model Predictions to a Data Frame

Adds predictions (and optionally confidence intervals) from one or more
`glm` models to a data frame.

## Usage

``` r
add_prediction(data, ..., var = NULL, conf_int = FALSE, alpha = 0.1)
```

## Arguments

- data:

  A `data.frame` containing the new data for which predictions should be
  generated.

- ...:

  One or more fitted model objects of class `"glm"`.

- var:

  Optional character vector giving names for the new prediction columns.
  Must have the same length as the number of models supplied. If `NULL`
  (default), names are generated automatically.

- conf_int:

  Logical. If `TRUE`, add confidence intervals for predictions. Default
  is `FALSE`.

- alpha:

  Numeric between 0 and 1. Controls the confidence level for interval
  estimates. Default is `0.10`, corresponding to a 90% confidence
  interval.

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
mtpl_pred_ci <- add_prediction(MTPL, mod1, conf_int = TRUE)
```
