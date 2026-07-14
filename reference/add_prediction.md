# Add model predictions to a pricing data set

`add_prediction()` adds predictions from one or more fitted `glm` models
to a data frame.

In pricing workflows, this is often used to bring frequency and severity
model output together on the same portfolio. For example, predicted
claim frequency and predicted average claim amount can be multiplied to
create a pure premium proxy before further tariff refinement.

The function is deliberately small: it does not refit models or decide
how predictions should be combined. It only adds model predictions, and
optionally confidence intervals, using clear output column names.

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

A `data.frame` containing the original data and additional columns for
model predictions. If `confidence = TRUE`, confidence interval columns
are added as well.

## Details

Predictions are calculated on the response scale using
`stats::predict(..., type = "response")`. For GLMs with a log link, such
as Poisson frequency models or Gamma severity models, the added columns
are therefore already on the original scale.

If `confidence = TRUE`, lower and upper confidence interval columns are
added next to each prediction column. The default interval suffixes are
`"lower"` and `"upper"`.

## Author

Martin Haringa

## Examples

``` r
mod1 <- glm(nclaims ~ age_policyholder,
            data = MTPL,
            offset = log(exposure),
            family = poisson())

# Add predicted claim frequency
mtpl_pred <- add_prediction(MTPL, mod1, predictions = "pred_frequency")

# Add predicted values with confidence bounds
mtpl_pred_ci <- add_prediction(
  MTPL,
  mod1,
  predictions = "pred_frequency",
  confidence = TRUE
)

# Combine frequency and severity predictions into a pure premium proxy
freq <- glm(nclaims ~ bm + zip,
            data = MTPL,
            offset = log(exposure),
            family = poisson())

sev <- glm(amount ~ bm + zip,
           data = MTPL[MTPL$amount > 0, ],
           weights = nclaims,
           family = Gamma(link = "log"))

premium_proxy <- add_prediction(
  MTPL,
  freq,
  sev,
  predictions = c("pred_frequency", "pred_severity")
)

premium_proxy$pred_pure_premium <-
  premium_proxy$pred_frequency * premium_proxy$pred_severity
```
