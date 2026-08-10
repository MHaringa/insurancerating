# Add model predictions to a pricing data set

`add_prediction()` adds predictions from one or more fitted `glm` models
to a data frame.

In pricing workflows, this is often used to bring count and severity
model output together on the same portfolio. For example, an expected
claim count can be normalised by exposure and multiplied by an expected
average claim amount to calculate a risk premium per exposure unit.

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
as Poisson count models or Gamma severity models, the added columns are
already on the original response scale. For a Poisson claim-count model
containing an exposure offset, this is the expected claim count for the
supplied exposure, not frequency per exposure unit. Divide by exposure
when a rate is required.

If `confidence = TRUE`, lower and upper confidence interval columns are
added next to each prediction column. The default interval suffixes are
`"lower"` and `"upper"`.

Predictions containing missing values are retained. If one or more `NA`
predictions are produced, the function issues a warning with the
affected prediction columns and number of missing predictions. This is
typically caused by missing predictor values in `data` or by predictor
values outside the domain supported by the fitted model.

## Author

Martin Haringa

## Examples

``` r
mod1 <- glm(nclaims ~ age_policyholder,
            data = MTPL,
            offset = log(exposure),
            family = poisson())

# Add the expected claim count for each record's exposure
mtpl_pred <- add_prediction(
  MTPL,
  mod1,
  predictions = "expected_claim_count"
)

# Add predicted values with confidence bounds
mtpl_pred_ci <- add_prediction(
  MTPL,
  mod1,
  predictions = "expected_claim_count",
  confidence = TRUE
)

# Combine frequency and severity predictions into a risk premium
freq <- glm(nclaims ~ bm + zip,
            data = MTPL,
            offset = log(exposure),
            family = poisson())

severity_data <- MTPL[MTPL$nclaims > 0 & MTPL$amount > 0, ]
severity_data$average_claim_amount <-
  severity_data$amount / severity_data$nclaims

sev <- glm(average_claim_amount ~ bm + zip,
           data = severity_data,
           weights = nclaims,
           family = Gamma(link = "log"))

pricing <- add_prediction(
  MTPL,
  freq,
  sev,
  predictions = c("expected_claim_count", "expected_average_severity")
)

pricing$claim_frequency <-
  pricing$expected_claim_count / pricing$exposure
pricing$expected_loss <-
  pricing$expected_claim_count * pricing$expected_average_severity
pricing$risk_premium <-
  pricing$claim_frequency * pricing$expected_average_severity
```
