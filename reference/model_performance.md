# Compare GLM fit and predictive error metrics

Compare GLM fit and predictive error metrics. The table reports AIC,
BIC, response-scale RMSE and residual deviance for one or more fitted
GLMs.

## Usage

``` r
model_performance(...)
```

## Arguments

- ...:

  One or more objects of class `"glm"`.

## Value

A data frame of class `"model_performance"`, with columns:

- Model:

  Name of the model object as passed to the function.

- AIC:

  AIC value.

- BIC:

  BIC value.

- RMSE:

  Response-scale root mean squared error.

- Residual_Deviance:

  Residual deviance.

## Details

The following measures are reported:

- AIC:

  Akaike information criterion, balancing likelihood fit and model
  complexity.

- BIC:

  Bayesian information criterion, applying a stronger
  sample-size-dependent complexity penalty.

- RMSE:

  Root mean squared error between observed and response-scale predicted
  values.

- Residual_Deviance:

  Residual deviance of the fitted GLM, as returned by
  [`stats::deviance()`](https://rdrr.io/r/stats/deviance.html).

AIC, BIC and residual deviance describe in-sample fit. RMSE measures
prediction error on the response scale, comparing observed responses
with fitted values from the model. It uses the model's stored data, or
the model frame if those data are unavailable. With the same records as
in the fit, RMSE is an in-sample measure of prediction error. None of
these metrics establishes out-of-sample predictive performance.

For a Poisson frequency model with `offset(log(exposure))`, the fitted
values are expected claim counts for each observation's exposure. RMSE
therefore measures error in claim counts, not claim frequencies per unit
exposure.

Compare models on the same response, estimation records, weights and
offsets. For residual deviance, also use the same model family.

The table does not select a pricing model automatically. In actuarial
model assessment, statistical fit should be considered together with
portfolio calibration, residual behaviour, coefficient stability,
exposure by level and the practical interpretability of the resulting
tariff structure.

The implementation is adapted from `performance::model_performance()`.

## See also

[`rmse()`](https://mharinga.github.io/insurancerating/reference/rmse.md),
[`resample_performance()`](https://mharinga.github.io/insurancerating/reference/resample_performance.md),
[`check_overdispersion()`](https://mharinga.github.io/insurancerating/reference/check_overdispersion.md),
[`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md)

## Author

Martin Haringa

## Examples

``` r
m1 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
          data = MTPL2)
m2 <- glm(nclaims ~ area + premium, offset = log(exposure), family = poisson(),
          data = MTPL2)
# RMSE is in claim counts for these exposure-offset models.
model_performance(m1, m2)
#> # Comparison of Model Performance Indices
#> 
#> Model |   AIC    |   BIC    | RMSE  | Residual_Deviance 
#> ------+----------+----------+-------+------------------ 
#>    m1 | 2285.729 | 2297.741 | 0.356 |          1608.024 
#>    m2 | 2287.546 | 2305.566 | 0.356 |          1607.842 
```
