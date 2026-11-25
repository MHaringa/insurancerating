# Compare GLM fit and predictive error metrics

Report AIC, BIC, response-scale RMSE and residual deviance for one or
more fitted GLMs.

## Usage

``` r
model_performance(..., metrics = NULL)
```

## Arguments

- ...:

  One or more objects of class `"glm"`.

- metrics:

  Optional additional ranking measures, `"gini"` and/or
  `"normalized_gini"`. By default no ranking measures are added.

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

Requested ranking measures are appended as `Gini` and `Normalized_Gini`.

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
in-sample prediction error on the response scale, comparing observed
responses with fitted values on the same rows used to estimate the
model. It is unweighted and uses the same response scale as
[`validate_performance()`](https://mharinga.github.io/insurancerating/reference/validate_performance.md).
None of these metrics establishes out-of-sample predictive performance.
Optional Gini measures also use the estimation rows. They measure
ranking rather than fit or calibration; see
[`discrimination_analysis()`](https://mharinga.github.io/insurancerating/reference/discrimination_analysis.md)
for the exposure definition and supported families.

For a Poisson frequency model with `offset(log(exposure))`, the fitted
values are expected claim counts for each observation's exposure. RMSE
therefore measures error in claim counts, not claim frequencies per unit
exposure. For binomial GLMs it measures errors on the probability scale.

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
[`validate_performance()`](https://mharinga.github.io/insurancerating/reference/validate_performance.md),
[`check_overdispersion()`](https://mharinga.github.io/insurancerating/reference/check_overdispersion.md),
[`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md)

## Author

Martin Haringa

## Examples

``` r
portfolio <- as.data.frame(MTPL)
portfolio$zip <- factor(portfolio$zip)
m1 <- glm(nclaims ~ zip + offset(log(exposure)), family = poisson(),
          data = portfolio)
m2 <- glm(nclaims ~ zip + age_policyholder + offset(log(exposure)),
          family = poisson(), data = portfolio)
# RMSE is in claim counts for these exposure-offset models.
model_performance(m1, m2)
#> # Comparison of Model Performance Indices
#> 
#> Model |    AIC    |    BIC    | RMSE  | Residual_Deviance 
#> ------+-----------+-----------+-------+------------------ 
#>    m1 | 23188.832 | 23222.068 | 0.364 |         16327.135 
#>    m2 | 22967.939 | 23009.484 | 0.362 |         16104.242 
```
