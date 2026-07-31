# Compare fitted GLMs using common performance measures

Compare one or more fitted GLMs using AIC, BIC and response-scale RMSE.
The resulting table provides a concise first comparison of alternative
pricing-model specifications fitted to the same portfolio outcome.

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

  Root mean squared error.

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

Lower values are preferred within each measure, but the measures answer
different questions. AIC and BIC depend on the model likelihood, whereas
RMSE measures error on the response scale. Comparisons are therefore
most meaningful when models use the same response, estimation records,
weights and offsets.

The table does not select a pricing model automatically. In actuarial
model assessment, statistical fit should be considered together with
portfolio calibration, residual behaviour, coefficient stability,
exposure by level and the practical interpretability of the resulting
tariff structure.

The implementation is adapted from `performance::model_performance()`.

## See also

[`rmse()`](https://mharinga.github.io/insurancerating/reference/rmse.md),
[`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md),
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
model_performance(m1, m2)
#> # Comparison of Model Performance Indices
#> 
#> Model |   AIC    |   BIC    | RMSE  
#> ------+----------+----------+------ 
#>    m1 | 2285.729 | 2297.741 | 0.356 
#>    m2 | 2287.546 | 2305.566 | 0.356 
```
