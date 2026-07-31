# Calculate response-scale prediction error

Calculate the root mean squared error (RMSE) between observed outcomes
and response-scale predictions from a fitted model. RMSE summarises the
typical absolute prediction error in the same unit as the model
response.

## Usage

``` r
rmse(x, data = NULL)
```

## Arguments

- x:

  A fitted model object, for example a `"glm"`.

- data:

  Optional data frame on which the observed response and predictions are
  evaluated. If `NULL`, the data stored with the fitted model are used.

## Value

A numeric value: the root mean squared error.

## Details

RMSE is defined as

\$\$\sqrt{\frac{1}{n}\sum\_{i=1}^{n}(y_i-\hat{y}\_i)^2}.\$\$

In pricing work, RMSE can be used to compare alternative specifications
for the same response, portfolio and exposure treatment. Lower values
indicate smaller response-scale errors. Because errors are squared,
individual large deviations receive relatively high weight. This can be
relevant for severity models, but it also makes RMSE sensitive to large
claims.

RMSE values should not be compared across responses with different units
or scales. A value calculated on the estimation data is an in-sample
diagnostic, not an estimate of future predictive performance. Use
resampling or separate validation data when out-of-sample performance is
required, and interpret RMSE together with calibration, residual and
distributional diagnostics.

## See also

[`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md),
[`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md),
[`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md)

## Author

Martin Haringa

## Examples

``` r
x <- glm(nclaims ~ area, offset = log(exposure),
         family = poisson(), data = MTPL2)
rmse(x, MTPL2)
#> [1] 0.3564342
```
