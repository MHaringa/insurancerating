# Performance of fitted GLMs

Computes model performance indices for one or more fitted GLMs.

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

The following indices are reported:

- AIC:

  Akaike's Information Criterion.

- BIC:

  Bayesian Information Criterion.

- RMSE:

  Root mean squared error, computed from observed and predicted values.

This function is adapted from `performance::model_performance()`.

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
