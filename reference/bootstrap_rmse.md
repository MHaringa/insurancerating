# Deprecated alias for `bootstrap_performance()`

`bootstrap_rmse()` is deprecated in favour of
[`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md).
Objects returned by `bootstrap_rmse()` keep class `"bootstrap_rmse"` for
backward compatibility and also inherit from `"bootstrap_performance"`.

## Usage

``` r
bootstrap_rmse(
  model,
  data,
  n = 50,
  frac = 1,
  metric = "rmse",
  sampling = c("bootstrap", "split"),
  show_progress = TRUE,
  rmse_model = NULL
)
```

## Arguments

- model:

  A fitted model object that can be updated on resampled data.

- data:

  Data frame containing the model response and predictors.

- n:

  Deprecated. Use `n_resamples` in
  [`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md)
  instead.

- frac:

  Deprecated. Use `sample_fraction` in
  [`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md)
  instead.

- metric:

  Character string. Performance metric to compute. Currently only
  `"rmse"` is supported.

- sampling:

  Character string. Sampling scheme. `"bootstrap"` samples training rows
  with replacement and evaluates on out-of-bag rows when
  `sample_fraction < 1`. `"split"` samples training rows without
  replacement and evaluates on the remaining rows when
  `sample_fraction < 1`.

- show_progress:

  Logical. Show a progress bar during resampling. Default is `TRUE`.

- rmse_model:

  Optional finite numeric RMSE for the original fitted model. If `NULL`,
  it is calculated from `model` and `data`.

## Value

See
[`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md).
