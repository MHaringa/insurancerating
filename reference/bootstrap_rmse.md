# Deprecated RMSE wrapper for `resample_performance()`

`bootstrap_rmse()` is deprecated in favour of
[`resample_performance()`](https://mharinga.github.io/insurancerating/reference/resample_performance.md).
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
  [`resample_performance()`](https://mharinga.github.io/insurancerating/reference/resample_performance.md)
  instead.

- frac:

  Deprecated. Use `sample_fraction` in
  [`resample_performance()`](https://mharinga.github.io/insurancerating/reference/resample_performance.md)
  instead.

- metric:

  Legacy argument that accepts only `"rmse"`. Use `metrics` to select
  metrics.

- sampling:

  Random sampling scheme: `"bootstrap"` or `"split"`.

- show_progress:

  Logical. Show a progress bar during resampling. Default is `TRUE`.

- rmse_model:

  Optional finite numeric RMSE for the original fitted model. If `NULL`
  and RMSE is selected, it is calculated from `model` and `data`.

## Value

See
[`resample_performance()`](https://mharinga.github.io/insurancerating/reference/resample_performance.md).
