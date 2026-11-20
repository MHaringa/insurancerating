# Deprecated alias for `resample_performance()`

`bootstrap_performance()` is deprecated. Use
[`resample_performance()`](https://mharinga.github.io/insurancerating/reference/resample_performance.md)
instead.

## Usage

``` r
bootstrap_performance(
  model,
  data,
  n_resamples = 50,
  sample_fraction = 1,
  metric = "rmse",
  sampling = c("bootstrap", "split"),
  show_progress = TRUE,
  rmse_model = NULL,
  n = NULL,
  frac = NULL,
  metrics = "rmse"
)
```

## Arguments

- model:

  A fitted model object that can be updated on resampled data.

- data:

  Data frame containing the model response and predictors.

- n_resamples:

  Positive whole number. Number of resampling replicates. Default is 50.

- sample_fraction:

  Fraction of the data used in the training sample. Must be in `(0, 1]`.
  Default is 1.

- metric:

  Legacy argument that accepts only `"rmse"`. Use `metrics` to select
  metrics.

- sampling:

  Character string. Use `"bootstrap"` (default) to sample training rows
  with replacement or `"split"` to sample without replacement.

- show_progress:

  Logical. Show a progress bar during resampling. Default is `TRUE`.

- rmse_model:

  Optional finite numeric RMSE for the original fitted model. If `NULL`
  and RMSE is selected, it is calculated from `model` and `data`.

- n, frac:

  Deprecated argument names. Use `n_resamples` and `sample_fraction`
  instead.

- metrics:

  Character vector of distinct metrics in the desired output order:
  `"rmse"` (default), `"mae"`, or `"deviance"`.

## Value

See
[`resample_performance()`](https://mharinga.github.io/insurancerating/reference/resample_performance.md).
