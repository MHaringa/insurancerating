# Deprecated alias for `validate_performance()`

`bootstrap_performance()` is deprecated. Use
[`validate_performance()`](https://mharinga.github.io/insurancerating/reference/validate_performance.md)
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

  Fraction of input rows to draw for each random training sample, before
  adding any rows for missing factor levels. Must be in `(0, 1]`. The
  default, 1, scores the training rows. For held-out scores, use a
  fraction below 1: bootstrap scores out-of-bag rows, while split scores
  the rows left out of the random training draw. Not used for rolling
  validation, which always scores subsequent periods.

- metric:

  Legacy argument that accepts only `"rmse"`. Use `metrics` to select
  metrics.

- sampling:

  Random sampling scheme: `"bootstrap"` or `"split"`. For rolling
  validation, call
  [`validate_performance()`](https://mharinga.github.io/insurancerating/reference/validate_performance.md)
  directly.

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
  `"rmse"` (default), `"mae"`, `"deviance"`, `"gini"` or
  `"normalized_gini"`. The Gini measures assess ranking on the
  evaluation rows; see
  [`discrimination_analysis()`](https://mharinga.github.io/insurancerating/reference/discrimination_analysis.md)
  for supported families.

## Value

See
[`validate_performance()`](https://mharinga.github.io/insurancerating/reference/validate_performance.md).
