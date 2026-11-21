# Deprecated alias for `coefficient_stability()`

`bootstrap_coefficients()` is deprecated. Use
[`coefficient_stability()`](https://mharinga.github.io/insurancerating/reference/coefficient_stability.md)
instead.

## Usage

``` r
bootstrap_coefficients(
  object,
  n_resamples = 500,
  seed = NULL,
  show_progress = interactive()
)
```

## Arguments

- object:

  A fitted `glm` object. Refined GLMs are accepted when their estimation
  data can be recovered from the model object.

- n_resamples:

  Positive whole number. Number of bootstrap samples. Default is 500.

- seed:

  Optional single numeric seed for reproducible resampling.

- show_progress:

  Logical. If `TRUE`, display a text progress bar.

## Value

See
[`coefficient_stability()`](https://mharinga.github.io/insurancerating/reference/coefficient_stability.md).
