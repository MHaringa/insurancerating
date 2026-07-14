# Deprecated alias for `fit_truncated_severity()`

`fit_truncated_dist()` is deprecated as of version 0.9.0. Use
[`fit_truncated_severity()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_severity.md)
instead.

## Usage

``` r
fit_truncated_dist(
  losses = NULL,
  distribution = c("gamma", "lognormal"),
  lower_truncation = NULL,
  upper_truncation = NULL,
  start_values = NULL,
  print_initial = TRUE,
  n_variants = 1,
  n_shape_grid = 8,
  n_scale_grid = 8,
  show_progress = FALSE,
  show_summary = TRUE,
  y = NULL,
  dist = NULL,
  left = NULL,
  right = NULL,
  start = NULL,
  trace = NULL,
  report = NULL
)
```

## Arguments

- losses:

  Numeric vector with observed claim severities.

- distribution:

  Severity distribution to fit: `"gamma"` or `"lognormal"`.

- lower_truncation:

  Numeric lower truncation point. Claims at or below this value are
  assumed not to be present in `losses`. Defaults to `0`.

- upper_truncation:

  Numeric upper truncation point. Claims at or above this value are
  assumed not to be present in `losses`. Defaults to `Inf`.

- start_values:

  Optional named list of starting values. If `NULL`, a multi-start
  strategy is used. For a gamma distribution use `shape` and `scale`;
  for a lognormal distribution use `meanlog` and `sdlog`.

- print_initial:

  Deprecated logical retained for backward compatibility.

- n_variants:

  Controls how many local variations around base starts are used.

- n_shape_grid:

  Number of grid points for gamma shape.

- n_scale_grid:

  Number of grid points for gamma scale.

- show_progress:

  Logical. If `TRUE`, prints periodic progress during the fitting loop.

- show_summary:

  Logical. If `TRUE`, prints a short summary at the end.

- y, dist, left, right, start, trace, report:

  Deprecated argument names kept for backward compatibility.

## Value

See
[`fit_truncated_severity()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_severity.md).
