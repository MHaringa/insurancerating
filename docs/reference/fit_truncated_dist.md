# Fit a distribution to truncated severity (loss) data

**\[experimental\]** Estimate the original distribution from truncated
data.

## Usage

``` r
fit_truncated_dist(
  y,
  dist = c("gamma", "lognormal"),
  left = NULL,
  right = NULL,
  start = NULL,
  print_initial = TRUE,
  n_variants = 1,
  n_shape_grid = 8,
  n_scale_grid = 8,
  trace = FALSE,
  report = TRUE
)
```

## Arguments

- y:

  Vector with observations of losses.

- dist:

  Distribution for severity: `"gamma"` or `"lognormal"`.

- left:

  Numeric. Observations below this threshold are not present in the
  sample.

- right:

  Numeric. Observations above this threshold are not present in the
  sample.

- start:

  Optional list of starting values. If `NULL`, a multi-start strategy is
  used.

- print_initial:

  Deprecated logical retained for backward compatibility.

- n_variants:

  Controls how many local variations around base starts are used.

- n_shape_grid:

  Number of grid points for gamma shape.

- n_scale_grid:

  Number of grid points for gamma scale.

- trace:

  Logical. If `TRUE`, prints periodic progress during the fitting loop.

- report:

  Logical. If `TRUE`, prints a short summary at the end.

## Value

An object of class `c("truncated_dist", "fitdist")` with additional
attributes describing the attempted fits.
