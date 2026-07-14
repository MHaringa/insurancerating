# Deprecated smoothing helper

`smooth_coef()` is deprecated as of version 0.9.0. Use
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
instead.


    prepare_refinement(model) |>
      add_smoothing(...) |>
      refit()

## Usage

``` r
smooth_coef(
  model,
  x_cut,
  x_org,
  degree = NULL,
  breaks = NULL,
  smoothing = "spline",
  k = NULL,
  weights = NULL
)
```

## Arguments

- model:

  A fitted model object.

- x_cut:

  Deprecated model variable used in the GLM.

- x_org:

  Deprecated source variable used to fit the smoothing curve.

- degree:

  Deprecated polynomial degree.

- breaks:

  Deprecated smoothing break points.

- smoothing:

  Deprecated smoothing type.

- k:

  Deprecated spline basis dimension.

- weights:

  Deprecated weights column.

## Value

A legacy smooth object. New code should use
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
and
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).

## See also

[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
