# Add smoothing to a refinement workflow

Adds a smoothing step to a `rating_refinement` object.

`smooth_coef()` is deprecated as of version 0.9.0. Please use the
refinement workflow instead:

    prepare_refinement(model) |>
      add_smoothing(...) |>
      refit()

## Usage

``` r
add_smoothing(
  model,
  x_cut,
  x_org,
  degree = NULL,
  breaks = NULL,
  smoothing = "spline",
  k = NULL,
  weights = NULL
)

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

  Object of class `rating_refinement`.

- x_cut:

  column name with breaks/cut

- x_org:

  column name where x_cut is based on

- degree:

  order of polynomial

- breaks:

  numerical vector with new clusters for x

- smoothing:

  smoothing specification

- k:

  number of basis functions

- weights:

  weights column name, usually exposure

## Value

Object of class `rating_refinement`.

## Details

`add_smoothing()` is part of the new refinement workflow and is intended
to be used in combination with
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md)
and
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).

The legacy function `smooth_coef()` remains the only function that
should be applied directly to a model object.

## See also

`add_smoothing()`,
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
