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
  model_variable = NULL,
  source_variable = NULL,
  degree = NULL,
  breaks = NULL,
  smoothing = "spline",
  k = NULL,
  weights = NULL,
  tariff_class = NULL,
  rating_variable = NULL,
  x_cut = NULL,
  x_org = NULL
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

  Object of class `rating_refinement`, usually created with
  [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md).

- model_variable:

  Character string. Existing grouped or binned variable in the GLM. This
  is the model term that will be replaced by a smoothed tariff factor.

- source_variable:

  Character string. Original numeric portfolio variable underlying
  `model_variable`.

- degree:

  Optional single whole number. Polynomial degree.

- breaks:

  Numeric vector with the new tariff class boundaries.

- smoothing:

  Character string with the smoothing method.

- k:

  Optional single positive whole number. Number of basis functions for
  smoothing methods that use a basis dimension.

- weights:

  Optional character string. Weights column, usually exposure.

- tariff_class, rating_variable:

  Deprecated. Use `model_variable` and `source_variable` instead.

- x_cut, x_org:

  Deprecated. Use `model_variable` and `source_variable` instead.

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
