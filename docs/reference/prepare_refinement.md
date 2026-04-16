# Prepare a model refinement workflow

Start a refinement workflow for a fitted GLM. Refinement steps such as
smoothing, restrictions and expert-based relativities can be added
sequentially and are only applied once
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

## Usage

``` r
prepare_refinement(model, data = NULL)
```

## Arguments

- model:

  Object of class `glm`.

- data:

  Optional data.frame. If `NULL`, the data are retrieved from the model
  object.

## Value

Object of class `rating_refinement`.
