# Add expert-based relativities to a refinement workflow

Adds a relativities step to a `rating_refinement` object.

## Usage

``` r
add_relativities(
  model,
  risk_factor,
  risk_factor_split,
  relativities,
  exposure,
  normalize = TRUE
)
```

## Arguments

- model:

  Object of class `rating_refinement`.

- risk_factor:

  Character string. Name of existing risk factor in the model.

- risk_factor_split:

  Character string. More granular split column.

- relativities:

  Named list of data.frames.

- exposure:

  Character string. Exposure column.

- normalize:

  Logical.

## Value

Object of class `rating_refinement`.

## Details

`add_relativities()` is only available within the new refinement
workflow and should be used in combination with
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md)
and
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).
