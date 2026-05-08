# Add expert-based relativities to a refinement workflow

Adds a relativities step to a `rating_refinement` object.

## Usage

``` r
add_relativities(
  model,
  model_variable,
  split_variable,
  relativities,
  exposure,
  normalize = TRUE
)
```

## Arguments

- model:

  Object of class `rating_refinement`, usually created with
  [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md).

- model_variable:

  Character string. Existing variable in the GLM. Levels of this
  variable can be split into more detailed groups.

- split_variable:

  Character string. More granular portfolio variable that defines the
  split groups.

- relativities:

  Named list of data.frames, usually created with
  [`relativities_list()`](https://mharinga.github.io/insurancerating/reference/relativities_list.md)
  and
  [`split_level()`](https://mharinga.github.io/insurancerating/reference/split_level.md).

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
