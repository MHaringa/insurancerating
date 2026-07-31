# Deprecated alias for `rating_table()`

`rating_factors()` is deprecated as of version 0.8.0. Use
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
instead.

## Usage

``` r
rating_factors(
  ...,
  model_data = NULL,
  exposure = TRUE,
  exposure_name = NULL,
  signif_stars = FALSE,
  exponentiate = TRUE,
  round_exposure = 0
)
```

## Arguments

- ...:

  One or more fitted `glm` objects, including models returned by
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).
  Object expressions are used to construct the dynamic estimate column
  names.

- model_data:

  Optional data frame used to fit the models. If `NULL`, the function
  tries to use `model$data` for each supplied model.

- exposure:

  Logical or character string. If `TRUE`, exposure is added if it can be
  inferred from the model. If `FALSE`, no exposure is added. If a
  character string is supplied, it is interpreted as the exposure column
  name.

- exposure_name:

  Deprecated. Use `exposure_output` in
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  instead.

- signif_stars:

  Deprecated. Use `significance` in
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  instead.

- exponentiate:

  Logical. If `TRUE`, coefficients are exponentiated and shown as
  relativities. If `FALSE`, coefficients are shown on the model scale.

- round_exposure:

  Non-negative number of digits used to round exposure.

## Value

See
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md).
