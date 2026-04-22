# Include reference group in regression output

Extract coefficients in terms of the original levels of the coefficients
rather than the coded variables.

`rating_table()` is intended for fitted models:

- plain `glm` objects

- models obtained after
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)

- models obtained after
  [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)

For pre-refit objects (`rating_refinement`, `restricted`, `smooth`) use
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
instead.

## Usage

``` r
rating_table(
  ...,
  model_data = NULL,
  exposure = TRUE,
  exposure_name = NULL,
  exponentiate = TRUE,
  signif_stars = FALSE,
  round_exposure = 0
)

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

  glm object(s) produced by [`glm()`](https://rdrr.io/r/stats/glm.html),
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
  or
  [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)

- model_data:

  Optional data.frame used to create the model(s). If `NULL`, the
  function tries to use `model$data` for each supplied model.

- exposure:

  Logical or character. If `TRUE` (default), exposure is added if it can
  be inferred from the model. If `FALSE`, no exposure is added. If a
  character string is supplied, it is interpreted as the exposure column
  name.

- exposure_name:

  Optional name for the exposure column in the output. If `NULL`, the
  original exposure column name is used.

- exponentiate:

  logical indicating whether or not to exponentiate the coefficient
  estimates. Defaults to TRUE.

- signif_stars:

  show significance stars for p-values (defaults to FALSE)

- round_exposure:

  number of digits for exposure (defaults to 0)

## Value

Object of class `riskfactor`
