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
  exposure = NULL,
  exponentiate = TRUE,
  signif_stars = FALSE,
  round_exposure = 0
)

rating_factors(
  ...,
  model_data = NULL,
  exposure = NULL,
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

  data.frame used to create glm object(s), this should only be specified
  in case the exposure is desired in the output, default value is NULL

- exposure:

  column in `model_data` with exposure, default value is NULL

- exponentiate:

  logical indicating whether or not to exponentiate the coefficient
  estimates. Defaults to TRUE.

- signif_stars:

  show significance stars for p-values (defaults to FALSE)

- round_exposure:

  number of digits for exposure (defaults to 0)

## Value

Object of class `riskfactor`
