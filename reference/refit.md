# Refit a prepared refinement workflow

Applies the refinement steps stored in a `rating_refinement` object and
returns a refitted GLM. This is the final step in the refinement
workflow after
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
or
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
have been used to define the proposed tariff structure.

## Usage

``` r
refit(object, intercept_only = FALSE, ...)
```

## Arguments

- object:

  Object of class `rating_refinement`, usually created with
  [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md).

- intercept_only:

  Logical. If `FALSE` (default), fit the refined model with remaining
  model terms still free. If `TRUE`, keep remaining existing
  relativities fixed as offsets and estimate only the intercept.

- ...:

  Additional arguments passed to
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html), such as `control`.

## Value

A refitted object that inherits from `glm` and additionally from
`refitrestricted`, `refitsmooth`, or both, depending on the applied
refinement steps. The returned model stores attributes used by
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
and
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
to recognise refined rating factors, fixed relativities and smoothing
metadata.

## Details

Refinement steps are not applied to the fitted model immediately. They
are collected on the `rating_refinement` object so they can be inspected
first, for example with
[`autoplot.rating_refinement()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_refinement.md).
`refit()` then applies the steps in order, updates the model formula and
data, and calls [`stats::glm()`](https://rdrr.io/r/stats/glm.html) with
the original model family and any additional arguments passed through
`...`.

With `intercept_only = FALSE`, the refined GLM is fitted with the
remaining free model terms that are still present after applying the
refinement steps. With `intercept_only = TRUE`, remaining original model
effects are fixed as offsets based on the existing fitted relativities.
The refit then estimates only the intercept. This can be useful when the
relative tariff structure should remain fixed and only the overall
premium level should be recalibrated.

Printing the returned model first shows the original and refitted
formulas, the model family, whether an intercept-only refit was used,
and a concise description of every restriction, smoothing or relativity
step. This is followed by the regular `glm` output with the model call,
coefficients, degrees of freedom, deviance and AIC. The object continues
to inherit from `glm`, so standard methods such as
[`stats::predict.glm()`](https://rdrr.io/r/stats/predict.glm.html) and
[`summary.glm()`](https://rdrr.io/r/stats/summary.glm.html) remain
available.

## Author

Martin Haringa

## Examples

``` r
zip_df <- data.frame(
  zip = c(0, 1, 2, 3),
  zip_adj = c(0.8, 0.9, 1.0, 1.2)
)

model <- glm(
  nclaims ~ zip + offset(log(exposure)),
  family = poisson(),
  data = MTPL
)

refined_model <- prepare_refinement(model) |>
  add_restriction(zip_df) |>
  refit(intercept_only = TRUE)
```
