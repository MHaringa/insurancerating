# Fit a prepared refinement specification

Apply the ordered steps stored in a `rating_refinement` object and fit
the resulting pricing GLM. This evaluates the current refinement
specification; it may be called repeatedly while smoothing, restrictions
or sublevel relativities are being reviewed.

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

A fitted object inheriting from `glm`. Compatibility classes
`refitrestricted`, `refitsmooth`, or both are added when relevant. The
object stores refinement metadata used by
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
and
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
to identify fixed relativities, smoothed variables and derived tariff
factors.

## Details

`refit()` applies the stored steps in their recorded order, constructs
the required tariff variables and offsets, updates the model formula and
calls [`stats::glm()`](https://rdrr.io/r/stats/glm.html) with the
original model family. Additional fitting arguments can be supplied
through `...`.

### Actuarial interpretation

The refitted model represents the combined effect of the original GLM
structure and the explicit actuarial assumptions stored in the
refinement. Its coefficients and predictions should be assessed against
exposure, observed experience, model diagnostics and the unrestricted
model. A refit does not establish that a manual restriction or curve
edit is statistically estimated; it applies that assumption as
specified.

### Intercept-only recalibration

With `intercept_only = FALSE`, the refined GLM is fitted with the
remaining free model terms that are still present after applying the
refinement steps. With `intercept_only = TRUE`, remaining original model
effects are fixed as offsets based on their existing fitted
relativities. Only the intercept is then estimated. Consequently,
relative differences between those fixed effects remain unchanged while
the overall expected premium level is recalibrated to the supplied model
data.

### Model result and further refinement

Printing the returned model first shows the original and refitted
formulas, the model family, whether an intercept-only refit was used,
and a concise description of every restriction, smoothing or relativity
step. This is followed by the regular `glm` output with the model call,
coefficients, degrees of freedom, deviance and AIC. The object continues
to inherit from `glm`, so standard methods such as
[`stats::predict.glm()`](https://rdrr.io/r/stats/predict.glm.html) and
[`summary.glm()`](https://rdrr.io/r/stats/summary.glm.html) remain
available.

The returned GLM is a fitted result, not an editable refinement
specification. Retain the original `rating_refinement` object when
further changes may be required. Passing the refitted GLM to
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md)
starts a new workflow from that model and does not reconstruct the
earlier sequence of refinement steps.

## See also

[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md),
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md),
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md),
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)

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

refinement <- prepare_refinement(model) |>
  add_restriction(zip_df)

refined_model <- refit(refinement, intercept_only = TRUE)
```
