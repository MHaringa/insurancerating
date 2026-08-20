# Calibrate the overall level of a refined pricing model

Adjust the overall prediction level of a fitted model returned by
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
without re-estimating its relative tariff structure. Calibration is a
final model-level operation: all refinement decisions must be completed
before calling `calibrate_model()`.

## Usage

``` r
calibrate_model(model, factor)
```

## Arguments

- model:

  A fitted refined GLM returned by
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).
  It must inherit from `refitrestricted` or `refitsmooth` and use a log
  link.

- factor:

  Positive finite numeric scalar. `1` retains the prediction level,
  values above 1 increase it, and values below 1 decrease it.

## Value

A fitted `glm` that also inherits from `calibrated_model`. Attributes
`calibration_factor`, `calibration_original_intercept`,
`calibration_intercept`, `calibration_log_shift`, `calibration_call` and
`calibrated_at` record the calibration.

## Details

For a refined GLM with a log link, calibration adds `log(factor)` to the
intercept. Consequently, every response-scale prediction is multiplied
by `factor`, while all non-intercept coefficients and tariff
relativities remain unchanged.

The returned object is a copied, internally consistent fitted model. Its
coefficients, linear predictors, fitted values, working residuals,
deviance and AIC are updated to the calibrated level. The original
refined model is not modified. Calibration metadata store the factor,
log shift, original and calibrated intercept, creation time and call.

### Refinement and calibration

Model refinement changes or constrains the relative tariff structure and
is evaluated through
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
one or more `add_*()` operations, and
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).
Model calibration changes only the final overall level. A calibrated
model cannot be calibrated again or used as the starting point for
further refinement. Retain the `rating_refinement` specification and
recalibrate a newly refitted model if earlier decisions need to be
revised.

## See also

[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md),
[`add_prediction()`](https://mharinga.github.io/insurancerating/reference/add_prediction.md),
[`audit_refinement()`](https://mharinga.github.io/insurancerating/reference/audit_refinement.md)

## Examples

``` r
restrictions <- data.frame(
  zip = c(0, 1, 2, 3),
  zip_restricted = c(0.90, 1.00, 1.05, 1.10)
)

mod_initial <- glm(
  nclaims ~ zip + offset(log(exposure)),
  family = poisson(),
  data = MTPL
)

mod_refined <- mod_initial |>
  prepare_refinement() |>
  add_restriction(restrictions) |>
  refit(intercept_only = TRUE)

mod_calibrated <- calibrate_model(mod_refined, factor = 1.05)

rating_table(mod_calibrated)
#>      risk_factor       level est_mod_calibrated
#> 1    (Intercept) (Intercept)           0.138615
#> 2 zip_restricted           0           0.900000
#> 3 zip_restricted           1           1.000000
#> 4 zip_restricted           2           1.050000
#> 5 zip_restricted           3           1.100000

data_final <- mod_refined$data |>
  add_prediction(
    mod_calibrated,
    predictions = "net_risk_premium"
  )
```
