# Audit the effect of a fitted model refinement

Compare an unrestricted GLM with the model returned by
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
on the same observed portfolio. The audit records the refinement
specification and quantifies how the fitted response or fitted rate
changes for the portfolio and for each final tariff-factor level.

## Usage

``` r
audit_refinement(
  object,
  exposure = NULL,
  risk_factors = NULL,
  scale = c("auto", "response", "per_exposure"),
  metric = NULL
)
```

## Arguments

- object:

  A fitted model returned by
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).
  Ordinary GLMs do not contain the stored baseline and refinement
  metadata required for the comparison.

- exposure:

  Optional character string naming the exposure column. With
  `scale = "auto"`, the function attempts to infer a single exposure
  column from the original model offset. Supply this argument explicitly
  when that interpretation is ambiguous.

- risk_factors:

  Optional character vector identifying final tariff factors for the
  level comparison. If `NULL`, the final factors reported by
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  and available in
  [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
  are used.

- scale:

  Character string. `"per_exposure"` compares fitted values per unit of
  exposure. `"response"` compares predictions on the response scale.
  `"auto"` selects `"per_exposure"` when one exposure variable can be
  identified from the model offset and otherwise selects `"response"`.

- metric:

  Optional character string used to describe the audited measure, for
  example `"risk_premium"`, `"frequency"` or `"average_severity"`. If
  `NULL`, the audit uses `"fitted_rate"` or `"fitted_response"`.

## Value

An object of class `refinement_audit`. The object contains package and
model metadata, the ordered refinement steps, portfolio-level results,
results by risk factor and level, and the model points used in the
calculation. Use
[`summary.refinement_audit()`](https://mharinga.github.io/insurancerating/reference/summary.refinement_audit.md)
for a concise audit report,
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) for the
level results and
[`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)
for a formatted table.

## Details

Direct coefficient comparisons are generally not a sufficient refinement
audit. A coefficient can change because the intercept or another model
term changes, while the combined fitted value for a policy remains
similar. `audit_refinement()` therefore compares predictions from the
original and refined models on common observed model-point combinations.

The model points are obtained with
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md).
Portfolio and level results are weighted by the number of records or,
when supplied, by `exposure`. With `scale = "per_exposure"`, predictions
that include an exposure offset are divided by total exposure after
aggregation. This gives an exposure-weighted fitted rate rather than an
unweighted average over unique model points.

The resulting measure should be named according to the model being
audited. For a frequency model it is normally a fitted frequency; for a
severity model it is a fitted average severity; and for a direct
risk-premium model it can be labelled `"risk_premium"`. A complete
risk-premium comparison requires either a direct risk-premium model or
an explicit combination of frequency and severity predictions.

## See also

[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md),
[`summary.rating_refinement()`](https://mharinga.github.io/insurancerating/reference/summary.rating_refinement.md)

## Author

Martin Haringa

## Examples

``` r
portfolio <- data.frame(
  claims = c(1, 2, 1, 3, 2, 4),
  exposure = rep(1, 6),
  risk_class = factor(c("A", "B", "A", "B", "A", "B"))
)

base_model <- glm(
  claims ~ risk_class + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)

refinement <- prepare_refinement(base_model, data = portfolio) |>
  add_restriction(data.frame(
    risk_class = "B",
    risk_class_restricted = 1.15
  ))

summary(refinement)
#> Refinement specification
#> 
#> Package: insurancerating 0.8.1.9000
#> Created: 2026-08-12 14:04:49 UTC
#> Observations: 6
#> Family: poisson (log link)
#> Base formula:
#>   claims ~ risk_class + offset(log(exposure))
#> Offset: log(exposure)
#> 
#> Refinement steps: 1
#>   1. Restriction: risk_class -> risk_class_restricted (2 levels)
#>      A = 1.00; B = 1.15

refined_model <- refit(refinement)
audit <- audit_refinement(
  refined_model,
  exposure = "exposure",
  metric = "frequency"
)

summary(audit)
#> Refinement audit
#> 
#> Package: insurancerating 0.8.1.9000
#> Prepared: 2026-08-12 14:04:49 UTC
#> Refitted: 2026-08-12 14:04:49 UTC
#> Audited: 2026-08-12 14:04:49 UTC
#> Measure: frequency (per_exposure)
#> Exposure: exposure
#> 
#> Original formula:
#>   claims ~ risk_class + offset(log(exposure))
#> Refitted formula:
#>   claims ~ offset(log(risk_class_restricted) + log(exposure))
#> 
#> Refinement steps: 1
#>   1. Restriction: risk_class -> risk_class_restricted (2 levels)
#>      A = 1.00; B = 1.15
#> 
#> Portfolio effect
#>   Before: 2.16667
#>   After:  2.16667
#>   Change: 5.01603e-11 (2.315e-09%)
#> 
#> Largest level changes (2 of 2)
#>            risk_factor level   before    after     change change_ratio
#>  risk_class_restricted     A 1.333333 2.015504  0.6821705    0.5116279
#>  risk_class_restricted     B 3.000000 2.317829 -0.6821705   -0.2273902
as.data.frame(audit)
#>             risk_factor level model_points records exposure   before    after
#> 1 risk_class_restricted     A            1       3        3 1.333333 2.015504
#> 2 risk_class_restricted     B            1       3        3 3.000000 2.317829
#>       change change_ratio
#> 1  0.6821705    0.5116279
#> 2 -0.6821705   -0.2273902

if (requireNamespace("gt", quietly = TRUE)) {
  as_gt(audit)
}


  


Refinement impact
```
