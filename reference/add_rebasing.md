# Rebase categorical tariff relativities to a reference level

Rescale the current relativities of one categorical risk factor so that
a selected level has relativity 1. `add_rebasing()` is an ordered
refinement step: it uses the relativities available at that point in the
workflow and retains all ratios between levels.

## Usage

``` r
add_rebasing(model, model_variable, reference_level = NULL, weights = NULL)
```

## Arguments

- model:

  A `rating_refinement` object created with
  [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md).
  Rebasing uses the current relativities at this point in the ordered
  workflow.

- model_variable:

  Character string naming the categorical risk factor to rebase. This
  may identify an original GLM factor or a tariff factor created by an
  earlier
  [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md),
  [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  or
  [`add_shrinkage()`](https://mharinga.github.io/insurancerating/reference/add_shrinkage.md)
  step.

- reference_level:

  Optional single character value naming the level that should receive
  relativity 1. When `NULL`, the level with the largest aggregated
  weight is selected automatically.

- weights:

  `NULL` or a character string naming a numeric, non-negative column in
  the refinement data. The weights are used only when
  `reference_level = NULL`. `NULL` derives the basis from explicit model
  weights or a simple exposure offset.

## Value

A `rating_refinement` object containing an ordered rebasing step. The
step stores the original and rebased relativities, the selected
reference level, its original relativity, the selection method and, when
applicable, the aggregated level weights. The GLM is fitted only when
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

## Details

Rebasing changes the numerical reference of a tariff factor, but does
not change its relative differentiation. If the current relativity of
reference level \\j\\ is \\r_j\\, every level is transformed as

\$\$ r_i^{new} = \frac{r_i}{r_j}. \$\$

The selected reference level therefore becomes 1, while the ratio
between any two levels remains unchanged. For example, relativities 0.8,
1.0 and 1.2 rebased to the first level become 1.0, 1.25 and 1.5. This is
different from
[`add_shrinkage()`](https://mharinga.github.io/insurancerating/reference/add_shrinkage.md),
which deliberately reduces the spread between levels.

### Selecting the reference level

Supply `reference_level` when the tariff has an established reference
class or when governance requires a particular level to remain at 1. If
`reference_level = NULL`, the level with the largest aggregated weight
is selected. Ties are resolved by the order of the current factor
levels.

With `weights = NULL`, explicit GLM weights are used when available;
otherwise a single offset of the form `log(column)` is used. This
commonly selects claim count for a weighted severity GLM and exposure
for a frequency or risk-premium GLM. An explicit numeric column can be
supplied when another portfolio basis is required. `weights` is ignored
when `reference_level` is supplied because no automatic selection is
then needed.

### Position in the refinement workflow

Rebasing is generally applied after the step that creates the final
tariff levels. For example,
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
may replace a broad level by several sublevels; `add_rebasing()` can
then select one of those resulting sublevels as the new reference. It
can also follow
[`add_shrinkage()`](https://mharinga.github.io/insurancerating/reference/add_shrinkage.md)
when the shrunken relativities should be reported relative to an
established level.

[`set_reference_level()`](https://mharinga.github.io/insurancerating/reference/set_reference_level.md)
serves a different purpose. It changes the contrast reference of a
factor before fitting a GLM. `add_rebasing()` rescales current tariff
relativities inside an existing refinement specification. The refinement
step and selected reference are retained for review by
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`audit_refinement()`](https://mharinga.github.io/insurancerating/reference/audit_refinement.md).

## See also

[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`set_reference_level()`](https://mharinga.github.io/insurancerating/reference/set_reference_level.md),
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md),
[`add_shrinkage()`](https://mharinga.github.io/insurancerating/reference/add_shrinkage.md),
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
[`audit_refinement()`](https://mharinga.github.io/insurancerating/reference/audit_refinement.md)

## Author

Martin Haringa

## Examples

``` r
portfolio <- data.frame(
  claims = c(1, 2, 1, 3, 2, 4, 1, 5),
  exposure = c(1, 1, 1, 1, 2, 1, 1, 1),
  sector = factor(rep(c("Industry", "Office", "Retail", "Transport"), 2))
)

model <- glm(
  claims ~ sector + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)

# Keep Office as the explicit tariff reference after shrinkage.
refinement <- prepare_refinement(model, data = portfolio) |>
  add_shrinkage(
    model_variable = "sector",
    credibility = 0.9,
    weights = "exposure"
  ) |>
  add_rebasing(
    model_variable = "sector",
    reference_level = "Office"
  )

summary(refinement)
#> Refinement specification
#> 
#> Package: insurancerating 0.8.1.9000
#> Created: 2026-08-21 09:36:01 UTC
#> Observations: 8
#> Family: poisson (log link)
#> Base formula:
#>   claims ~ sector + offset(log(exposure))
#> Offset: log(exposure)
#> 
#> Refinement steps: 2
#>   1. Shrinkage: sector (credibility: 0.9, weights: exposure, weighted mean preserved)
#>      credibility = 0.9; weights = exposure; weighted mean preserved
#>   2. Rebasing: sector (reference: Office, selection: explicit, original reference relativity: 2.945166)
#>      reference = Office; original relativity = 2.945166; selection = explicit reference; relative level ratios preserved
refined_model <- refit(refinement)
rating_table(refined_model)
#>   risk_factor       level est_refined_model
#> 1 (Intercept) (Intercept)         2.9451658
#> 2      sector   Transport         1.2955222
#> 3      sector      Office         1.0000000
#> 4      sector    Industry         0.3720411
#> 5      sector      Retail         0.3720411

# Omitting reference_level selects the level with the largest exposure.
exposure_reference <- prepare_refinement(model, data = portfolio) |>
  add_rebasing(
    model_variable = "sector",
    weights = "exposure"
  )
```
