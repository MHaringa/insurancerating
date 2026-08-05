# Add coefficient restrictions to a refinement workflow

Fix selected risk-factor levels at user-supplied relativities before the
refined pricing GLM is fitted. This can be appropriate when sampling
variation produces an implausible local effect, when an actuarial
assumption is supported by additional information, or when a documented
tariff constraint must be applied consistently.

## Usage

``` r
add_restriction(
  model,
  restrictions,
  allow_new_levels = TRUE,
  allow_new_risk_factors = FALSE
)
```

## Arguments

- model:

  Object of class `rating_refinement`, created with
  [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md).
  A fitted GLM, including a model returned by
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
  is not accepted directly; retain and modify the corresponding
  refinement specification instead.

- restrictions:

  Data frame with exactly two columns. The first column must have the
  same name as the risk factor to restrict and contains the levels to
  adjust. This can also be the `output_variable` from an earlier
  [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
  step. The second column contains the replacement relativities. Levels
  that are not supplied are fixed at their current effective
  relativities.

- allow_new_levels:

  Logical. If `TRUE` (default), `restrictions` may contain levels that
  were not observed in the model data. Their supplied relativities are
  treated as explicit tariff assumptions rather than model estimates. If
  `FALSE`, an unknown level results in an error.

- allow_new_risk_factors:

  Logical. If `FALSE` (default), the first column of `restrictions` must
  identify a variable included in the fitted GLM or a tariff factor
  created by an earlier refinement step. Set this to `TRUE` to add an
  external variable that is present in the refinement data but absent
  from both the model and preceding refinement steps. All observed
  levels must then have supplied relativities, which are treated as
  fixed tariff assumptions.

## Value

A `rating_refinement` object containing the stored restriction
specification. The pricing GLM is not fitted again until
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

## Details

`add_restriction()` stores a restriction step on a `rating_refinement`
object. It does not alter the fitted GLM immediately. The restriction is
evaluated in the recorded step order and applied when
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called. Retain the refinement object when reviewing or revising the
specification.

The `restrictions` data frame identifies the risk factor to restrict by
its first column. This may be a variable from the original GLM or a
tariff factor created by an earlier refinement step. The second column
contains the relativities used for those levels in the refined model.

### Actuarial interpretation

The restriction table may contain all levels of the model variable, or
only the levels that need a manual adjustment. If only a subset is
supplied, the missing levels are automatically filled with their current
effective relativities at that point in the refinement workflow. These
may be the original fitted GLM relativities or values produced by
preceding refinement steps. This makes it possible to change one level
explicitly while fixing all other levels at their current values.

Levels that were not observed when the GLM was fitted can also be
supplied. Such a level has no coefficient estimate from the model data.
Its relativity is therefore an explicit tariff assumption, for example
based on expert judgement, external experience or a planned extension of
the tariff. Existing levels that are not supplied remain fixed at their
fitted relativities.

With `allow_new_levels = TRUE`, which is the default, these new tariff
levels are retained in the refinement metadata and subsequently shown by
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md).
An informational message identifies every newly added level, its
supplied relativity and the fact that it was not observed in the model
data. Set `allow_new_levels = FALSE` when the restriction table should
be checked strictly against the levels observed by the fitted model, for
example to detect spelling errors in level names.

A variable that is present in the refinement data but was not included
in the fitted GLM can be added with `allow_new_risk_factors = TRUE`. In
that case all observed levels must have a supplied relativity. The new
factor is applied as a fixed tariff factor during
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md);
its effects are not estimated from the model data. This can be
appropriate when an external classification or expert assumption must be
incorporated, such as a hail zone derived from geographic information.

`allow_new_risk_factors` does not create the portfolio variable itself.
The refinement data must already contain a column assigning every
observation to a level. This is required to apply the supplied
relativities to individual records.

### Updating an existing restriction

A later call to `add_restriction()` for the same risk factor and the
same restricted model variable updates the restriction already stored in
the refinement. Relativities supplied in the later call replace the
previously stored values for those levels. Restrictions for levels that
are not supplied again are retained.

The existing and new values are first combined and the resulting
restriction table is then validated as one specification. This is useful
when an actuarial assumption is revised during model refinement: only
the affected levels need to be supplied again, while the remaining
tariff assumptions stay unchanged. The restriction step keeps its
original position in the workflow, so subsequent steps such as
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
use the revised restricted coefficients.

The second column must retain the same name when an existing restriction
is updated, because that name identifies the restricted model variable
used by
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).
A message reports levels whose previously supplied relativity is
changed.

### Restricting a factor created by add_relativities()

An `output_variable` introduced by an earlier
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
step is already part of the ordered refinement specification. It is
therefore not treated as a new external risk factor and does not require
`allow_new_risk_factors = TRUE`. `add_restriction()` identifies the
preceding relativity step from its stored metadata and replaces the
corresponding derived tariff effect during
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).

When only one level of such a refined variable is supplied, that level
receives the new relativity and every other level is fixed at the
relativity produced by
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).
Mathematically, the resulting restriction therefore covers all current
levels. Only the explicitly supplied level changes. This is useful when
actuarial review supports a local adjustment but the remaining expert
split should not be re-estimated.

Refinement order remains material. A restriction added after
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
operates on the derived split relativities. A restriction added before
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
instead changes the coefficient basis from which the split is derived.

## See also

[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`add_shrinkage()`](https://mharinga.github.io/insurancerating/reference/add_shrinkage.md),
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md),
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)

## Author

Martin Haringa

## Examples

``` r
portfolio <- data.frame(
  claims = c(1, 2, 1, 3, 2, 4),
  exposure = rep(1, 6),
  postal_area = factor(c("A", "B", "C", "A", "B", "C"))
)

model <- glm(
  claims ~ postal_area + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)

restrictions <- data.frame(
  postal_area = c("C", "D"),
  relativity = c(1.10, 1.20)
)

refined <- prepare_refinement(model, data = portfolio) |>
  add_restriction(restrictions)
#> Added new level `D` to risk factor `postal_area` with relativity 1.2. This level was not observed in the model data.

# Postal area D was not observed in the portfolio. Its relativity is an
# explicit tariff assumption and becomes available after refitting.
refined_model <- refit(refined)
rating_table(refined_model, exposure = FALSE)
#>   risk_factor       level est_refined_model
#> 1 (Intercept) (Intercept)          2.096774
#> 2  relativity           A          1.000000
#> 3  relativity           B          1.000000
#> 4  relativity           C          1.100000
#> 5  relativity           D          1.200000

# A risk factor absent from the fitted GLM can be added explicitly. The
# portfolio must already assign every observation to a hail zone.
portfolio$hail_zone <- factor(c("low", "high", "low", "high", "low", "high"))
hail_restrictions <- data.frame(
  hail_zone = c("low", "high"),
  hail_relativity = c(1.00, 1.20)
)

prepare_refinement(model, data = portfolio) |>
  add_restriction(
    hail_restrictions,
    allow_new_risk_factors = TRUE
  )
#> <rating_refinement>
#> Base model: Poisson GLM (log link)
#> Steps: 1
#>   1. Restriction: hail_zone -> hail_relativity (2 levels) [expert-specified new risk factor]

# A later actuarial review changes only the relativity for the low hail zone.
# The high-zone relativity remains 1.20 and the existing step is updated.
revised_hail_restrictions <- data.frame(
  hail_zone = "low",
  hail_relativity = 1.10
)

hail_refinement <- prepare_refinement(model, data = portfolio) |>
  add_restriction(
    hail_restrictions,
    allow_new_risk_factors = TRUE
  ) |>
  add_restriction(revised_hail_restrictions)
#> Updated existing restriction for `hail_zone = "low"`: 1 -> 1.1

refit(hail_refinement)
#> Refined generalized linear model
#> 
#> Original formula:
#>   claims ~ postal_area + offset(log(exposure))
#> 
#> Refitted formula:
#>   claims ~ postal_area + offset(log(hail_relativity) + log(exposure))
#> 
#> Family: poisson (link: log)
#> Intercept-only refit: no
#> Refinement steps:
#>   1. Restriction: hail_zone -> hail_relativity (2 levels) [expert-specified new risk factor]
#> 
#> 
#> Call:  glm(formula = claims ~ postal_area + offset(log(hail_relativity) + 
#>     log(exposure)), family = poisson(link = "log"), data = refined_data)
#> 
#> Coefficients:
#>  (Intercept)  postal_areaB  postal_areaC  
#>    5.534e-01    -6.563e-11     2.231e-01  
#> 
#> Degrees of Freedom: 5 Total (i.e. Null);  3 Residual
#> Null Deviance:       2.714 
#> Residual Deviance: 2.563     AIC: 24.05
```
