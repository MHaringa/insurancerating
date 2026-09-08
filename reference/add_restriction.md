# Add coefficient restrictions to a refinement workflow

Fix selected risk-factor levels at user-supplied relativities or apply
multiplicative adjustments to their current relativities before the
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
  allow_new_risk_factors = FALSE,
  replaces = NULL,
  restriction_type = c("fixed", "multiplier"),
  model_variable = NULL,
  output_variable = NULL
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

  Preferably a named numeric vector: names identify levels and values
  contain fixed relativities or multipliers. In this form,
  `model_variable` is required. Alternatively, a data frame with exactly
  two columns remains fully supported. Its first column identifies the
  risk factor and contains levels; its second column names the generated
  tariff column and contains values. Unspecified levels are fixed at
  their current values or receive multiplier 1, respectively.

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

- replaces:

  `NULL` (default) or a character string naming an existing standalone
  model term that the new fixed risk factor replaces. During
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
  this term is removed before the restricted relativity column is added.
  Supplying `replaces` also provides the explicit opt-in required for a
  new risk factor; `allow_new_risk_factors = TRUE` is then unnecessary.
  Existing terms used in transformations or interactions cannot be
  replaced through this argument.

- restriction_type:

  Character string. `"fixed"` (default) sets selected levels to the
  supplied relativities. `"multiplier"` multiplies the relativity
  present at that point in the refinement workflow by each supplied
  value.

- model_variable:

  `NULL` for the two-column data-frame form, or a character string
  identifying the risk factor when `restrictions` is a named numeric
  vector. This can also identify the `output_variable` from an earlier
  [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
  step.

- output_variable:

  Optional name for the generated tariff column when using named-vector
  `restrictions`. Defaults to `paste0(model_variable, "_restricted")`
  for fixed restrictions and `paste0(model_variable, "_multiplier")` for
  multiplier restrictions. With data-frame `restrictions`, the second
  column name fulfils this role.

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

With `restriction_type = "fixed"`, the supplied values become the final
relativities for the selected levels. With
`restriction_type = "multiplier"`, each supplied value multiplies the
relativity that exists at that point in the ordered refinement workflow.
The multiplier is retained as a relative adjustment: it is not converted
prematurely into a fixed relativity. A multiplier of 1 leaves a level
unchanged, 1.10 increases it by 10%, and 0.95 decreases it by 5%.

The preferred input is a named numeric `restrictions` vector together
with `model_variable`. Vector names identify the levels and values
contain the fixed relativities or multipliers. This keeps every level
directly beside its value. `output_variable` names the generated fixed
tariff column and has a deterministic default.

Alternatively, supply the existing two-column data-frame form. Its first
column name identifies the risk factor and its second column name
identifies the generated tariff column. Both forms are fully supported;
the named-vector form is preferred for concise specifications.

### Actuarial interpretation

A fixed restriction table may contain all levels of the model variable,
or only the levels that need a manual adjustment. If only a subset is
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

When a newly supplied level is written as a numeric interval, the
function checks it against the existing interval levels. A warning is
issued if the new interval overlaps the current classification. The
level is still retained because overlapping labels may occasionally be
intentional, but they do not form an unambiguous tariff partition. To
replace an interval classification, first add the complete new
classification as a separate column in the refinement data, use that
column as the first column of `restrictions`, and identify the old model
term with `replaces`.

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

### Replacing an existing model variable

A new fixed tariff factor can either supplement the fitted GLM or
replace an existing model variable. Supply `replaces` when the new
factor represents an alternative tariff classification for an effect
already present in the model. During
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
the named existing term is removed and the supplied fixed relativities
are inserted in its place. With `replaces = NULL`, the new factor is
added alongside the existing model terms, which preserves the previous
behaviour.

Supplying `replaces` is itself an explicit request to add the new risk
factor, so `allow_new_risk_factors = TRUE` does not also need to be
supplied. The replacement relationship is retained in the ordered
refinement metadata and is shown by
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`audit_refinement()`](https://mharinga.github.io/insurancerating/reference/audit_refinement.md).
This makes clear that the new factor substitutes for an earlier model
effect rather than adding further multiplicative differentiation.

`replaces` is intentionally limited to a standalone main-effect term in
the current refinement formula. A variable used in an interaction or
transformed expression cannot be removed unambiguously through this
argument. Such model structures should be revised explicitly before the
refinement is prepared. This argument is therefore not a general-purpose
facility for deleting model terms.

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

### Ordered fixed and multiplier restrictions

Restriction steps are evaluated in their recorded order. A multiplier
after a fixed restriction scales that fixed relativity. A later fixed
restriction replaces the complete effect at the selected levels and
therefore supersedes earlier multipliers for the same risk factor. For
example, fixing a level at 0.95 and then multiplying by 1.10 gives
1.045; applying those steps in the opposite order gives 0.95.

Multipliers require an active underlying relativity. They cannot create
a new risk factor or level, and cannot be combined with `replaces`.
Their values must be finite and strictly positive because they are
applied on the log scale. The stored multiplier remains visible in
refinement summaries and audit metadata.

## See also

[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`add_shrinkage()`](https://mharinga.github.io/insurancerating/reference/add_shrinkage.md),
[`add_rebasing()`](https://mharinga.github.io/insurancerating/reference/add_rebasing.md),
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

# Preferred form: each level is directly beside its relativity.
refined <- prepare_refinement(model, data = portfolio) |>
  add_restriction(
    restrictions = c(C = 1.10, D = 1.20),
    model_variable = "postal_area"
  )
#> Added new level `D` to risk factor `postal_area` with relativity 1.2. This level was not observed in the model data.

# Postal area D was not observed in the portfolio. Its relativity is an
# explicit tariff assumption and becomes available after refitting.
refined_model <- refit(refined)
rating_table(refined_model, exposure = FALSE)
#>              risk_factor       level est_refined_model
#> 1            (Intercept) (Intercept)          2.096774
#> 2 postal_area_restricted           D          1.200000
#> 3 postal_area_restricted           C          1.100000
#> 4 postal_area_restricted           A          1.000000
#> 5 postal_area_restricted           B          1.000000

# The two-column data-frame form is also fully supported.
restrictions <- data.frame(
  postal_area = c("C", "D"),
  relativity = c(1.10, 1.20)
)
prepare_refinement(model, data = portfolio) |>
  add_restriction(restrictions)
#> Added new level `D` to risk factor `postal_area` with relativity 1.2. This level was not observed in the model data.
#> <rating_refinement>
#> Base model: Poisson GLM (log link)
#> Steps: 1
#>   1. Restriction: postal_area -> relativity (4 levels) [new level: D]

# Multipliers remain relative to the underlying modelled relativities.
prepare_refinement(model, data = portfolio) |>
  add_restriction(
    restrictions = c(B = 1.10, C = 0.95),
    model_variable = "postal_area",
    restriction_type = "multiplier"
  ) |>
  refit()
#> Refined generalized linear model
#> 
#> Original formula:
#>   claims ~ postal_area + offset(log(exposure))
#> 
#> Refitted formula:
#>   claims ~ offset(log(.ir_multiplier_1_postal_area) + log(exposure))
#> 
#> Family: poisson (link: log)
#> Intercept-only refit: no
#> Refinement steps:
#>   1. Multiplier restriction: postal_area -> postal_area_multiplier (3 levels)
#> 
#> 
#> Call:  glm(formula = claims ~ offset(log(.ir_multiplier_1_postal_area) + 
#>     log(exposure)), family = poisson(link = "log"), data = refined_data)
#> 
#> Coefficients:
#> (Intercept)  
#>      0.6817  
#> 
#> Degrees of Freedom: 5 Total (i.e. Null);  5 Residual
#> Null Deviance:       3.023 
#> Residual Deviance: 3.023     AIC: 20.51

# A factor absent from the fitted GLM can replace an existing model term.
# The portfolio must already assign every observation to a hail zone.
portfolio$hail_zone <- factor(c("low", "high", "low", "high", "low", "high"))
hail_restrictions <- data.frame(
  hail_zone = c("low", "high"),
  hail_relativity = c(1.00, 1.20)
)

prepare_refinement(model, data = portfolio) |>
  add_restriction(
    hail_restrictions,
    replaces = "postal_area"
  )
#> <rating_refinement>
#> Base model: Poisson GLM (log link)
#> Steps: 1
#>   1. Restriction: hail_zone -> hail_relativity (2 levels) [expert-specified new risk factor] [replaces postal_area]
# During refit(), hail_zone replaces postal_area rather than supplementing it.

# Without `replaces`, a new fixed factor supplements the existing terms.
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
