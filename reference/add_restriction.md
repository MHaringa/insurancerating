# Add coefficient restrictions to a refinement workflow

Fixes selected model levels to user-supplied relativities in a
refinement workflow. This is useful when the fitted GLM coefficients
need to be adjusted before the final tariff is refitted, for example to
apply expert judgement, enforce a business rule, remove an implausible
local effect, or make a tariff structure easier to explain.

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
  same name as the model variable to restrict and contains the levels to
  adjust. The second column contains the replacement relativities.
  Levels that are not supplied are filled with the currently fitted GLM
  relativities.

- allow_new_levels:

  Logical. If `TRUE` (default), `restrictions` may contain levels that
  were not observed in the model data. Their supplied relativities are
  treated as explicit tariff assumptions rather than model estimates. If
  `FALSE`, an unknown level results in an error.

- allow_new_risk_factors:

  Logical. If `FALSE` (default), the first column of `restrictions` must
  identify a variable included in the fitted GLM. Set this to `TRUE` to
  add a variable that is present in the refinement data but absent from
  the model. All observed levels must then have supplied relativities,
  which are treated as fixed tariff assumptions.

## Value

Object of class `rating_refinement`.

## Details

`add_restriction()` stores a restriction step on a `rating_refinement`
object. It does not refit the GLM immediately. The restrictions are
applied when
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

The `restrictions` data frame identifies the model variable to restrict
by its first column. The second column contains the relativities that
should be used for those levels in the refined model. New code should
use this function after
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md);
the deprecated
[`restrict_coef()`](https://mharinga.github.io/insurancerating/reference/restrict_coef.md)
wrapper is only kept for backwards compatibility.

The restriction table may contain all levels of the model variable, or
only the levels that need a manual adjustment. If only a subset is
supplied, the missing levels are automatically filled with their current
fitted GLM relativities. This makes it possible to fix one level
explicitly while keeping the other levels at their already estimated
values.

Levels that were not observed when the GLM was fitted can also be
supplied. Such a level has no coefficient estimate from the model data.
Its relativity is therefore an explicit tariff assumption, for example
based on expert judgement, external experience or a planned extension of
the tariff. Existing levels that are not supplied remain fixed at their
fitted relativities.

With `allow_new_levels = TRUE`, which is the default, these new tariff
levels are retained in the refinement metadata and subsequently shown by
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md).
Set `allow_new_levels = FALSE` when the restriction table should be
checked strictly against the levels observed by the fitted model, for
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
  ) |>
  refit()
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
#>    5.978e-01    -1.179e-11     2.231e-01  
#> 
#> Degrees of Freedom: 5 Total (i.e. Null);  3 Residual
#> Null Deviance:       2.321 
#> Residual Deviance: 2.17  AIC: 23.66
```
