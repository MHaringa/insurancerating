# Shrink categorical tariff relativities towards a common level

Reduce differences between the relativities of one categorical risk
factor before the refined GLM is fitted. `add_shrinkage()` combines each
current relativity with a central level on the logarithmic scale.
Extreme relativities move further in absolute terms, while their
ordering is retained.

## Usage

``` r
add_shrinkage(model, model_variable, credibility = 0.9, weights = NULL)
```

## Arguments

- model:

  A `rating_refinement` object created with
  [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md).
  Shrinkage is applied to the current relativities at this point in the
  ordered refinement workflow.

- model_variable:

  Character string naming the categorical risk factor to shrink. This
  may also identify a tariff factor created by an earlier
  [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
  step. After
  [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
  use the first column of its restriction table; the second column is
  resolved internally.

- credibility:

  Numeric scalar between 0 and 1. This is the weight given to the
  current risk-factor relativity. The remaining weight is assigned to
  the common centre. The default `0.9` retains 90 percent of the current
  logarithmic effect.

- weights:

  `NULL`, `"equal"`, or a character string naming a numeric,
  non-negative column in the refinement data. `NULL` derives the
  weighting basis from explicit model weights or a simple exposure
  offset. `"equal"` gives every level equal weight.

## Value

A `rating_refinement` object containing an ordered shrinkage step. The
returned object stores the original and adjusted relativities, level
weights, inferred weight source and normalization information. The GLM
is fitted only when
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

## Details

Shrinkage can be used when the direction of a fitted risk-factor pattern
is credible, but the difference between its highest and lowest
relativities is considered too large for the available experience or the
intended tariff. It is a structured actuarial adjustment rather than a
new statistical fit.

For level \\i\\, the unnormalised adjusted relativity is

\$\$ \tilde{r}\_i = \exp\\Z \log(r_i) + (1-Z)\log(c)\\, \$\$

where \\r_i\\ is the current relativity, \\Z\\ is `credibility`, and
\\c\\ is the weighted geometric centre. A credibility of 1 leaves the
relativities unchanged. A credibility of 0 removes the differences
between levels.

The adjusted relativities are subsequently rescaled so that their
weighted arithmetic mean equals the weighted arithmetic mean before
shrinkage. With portfolio weights such as exposure or claim count, this
prevents shrinkage itself from changing the weighted level of the risk
factor. The final GLM refit may still change the intercept or other
fitted quantities; use
[`audit_refinement()`](https://mharinga.github.io/insurancerating/reference/audit_refinement.md)
to assess that combined portfolio effect.

### Weight selection

`weights = NULL` first uses explicit GLM weights when these were
supplied during model fitting. Otherwise, a single column in an offset
of the form `log(column)` is used. This commonly selects claim count for
a weighted severity GLM and exposure for a frequency or risk-premium
GLM. If neither source is unambiguous, the function asks for an explicit
choice.

Set `weights` to a column name to control the basis directly. For
example, exposure is generally appropriate for frequency or risk-premium
relativities, while claim count is generally appropriate for severity
relativities. Set `weights = "equal"` to give every risk-factor level
the same weight. In that case the equal-level mean is preserved, which
does not necessarily preserve the level of the observed portfolio.

### Interpretation

`credibility` is a user-supplied refinement parameter. It should not be
interpreted as an automatically estimated Buhlmann or Buhlmann-Straub
credibility factor. Its value should be supported by portfolio
stability, validation over time and the intended degree of tariff
differentiation. The selected value and weighting basis are retained in
the refinement specification and shown by
[`summary()`](https://rdrr.io/r/base/summary.html).

### Following a restriction

When shrinkage follows
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
`model_variable` remains the first column of the restriction table: the
categorical risk factor whose levels are being adjusted. The second
column is a numeric implementation column containing the fixed
relativities used during
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md);
it is not a separate categorical risk factor. `add_shrinkage()` resolves
that internal column automatically from the stored restriction metadata.

## See also

[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
[`add_rebasing()`](https://mharinga.github.io/insurancerating/reference/add_rebasing.md),
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md),
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

refinement <- prepare_refinement(model, data = portfolio) |>
  add_shrinkage(
    model_variable = "sector",
    credibility = 0.9,
    weights = "exposure"
  )

summary(refinement)
#> Refinement specification
#> 
#> Package: insurancerating 0.8.1.9000
#> Created: 2026-08-20 15:23:09 UTC
#> Observations: 8
#> Family: poisson (log link)
#> Base formula:
#>   claims ~ sector + offset(log(exposure))
#> Offset: log(exposure)
#> 
#> Refinement steps: 1
#>   1. Shrinkage: sector (credibility: 0.9, weights: exposure, weighted mean preserved)
#>      credibility = 0.9; weights = exposure; weighted mean preserved
refined_model <- refit(refinement)
rating_table(refined_model)
#>   risk_factor       level est_refined_model
#> 1 (Intercept) (Intercept)          1.000000
#> 2      sector   Transport          3.815528
#> 3      sector      Office          2.945166
#> 4      sector    Industry          1.095723
#> 5      sector      Retail          1.095723

# Use equal level weights explicitly when portfolio weighting is not wanted.
equal_level_refinement <- prepare_refinement(model, data = portfolio) |>
  add_shrinkage(
    model_variable = "sector",
    credibility = 0.8,
    weights = "equal"
  )
```
