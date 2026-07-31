# Model validation

## Introduction

Model validation is a common part of actuarial pricing work.

After model estimation and coefficient interpretation, validation
assesses whether the fitted model behaves consistently with its
statistical assumptions and intended pricing use.

In practice, model validation typically considers several dimensions:

- comparative model performance
- coefficient structure
- predictive stability
- distributional diagnostics
- portfolio-level behaviour

`insurancerating` provides tools for several of these validation tasks.

No single diagnostic establishes that a model is suitable for pricing.
The results need to be interpreted together with data quality, portfolio
structure, coefficient stability and the intended tariff application.

## Example setup

The examples below use a simple frequency modelling setup based on
`MTPL2`.

``` r


library(insurancerating)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

df <- MTPL2 |>
  mutate(across(c(area), as.factor)) |>
  mutate(across(c(area), ~ set_reference_level(., exposure)))

mod1 <- glm(
  nclaims ~ area,
  offset = log(exposure),
  family = poisson(),
  data = df
)

mod2 <- glm(
  nclaims ~ area + premium,
  offset = log(exposure),
  family = poisson(),
  data = df
)
```

## Step 1 — Comparative model performance

A first validation step is to compare alternative model specifications.

``` r


model_performance(mod1, mod2)
#> # Comparison of Model Performance Indices
#> 
#> Model |   AIC    |   BIC    | RMSE  
#> ------+----------+----------+------ 
#>  mod1 |  2287.25 | 2311.275 | 0.356 
#>  mod2 | 2289.054 | 2319.086 | 0.356
```

This reports AIC, BIC and response-scale RMSE. AIC and BIC compare
likelihood fit with model complexity, while RMSE measures prediction
error in the response unit.

The purpose of this step is to assess whether the addition or removal of
model terms leads to a materially different fit.

The measures are comparable only when the models use the same response,
estimation records, weights and offsets. They support model comparison
but do not select a specification automatically.

## Step 2 — Coefficient inspection

Model validation is not limited to summary fit statistics. The
coefficient structure also needs to be reviewed.

``` r


rating_table(mod1, mod2, model_data = df, exposure = "exposure") |>
  autoplot()
```

![](model-validation_files/figure-html/unnamed-chunk-4-1.png)

This is used to assess:

- the relative size of fitted effects
- the ordering of factor levels
- the exposure behind each level
- whether differences are plausible and stable

In pricing practice, this is often part of validation, because a model
with slightly better fit may still be less suitable if its coefficient
structure is difficult to interpret or unstable in low-exposure
segments.

## Step 3 — Predictive stability

Single performance measures provide only a point estimate. In many
pricing contexts, it is also relevant to assess how stable that
performance is under small variations in the data.

``` r


bootstrap_performance(
  mod1,
  df,
  n_resamples = 100,
  sample_fraction = 0.8,
  sampling = "bootstrap",
  show_progress = FALSE
) |>
  autoplot()
```

![](model-validation_files/figure-html/unnamed-chunk-5-1.png)

This repeatedly refits the model on bootstrap samples and evaluates RMSE
on out-of-bag records. The resulting distribution measures sensitivity
to portfolio sampling rather than the full uncertainty in future claims.

The output is used to assess:

- the variability of model performance
- whether the fitted model behaves consistently
- whether the model is highly sensitive to changes in the underlying
  sample

This is particularly relevant when portfolios contain sparse segments or
large claim volatility.

## Step 4 — Dispersion checks

For Poisson models, it is common practice to check whether the variance
assumption is broadly appropriate.

``` r


check_overdispersion(mod1)
#> Dispersion ratio =    1.220
#> Pearson's Chi-squared = 3655.711
#> p-value =  < 0.001
#> Overdispersion detected.
```

A dispersion ratio above 1 indicates that the observed variance exceeds
the variance implied by the Poisson model.

This does not automatically invalidate the model, but it does provide an
important diagnostic signal. In pricing practice, overdispersion may
indicate:

- omitted heterogeneity
- model misspecification
- clustering in the data
- or unmodelled portfolio structure

## Step 5 — Residual diagnostics

Residual diagnostics provide an additional view of model adequacy.

``` r


check_residuals(mod1, n_simulations = 600) |>
  autoplot()
#> Residuals consistent with expected distribution (p = 0.934)
```

![](model-validation_files/figure-html/unnamed-chunk-7-1.png)

This step is used to assess whether the residual behaviour is broadly
consistent with the fitted model assumptions.

In GLM settings, simulation-based residual diagnostics are often more
useful than classical residual plots, because they allow the fitted
model to be evaluated relative to its own implied distribution.

The uniformity-test p-value is a diagnostic signal, not a stand-alone
acceptance rule. Its interpretation should be combined with the shape of
the QQ plot, exposure and relevant risk-factor levels.

## Step 6 — Portfolio-level structure

Validation is also performed at portfolio or model-point level.

``` r


grid <- rating_grid(mod1)
head(grid)
#>   area count   exposure
#> 1    1  1194 1065.74795
#> 2    0    15   13.30685
#> 3    2   921  818.53973
#> 4    3   870  764.99178
```

[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
aggregates the fitted model to observed model-point combinations. This
is useful when validation requires a more structured view of:

- observed portfolio composition
- combinations of rating factors
- model-point level summaries
- compact portfolio input for further review

This step is particularly relevant when moving from model validation to
tariff review or implementation support.

## Validation in context

In practice, model validation is rarely based on a single statistic.

A validation exercise often combines:

- comparative performance measures
- coefficient inspection
- predictive stability
- residual and dispersion diagnostics
- portfolio-level review

These steps serve different purposes:

- performance measures assess fit
- coefficient inspection assesses interpretability
- bootstrap analysis assesses stability
- diagnostics assess model adequacy
- portfolio review assesses practical usability

Taken together, they provide evidence for actuarial review of the fitted
model.

## Summary

One possible validation sequence in `insurancerating` is:

``` r


model_performance(...)        # compare fitted models
rating_table(...) |> autoplot()   # inspect coefficient structure
bootstrap_performance(...)    # assess predictive stability
check_overdispersion(...)     # assess dispersion
check_residuals(...)          # inspect residual behaviour
rating_grid(...)              # review model-point structure
```

The diagnostics address different aspects of model behaviour. Their
relevance and materiality depend on the portfolio, model purpose and
validation criteria.

## Next steps

For a modelling example, see:

- [Getting
  started](https://mharinga.github.io/insurancerating/articles/getting-started.md)

For the refinement step after validation, see:

- [Refinement building
  blocks](https://mharinga.github.io/insurancerating/articles/refinement-workflow.md).

For the conceptual background to exposure, risk premium, and tariff
structure, see:

- [Pricing workflow building
  blocks](https://mharinga.github.io/insurancerating/articles/pricing-workflow-building-blocks.md)
