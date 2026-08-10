# insurancerating

`insurancerating` provides actuarial tools and building blocks for
traditional insurance pricing workflows. GLMs remain at the core, with
supporting functions for portfolio analysis, risk-factor structuring,
model interpretation, tariff refinement, validation, large-loss
treatment and scalable data preparation.

The package works alongside standard R modelling tools such as
[`glm()`](https://rdrr.io/r/stats/glm.html). Models are fitted with
familiar R functions and can subsequently be analysed, interpreted,
refined and validated in actuarial terms with `insurancerating`. The
building blocks can be used independently or combined according to the
portfolio and pricing question.

The package is developed independently as a generally applicable
actuarial R package. Its examples illustrate possible combinations of
the building blocks; they are not a universal industry workflow or a
description of the pricing methodology or governance process of any
particular organisation.

## Installation

Install the CRAN version, or install the development version from
GitHub:

``` r

install.packages("insurancerating")

# install.packages("remotes")
remotes::install_github("MHaringa/insurancerating")
```

## Explore portfolio experience

[`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
summarises observed experience and the exposure supporting each level of
a rating factor. These unadjusted portfolio patterns are useful before
modelling, but should not be interpreted as conditional tariff
relativities.

``` r

library(insurancerating)

portfolio <- as.data.frame(MTPL)
portfolio$zip <- factor(portfolio$zip)

zip_analysis <- factor_analysis(
  portfolio,
  risk_factors = "zip",
  claim_count = "nclaims",
  exposure = "exposure",
  claim_amount = "amount"
)

autoplot(
  zip_analysis,
  metrics = c("frequency", "average_severity", "risk_premium")
)
```

![Factor analysis plot showing claim frequency, average severity and
risk premium by ZIP code.](reference/figures/unnamed-chunk-3-1.png)

## From model to refined tariff

A standard Poisson GLM can be translated into an explicit tariff
adjustment before it is refitted. The restriction below is deliberately
small: it shows the refinement architecture without replacing the
specialist documentation.

``` r

frequency_model <- glm(
  nclaims ~ zip + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)

zip_restrictions <- data.frame(
  zip = c("0", "1", "2", "3"),
  zip_restricted = c(0.90, 0.95, 1.00, 1.10)
)

refined_model <- frequency_model |>
  prepare_refinement(data = portfolio) |>
  add_restriction(zip_restrictions) |>
  refit()

rating_table(refined_model, exposure = FALSE)
```

``` R
##      risk_factor       level est_refined_model
## 1    (Intercept) (Intercept)          0.136653
## 2 zip_restricted           0          0.900000
## 3 zip_restricted           1          0.950000
## 4 zip_restricted           2          1.000000
## 5 zip_restricted           3          1.100000
```

The refinement steps remain explicit and reviewable. Their actuarial
rationale and effect on the portfolio should still be assessed before
implementation.

## What does insurancerating help with?

| Pricing task | Main tools |
|----|----|
| Explore portfolio experience | [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md), [`outlier_histogram()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md) |
| Structure continuous risk factors | [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md), [`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md) |
| Interpret fitted model effects | [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md), [`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md) |
| Refine tariff structures | [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md), [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md), [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md), [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md) |
| Validate fitted models | [`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md), [`check_overdispersion()`](https://mharinga.github.io/insurancerating/reference/check_overdispersion.md), [`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md), [`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md) |
| Treat large losses | [`assess_excess_threshold()`](https://mharinga.github.io/insurancerating/reference/assess_excess_threshold.md), [`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md) |
| Reduce portfolio data locally | [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md), [`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md) |
| Reduce portfolios in a database | [`rating_grid_db()`](https://mharinga.github.io/insurancerating/reference/rating_grid_db.md), [`merge_date_ranges_db()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges_db.md) |

## Where should I start?

- [Getting
  Started](https://mharinga.github.io/insurancerating/articles/getting-started.html)
  Follow one portfolio through risk-factor analysis, modelling,
  technical risk premium, interpretation, validation and refinement.

- [Pricing workflow and package building
  blocks](https://mharinga.github.io/insurancerating/articles/pricing-workflow-building-blocks.html)
  See how the main actuarial pricing tasks and package components fit
  together.

- [Refinement building
  blocks](https://mharinga.github.io/insurancerating/articles/refinement-workflow.html)
  Translate estimated model effects into an explicit, reviewable and
  implementable tariff structure.

- [Model
  validation](https://mharinga.github.io/insurancerating/articles/model-validation.html)
  Assess statistical adequacy, resampling stability, tariff plausibility
  and observed portfolio behaviour.

- [Large
  Portfolios](https://mharinga.github.io/insurancerating/articles/large-portfolios.html)
  Reduce production-sized portfolios with model-point aggregation and
  database-backed workflows.

## Project information

- [Package website](https://mharinga.github.io/insurancerating/)
- [CRAN package](https://cran.r-project.org/package=insurancerating)
- [Issue tracker](https://github.com/MHaringa/insurancerating/issues)
- Citation information is available with `citation("insurancerating")`.
- The package is distributed under the GPL (\>= 2) licence.
