
<!-- README.md is generated from README.Rmd. Please edit that file -->

# insurancerating <img src="logo.png" align="right" width="120"/>

<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/insurancerating)](https://cran.r-project.org/package=insurancerating)
[![Downloads](https://cranlogs.r-pkg.org/badges/insurancerating?color=blue)](https://cran.r-project.org/package=insurancerating)

<!-- badges: end -->

`insurancerating` provides functions for common actuarial pricing tasks
in R, including portfolio analysis, GLM estimation, coefficient
refinement and model diagnostics.

The package is organised around GLM-based pricing work and the
translation of statistical model output into tariff structures that can
be reviewed, documented and implemented. The functions can be used
independently or combined into a workflow appropriate for the portfolio
and pricing question.

## Scope

The package supports common tasks that often occur in actuarial pricing
work:

- exploratory analysis of risk factors
- estimation of GLM-based pricing models
- controlled refinement of model coefficients
- construction and interpretation of tariff structures
- evaluation of model performance and stability

The appropriate combination of model choice, segmentation, refinement
and validation depends on the product, data, portfolio composition and
intended use of the tariff.

## Installation

Install the CRAN version:

``` r
install.packages("insurancerating")
```

Or development version:

``` r
# install.packages("remotes")
remotes::install_github("MHaringa/insurancerating")
```

## Quick example

``` r
library(insurancerating)

# Factor analysis
fa <- factor_analysis(
  MTPL,
  risk_factors = "zip",
  claim_count = "nclaims",
  exposure = "exposure",
  claim_amount = "amount"
)

autoplot(
  fa,
  metrics = c("frequency", "average_severity", "risk_premium")
)
```

<img src="man/figures/unnamed-chunk-3-1.png" alt="Factor analysis plot showing frequency, average severity and risk premium by ZIP code."  />

``` r
# Fit model
mod <- glm(
  nclaims ~ zip,
  offset = log(exposure),
  family = poisson(),
  data = MTPL
)

rating_table(mod)
```

    ##   risk_factor       level   est_mod
    ## 1 (Intercept) (Intercept) 0.1402024
    ## 2         zip           0 1.0000000
    ## 3         zip           1 1.0254064
    ## 4         zip           2 0.9238016
    ## 5         zip           3 0.9757522

``` r
# Refine coefficients
zip_df <- data.frame(
  zip = c("0", "1", "2", "3"),
  zip_adj = c(0.90, 0.95, 1.00, 1.10)
)

mod_refined <- prepare_refinement(mod) |>
  add_restriction(zip_df) |>
  refit()

rating_table(mod_refined)
```

    ##   risk_factor       level est_mod_refined exposure
    ## 1 (Intercept) (Intercept)        0.136653       NA
    ## 2     zip_adj           0        0.900000      207
    ## 3     zip_adj           1        0.950000    11081
    ## 4     zip_adj           2        1.000000     7783
    ## 5     zip_adj           3        1.100000     7588

## Combining building blocks

A possible sequence of steps is:

``` r
factor_analysis()      # analyse portfolio
glm()                  # estimate model
prepare_refinement()   # apply adjustments
rating_table()         # interpret coefficients
```

## Core components

### Factor analysis

`factor_analysis()` provides aggregated portfolio metrics such as:

- frequency
- average severity
- risk premium
- loss ratio

These summaries describe observed portfolio experience. They help
identify volume, heterogeneity and potentially unstable levels, but they
do not adjust for correlations with other risk factors and are not
fitted tariff relativities.

### Rating models

Models are estimated using widely used GLM specifications:

- Poisson for frequency
- Gamma for severity
- Gamma with a log link for severity or risk premium

`rating_table()` expresses fitted coefficients in terms of the original
factor levels. When coefficients are exponentiated, they are shown as
relativities.

### Refinement

Model output can be adjusted using:

``` r
prepare_refinement(model) |>
  add_smoothing(...) |>
  add_restriction(...) |>
  add_shrinkage(...) |>
  add_relativities(...) |>
  add_rebasing(...) |>
  refit()
```

Refinement records smoothing, restrictions, shrinkage, sublevel
relativities and rebasing explicitly before the model is refitted.
Shrinkage reduces differences between categorical relativities while
preserving their selected weighted level. Rebasing changes which
resulting tariff level equals 1 without changing the ratios between
levels. These adjustments should be supported by the available
experience, tariff interpretation or documented expert judgement.

### Model structure

``` r
extract_model_data(model)
rating_grid(model)
```

These functions recover the data represented by a fitted model and
construct observed model-point combinations. They are useful when
predictions and tariff reviews must remain aligned with the estimation
portfolio.

## Validation

``` r
model_performance(model)
bootstrap_performance(model, data)
```

These functions compare response-scale error and information criteria
and assess sensitivity under repeated sampling. They provide
complementary diagnostics rather than an automatic model-selection rule.

------------------------------------------------------------------------

## Notes

The examples represent general actuarial pricing applications. Their
modelling choices and thresholds are illustrative and should be assessed
against the portfolio, data quality and governance requirements of the
intended use.

## Learn more

Full documentation and examples are available in the articles:

- [Getting
  started](https://mharinga.github.io/insurancerating/articles/getting-started.html)
- [Pricing workflow
  overview](https://mharinga.github.io/insurancerating/articles/pricing-workflow-building-blocks.html)
- [Refinement building
  blocks](https://mharinga.github.io/insurancerating/articles/refinement-workflow.html)
- [Model
  validation](https://mharinga.github.io/insurancerating/articles/model-validation.html)
- [Working with large insurance
  portfolios](https://mharinga.github.io/insurancerating/articles/large-portfolios.html)
