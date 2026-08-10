# Pricing workflow and package building blocks

Insurance pricing is rarely one linear modelling exercise. Different
portfolios require different combinations of data preparation,
exploratory analysis, large-loss treatment, statistical modelling,
tariff refinement and validation. `insurancerating` provides building
blocks for these tasks; it does not prescribe one universal pricing
methodology.

This vignette is a map of those building blocks. It explains which
actuarial question each family of functions addresses and how the
families relate. For a linear tutorial in which one portfolio is
followed from exploratory analysis to a refined tariff model, see
[Getting
Started](https://mharinga.github.io/insurancerating/articles/getting-started.md).

> **A note on workflow**
>
> Insurance pricing workflows vary across organisations because
> portfolios, available data, regulatory requirements, commercial
> objectives and operational constraints differ. There is no single
> universally accepted sequence of analytical activities. The examples
> in this documentation show possible combinations of modular building
> blocks; individual components can be used independently, omitted,
> supplemented or reordered for the problem at hand. They do not
> describe the pricing methodology or governance process of any
> particular organisation.

``` r

library(insurancerating)
```

## Package map

The main building blocks can be placed in the following broad
architecture:

| Actuarial task | Purpose | Main building blocks |
|----|----|----|
| Portfolio analysis | Review exposure, claims and observed experience | [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md), [`outlier_histogram()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md) |
| Risk-factor analysis and structuring | Study continuous effects and derive candidate tariff segments | [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md), [`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md), [`add_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/add_tariff_segments.md) |
| Severity modelling | Inspect claim amounts and truncated severity distributions | [`plot_severity_distribution()`](https://mharinga.github.io/insurancerating/reference/plot_severity_distribution.md), [`fit_truncated_severity()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_severity.md) |
| Large-loss treatment | Assess a threshold and decide how excess loss enters the model | [`assess_excess_threshold()`](https://mharinga.github.io/insurancerating/reference/assess_excess_threshold.md), [`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md) |
| Estimate technical risk | Estimate frequency, severity and expected loss | standard R modelling functions such as [`glm()`](https://rdrr.io/r/stats/glm.html), supported by [`add_prediction()`](https://mharinga.github.io/insurancerating/reference/add_prediction.md) |
| Model interpretation | Express fitted effects as tariff relativities and compare them with experience | [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md), [`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md) |
| Tariff refinement | Apply explicit actuarial adjustments and refit the tariff model | [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md), [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md), [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md), [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md), [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md) |
| Model validation | Examine assumptions, residuals, performance and stability | [`check_overdispersion()`](https://mharinga.github.io/insurancerating/reference/check_overdispersion.md), [`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md), [`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md), [`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md) |
| Portfolio preparation and reduction | Consolidate periods and construct observed model points locally or in a database | [`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md), [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md), [`merge_date_ranges_db()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges_db.md), [`rating_grid_db()`](https://mharinga.github.io/insurancerating/reference/rating_grid_db.md) |
| Policy period operations | Split periods or match dated events to active policies | [`split_periods_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md), [`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md) |

These tasks are related, but their order is not fixed. Large-loss
analysis may change a severity specification, a continuous-factor
analysis may lead to new model variables, and validation may send the
analyst back to model development or tariff refinement.

## Understanding the portfolio

Before model estimation, an actuary commonly asks where the exposure and
claims are concentrated, whether observed differences are supported by
enough experience, and whether sparse levels or extreme observations
require closer attention.

[`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
summarises observed exposure, claim frequency, average severity and risk
premium by rating-factor level:

``` r

zip_experience <- factor_analysis(
  MTPL,
  risk_factors = "zip",
  claim_count = "nclaims",
  claim_amount = "amount",
  exposure = "exposure"
)

head(zip_experience)
#>   zip    amount nclaims   exposure frequency average_severity risk_premium
#> 1   1 116178669    1593 11080.6274 0.1437644         72930.74    10484.846
#> 2   2  59751985    1008  7782.6301 0.1295192         59277.76     7677.608
#> 3   3  58988962    1038  7587.5644 0.1368028         56829.44     7774.427
#> 4   0    821510      29   206.8438 0.1402024         28327.93     3971.644
```

The results are descriptive. They show unadjusted portfolio experience
and do not control for correlations with other rating factors. Their
main purpose is to identify patterns that warrant further analysis and
to show how much experience supports each pattern.

[`outlier_histogram()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)
inspects the central range and tail of a numeric variable.
[`plot_severity_distribution()`](https://mharinga.github.io/insurancerating/reference/plot_severity_distribution.md)
provides a more detailed comparison of claim distributions across
categories. These tools help determine whether an apparent severity
pattern is broad-based or dominated by a small number of large
observations.

## Preparing modelling data

Portfolio reduction can serve two different purposes:

- [`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
  performs **temporal consolidation** by combining compatible adjacent
  or overlapping coverage periods;
- [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
  performs **model-point aggregation** by combining records with
  identical observed rating-factor values.

A model point represents one observed combination of model covariates
together with additive quantities such as exposure, claim count and
claim amount.

``` r

claims_grid <- rating_grid(
  MTPL,
  group_by = c("zip", "bm"),
  exposure = "exposure",
  aggregate_cols = c("nclaims", "amount")
)

head(claims_grid)
#>   zip bm nclaims amount  exposure
#> 1   0  1      11 154173 74.263014
#> 2   0  2       2   7777 31.860274
#> 3   0  3       2 222411 13.657534
#> 4   0  4       1  27081  6.208219
#> 5   0  5       1   5178 12.679452
#> 6   0  6       4  87395 23.128767
```

For a Poisson frequency GLM, model-point aggregation can preserve
coefficient estimates exactly when every predictor is retained, claim
counts and exposure are summed, and the grouped model uses the same
`offset(log(exposure))`. Equivalent results are not automatic for every
model family or diagnostic. Severity aggregation, for example, requires
average claim amount as the response and claim count as the weight,
while record-level residual and influence information is no longer
available after aggregation.

Temporal consolidation normally precedes model-point aggregation when
the original interval structure is needed:

``` r

periods_reduced <- merge_date_ranges(
  policy_periods,
  period_start = "period_start",
  period_end = "period_end",
  group_by = c("policy_id", "coverage"),
  aggregate_cols = "earned_exposure"
)

grid <- rating_grid(
  periods_reduced,
  group_by = c("coverage", "region"),
  exposure = "earned_exposure",
  aggregate_cols = c("claim_count", "claim_amount")
)
```

The detailed in-memory and database-backed workflows are described in
[Large
Portfolios](https://mharinga.github.io/insurancerating/articles/large-portfolios.md).

## Treating large losses

Large claims can materially affect observed severity, fitted
relativities and the technical risk premium. Threshold selection and
excess-loss treatment are therefore modelling choices rather than purely
mechanical data operations.

[`assess_excess_threshold()`](https://mharinga.github.io/insurancerating/reference/assess_excess_threshold.md)
compares how much loss and risk premium remain below alternative
thresholds. It supports judgement about the balance between retaining
risk differentiation and limiting volatility; it does not select a
threshold automatically.

``` r

thresholds <- assess_excess_threshold(
  portfolio,
  claim_amount = "claim_amount",
  thresholds = c(50000, 100000, 150000),
  exposure = "earned_exposure",
  group = "sector",
  claim_count = "claim_count"
)
```

After a threshold has been selected,
[`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md)
supports two distinct uses of the same excess-loss allocation:

| Output | Interpretation | Typical model use |
|----|----|----|
| `"redistributed_claim"` | Add allocated excess loss to retained claim amounts | One severity response containing the complete allocated loss burden |
| `"excess_loading"` | Keep the allocation as an amount per unit of redistribution weight | Retained severity plus a separate excess component in the technical risk premium |

``` r

large_loss_result <- redistribute_excess_loss(
  portfolio,
  claim_amount = "claim_amount",
  threshold = 100000,
  claim_count = "claim_count",
  redistribution_weight = "earned_exposure",
  risk_factor = "sector",
  redistribution_method = "partial",
  output = "excess_loading"
)
```

Neither representation is universally preferable. A redistributed
response is simple to use in one severity model, but allocated excess is
then treated as part of a row’s model response. A separate loading keeps
observed retained severity and allocated excess conceptually distinct.
The decision should take account of claim volume, sparse levels, the
intended severity model and how the technical premium will be
implemented. The function reference pages provide the full allocation
and credibility details.

## Understanding and structuring risk factors

Continuous variables can be included directly in a model. In traditional
tariff structures, continuous relationships are also often translated
into a limited number of segments for stability, interpretation or
implementation.

[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
estimates a smooth univariate relationship. It helps the actuary inspect
shape, local volatility and areas with limited exposure.
[`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md)
then approximates that fitted relationship with candidate intervals;
[`add_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/add_tariff_segments.md)
adds those intervals to the portfolio.

``` r

age_effect <- risk_factor_gam(
  MTPL,
  risk_factor = "age_policyholder",
  claim_count = "nclaims",
  exposure = "exposure"
)

age_segments <- derive_tariff_segments(age_effect)
summary(age_segments)
#>   segment portfolio_records risk_factor_values   exposure claim_count
#> 1 [18,25]              1543                  8 1331.17534         348
#> 2 (25,32]              4254                  7 3648.72055         653
#> 3 (32,39]              4919                  7 4247.34795         615
#> 4 (39,51]              8366                 12 7421.35890        1009
#> 5 (51,58]              3594                  7 3245.45479         372
#> 6 (58,65]              3058                  7 2790.83288         272
#> 7 (65,84]              4181                 19 3900.75890         394
#> 8 (84,95]                85                 10   72.01644           5
#>    frequency
#> 1 0.26142311
#> 2 0.17896684
#> 3 0.14479624
#> 4 0.13595893
#> 5 0.11462184
#> 6 0.09746194
#> 7 0.10100599
#> 8 0.06942859
```

The smooth relationship and its segmented representation answer
different questions. The first estimates how observed risk changes
continuously; the second proposes an implementable tariff structure.
Candidate boundaries still require review against exposure, claim
volume, stability and operational constraints. A complete worked example
is available in [Getting
Started](https://mharinga.github.io/insurancerating/articles/getting-started.md).

## Estimating technical risk

`insurancerating` complements standard R modelling functions rather than
replacing them. A common actuarial decomposition is:

`claim frequency per exposure unit x expected severity = risk premium per exposure unit`.

For a Poisson model with claim count as response and `log(exposure)` as
offset, `predict(type = "response")` returns the expected claim count
for the record’s exposure. Dividing by exposure gives frequency per
exposure unit.

``` r

portfolio <- MTPL
portfolio$zip <- factor(portfolio$zip)

frequency_model <- glm(
  nclaims ~ zip + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)

portfolio$expected_claim_count <- predict(
  frequency_model,
  type = "response"
)
portfolio$claim_frequency <-
  portfolio$expected_claim_count / portfolio$exposure
```

When `amount` is total loss for a row containing several claims,
severity is modelled on `amount / nclaims`, with `nclaims` as the
weight. Multiplying the resulting expected severity by claim frequency
gives technical risk premium per exposure unit. Multiplying expected
severity by expected claim count instead gives expected loss for the
record’s actual exposure. These technical amounts do not yet include
commercial margins, expenses or other premium adjustments.

The full frequency-severity calculation is kept in [Getting
Started](https://mharinga.github.io/insurancerating/articles/getting-started.md),
where the units are followed through to a tariff representation.

## Interpreting model effects

[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
expresses fitted coefficients as tariff relativities and makes reference
levels explicit.
[`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md)
adds the unadjusted experience observed for those same risk-factor
levels.

``` r

rating_table(frequency_model, exposure = "exposure") |>
  add_portfolio_experience(
    data = portfolio,
    claim_count = "nclaims",
    exposure = "exposure",
    metric = "frequency"
  ) |>
  head()
#>   risk_factor       level est_frequency_model exposure
#> 1 (Intercept) (Intercept)           0.1402024       NA
#> 2         zip           0           1.0000000      207
#> 3         zip           1           1.0254064    11081
#> 4         zip           2           0.9238016     7783
#> 5         zip           3           0.9757522     7588
```

This comparison answers two related questions: what conditional effect
did the model estimate, and what pattern is visible directly in the
portfolio? The two need not coincide because the model adjusts for its
other terms. Exposure and claim volume remain important when deciding
whether either pattern is stable enough for tariff use.

## Refining the tariff

Refinement separates statistical estimation from explicit actuarial
tariff decisions. The architecture is:

`estimated model -> prepare refinement -> apply adjustments -> refit -> tariff model`.

``` r

zip_restrictions <- data.frame(
  zip = c("0", "3"),
  relativity = c(0.95, 1.05)
)

refined_model <- frequency_model |>
  prepare_refinement(data = portfolio) |>
  add_restriction(zip_restrictions) |>
  refit()
```

[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
addresses unstable or implausibly irregular adjacent effects.
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
records explicit coefficient choices.
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
introduces a finer tariff structure within broader model levels. Other
steps, such as shrinkage and rebasing, address related implementation
questions.

Refinement should have an actuarial rationale, such as stability,
credibility, monotonicity or an explicit implementation constraint. It
is not a substitute for correcting a misspecified statistical model. The
complete object workflow, including audit output, is described in
[Refinement building
blocks](https://mharinga.github.io/insurancerating/articles/refinement-workflow.md).

## Validating the model

Validation consists of several questions rather than one performance
measure:

| Question | Building block |
|----|----|
| Does a Poisson model show material overdispersion? | [`check_overdispersion()`](https://mharinga.github.io/insurancerating/reference/check_overdispersion.md) |
| Do simulated residuals show systematic structure? | [`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md) |
| How do comparable fitted models differ in likelihood and response-scale error? | [`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md) |
| How sensitive is measured performance to portfolio resampling? | [`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md) |

``` r

check_overdispersion(frequency_model)
#> Dispersion ratio =     1.197
#> Pearson's Chi-squared = 35907.391
#> p-value =   < 0.001
#> Overdispersion detected.
```

These diagnostics provide evidence about assumptions, unexplained
structure and stability. They do not replace review of exposure by
level, coefficient plausibility, observed versus fitted experience,
out-of-sample behaviour or changes in portfolio mix. See [Model
validation](https://mharinga.github.io/insurancerating/articles/model-validation.md)
for a more complete diagnostic workflow.

## Scaling to large portfolios

Scale changes where a building block is executed, but not necessarily
its actuarial purpose.
[`rating_grid_db()`](https://mharinga.github.io/insurancerating/reference/rating_grid_db.md)
performs model-point aggregation lazily in a database.
[`merge_date_ranges_db()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges_db.md)
performs temporal consolidation in DuckDB. The main principle is to
perform the expensive reduction close to the data and collect only the
compact modelling table into R.

``` r

grid_query <- rating_grid_db(
  portfolio_db,
  group_by = c("sector", "region"),
  exposure = "earned_exposure",
  aggregate_cols = c("claim_count", "claim_amount")
)

grid <- dplyr::collect(grid_query)
```

The database-backed functions preserve the same conceptual distinction
as the local functions: period consolidation is temporal, while
rating-grid reduction constructs model points. Memory planning, DuckDB
examples and the preferred order of operations are covered in [Large
Portfolios](https://mharinga.github.io/insurancerating/articles/large-portfolios.md).

## Choosing the building blocks

Not every pricing exercise needs every component:

- A compact, stable portfolio may require only exploratory analysis, a
  GLM,
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  and targeted diagnostics.
- A portfolio with material continuous effects may add
  [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
  and tariff segmentation.
- A portfolio exposed to volatile large losses may add threshold
  assessment and an explicit excess-loss treatment.
- A large policy-period table may first require temporal consolidation
  and database-backed model-point aggregation.
- A mature tariff may require explicit refinement, audit and comparison
  with observed experience.

These are possible combinations, not mandatory recipes. The appropriate
set depends on the response definition, available experience, portfolio
scale, modelling objective and intended tariff implementation.

## Where to go next

- [Getting
  Started](https://mharinga.github.io/insurancerating/articles/getting-started.md)
  is the primary worked tutorial and follows one portfolio through
  modelling, interpretation, validation and a small refinement.
- [Refinement building
  blocks](https://mharinga.github.io/insurancerating/articles/refinement-workflow.md)
  develops smoothing, restrictions, relativities, refitting and audit in
  detail.
- [Model
  validation](https://mharinga.github.io/insurancerating/articles/model-validation.md)
  covers residual, dispersion and resampling diagnostics.
- [Large
  Portfolios](https://mharinga.github.io/insurancerating/articles/large-portfolios.md)
  covers local and database-backed portfolio reduction.
- The [reference
  index](https://mharinga.github.io/insurancerating/reference/index.html)
  maps these actuarial tasks to the exact public functions and
  arguments.
