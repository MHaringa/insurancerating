# Pricing principles

Insurance pricing is not only a statistical modelling exercise. It is
the process of translating observed experience into a tariff that is:

- statistically sound
- commercially viable
- stable over time
- interpretable and explainable

This vignette outlines key concepts that are commonly used with the
building blocks in `insurancerating`.

------------------------------------------------------------------------

### Exposure

Exposure measures the amount of risk observed in the portfolio,
typically as **time under coverage**.

In practice, exposure equals *policy-years*:

- one year –\> exposure = 1\
- six months –\> exposure = 0.5

In motor insurance, this is often expressed as *vehicle-years*.

- more exposure –\> more credible observations\
- less exposure –\> more volatile outcomes

> **Key principle**\
> Pricing metrics are expressed per unit of exposure.

------------------------------------------------------------------------

### Frequency, severity, and risk premium

Insurance losses are typically decomposed into two components:

- frequency: number of claims per unit of exposure
- severity: average claim size

From these, the risk premium is derived:

- risk premium = expected loss per unit of exposure

This can be written as:

- risk premium = frequency × severity

or equivalently:

- risk premium = total loss / exposure

The risk premium is also referred to as:

- pure premium
- burning cost

It represents the expected cost of claims, excluding expenses and
margins.

------------------------------------------------------------------------

### Why this decomposition matters

Separating frequency and severity is useful because:

- they are driven by different risk factors
- they often require different model assumptions
- they behave differently across segments

Typical modelling choices:

- frequency → Poisson GLM
- severity → Gamma GLM
- risk premium → derived or modelled directly

In practice, both approaches are used:

- separate frequency/severity models
- or a direct burning cost model

------------------------------------------------------------------------

### From analysis to tariff

Pricing is not just about estimating expected losses. The process
typically consists of four steps:

1.  **Exploration**\
    Analyse risk factors and identify patterns in the data

2.  **Estimation**\
    Fit statistical models (typically GLMs)

3.  **Refinement**\
    Adjust coefficients to ensure:

    - stability\
    - monotonicity\
    - commercial acceptability

4.  **Translation**\
    Convert model output into a tariff structure

> **Key principle**\
> The refinement step is where actuarial judgement plays a key role.

------------------------------------------------------------------------

### The role of factor analysis

Before fitting models, it is usually useful to understand the data.
[`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
provides a structured way to analyse:

- frequency
- severity
- risk premium
- exposure

Example:

``` r


library(insurancerating)

fa <- factor_analysis(
  MTPL,
  risk_factors = "zip",
  claim_count = "nclaims",
  exposure = "exposure",
  claim_amount = "amount"
)

head(fa)
#>   zip    amount nclaims   exposure frequency average_severity risk_premium
#> 1   1 116178669    1593 11080.6274 0.1437644         72930.74    10484.846
#> 2   2  59751985    1008  7782.6301 0.1295192         59277.76     7677.608
#> 3   3  58988962    1038  7587.5644 0.1368028         56829.44     7774.427
#> 4   0    821510      29   206.8438 0.1402024         28327.93     3971.644
```

This helps to answer questions such as:

- Are differences between segments credible?
- Are there segments with low exposure?
- Are patterns stable or driven by noise?

------------------------------------------------------------------------

### From model to tariff

GLMs are widely used in insurance pricing because they provide:

- interpretable coefficients
- multiplicative structure
- compatibility with tariff construction

However, raw model output is rarely used directly.

Typical issues include:

- non-monotonic patterns
- volatility in low-exposure segments
- overly granular differences

This is why refinement is often useful.

------------------------------------------------------------------------

### Refinement: beyond pure modelling

Refinement includes:

- smoothing coefficients
- imposing monotonic trends
- applying practical constraints
- incorporating expert judgement

The goal is not necessarily to improve statistical fit, but to support a
tariff structure that is:

- stable
- explainable
- commercially usable

In `insurancerating`, this is done through:

``` r


prepare_refinement(model) |>
  add_smoothing(...) |>
  add_restriction(...) |>
  refit()
```

#### Balancing model fit and usability

A key principle in pricing is:

> The best statistical model is not always the best tariff.

Trade-offs include:

- accuracy vs stability
- granularity vs interpretability
- statistical fit vs commercial constraints

For example:

- a highly flexible model may overfit noise
- a perfectly smooth tariff may ignore real risk differences

The role of the actuary is to balance these aspects.

------------------------------------------------------------------------

### Summary

Insurance pricing combines:

- data analysis
- statistical modelling
- business judgement

Key concepts include:

- exposure as the measure of risk volume
- frequency and severity as building blocks of losses
- risk premium as the core pricing metric

The goal is not only to model risk, but to translate it into a tariff
that works in practice.

------------------------------------------------------------------------

### Next steps

For a practical introduction, see:

- [Getting
  started](https://mharinga.github.io/insurancerating/articles/articles/getting-started.md)

For coefficient refinement:

- [Refinement building
  blocks](https://mharinga.github.io/insurancerating/articles/articles/refinement-workflow.md)

------------------------------------------------------------------------

## Actuarial pricing philosophy

Insurance pricing is often presented as a modelling exercise. In
practice, it is primarily a process of **portfolio steering**.

Models estimate expected losses. Tariffs determine which risks enter and
remain in the portfolio.

------------------------------------------------------------------------

### Pricing as portfolio steering

A pricing model does not only describe risk — it influences it.

- Higher premiums discourage certain risks\
- Lower premiums attract others

As a result, pricing decisions directly affect:

- portfolio composition\
- future claims experience\
- overall profitability

This means pricing is often considered in a **forward-looking context**.

------------------------------------------------------------------------

### Risk differentiation as a core principle

A central objective of pricing is **risk differentiation**:

- higher-risk segments → higher premiums\
- lower-risk segments → lower premiums

Well-calibrated differentiation improves:

- portfolio quality\
- predictability of results\
- alignment between price and risk

Poor differentiation leads to:

- adverse selection\
- cross-subsidisation\
- unstable performance

------------------------------------------------------------------------

### Why refinement is often useful

Pure statistical output is rarely suitable for direct use in tariffs.

This is because:

- data can be sparse in certain segments\
- models can capture noise instead of signal\
- coefficients may fluctuate across adjacent levels

Refinement introduces structure:

- smoothing reduces volatility\
- monotonicity enforces logical consistency\
- restrictions incorporate practical constraints

The goal is not necessarily to “improve the model”, but to support a
tariff structure where:

> the tariff behaves in a predictable and explainable way.

------------------------------------------------------------------------

### Stability over time

A good tariff is not only accurate today, but also **stable over time**.
Large fluctuations between renewals can lead to:

- poor customer experience\
- operational complexity\
- unintended portfolio shifts

This requires:

- controlled updates\
- gradual changes\
- monitoring of portfolio impact

------------------------------------------------------------------------

### The role of expert judgement

Insurance pricing cannot be fully automated. Expert judgement is
required to:

- interpret model output\
- decide on appropriate smoothing\
- apply constraints based on business context\
- balance competing objectives

This is particularly important when:

- exposure is low\
- historical data is not representative\
- external factors influence risk

------------------------------------------------------------------------

### Balancing objectives

Pricing involves multiple, often competing objectives:

- statistical accuracy\
- commercial competitiveness\
- interpretability\
- operational simplicity

No single model optimises all dimensions. A useful pricing framework
makes these trade-offs:

- explicit\
- consistent\
- reproducible

------------------------------------------------------------------------

### Pricing as a controlled process

A structured set of tools can help make pricing decisions:

- transparent\
- auditable\
- consistent across portfolios

In `insurancerating`, this is supported through:

- separation of analysis, modelling, and refinement\
- explicit transformation steps\
- reproducible outputs

------------------------------------------------------------------------

### Final perspective

Insurance pricing is not about finding the “best model”. It is about
constructing a tariff that:

- reflects underlying risk\
- behaves predictably\
- supports portfolio objectives

Statistical models are a tool in that process — not the end goal.
