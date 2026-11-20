# Model validation

## What does model validation mean?

Validation of an insurance pricing model is broader than comparing fit
statistics. A model may describe the estimation data well while
producing unstable, weakly supported or actuarially implausible tariff
relativities. Conversely, a statistically detectable imperfection may
have little practical pricing effect in the insured portfolio.

No single metric determines whether a model is adequate. A validation
review normally combines several dimensions:

| Dimension | Practical question |
|----|----|
| Statistical adequacy | Are the model structure and distribution broadly consistent with the data? |
| Predictive performance | How well does the model predict the relevant outcome, preferably outside the estimation sample where appropriate? |
| Stability | How sensitive are estimates or performance to variation in the observed sample? |
| Tariff plausibility | Are the effects interpretable, credible and suitable for the intended tariff? |
| Portfolio behaviour | Where does expected experience differ systematically from observed experience? |

These dimensions are complementary rather than a mandatory sequence.
Their relative importance depends on the response, portfolio, modelling
objective and intended use of the tariff.

This vignette starts from fitted models. [Getting
Started](https://mharinga.github.io/insurancerating/articles/getting-started.md)
provides the worked modelling example, while [Pricing workflow and
package building
blocks](https://mharinga.github.io/insurancerating/articles/pricing-workflow-building-blocks.md)
maps validation within the wider package.

## Validation map

The following functions support different validation questions. Not
every model requires every diagnostic.

| Validation question | Typical tool |
|----|----|
| How do alternative fitted models compare? | [`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md) |
| Are tariff relativities plausible and sufficiently supported? | [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md) |
| Does marginal observed experience support the fitted pattern? | [`add_factor_experience()`](https://mharinga.github.io/insurancerating/reference/add_factor_experience.md) |
| How sensitive is measured performance to portfolio resampling? | [`resample_performance()`](https://mharinga.github.io/insurancerating/reference/resample_performance.md) |
| Is a Poisson frequency model materially overdispersed? | [`check_overdispersion()`](https://mharinga.github.io/insurancerating/reference/check_overdispersion.md) |
| Do simulation-based residuals show remaining structure? | [`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md) |
| Where does the model over- or under-predict? | [`check_calibration()`](https://mharinga.github.io/insurancerating/reference/check_calibration.md) |

## Compact example setup

The examples use two Poisson frequency models fitted to the same
records, response and exposure definition. Each prediction is an
expected claim count for the observed policy period because it includes
the earned-exposure offset.

``` r

library(insurancerating)

portfolio <- as.data.frame(MTPL2)
portfolio$area <- factor(portfolio$area)
portfolio$area <- set_reference_level(portfolio$area, portfolio$exposure)

intercept_model <- glm(
  nclaims ~ 1 + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)

area_model <- glm(
  nclaims ~ area + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)
```

The fitted model is starting material here; construction of frequency,
severity and technical risk-premium models is covered elsewhere.

## Comparing model performance

``` r

model_performance(intercept_model, area_model)
#> # Comparison of Model Performance Indices
#> 
#>      Model      |   AIC    |   BIC    | RMSE  | Residual_Deviance 
#> ----------------+----------+----------+-------+------------------ 
#> intercept_model | 2284.056 | 2290.063 | 0.356 |          1608.352 
#>      area_model |  2287.25 | 2311.275 | 0.356 |          1605.545
```

[`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md)
reports AIC, BIC, response-scale RMSE and residual deviance:

- AIC and BIC compare likelihood fit while penalising model complexity;
- residual deviance describes the remaining lack of fit on the
  estimation data;
- RMSE summarises prediction error in the response unit and gives
  relatively high weight to large errors because it squares them. The
  RMSE itself is unweighted. Here the response unit is claim count, not
  claims per unit exposure.

These values are most directly comparable when models use the same
response, estimation records, weights and offsets, as they do here.
Residual deviances should also be compared within the same model family.
RMSE uses only the rows retained in the fit. None of these values
establishes performance on a future portfolio.

Lower values can support a model comparison, but a small improvement
does not automatically imply a better tariff. Exposure by level,
coefficient stability, residual behaviour, observed experience and
practical interpretability remain part of the assessment.

## Inspecting tariff structure and observed experience

[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
expresses fitted GLM effects as tariff relativities and shows the
exposure supporting each level. This makes the statistical output easier
to review in actuarial terms. Observed experience can be attached to the
same table for a direct graphical comparison.

``` r

area_table <- rating_table(
  area_model,
  model_data = portfolio,
  exposure = "exposure"
)

area_table
#>   risk_factor       level est_area_model exposure
#> 1 (Intercept) (Intercept)      0.1369930       NA
#> 2        area           0      0.5485629       13
#> 3        area           1      1.0000000     1066
#> 4        area           2      0.8739528      819
#> 5        area           3      1.0782596      765

area_review <- area_table |>
  add_factor_experience(
    data = portfolio,
    claim_count = "nclaims",
    exposure = "exposure",
    metric = "frequency"
  )

autoplot(
  area_review,
  risk_factors = "area",
  metric = "frequency"
)
```

![](model-validation_files/figure-html/unnamed-chunk-4-1.png)

A model with slightly better aggregate fit can still be unattractive
when it creates volatile neighbouring relativities, extreme effects
supported by little exposure or shapes without a plausible risk
interpretation. An extreme coefficient in a small segment should
therefore not be interpreted in the same way as the same coefficient
supported by a substantial share of the portfolio.

The model line shows conditional tariff relativities. The observed line
shows unadjusted portfolio experience by area. They answer different
questions and need not coincide exactly: observed experience also
reflects differences in the mix of other risk characteristics. The
comparison is useful for identifying levels that require further
investigation, especially when interpreted with their exposure. This
plot does not assess calibration: use
[`check_calibration()`](https://mharinga.github.io/insurancerating/reference/check_calibration.md)
to compare full-model predictions with actual outcomes.

## Assessing resampling stability

``` r

set.seed(123)

resample_result <- resample_performance(
  area_model,
  portfolio,
  n_resamples = 50,
  sample_fraction = 0.8,
  sampling = "bootstrap",
  show_progress = FALSE,
  metrics = c("rmse", "mae", "deviance")
)

head(resample_result$scores)
#>        rmse       mae deviance
#> 1 0.3436239 0.2051759 708.7991
#> 2 0.3715056 0.2115858 746.2368
#> 3 0.3766939 0.2101049 785.8242
#> 4 0.3584633 0.2047888 761.8024
#> 5 0.3542650 0.2069595 716.3567
#> 6 0.3422794 0.2106021 687.1987
autoplot(resample_result, metric = "rmse")
```

![](model-validation_files/figure-html/unnamed-chunk-5-1.png)

With this specification,
[`resample_performance()`](https://mharinga.github.io/insurancerating/reference/resample_performance.md)
samples portfolio rows with replacement, refits the model and evaluates
on out-of-bag rows. RMSE and MAE measure error in claim counts here.
Predictive deviance uses the GLM family and evaluation weights. It is
the same calculation as residual deviance, applied to the evaluation
rows and predictions instead. As a total, it also depends on the number
and weight of the evaluated rows, so the original model’s reference line
is not directly comparable when those differ. Unlike
[`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md),
each resampling score comes from a newly fitted model.

This is a resampling-stability diagnostic. It is not independent
validation on a genuinely later or external portfolio, and it does not
capture all future claim, trend, mix or specification uncertainty. A
narrow distribution indicates less sensitivity to the sampled records; a
wide distribution may point to sparse segments or an unstable
specification.

When prediction on unseen data is central to the objective, this
evidence should be complemented by a suitable holdout, cross-validation
or temporal validation design. With `sampling = "split"` and
`sample_fraction < 1`, the evaluation uses held-out rows from a split
without replacement. At `sample_fraction = 1`, both sampling schemes
evaluate on their training rows and measure in-sample stability.

## Testing on later periods

Rolling validation fits the specification on earlier periods and scores
it on the next period or periods. The training window expands one period
at a time. The example years here illustrate the interface; MTPL2 has no
actual calendar-year information.

``` r

temporal_data <- as.data.frame(MTPL2)
temporal_data$year <- rep(2020:2025, length.out = nrow(temporal_data))
temporal_model <- glm(
  nclaims ~ factor(area) + offset(log(exposure)),
  family = poisson(), data = temporal_data
)
resample_performance(
  temporal_model, temporal_data, sampling = "rolling", time = "year",
  initial = 3, metrics = c("rmse", "mae", "deviance"), show_progress = FALSE
)
#>   iteration train_start train_end test_start test_end n_train n_test      rmse
#> 1         1        2020      2022       2023     2023    1500    500 0.3148918
#> 2         2        2020      2023       2024     2024    2000    500 0.3402196
#> 3         3        2020      2024       2025     2025    2500    500 0.3490226
#>         mae deviance
#> 1 0.2056950 237.7274
#> 2 0.2084982 259.1681
#> 3 0.2086374 255.4882
```

Use actual historical periods to assess temporal predictive performance.
Unlike plotting calibration by year for a fixed model, this refits the
model on only the information available in each training window.

## Checking distributional assumptions

### Overdispersion

``` r

dispersion_check <- check_overdispersion(area_model)
dispersion_check
#> Dispersion ratio =    1.220
#> Pearson's Chi-squared = 3655.711
#> p-value =  < 0.001
#> Overdispersion detected.
```

For a Poisson frequency GLM, the dispersion ratio is Pearson’s
chi-squared statistic divided by the residual degrees of freedom. A
value above one means that observed variation exceeds the variance
implied by the Poisson model.

Both statistical evidence and practical magnitude matter. In a large
insurance portfolio, a small departure can produce a very small p-value
simply because many observations are available. The size of the
dispersion ratio and its effect on uncertainty and tariff decisions are
generally more informative than the p-value alone. Overdispersion can
indicate omitted heterogeneity, clustering, unusual observations or
model misspecification, but does not by itself identify the cause.

### Simulation-based residuals

``` r

set.seed(123)

residual_check <- check_residuals(
  area_model,
  n_simulations = 250
)

autoplot(residual_check)
```

![](model-validation_files/figure-html/unnamed-chunk-8-1.png)

[`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md)
uses DHARMa simulations from the fitted model to construct scaled
residuals. The QQ plot and uniformity test assess whether the observed
response behaves consistently with the distribution implied by that
model.

Systematic departures can indicate remaining structure, distributional
mismatch or unusual observations. The uniformity p-value is a diagnostic
signal, not a stand-alone acceptance rule. Its interpretation should be
combined with the location and shape of departures, fitted values,
exposure and relevant risk-factor levels.

## Reviewing observed versus expected experience

Use
[`check_calibration()`](https://mharinga.github.io/insurancerating/reference/check_calibration.md)
to compare observed and expected outcomes overall and within segments.
Here the predictions include earned exposure, so both totals are claim
counts.

``` r

check_calibration(area_model, portfolio)
#>   Observed Expected OE    N Exposure
#> 1      358      358  1 3000 2662.586
area_calibration <- check_calibration(area_model, portfolio, by = "area")
area_calibration
#>   area Observed Expected OE    N   Exposure
#> 1    1      146      146  1 1194 1065.74795
#> 2    0        1        1  1   15   13.30685
#> 3    2       98       98  1  921  818.53973
#> 4    3      113      113  1  870  764.99178
autoplot(area_calibration)
```

![](model-validation_files/figure-html/unnamed-chunk-9-1.png)

An O/E ratio above one indicates more observed claims than expected; a
ratio below one indicates fewer. The overall ratio describes
calibration-in-the-large.

Use `by = "year"` when the evaluation data contain a calendar year. A
change in O/E across years can indicate a change in the overall
prediction level, even if the model still ranks high- and low-risk
policies equally well.
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
shows that comparison as an O/E line over time.

### Does the model get claim frequency right at different risk levels?

With `groups = 5`, the function sorts policy records from lowest to
highest **predicted claims per policy-year**. Dividing each prediction
by its exposure for this ranking means a longer policy period is not
treated as a higher risk just because more claims are expected during
it.

The sorted records are split into five groups. Group 1 contains the
lowest predicted risks and group 5 the highest. Each group aims for
about 20% of the portfolio’s total exposure, not 20% of the policies.
These are groups of model predictions, not rating-factor levels such as
area. Policies with the same predicted frequency can fall on either side
of a group boundary; ties follow the input row order.

``` r

risk_calibration <- check_calibration(area_model, portfolio, groups = 5)
knitr::kable(risk_calibration, digits = 2)
```

| Group | Observed | Expected |   OE |   N | Exposure |
|------:|---------:|---------:|-----:|----:|---------:|
|     1 |       58 |    63.22 | 0.92 | 599 |   532.99 |
|     2 |       76 |    67.69 | 1.12 | 598 |   531.76 |
|     3 |       70 |    72.95 | 0.96 | 593 |   532.48 |
|     4 |       78 |    75.49 | 1.03 | 608 |   532.89 |
|     5 |       76 |    78.65 | 0.97 | 602 |   532.46 |

``` r

autoplot(risk_calibration)
```

![Observed against expected claims per policy-year for five groups of
predicted risk, with a diagonal line where the frequencies are
equal.](model-validation_files/figure-html/unnamed-chunk-10-1.png)

Each dot is one group. Its x-coordinate is total expected claims divided
by the group’s total policy-years; its y-coordinate is total observed
claims divided by the same policy-years. For group 2 (the second dot
from the left), there were 76 observed claims versus about 68 expected,
so the dot lies **above** the diagonal: the model predicted too few
claims for that group. A dot below the diagonal means it predicted too
many.

This is a visual check for systematic over- or under-prediction, not a
statistical test or a measure of how well the model ranks individual
risks. Read a gap alongside the group’s exposure and claim volume: a
large O/E deviation based on few claims may have little credibility.
This example uses the estimation data; use separate evaluation data to
assess calibration on unseen policies. For model-point aggregation
before modelling rather than calibration, see [Large
Portfolios](https://mharinga.github.io/insurancerating/articles/large-portfolios.md).

## Validation, investigation and refinement

### Comparing observed claims before and after refinement

For a model returned by
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
the original portfolio may not contain the columns added by smoothing or
restrictions. To check the refit’s own portfolio, use
`extract_model_data(refined_model)`. For a smoothed model, group by the
column containing the new tariff bands (for example, `by = "age_smooth"`
if the source variable was `age`), rather than by the numeric relativity
column used in the offset. On separate evaluation data, apply the same
refinement mappings to those rows first.

To check a restriction, put **observed claims, expected claims before
and expected claims after** side by side. Use the same records and the
same grouping column for both models so that any change comes from the
predictions rather than a change in portfolio mix.

``` r

restricted_model <- prepare_refinement(area_model, data = portfolio) |>
  add_restriction(c("3" = 1.10), model_variable = "area") |>
  refit()

evaluation <- extract_model_data(restricted_model)
before <- check_calibration(area_model, evaluation, by = "area")
after <- check_calibration(restricted_model, evaluation, by = "area")

knitr::kable(data.frame(
  Area = after$area,
  Exposure = after$Exposure,
  Observed = after$Observed,
  Expected_before = before$Expected,
  Expected_after = after$Expected
), digits = 2)
```

| Area | Exposure | Observed | Expected_before | Expected_after |
|:-----|---------:|---------:|----------------:|---------------:|
| 1    |  1065.75 |      146 |             146 |         145.08 |
| 0    |    13.31 |        1 |               1 |           0.99 |
| 2    |   818.54 |       98 |              98 |          97.38 |
| 3    |   764.99 |      113 |             113 |         114.55 |

Use fixed bands or rating-factor levels for this comparison. With
`groups = 5`, each model can rank policies differently, so its group 1
need not contain the same policies as group 1 from the other model.

### Acting on validation findings

Validation identifies evidence; it should not silently alter the model.
A useful response to an issue is:

1.  identify the affected observations or tariff levels;
2.  investigate the likely cause;
3.  revise the data or model when the statistical specification is
    responsible;
4.  apply an explicit actuarial refinement only when it has a defensible
    tariff rationale;
5.  validate the resulting model again.

Incorrect exposure, data-quality problems, missing interactions, poor
segmentation, inappropriate distributions and large-loss treatment can
all produce unattractive diagnostics. These issues may require
revisiting the model rather than smoothing or restricting its
coefficients. The [Refinement building
blocks](https://mharinga.github.io/insurancerating/articles/refinement-workflow.md)
vignette explains how justified tariff adjustments can be recorded and
refitted explicitly.

## Putting the evidence together

Validation should combine evidence rather than rank models on one
metric. A model may have a slightly lower AIC but unstable coefficients,
weak observed support and material residual structure. Such a model may
be less suitable for tariff implementation than a more stable
alternative.

Conversely, a statistically significant diagnostic result may have
little practical effect when its magnitude is small and portfolio
behaviour remains stable. The conclusion should reflect statistical
adequacy, predictive evidence, sampling stability, exposure and
credibility, actuarial plausibility and the intended pricing use.

## Where to go next

- [Getting
  Started](https://mharinga.github.io/insurancerating/articles/getting-started.md)
  constructs and interprets a pricing model in one worked example.
- [Pricing workflow and package building
  blocks](https://mharinga.github.io/insurancerating/articles/pricing-workflow-building-blocks.md)
  maps the complete package architecture.
- [Refinement building
  blocks](https://mharinga.github.io/insurancerating/articles/refinement-workflow.md)
  translates identified tariff issues into explicit, reviewable
  adjustments.
- [Large
  Portfolios](https://mharinga.github.io/insurancerating/articles/large-portfolios.md)
  covers model-point aggregation and database-backed reduction.
