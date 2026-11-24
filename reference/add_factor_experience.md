# Add observed factor experience to a rating table

Add observed claims experience by rating factor to a rating table for
comparison with estimated GLM relativities.

## Usage

``` r
add_factor_experience(x, ...)

# S3 method for class 'rating_table'
add_factor_experience(
  x,
  observed = NULL,
  data = NULL,
  risk_factors = NULL,
  claim_count = NULL,
  exposure = NULL,
  claim_amount = NULL,
  metric = NULL,
  label = "Observed experience",
  color = NULL,
  scale = c("reference", "mean"),
  experience = NULL,
  ...
)
```

## Arguments

- x:

  A `rating_table` object returned by
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md).

- ...:

  Unused.

- observed:

  Optional
  [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  object or list of
  [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  objects. If supplied, these observed summaries are attached directly.

- data:

  Optional `data.frame`. If `observed = NULL`, observed experience is
  calculated from this data.

- risk_factors:

  Optional character vector. Risk factors for which observed experience
  should be calculated. If `NULL`, all risk factors in the rating table
  are used.

- claim_count:

  Optional character string. Claim count column used by
  [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md).

- exposure:

  Optional character string. Exposure column used by
  [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md).

- claim_amount:

  Optional character string. Claim amount column used by
  [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md).

- metric:

  Optional character string. Observed metric for the table and default
  plot. Common choices are `"frequency"`,
  `"severity"`/`"average_severity"` and `"risk_premium"`. If `NULL`, the
  first available metric in that order is used, followed by loss ratio
  and average premium.

- label:

  Character; legend label for the observed experience line.

- color:

  Optional line color. If `NULL`, the internal risk premium color is
  used.

- scale:

  Character; scaling for the observed values. One of `"reference"` or
  `"mean"`.

- experience:

  Deprecated alias for `observed`.

## Value

A `rating_table` object with marginal observed experience attached.

## Details

The observed experience is marginal: it is summarised for each rating
factor separately. Supply `data` to calculate it for all factors
available in both the rating table and the data (or specify
`risk_factors`), or pass existing
[`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
results through `observed`.

Estimated GLM relativities adjust for other predictors; marginal
observed experience does not. Differences can reflect the mix of other
risks and do not necessarily indicate model misspecification. Use this
comparison for model interpretation, not formal calibration. Use
[`calibration_analysis()`](https://mharinga.github.io/insurancerating/reference/calibration_analysis.md)
to compare full-model predictions with observed outcomes.

The returned table displays the selected metric as
`observed_relativity`, matched by risk factor and level. The full
observed summaries remain available so
[`autoplot.rating_table()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_table.md)
can display another metric without changing the table column or
recalculating those summaries.

The table and plot use the same scaling. With `scale = "reference"`, the
metric is divided by its observed value at the reference level recorded
in the rating table. If the factor has no recorded reference, the mean
across its observed levels is used. With `scale = "mean"` this mean is
always used. Unmatched rows or ratios with no finite, non-zero
denominator are `NA`. Supply only one observed summary per factor and
level.

## Author

Martin Haringa

## Examples

``` r
df <- as.data.frame(MTPL)
df$zip <- set_reference_level(factor(df$zip), df$exposure)

model <- glm(
  nclaims ~ zip + age_policyholder + offset(log(exposure)),
  family = poisson(),
  data = df
)
effects <- rating_table(
  "Age-adjusted model" = model, model_data = df, exposure = "exposure"
)

comparison <- effects |>
  add_factor_experience(
    data = df,
    risk_factors = "zip",
    claim_count = "nclaims",
    exposure = "exposure",
    label = "Observed frequency"
  )
comparison
#>        risk_factor            level est_Age-adjusted model exposure
#> 1      (Intercept)      (Intercept)              0.3121503       NA
#> 2              zip                0              1.0006508      207
#> 3              zip                1              1.0000000    11081
#> 4              zip                2              0.9020158     7783
#> 5              zip                3              0.9559945     7588
#> 6 age_policyholder age_policyholder              0.9831025       NA
#>   observed_relativity
#> 1                  NA
#> 2           0.9752231
#> 3           1.0000000
#> 4           0.9009127
#> 5           0.9515761
#> 6                  NA
autoplot(comparison, risk_factors = "zip")


observed <- factor_analysis(
  df,
  risk_factors = "zip",
  claim_count = "nclaims",
  exposure = "exposure"
)

effects |>
  add_factor_experience(observed = observed, label = "Observed frequency") |>
  autoplot(risk_factors = "zip", metric = "frequency")

```
