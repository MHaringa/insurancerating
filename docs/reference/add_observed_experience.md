# Add observed portfolio experience to a rating table

Attach the output of
[`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
to a
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
object so it can be shown in
[`autoplot.rating_table()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_table.md).
This is useful when you want to compare fitted GLM relativities with the
observed portfolio pattern for the same rating factor.

The observed metric is scaled before plotting. With
`scale = "reference"` the metric is divided by the observed value of the
model reference level. If a clear reference level cannot be found, the
metric is scaled to its mean. With `scale = "mean"`, the metric is
always scaled to its mean.

## Usage

``` r
add_observed_experience(
  object,
  experience,
  metric = "risk_premium",
  label = "Observed experience",
  color = NULL,
  scale = c("reference", "mean")
)
```

## Arguments

- object:

  A `rating_table` object returned by
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md).

- experience:

  A `factor_analysis` object returned by
  [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md).

- metric:

  Character; metric from `experience` to plot. Common choices are
  `"frequency"`, `"average_severity"`, `"risk_premium"`, `"loss_ratio"`
  and `"average_premium"`, depending on which columns were supplied to
  [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md).

- label:

  Character; legend label for the observed experience line.

- color:

  Optional line color. If `NULL`, the internal risk premium color is
  used.

- scale:

  Character; scaling applied before plotting. One of `"reference"` or
  `"mean"`.

## Value

A `rating_table` object with observed portfolio experience attached.

## Author

Martin Haringa

## Examples

``` r
df <- MTPL2
df$area <- as.factor(df$area)

model <- glm(
  nclaims ~ area + offset(log(exposure)),
  family = poisson(),
  data = df
)

observed <- factor_analysis(
  df,
  risk_factors = "area",
  claim_count = "nclaims",
  exposure = "exposure"
)

rating_table(model, model_data = df, exposure = "exposure") |>
  add_observed_experience(observed, metric = "frequency") |>
  autoplot(risk_factors = "area")

```
