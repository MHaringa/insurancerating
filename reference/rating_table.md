# Build rating tables from fitted pricing models

`rating_table()` extracts model coefficients in tariff-table form. It
adds the reference level for factor variables, can exponentiate GLM
coefficients into relativities, and can add exposure by risk-factor
level when the model data are available.

In pricing work, this function is useful after fitting or refining a
GLM. It turns model output into a table that is easier to inspect,
compare and use in tariff notes. When `exponentiate = TRUE`,
coefficients are shown as relativities. This is often the most practical
scale for multiplicative GLM tariffs, because each level is expressed
relative to the reference level.

`rating_table()` is intended for fitted models:

- plain `glm` objects

- models obtained after
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)

- models obtained after
  [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)

For pre-refit objects (`rating_refinement`, `restricted`, `smooth`) use
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
instead.

## Usage

``` r
rating_table(
  ...,
  model_data = NULL,
  exposure = TRUE,
  exposure_output = NULL,
  exponentiate = TRUE,
  significance = FALSE,
  round_exposure = 0,
  exposure_name = NULL,
  signif_stars = NULL
)
```

## Arguments

- ...:

  glm object(s) produced by [`glm()`](https://rdrr.io/r/stats/glm.html),
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
  or
  [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)

- model_data:

  Optional data.frame used to create the model(s). If `NULL`, the
  function tries to use `model$data` for each supplied model.

- exposure:

  Logical or character. If `TRUE` (default), exposure is added if it can
  be inferred from the model. If `FALSE`, no exposure is added. If a
  character string is supplied, it is interpreted as the exposure column
  name.

- exposure_output:

  Optional name for the exposure column in the output. If `NULL`, the
  original exposure column name is used.

- exponentiate:

  Logical. If `TRUE` (default), coefficients are exponentiated and shown
  as relativities. If `FALSE`, coefficients are shown on the model
  scale.

- significance:

  Logical; if `TRUE`, show significance stars for p-values.

- round_exposure:

  number of digits for exposure (defaults to 0)

- exposure_name:

  Deprecated. Use `exposure_output` instead.

- signif_stars:

  Deprecated. Use `significance` instead.

## Value

Object of class `"rating_table"` and legacy class `"riskfactor"`.

## Details

A `rating_table` contains one row per model term level. For factor
variables, the reference level is added explicitly with relativity `1`
when `exponentiate = TRUE`, or coefficient `0` when
`exponentiate = FALSE`.

If exposure is supplied or can be inferred from the model data, exposure
is aggregated by risk-factor level. This helps to assess whether fitted
relativities are supported by enough portfolio volume.

Multiple models can be supplied to compare fitted effects side by side.
This is useful when comparing unrestricted and refined models, or
frequency, severity and pure premium models built on the same rating
factors.

## Examples

``` r
df <- MTPL
df$zip <- as.factor(df$zip)

freq <- glm(
  nclaims ~ bm + zip + offset(log(exposure)),
  family = poisson(),
  data = df
)

# Inspect fitted relativities by risk-factor level
rating_table(freq, model_data = df, exposure = "exposure")
#>   risk_factor       level  est_freq exposure
#> 1 (Intercept) (Intercept) 0.1415051       NA
#> 2         zip           0 1.0000000      207
#> 3         zip           1 1.0252572    11081
#> 4         zip           2 0.9237868     7783
#> 5         zip           3 0.9756337     7588
#> 6          bm          bm 0.9978465       NA

# Keep coefficients on the model scale instead of exponentiating
rating_table(
  freq,
  model_data = df,
  exposure = "exposure",
  exponentiate = FALSE
)
#>   risk_factor       level     est_freq exposure
#> 1 (Intercept) (Intercept) -1.955419230       NA
#> 2         zip           0  0.000000000      207
#> 3         zip           1  0.024943493    11081
#> 4         zip           2 -0.079273991     7783
#> 5         zip           3 -0.024668088     7588
#> 6          bm          bm -0.002155826       NA

# Add significance indicators when reviewing model terms
rating_table(
  freq,
  model_data = df,
  exposure = "exposure",
  significance = TRUE
)
#> Significance levels: *** p < 0.001; ** p < 0.01; * p < 0.05; . p < 0.1
#>   risk_factor       level     est_freq exposure
#> 1 (Intercept) (Intercept) 0.141505 ***       NA
#> 2         zip           0 1.000000          207
#> 3         zip           1 1.025257        11081
#> 4         zip           2 0.923787         7783
#> 5         zip           3 0.975634         7588
#> 6          bm          bm 0.997846           NA

# Compare two fitted models side by side
freq_simple <- glm(
  nclaims ~ bm + offset(log(exposure)),
  family = poisson(),
  data = df
)

rating_table(
  freq_simple,
  freq,
  model_data = df,
  exposure = FALSE
)
#>   risk_factor       level est_freq_simple  est_freq
#> 1 (Intercept) (Intercept)       0.1388998 0.1415051
#> 2          bm          bm       0.9977808 0.9978465
#> 3         zip           0              NA 1.0000000
#> 4         zip           1              NA 1.0252572
#> 5         zip           2              NA 0.9237868
#> 6         zip           3              NA 0.9756337
```
