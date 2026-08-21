# Present fitted pricing-model effects as a rating table

Extract coefficients from one or more fitted GLMs and organise them by
risk factor and level. Reference levels are made explicit, coefficients
can be expressed as multiplicative relativities, and portfolio exposure
can be attached to support actuarial review.

## Usage

``` r
rating_table(
  ...,
  model_data = NULL,
  exposure = TRUE,
  exposure_output = NULL,
  estimate_name = NULL,
  exponentiate = TRUE,
  significance = FALSE,
  reference_first = TRUE,
  level_order = c("estimate_descending", "estimate_ascending", "model", "alphabetical"),
  level_order_by_risk_factor = NULL,
  numeric_level_order = c("ascending", "as_specified"),
  risk_factor_order = c("model", "alphabetical"),
  order_model = NULL,
  round_exposure = 0,
  exposure_name = NULL,
  signif_stars = NULL
)
```

## Arguments

- ...:

  One or more fitted `glm` objects, including models returned by
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).
  Object expressions are used to construct the dynamic estimate column
  names.

- model_data:

  Optional data frame used to fit the models. If `NULL`, the function
  tries to use `model$data` for each supplied model.

- exposure:

  Logical or character string. If `TRUE`, exposure is added if it can be
  inferred from the model. If `FALSE`, no exposure is added. If a
  character string is supplied, it is interpreted as the exposure column
  name.

- exposure_output:

  Optional character string naming the exposure column in the output. If
  `NULL`, the original exposure column name is used.

- estimate_name:

  Optional character vector with the exact output column name for each
  model estimate. Supply one value for one model, an unnamed vector in
  model order, or a named vector whose names identify the supplied model
  objects. If `NULL`, columns retain the default `est_<model>` names.

- exponentiate:

  Logical. If `TRUE`, coefficients are exponentiated and shown as
  relativities. If `FALSE`, coefficients are shown on the model scale.

- significance:

  Logical. If `TRUE`, add a separate `signif_*` column for each model
  containing significance indicators based on coefficient p-values. The
  corresponding `est_*` columns remain numeric.

- reference_first:

  Logical. If `TRUE`, place the reference level first when the global
  ordering of a nominal risk factor is `"model"` or `"alphabetical"`.
  Numeric levels, ordered factors, estimate-based ordering and explicit
  per-factor overrides retain their selected order. For an ordinary GLM,
  the reference is obtained from the fitted factor contrasts. After
  [`add_rebasing()`](https://mharinga.github.io/insurancerating/reference/add_rebasing.md),
  the selected rebasing level is used.

- level_order:

  Character string controlling the default order of nominal factor
  levels. `"estimate_descending"` (default) places the highest fitted
  effect first, `"estimate_ascending"` places the lowest first,
  `"model"` retains the fitted model order and `"alphabetical"` sorts
  labels. Numeric levels and explicitly ordered factors use their
  substantive order instead.

- level_order_by_risk_factor:

  Optional named character vector providing an ordering override for
  individual risk factors. Names identify risk factors and values must
  be `"model"`, `"alphabetical"`, `"estimate_ascending"` or
  `"estimate_descending"`. For example,
  `c(urbanisation = "model", sector = "estimate_descending")` preserves
  an ordinal urbanisation scale while ordering sector relativities from
  high to low. Numeric ordering still takes precedence.

- numeric_level_order:

  Character string controlling levels that are all recognisable as
  numbers or numeric intervals. `"ascending"` orders them by numeric
  value, or by the lower and then upper interval boundary, regardless of
  `level_order`. This correctly orders labels such as `(100,200]` and
  `(1000,2000]`. `"as_specified"` leaves these levels to `level_order`.
  Numeric ordering takes precedence over `reference_first`, so the
  reference level is not moved away from its numerical position.

- risk_factor_order:

  Character string controlling risk-factor order. `"model"` retains the
  order in the fitted model; `"alphabetical"` sorts risk-factor names.
  The intercept, when present, remains first.

- order_model:

  Optional character string naming the supplied model whose level order,
  reference levels and estimates are used for sorting. This is mainly
  relevant when several models are compared. If `NULL`, the first
  supplied model is used. Both `"frequency"` and `"est_frequency"` are
  accepted for a model object named `frequency`. If that model does not
  contain a particular risk factor, the first supplied model containing
  the factor provides its order and reference level.

- round_exposure:

  Non-negative number of digits used to round exposure.

- exposure_name:

  Deprecated. Use `exposure_output` instead.

- signif_stars:

  Deprecated. Use `significance` instead.

## Value

A data frame with classes `"rating_table"`, legacy `"riskfactor"` and
`"data.frame"`. It can be inspected and manipulated directly with
ordinary data-frame operations. For backward compatibility, `x$df`
returns the same table without the package-specific class and metadata.
The table contains:

- risk_factor:

  Model term or risk-factor name.

- level:

  Factor level or term representation.

- Estimate column:

  Coefficient or exponentiated relativity for each supplied model. Its
  default `est_*` name is derived from the model expression and can be
  replaced with `estimate_name`.

- `signif_*`:

  Optional significance indicator for each model.

- Exposure column:

  Optional aggregated exposure, retaining the requested output name.

## Details

### Coefficients and relativities

The table contains one row per model term level. For factor variables,
the reference level is added explicitly with relativity `1` when
`exponentiate = TRUE`, or coefficient `0` when `exponentiate = FALSE`.
Numeric model terms are retained on the scale supplied by the fitted
model structure.

By default, estimate columns are named from the supplied model
expressions, for example `est_frequency` for an object named
`frequency`. `estimate_name` can replace these with exact user-supplied
names. With several models, use an unnamed vector in model order or a
named vector such as
`c(frequency = "freq_relativity", severity = "sev_relativity")`. Effects
are joined by risk factor and level.

### Actuarial interpretation

With a log-link GLM, exponentiated coefficients represent conditional
multiplicative effects relative to the model reference level. They
should be interpreted together with the model specification and should
not be confused with the unadjusted observed measures returned by
[`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md).

Exposure by level provides context for the amount of portfolio
information supporting each fitted effect. Significance indicators
describe evidence conditional on the fitted model; they do not measure
practical materiality, temporal stability or suitability for direct
tariff implementation.

Comparing multiple models is useful for assessing changes between
unrestricted and refined specifications, or between alternative model
formulations. Comparable response definitions and coefficient scales
remain the responsibility of the analyst.

### Row order and reference levels

By default, risk factors follow the model formula. Numeric levels and
intervals are shown from low to high, explicitly ordered factors retain
their factor-level sequence, and remaining nominal factors are shown
from highest to lowest fitted effect. This separates structural order
from an ordering used to compare tariff differentiation.

`reference_first` applies only when a nominal factor uses model or
alphabetical order. It does not move the reference level ahead of a
numeric, ordinal or estimate-based sequence. The reference remains
recorded in the rating-table metadata, including a reference selected
with
[`add_rebasing()`](https://mharinga.github.io/insurancerating/reference/add_rebasing.md).

Alternative level ordering is useful for specific review tasks.
Alphabetical order supports lookup and export, while model order can
retain a deliberately specified factor sequence. Use
`level_order_by_risk_factor` when nominal and ordinal factors require
different treatment in the same table. With several models,
`order_model` defines which fitted specification provides estimate-based
ordering.
[`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)
and
[`autoplot.rating_table()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_table.md)
retain the row order established here.

Only a factor stored with `ordered = TRUE` is identified automatically
as an ordinal scale. A regular factor may also have deliberately
arranged levels, but that intention cannot be distinguished reliably
from an arbitrary model order. Use
`level_order_by_risk_factor = c(variable = "model")` to preserve that
sequence explicitly.

Numeric labels and intervals receive separate treatment because
alphabetical ordering can give an incorrect tariff sequence. With the
default `numeric_level_order = "ascending"`, a risk factor is sorted
numerically only when every displayed level is either a complete number
or a valid interval with two numeric boundaries. Mixed labels such as
`"Industry 1"` remain categorical. Set
`numeric_level_order = "as_specified"` when the fitted model order or
another `level_order` should be retained deliberately.

### Significance indicators

When `significance = TRUE`, every model receives its own `signif_*`
column. For example, models named `frequency` and `severity` produce
`est_frequency`, `signif_frequency`, `est_severity` and
`signif_severity`. Keeping estimates and indicators separate preserves
the numeric type of the fitted effects for subsequent calculations,
filtering and export.

[`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)
combines each estimate with its corresponding significance indicator for
presentation and adds the significance thresholds as a source note below
the table. Reference levels generally have no separate coefficient test
and therefore have no significance indicator.

`rating_table()` accepts fitted models only. A `rating_refinement`
specification must first be fitted with
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).

## See also

[`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)
for grouped tabular presentation,
[`autoplot.rating_table()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_table.md)
for graphical comparison,
[`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
for observed portfolio experience, and
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
for fitting a refinement specification.

## Author

Martin Haringa

## Examples

``` r
df <- MTPL
df$zip <- as.factor(df$zip)

freq <- glm(
  nclaims ~ bm + zip + offset(log(exposure)),
  family = poisson(),
  data = df
)

fitted_effects <- rating_table(
  freq,
  model_data = df,
  exposure = "exposure"
)

fitted_effects
#>   risk_factor       level  est_freq exposure
#> 1 (Intercept) (Intercept) 0.1415051       NA
#> 2         zip           0 1.0000000      207
#> 3         zip           1 1.0252572    11081
#> 4         zip           2 0.9237868     7783
#> 5         zip           3 0.9756337     7588
#> 6          bm          bm 0.9978465       NA
head(fitted_effects)
#>   risk_factor       level  est_freq exposure
#> 1 (Intercept) (Intercept) 0.1415051       NA
#> 2         zip           0 1.0000000      207
#> 3         zip           1 1.0252572    11081
#> 4         zip           2 0.9237868     7783
#> 5         zip           3 0.9756337     7588
#> 6          bm          bm 0.9978465       NA

# Give the estimate column an explicit name
rating_table(
  freq,
  model_data = df,
  exposure = "exposure",
  estimate_name = "frequency_relativity"
)
#>   risk_factor       level frequency_relativity exposure
#> 1 (Intercept) (Intercept)            0.1415051       NA
#> 2         zip           0            1.0000000      207
#> 3         zip           1            1.0252572    11081
#> 4         zip           2            0.9237868     7783
#> 5         zip           3            0.9756337     7588
#> 6          bm          bm            0.9978465       NA

# For several models, names can be supplied in model order or by model name
freq_alternative <- update(freq, . ~ . - bm)
rating_table(
  freq,
  freq_alternative,
  model_data = df,
  exposure = "exposure",
  estimate_name = c(
    freq = "current_relativity",
    freq_alternative = "alternative_relativity"
  )
)
#>   risk_factor       level current_relativity alternative_relativity exposure
#> 1 (Intercept) (Intercept)          0.1415051              0.1402024       NA
#> 2         zip           0          1.0000000              1.0000000      207
#> 3         zip           1          1.0252572              1.0254064    11081
#> 4         zip           2          0.9237868              0.9238016     7783
#> 5         zip           3          0.9756337              0.9757522     7588
#> 6          bm          bm          0.9978465                     NA       NA

# The historical accessor remains available for existing code
identical(fitted_effects$df, as.data.frame(fitted_effects))
#> [1] TRUE
if (requireNamespace("gt", quietly = TRUE)) {
  as_gt(fitted_effects)
}


  

Risk factor
```
