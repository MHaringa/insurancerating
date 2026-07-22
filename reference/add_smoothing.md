# Add smoothing to a refinement workflow

Replaces a grouped or binned model effect by a smoother tariff curve in
a refinement workflow. This is commonly used for numeric rating factors
such as age, vehicle age, insured value or bonus-malus years, where a
raw GLM factor can be too jagged for a stable and explainable tariff.

## Usage

``` r
add_smoothing(
  model,
  model_variable = NULL,
  source_variable = NULL,
  degree = NULL,
  breaks = NULL,
  smoothing = "spline",
  k = NULL,
  weights = NULL,
  tariff_class = NULL,
  rating_variable = NULL,
  x_cut = NULL,
  x_org = NULL
)
```

## Arguments

- model:

  Object of class `rating_refinement`, usually created with
  [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md).

- model_variable:

  Character string. Existing grouped or binned variable in the GLM. This
  is the model term that will be replaced by a smoothed tariff factor.
  The column must not contain missing values; remove or impute missing
  values before adding the smoothing step.

- source_variable:

  Character string. Original numeric portfolio variable underlying
  `model_variable`.

- degree:

  Optional single whole number. Polynomial degree, used by polynomial
  smoothing methods.

- breaks:

  Numeric vector with the tariff segment boundaries to use after
  smoothing. Values must be finite and strictly increasing.

- smoothing:

  Character string with the smoothing method, for example `"spline"`
  when supported by the fitted workflow.

- k:

  Optional single positive whole number. Number of basis functions for
  smoothing methods that use a basis dimension.

- weights:

  Optional character string. Weights column, usually exposure.

- tariff_class, rating_variable:

  Deprecated. Use `model_variable` and `source_variable` instead.

- x_cut, x_org:

  Deprecated. Use `model_variable` and `source_variable` instead.

## Value

Object of class `rating_refinement`.

## Details

`add_smoothing()` stores a smoothing step on a `rating_refinement`
object. The original GLM contains `model_variable`, usually a factor or
grouped tariff segment. The smoother is fitted against
`source_variable`, the original numeric portfolio variable behind those
groups. The smoothed result is then converted back to tariff segments
using `breaks` and applied when
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

This makes the intended API explicit: first prepare the model with
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
then add a smoothing step, optionally adjust it with
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md),
and finally call
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).
The deprecated
[`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)
wrapper is only kept for backwards compatibility.

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

age_policyholder_frequency <- risk_factor_gam(
  data = MTPL,
  claim_count = "nclaims",
  risk_factor = "age_policyholder",
  exposure = "exposure"
)

age_segments_freq <- derive_tariff_segments(age_policyholder_frequency)

dat <- MTPL |>
  add_tariff_segments(age_segments_freq, name = "age_policyholder_freq_cat") |>
  mutate(across(where(is.character), as.factor)) |>
  mutate(across(where(is.factor), ~ set_reference_level(., exposure)))

freq <- glm(
  nclaims ~ bm + age_policyholder_freq_cat,
  offset = log(exposure),
  family = poisson(),
  data = dat
)

sev <- glm(
  amount ~ zip,
  weights = nclaims,
  family = Gamma(link = "log"),
  data = dat |> filter(amount > 0)
)

premium_df <- dat |>
  add_prediction(freq, sev) |>
  mutate(premium = pred_nclaims_freq * pred_amount_sev)

burn_unrestricted <- glm(
  premium ~ zip + bm + age_policyholder_freq_cat,
  weights = exposure,
  family = Gamma(link = "log"),
  data = premium_df
)

ref <- prepare_refinement(burn_unrestricted) |>
  add_smoothing(
    model_variable = "age_policyholder_freq_cat",
    source_variable = "age_policyholder",
    breaks = seq(18, 95, 5),
    weights = "exposure"
  )
} # }
```
