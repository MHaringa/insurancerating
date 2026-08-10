# Smooth grouped tariff relativities in a refinement workflow

Replace independently estimated relativities of an ordered, grouped
model variable with a smooth tariff curve. This can reduce sampling
variation between adjacent levels of risk factors such as age, vehicle
age, insured value or bonus-malus years while retaining the broad effect
estimated by the GLM.

## Usage

``` r
add_smoothing(
  model,
  model_variable = NULL,
  source_variable = NULL,
  breaks,
  smoothing = "spline",
  k = NULL,
  degree = NULL,
  weights = NULL,
  effect_strength = 1,
  tariff_class = NULL,
  rating_variable = NULL,
  x_cut = NULL,
  x_org = NULL
)
```

## Arguments

- model:

  Object of class `rating_refinement`, created with
  [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md).
  A fitted GLM, including a model returned by
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
  is not accepted directly; retain and modify the corresponding
  refinement specification instead.

- model_variable:

  Character string. Existing grouped or binned variable in the GLM. This
  is the model term that will be replaced by a smoothed tariff factor.
  The column must not contain missing values; remove or impute missing
  values before adding the smoothing step.

- source_variable:

  Character string. Original numeric portfolio variable underlying
  `model_variable`. Its name is also used for the resulting smoothed
  tariff variable. The column must contain only finite, non-missing
  numeric values.

- breaks:

  Numeric vector with the tariff segment boundaries to use after
  smoothing. These boundaries determine the final tariff segmentation,
  not the number of portfolio observations used to estimate the curve.
  Values must be finite, strictly increasing and cover every observed
  value of `source_variable`. Boundaries outside the interval range
  represented by `model_variable` are allowed, but produce a warning
  because the resulting relativities rely on extrapolation beyond the
  fitted GLM levels. This argument is required.

- smoothing:

  Character string selecting the smoothing method. Available values are
  `"spline"` (default), `"poly"`, `"gam"`, `"increasing"`,
  `"decreasing"`, `"convex"`, `"concave"`, `"increasing_convex"`,
  `"increasing_concave"`, `"decreasing_convex"` and
  `"decreasing_concave"`. The former short SCOP codes remain accepted as
  compatibility aliases. See Details for the statistical interpretation
  and shape restrictions.

- k:

  Optional single positive whole number. Basis dimension for smoothing
  methods `"spline"`, `"gam"`, `"increasing"`, `"decreasing"`,
  `"convex"`, `"concave"` and the combined direction-curvature methods.
  It sets the maximum flexibility available to the smooth and is not
  necessarily equal to its estimated effective degrees of freedom.
  `NULL` uses the smaller of 10 and the number of unique grouped model
  points. At least three unique grouped values are required. The basis
  dimension cannot exceed the number of unique grouped covariate values
  available for fitting.

- degree:

  Optional single whole number. Polynomial degree, used only by
  `smoothing = "poly"`. The degree must be feasible for the number of
  unique grouped model points.

- weights:

  Optional character string. Numeric volume column, usually exposure,
  used to weight the grouped GLM relativities during smoothing.

- effect_strength:

  Non-negative finite numeric scalar controlling the spread of the
  fitted smoothing effect around its weighted mean on the ordinary
  relativity scale. The default `1` leaves the smoothing unchanged.
  Values below 1 flatten the complete effect and values above 1 make it
  steeper. The weighted arithmetic mean remains unchanged by
  construction; see Details.

- tariff_class, rating_variable:

  Deprecated. Use `model_variable` and `source_variable` instead.

- x_cut, x_org:

  Deprecated. Use `model_variable` and `source_variable` instead.

## Value

An object of class `rating_refinement` containing the stored smoothing
specification. The pricing GLM is not fitted again until
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

## Details

`add_smoothing()` stores a smoothing specification on a
`rating_refinement` object. It does not alter the fitted GLM
immediately. The smoothing is evaluated in the recorded step order and
applied when
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called. The original GLM contains `model_variable`, usually a factor
created by grouping a continuous risk factor. `source_variable`
identifies the original numeric variable represented by those groups.

The smoother is estimated from the fitted GLM relativities at the
midpoint of each model interval. Consequently, the amount of information
available to the smoother is primarily determined by the number of
grouped model levels, rather than by the number of individual portfolio
records. Exposure or another volume measure can be supplied through
`weights` so that model levels with more portfolio volume have greater
influence on the fitted curve.

The fitted curve is evaluated using `breaks` and converted back to a
grouped tariff variable. The original model term is replaced by that
smoothed tariff variable during refitting.

### Effect strength

`effect_strength` adjusts the overall spread of the fitted smoothing
curve without estimating a different curve. For a smoothed relativity
`r(x)` and weighted arithmetic mean `c`, the adjustment is
`c + a * (r(x) - c)`, where `a` is `effect_strength`. It multiplies the
vertical deviations from the centre on the ordinary relativity scale; it
is not a change in skewness or kurtosis. A value of 1 retains the fitted
smooth, values between 0 and 1 flatten the effect, values above 1
strengthen it, and 0 produces a constant effect. The weighted arithmetic
mean of the smoothed tariff levels remains unchanged by construction.
The `weights` column is used to determine this centre when supplied;
otherwise, tariff levels receive equal weight.

This adjustment changes the overall degree of tariff differentiation. It
does not selectively change only the upper or lower part of the curve.
Use
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md)
with interval boundaries and control points when a local part of the
relationship requires a separate actuarial adjustment. Monotonic
ordering and convexity or concavity on the relativity scale are
retained. For example, an increasing concave curve remains increasing
and concave while its complete rise becomes steeper when
`effect_strength` is above 1. Very large values are rejected when they
would produce a zero or negative relativity.

### Actuarial interpretation

Smoothing introduces a structural assumption: adjacent values of the
source variable are expected to have related tariff effects. The
selected method, basis dimension and breaks should therefore be assessed
against exposure, observed experience, coefficient uncertainty and
stability over time. A smooth curve should not be interpreted as
evidence that the underlying risk relationship is itself known without
uncertainty.

### Smoothing methods

The available methods represent different assumptions about the shape of
the tariff effect:

- `"spline"`:

  The general-purpose default. Fits an unconstrained penalized cubic
  regression spline. It is suitable when the tariff effect should be
  smooth but no monotonicity or curvature restriction is justified.

- `"poly"`:

  Fits a global polynomial through the grouped GLM relativities.
  `degree` determines its order. A low degree gives a compact parametric
  trend; higher degrees can follow more local variation but may
  oscillate, particularly near the boundaries.

- `"increasing"` and `"decreasing"`:

  Fit monotone smooths. These methods constrain the tariff effect to
  move in one direction, without imposing how quickly its slope changes.
  They are often the most directly interpretable constrained
  specifications when actuarial reasoning supports a consistently
  increasing or decreasing risk effect.

- `"convex"` and `"concave"`:

  Constrain curvature but not direction. For a convex curve, the slope
  increases as the source variable increases; for a concave curve, the
  slope decreases. A convex curve may therefore be U-shaped and a
  concave curve may be inverted U-shaped. These are advanced choices
  when curvature itself has a defensible interpretation.

- `"increasing_convex"` and `"increasing_concave"`:

  Fit increasing curves with an additional curvature constraint. An
  increasing convex effect rises at an increasing rate, for example when
  upper-tail risk causes marginal cost to accelerate. An increasing
  concave effect rises at a decreasing rate and gradually flattens, for
  example when risk cost rises with insured value but less than
  proportionally.

- `"decreasing_convex"` and `"decreasing_concave"`:

  Fit decreasing curves with an additional curvature constraint. A
  decreasing convex effect becomes less steep and tends to flatten. A
  decreasing concave effect becomes progressively steeper.

- `"gam"`:

  Fits an unconstrained thin-plate regression spline with
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html). It is mainly
  intended as a flexible reference when comparing the general spline and
  shape-constrained specifications. It does not impose the actuarial
  shape assumptions represented by the constrained methods.

The shape-constrained methods are fitted with
[`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html). Monotonicity
concerns the direction of the effect, whereas convexity and concavity
concern how its slope changes. In most tariff applications, a
directional assumption is easier to substantiate than a curvature
assumption. A constraint should reflect an actuarial or pricing
assumption that is defensible for the risk factor; it should not be
selected solely because it produces a smoother visual result. The
combined monotonicity and curvature methods are advanced specifications
and are most appropriate when both assumptions can be supported
independently.

The former short codes `"mpi"`, `"mpd"`, `"cx"`, `"cv"`, `"micx"`,
`"micv"`, `"mdcx"` and `"mdcv"` remain accepted as compatibility
aliases. New code should use the readable method names above. Both forms
produce the same smoothing specification.

### Basis dimension and polynomial degree

For `"spline"`, `"gam"` and the shape-constrained methods, `k` specifies
the basis dimension. It controls the maximum flexibility available to
the smooth, but it is not the final effective degrees of freedom of the
fitted curve. The estimated smoothing penalty can reduce the effective
degrees of freedom below this maximum.

A smaller `k` restricts the curve to broad movements. A larger `k`
permits more local variation, but requires enough distinct grouped
covariate values and may be unstable when only a few tariff levels are
available. If `k` is `NULL`, the function uses the smaller of 10 and the
number of unique grouped model points. Spline, GAM and shape-constrained
smoothing require at least three unique grouped values. The function
checks this dimension before fitting and reports the observed number of
unique values when the requested complexity is not feasible.

For `"poly"`, `degree` has the corresponding complexity role. A
polynomial of degree \\d\\ requires at least \\d + 1\\ unique grouped
values. When `degree` is omitted, the existing behaviour uses the
highest degree supported by the grouped model points. In practice, an
explicit low degree is generally preferable when a stable global trend
is intended.

`degree` is only accepted for `smoothing = "poly"`. Conversely, `k` is
only accepted for `"spline"`, `"gam"` and the shape-constrained methods.
This separation prevents a complexity argument from being supplied but
silently ignored.

The deprecated
[`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)
wrapper remains available for backwards compatibility.

## See also

[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md),
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
[`add_shrinkage()`](https://mharinga.github.io/insurancerating/reference/add_shrinkage.md),
[`add_rebasing()`](https://mharinga.github.io/insurancerating/reference/add_rebasing.md),
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md),
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)

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

age_segments_freq <- derive_tariff_segments(
  age_policyholder_frequency,
  segmentation_penalty = 10,
  seed = 1
)

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
    breaks = c(seq(18, 93, 5), 95),
    smoothing = "spline",
    k = 6,
    weights = "exposure",
    effect_strength = 1.1
  )

# When the tariff effect must not decrease, use the readable constrained
# method name. The former value "mpi" remains accepted for compatibility.
increasing_ref <- prepare_refinement(burn_unrestricted) |>
  add_smoothing(
    model_variable = "age_policyholder_freq_cat",
    source_variable = "age_policyholder",
    breaks = c(seq(18, 93, 5), 95),
    smoothing = "increasing",
    k = 6,
    weights = "exposure"
  )

# Limit the visible range without changing the fitted smoothing curve.
autoplot(ref, x_max = 80, y_max = 1.5)
} # }
```
