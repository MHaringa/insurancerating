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
  tariff variable.

- degree:

  Optional single whole number. Polynomial degree, used by
  `smoothing = "poly"`. The degree must be feasible for the number of
  unique grouped model points.

- breaks:

  Numeric vector with the tariff segment boundaries to use after
  smoothing. These boundaries determine the final tariff segmentation,
  not the number of portfolio observations used to estimate the curve.
  Values must be finite and strictly increasing.

- smoothing:

  Character string selecting the smoothing method. Available values are
  `"spline"` (default), `"poly"`, `"mpi"`, `"mpd"`, `"gam"`, `"cx"`,
  `"cv"`, `"micx"`, `"micv"`, `"mdcx"` and `"mdcv"`. See Details for the
  statistical interpretation and shape restrictions.

- k:

  Optional single positive whole number. Basis dimension for smoothing
  methods `"spline"`, `"gam"`, `"mpi"`, `"mpd"`, `"cx"`, `"cv"`,
  `"micx"`, `"micv"`, `"mdcx"` and `"mdcv"`. It sets the maximum
  flexibility available to the smooth and is not necessarily equal to
  its estimated effective degrees of freedom. `NULL` uses the smaller of
  10 and the number of unique grouped model points. At least three
  unique grouped values are required. The basis dimension cannot exceed
  the number of unique grouped covariate values available for fitting.

- weights:

  Optional character string. Numeric volume column, usually exposure,
  used to weight the grouped GLM relativities during smoothing.

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

- `"mpi"` and `"mpd"`:

  Fit monotone increasing and monotone decreasing smooths. These are
  often useful when actuarial reasoning implies that the tariff effect
  should move in only one direction.

- `"cx"` and `"cv"`:

  Fit convex and concave smooths, respectively.

- `"micx"` and `"micv"`:

  Fit monotone increasing curves that are, respectively, convex and
  concave.

- `"mdcx"` and `"mdcv"`:

  Fit monotone decreasing curves that are, respectively, convex and
  concave.

- `"gam"`:

  Fits an unconstrained thin-plate regression spline with
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html). It is mainly
  intended as a flexible reference when comparing the general spline and
  shape-constrained specifications. It does not impose the actuarial
  shape assumptions represented by the constrained methods.

The shape-constrained methods are fitted with
[`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html). A constraint
should reflect an actuarial or pricing assumption that is defensible for
the risk factor; it should not be selected solely because it produces a
smoother visual result. The combined monotonicity and curvature methods
are advanced specifications and are most appropriate when both
assumptions can be supported independently.

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
    smoothing = "spline",
    k = 6,
    weights = "exposure"
  )

# Limit the visible range without changing the fitted smoothing curve.
autoplot(ref, x_max = 80, y_max = 1.5)
} # }
```
