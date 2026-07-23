# Smooth grouped tariff relativities

Replace the estimated relativities of a grouped numeric model variable
by a smooth tariff curve. In actuarial pricing this can be useful for
ordered risk factors such as age, vehicle age, insured value or
bonus-malus years, where independently estimated factor levels may show
sampling variation that is not considered suitable for the final tariff
structure.

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
  `model_variable`. Its name is also used for the resulting smoothed
  tariff variable.

- degree:

  Optional single whole number. Polynomial degree, used by
  `smoothing = "spline"`. The degree must be feasible for the number of
  unique grouped model points.

- breaks:

  Numeric vector with the tariff segment boundaries to use after
  smoothing. These boundaries determine the final tariff segmentation,
  not the number of portfolio observations used to estimate the curve.
  Values must be finite and strictly increasing.

- smoothing:

  Character string selecting the smoothing method. Available values are
  `"spline"`, `"gam"`, `"mpi"`, `"mpd"`, `"cx"`, `"cv"`, `"micx"`,
  `"micv"`, `"mdcx"` and `"mdcv"`. See Details for the shape
  restrictions represented by these codes.

- k:

  Optional single positive whole number. Basis dimension for smoothing
  methods other than `"spline"`. It sets the maximum flexibility
  available to the smooth and is not necessarily equal to its estimated
  effective degrees of freedom. `NULL` uses an effective default of 10.
  The basis dimension cannot exceed the number of unique grouped
  covariate values available for fitting.

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
`rating_refinement` object. It does not immediately refit the pricing
GLM. The original GLM contains `model_variable`, usually a factor
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
smoothed tariff variable when
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called. The usual sequence is therefore
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
`add_smoothing()`, optionally
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md),
and finally
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).

### Smoothing methods

The available methods represent different assumptions about the shape of
the tariff effect:

- `"spline"`:

  Fits an unconstrained global polynomial through the grouped GLM
  relativities. `degree` determines its order. A low degree is generally
  easier to interpret and less sensitive near the boundaries; higher
  degrees can follow more local variation but may introduce oscillation.

- `"gam"`:

  Fits an unconstrained penalized smooth with
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html). This allows
  the tariff curve to adapt to the observed pattern while the smoothing
  penalty limits unnecessary variation.

- `"mpi"` and `"mpd"`:

  Fit monotone increasing and monotone decreasing P-splines,
  respectively.

- `"cx"` and `"cv"`:

  Fit convex and concave P-splines, respectively.

- `"micx"` and `"micv"`:

  Fit monotone increasing curves that are, respectively, convex and
  concave.

- `"mdcx"` and `"mdcv"`:

  Fit monotone decreasing curves that are, respectively, convex and
  concave.

The shape-constrained methods are fitted with
[`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html). A constraint
should reflect an actuarial or pricing assumption that is defensible for
the risk factor; it should not be selected solely because it produces a
smoother visual result.

### Basis dimension and polynomial degree

For `"gam"` and the shape-constrained methods, `k` specifies the basis
dimension. It controls the maximum flexibility available to the smooth,
but it is not the final effective degrees of freedom of the fitted
curve. For a penalized GAM, the estimated smoothing penalty can reduce
the effective degrees of freedom below this maximum.

A smaller `k` restricts the curve to broad movements. A larger `k`
permits more local variation, but requires enough distinct grouped
covariate values and may be unstable when only a few tariff levels are
available. If `k` is `NULL`, an effective default basis dimension of 10
is used. The function checks this dimension against the grouped values
before fitting and reports the observed number of unique values when the
requested complexity is not feasible.

For `"spline"`, `degree` has the corresponding complexity role. A
polynomial of degree \\d\\ requires at least \\d + 1\\ unique grouped
values. When `degree` is omitted, the existing behaviour uses the
highest degree supported by the grouped model points.

The deprecated
[`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)
wrapper remains available for backwards compatibility.

## See also

[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md),
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
    smoothing = "gam",
    k = 6,
    weights = "exposure"
  )

# Limit the visible range without changing the fitted smoothing curve.
autoplot(ref, x_max = 80)
} # }
```
