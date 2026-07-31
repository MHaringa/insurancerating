# Edit a smoothing curve in a refinement workflow

Modify a specified interval of a smoothing curve previously added with
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md).
The function can fix boundary values and introduce internal control
points, for example when actuarial review supports a flatter local
effect or a documented transition between tariff segments.

## Usage

``` r
edit_smoothing(
  model,
  model_variable = NULL,
  step = NULL,
  from,
  to,
  from_value = NULL,
  to_value = NULL,
  control_positions = NULL,
  control_values = NULL,
  allow_extrapolation = FALSE,
  extrapolation_step = NULL
)
```

## Arguments

- model:

  Object of class `rating_refinement`, created with
  [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md)
  and containing an existing smoothing step. Ordinary and refitted GLMs
  are not accepted directly. Legacy `smooth` and `restricted` objects
  are still accepted for backwards compatibility.

- model_variable:

  Character string. The `model_variable` of the smoothing step to edit.
  Required when more than one smoothing step exists and `step` is not
  supplied.

- step:

  Optional numeric index of the smoothing step to edit.

- from, to:

  Numeric values giving the start and end of the source-variable
  interval to modify.

- from_value, to_value:

  Optional numeric values used to override the smoothed curve value at
  `from` and `to`.

- control_positions, control_values:

  Optional numeric vectors of equal length. These define additional
  points that the edited smoothing curve should pass through.

- allow_extrapolation:

  Logical. Whether edits may extend beyond the observed source-variable
  range.

- extrapolation_step:

  Optional positive numeric scalar used to set the spacing of extra
  break points when extrapolation is allowed.

## Value

A `rating_refinement` object containing the edited smoothing
specification. The pricing GLM is not fitted again until
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

## Details

`edit_smoothing()` stores an edit on the selected smoothing step of a
`rating_refinement` object. It does not alter the fitted GLM
immediately. The edited curve is evaluated in the recorded step order
and applied when
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

Use `model_variable` or `step` to identify the smoothing step to edit.
The interval from `from` to `to` defines the part of the source variable
range that should be changed. `from_value` and `to_value` can be used to
force the curve values at the interval boundaries. `control_positions`
and `control_values` add additional points that the edited curve should
follow inside the interval.

### Actuarial interpretation

The edited interval is an explicit tariff assumption layered on the
statistically fitted smoothing curve. It should be supported by an
actuarial rationale and reviewed against exposure, observed experience
and the continuity of adjacent segments. The edit does not add
information to sparse parts of the portfolio and should not be
interpreted as a new model estimate.

Keep the `rating_refinement` object, call
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
to assess the current specification, edit that same refinement object,
and call
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
again. The previously fitted GLM remains unchanged. This retains the
order and content of manual adjustments as part of the reproducible
refinement specification.

## See also

[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md),
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)

## Author

Martin Haringa

## Examples

``` r
set.seed(42)
driver_age <- rep(seq(20, 59), each = 4)
exposure <- rep(1, length(driver_age))
age_band <- cut(
  driver_age,
  breaks = c(18, 30, 40, 50, 60),
  include.lowest = TRUE
)
expected_claims <- exp(
  -1.7 + 0.018 * (driver_age - 20) + 0.0006 * (driver_age - 40)^2
)
portfolio <- data.frame(
  claims = rpois(length(driver_age), exposure * expected_claims),
  exposure = exposure,
  driver_age = driver_age,
  age_band = age_band
)

model <- glm(
  claims ~ age_band + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)

refinement <- prepare_refinement(model, data = portfolio) |>
  add_smoothing(
    model_variable = "age_band",
    source_variable = "driver_age",
    breaks = c(18, 30, 40, 50, 60),
    weights = "exposure"
  )

# Fit and inspect the initial smoothing specification.
initial_model <- refit(refinement)

# Edit the retained specification and fit it again.
refinement <- refinement |>
  edit_smoothing(
    model_variable = "age_band",
    from = 30,
    to = 50,
    from_value = 1.00,
    to_value = 1.10,
    control_positions = c(40),
    control_values = c(1.05)
  )

refined_model <- refit(refinement)
```
