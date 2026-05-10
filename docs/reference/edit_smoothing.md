# Edit an existing smoothing step in a refinement workflow

Manually adjusts a smoothing step that was previously added with
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md).
This is intended for actuarial review of a smoothed tariff curve, for
example to flatten an unstable segment, align the end points of an
interval, or add extra knots where expert judgement should guide the
curve. The adjusted smoothing is applied when
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

## Usage

``` r
edit_smoothing(
  model,
  variable = NULL,
  step = NULL,
  x1,
  x2,
  overwrite_y1 = NULL,
  overwrite_y2 = NULL,
  knots_x = NULL,
  knots_y = NULL,
  allow_extrapolation = FALSE,
  extrapolation_break_size = NULL
)
```

## Arguments

- model:

  Object of class `rating_refinement`, usually created with
  [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md).
  Legacy `smooth` and `restricted` objects are still accepted for
  backwards compatibility.

- variable:

  Character string. The `model_variable` of the smoothing step to edit.
  Required when more than one smoothing step exists and `step` is not
  supplied.

- step:

  Optional numeric index of the smoothing step to edit.

- x1, x2:

  Numeric values giving the start and end of the source-variable
  interval to modify.

- overwrite_y1, overwrite_y2:

  Optional numeric values used to override the smoothed curve value at
  `x1` and `x2`.

- knots_x, knots_y:

  Optional numeric vectors of equal length. These define additional
  points that the edited smoothing curve should pass through.

- allow_extrapolation:

  Logical. Whether edits may extend beyond the observed source-variable
  range.

- extrapolation_break_size:

  Optional positive numeric scalar used to set the spacing of extra
  break points when extrapolation is allowed.

## Value

Object of class `rating_refinement`.

## Details

Use `variable` or `step` to identify the smoothing step to edit. The
interval from `x1` to `x2` defines the part of the source variable range
that should be changed. `overwrite_y1` and `overwrite_y2` can be used to
force the curve values at the interval boundaries. `knots_x` and
`knots_y` add additional points that the edited curve should follow
inside the interval.

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

refined <- prepare_refinement(model, data = portfolio) |>
  add_smoothing(
    model_variable = "age_band",
    source_variable = "driver_age",
    breaks = c(18, 30, 40, 50, 60),
    degree = 2,
    weights = "exposure"
  ) |>
  edit_smoothing(
    variable = "age_band",
    x1 = 30,
    x2 = 50,
    overwrite_y1 = 1.00,
    overwrite_y2 = 1.10
  )

refined_model <- refit(refined)
```
