# Edit a smoothing curve in a refinement workflow

Modify a specified interval of a smoothing curve previously added with
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md).
Use a relative adjustment when the existing shape is broadly
appropriate, or explicit values and control points when the curve should
follow known targets.

## Usage

``` r
edit_smoothing(
  model,
  model_variable = NULL,
  step = NULL,
  from = NULL,
  to = NULL,
  from_value = NULL,
  to_value = NULL,
  control_positions = NULL,
  control_values = NULL,
  adjustment = NULL,
  transition = NULL,
  scale = NULL,
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

  Optional numeric index of the original smoothing step or one of its
  later edit steps. In both cases, the new edit is linked to the same
  original smoothing and appended after the existing workflow steps.

- from, to:

  Optional numeric values giving the start and end of the
  source-variable interval to modify. For `adjustment`, either value may
  be omitted to use the beginning or end of the available smoothing
  range. Explicit target-value and control-point edits require both
  values.

- from_value, to_value:

  Optional numeric values used to override the smoothed curve value at
  `from` and `to`.

- control_positions, control_values:

  Optional numeric vectors of equal length. These define additional
  points that the edited smoothing curve should pass through.

- adjustment:

  Optional positive numeric scalar applied multiplicatively to the
  current smoothing within the selected interval. `1.05` requests an
  increase of up to 5 percent and `0.95` a decrease of up to 5 percent.
  With two boundaries, the default transition anchors the multiplier at
  1 at `from` and `to`; a one-sided edit is anchored only at the
  supplied boundary.

- transition:

  Optional character string controlling how `adjustment` connects to the
  unchanged smoothing. `NULL` inherits the original smoothing
  specification. `"linear"` gives continuous linear transitions and
  `"step"` permits immediate jumps. Smoothing methods accepted by
  [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  can be supplied as explicit structural overrides.

- scale:

  Optional smoothing scale. `NULL` inherits the scale recorded by
  [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md).
  Supplying the same value is allowed for clarity. Changing scale during
  an edit is deliberately rejected because that would reinterpret the
  previously fitted curve; rebuild the smoothing with
  [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  instead.

- allow_extrapolation:

  Logical. Whether edits may extend beyond the observed source-variable
  range.

- extrapolation_step:

  Optional positive numeric scalar used to set the spacing of extra
  break points when extrapolation is allowed.

## Value

A `rating_refinement` object with a separate smoothing-edit step
appended to the ordered specification. The pricing GLM is not fitted
again until
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

## Details

`edit_smoothing()` appends a separate, ordered edit step to a
`rating_refinement` object. It does not alter the fitted GLM
immediately. Repeated calls are cumulative: every new edit starts from
the smoothing produced by preceding edits to the same
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
step. The selected cumulative curve is applied when
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

Use `model_variable` or `step` to identify the smoothing to edit. `step`
may identify either its original
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
step or a later edit belonging to that smoothing. The interval from
`from` to `to` defines the part of the source-variable range that should
be changed. With `adjustment`, either boundary may be omitted. Supplying
only `from` edits the curve from that value to the end of the smoothing
range; supplying only `to` edits it from the beginning of the range to
that value. `adjustment` multiplies the current smoothing within the
selected range. For example, `adjustment = 1.05` requests an increase of
up to 5 percent relative to the existing smoothing.

With two boundaries, the multiplier is anchored at 1 at `from` and `to`
and reaches the requested adjustment near the middle. With only `from`,
it is anchored at 1 at `from` and moves towards the requested adjustment
at the end of the range. With only `to`, it starts at the requested
adjustment and reconnects to 1 at `to`. These one-sided forms are useful
for refining a lower or upper tail without introducing a jump at the
supplied boundary.

By default, `transition = NULL` inherits the smoothing specification
from the
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
step. The entry and exit are adapted to their opposite directions and
join the unchanged curve continuously. `"linear"` uses continuous
straight transitions. `"step"` applies the multiplier immediately at
both boundaries and therefore permits deliberate jumps. Explicit
shape-constrained transition names accepted by
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
can also be supplied. When a constrained transition is inherited or
selected, the edited curve is checked for the corresponding monotonicity
and curvature.

`from_value` and `to_value` instead prescribe curve values at the
interval boundaries. `control_positions` and `control_values` add points
that the edited curve should follow inside the interval. Relative
adjustments and explicit target values cannot be combined in one
`edit_smoothing()` call because they represent different actuarial
instructions. They may be used in separate consecutive edits, which are
then evaluated in their stored order.

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

The smoothing scale is part of that specification. `scale = NULL`
inherits the value recorded by
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
including `"log_relativity"`. Accepted edits and their transitions are
assessed on that inherited scale. A scale change requires rebuilding the
smoothing because changing it during an edit would reinterpret the curve
that forms the basis of the edit.

## See also

[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
[`add_shrinkage()`](https://mharinga.github.io/insurancerating/reference/add_shrinkage.md),
[`add_rebasing()`](https://mharinga.github.io/insurancerating/reference/add_rebasing.md),
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
explicit_refinement <- refinement |>
  edit_smoothing(
    model_variable = "age_band",
    from = 30,
    to = 50,
    from_value = 1.00,
    to_value = 1.10,
    control_positions = c(40),
    control_values = c(1.05)
  )

explicit_model <- refit(explicit_refinement)

# Keep the current shape as the basis and raise the middle of this interval
# by up to 5 percent. The inherited transition remains continuous.
adjusted_refinement <- refinement |>
  edit_smoothing(
    model_variable = "age_band",
    from = 30,
    to = 50,
    adjustment = 1.05
  )

adjusted_model <- refit(adjusted_refinement)

# A one-sided adjustment applies from age 40 to the end of the range.
upper_tail_refinement <- refinement |>
  edit_smoothing(
    model_variable = "age_band",
    from = 40,
    adjustment = 1.05,
    transition = "linear"
  )
```
