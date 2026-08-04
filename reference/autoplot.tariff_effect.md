# Inspect smooth risk-factor effects and tariff-segment boundaries

Plot the smooth effect estimated by
[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
or inspect that same effect together with candidate boundaries returned
by
[`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md).
Both methods use the same curve, confidence interval, observation and
axis layers.

## Usage

``` r
# S3 method for class 'tariff_segments'
autoplot(
  object,
  confidence = FALSE,
  color_gam = "steelblue",
  show_observations = FALSE,
  color_splits = "grey50",
  size_points = 1,
  color_points = "black",
  rotate_labels = FALSE,
  remove_outliers = NULL,
  conf_int = NULL,
  x_stepsize = NULL,
  show_segments = TRUE,
  ...
)

# S3 method for class 'riskfactor_gam'
autoplot(
  object,
  confidence = FALSE,
  color_gam = "steelblue",
  show_observations = FALSE,
  x_stepsize = NULL,
  size_points = 1,
  color_points = "black",
  rotate_labels = FALSE,
  remove_outliers = NULL,
  conf_int = NULL,
  ...
)
```

## Arguments

- object:

  An object returned by
  [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
  or
  [`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md).

- confidence:

  Logical. If `TRUE`, add pointwise 95 percent confidence intervals
  where finite values are available.

- color_gam:

  Colour for the fitted GAM line.

- show_observations:

  Logical. If `TRUE`, add the aggregated observed experience used for
  fitting.

- color_splits:

  Colour for segment boundaries. Used only for a `tariff_segments`
  object.

- size_points:

  Numeric point size for observed experience.

- color_points:

  Colour for observed experience.

- rotate_labels:

  Logical. If `TRUE`, rotate x-axis labels by 45 degrees.

- remove_outliers:

  Optional single numeric upper display limit for observed points. The
  fitted curve remains unchanged.

- conf_int:

  Deprecated. Use `confidence` instead.

- x_stepsize:

  Optional positive numeric step size for x-axis tick marks. If `NULL`,
  breaks are determined automatically.

- show_segments:

  Logical. For a `tariff_segments` object, show the candidate segment
  boundaries when `TRUE`. Default is `TRUE`.

- ...:

  Additional arguments reserved for method compatibility.

## Value

A `ggplot2` object.

## Details

The fitted line is shown on its natural response scale: claim frequency,
average severity or risk premium. Optional observed points represent
portfolio experience aggregated at the continuous risk-factor values
used for fitting.

For a `tariff_segments` object, vertical lines show the derived interval
boundaries. These lines support actuarial review of where the continuous
effect changes sufficiently to motivate a categorical tariff treatment.
Set `show_segments = FALSE` to inspect only the underlying smooth curve.

Confidence intervals describe uncertainty in the fitted curve
conditional on the selected GAM specification. They do not include
uncertainty from model selection, omitted risk factors or future
portfolio changes. Segment boundaries do not by themselves demonstrate
that adjacent segments are statistically or commercially distinct.
Exposure, claim volume, temporal stability and operational tariff
constraints should be considered separately.

`remove_outliers` affects displayed observed points only. It does not
remove observations from the fitted GAM or alter the prediction curve or
segment boundaries.

## See also

[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md),
[`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md),
[`add_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/add_tariff_segments.md)

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- risk_factor_gam(
  MTPL,
  risk_factor = "age_policyholder",
  claim_count = "nclaims",
  exposure = "exposure"
)

# Inspect the continuous effect before deriving tariff segments.
autoplot(fit, confidence = TRUE, show_observations = TRUE)

segments <- derive_tariff_segments(
  fit,
  segmentation_penalty = 10,
  seed = 1
)

# Inspect the same effect with the candidate segment boundaries.
autoplot(segments, confidence = TRUE, show_observations = TRUE)
autoplot(segments, show_segments = FALSE)
} # }
```
