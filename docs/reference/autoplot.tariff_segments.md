# Inspect derived tariff-segment boundaries

Plot the fitted continuous risk-factor effect together with the
boundaries returned by
[`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md).
Observed aggregated experience and pointwise confidence intervals can be
added to support review of the proposed segmentation.

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
  ...
)
```

## Arguments

- object:

  An object of class `"tariff_segments"`, produced by
  [`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md).

- confidence:

  Logical. If `TRUE`, show pointwise 95 percent confidence intervals
  where finite values are available.

- color_gam:

  Colour of the fitted GAM line.

- show_observations:

  Logical. If `TRUE`, add the aggregated observed experience used for
  the GAM.

- color_splits:

  Colour of the vertical segment boundaries.

- size_points:

  Numeric point size for observed experience.

- color_points:

  Colour for observed experience.

- rotate_labels:

  Logical. If `TRUE`, rotate x-axis labels by 45 degrees.

- remove_outliers:

  Optional single numeric upper display limit for observed points.

- conf_int:

  Deprecated. Use `confidence` instead.

- ...:

  Additional arguments reserved for method compatibility.

## Value

A `ggplot2` object.

## Details

Vertical lines indicate the derived interval boundaries; the fitted GAM
line remains the underlying continuous effect. The plot can be used to
assess whether boundaries occur at plausible changes in the fitted
pattern and whether tail segments have visible empirical support.

The visualisation does not refit a categorical GLM and does not
establish that adjacent segments are statistically or commercially
distinct. Exposure, claim volume, temporal stability and operational
tariff constraints remain separate considerations. `remove_outliers`
affects displayed observed points only and does not alter either the GAM
or the derived boundaries.

## See also

[`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md),
[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md),
[`add_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/add_tariff_segments.md)

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
segments <- risk_factor_gam(
  MTPL,
  risk_factor = "age_policyholder",
  claim_count = "nclaims",
  exposure = "exposure"
) |>
  derive_tariff_segments()

autoplot(segments, confidence = TRUE, show_observations = TRUE)
} # }
```
