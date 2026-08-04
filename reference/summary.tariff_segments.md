# Summarise candidate tariff segments

Return the portfolio diagnostics stored when
[`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md)
created the candidate segmentation. The summary can be used to assess
whether the proposed intervals contain sufficient exposure and claim
information before they are used in a GLM or tariff structure.

## Usage

``` r
# S3 method for class 'tariff_segments'
summary(object, ...)
```

## Arguments

- object:

  A `"tariff_segments"` object returned by
  [`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md).

- ...:

  Additional arguments reserved for method compatibility.

## Value

A data frame with one row per candidate segment and the columns:

- segment:

  Candidate tariff interval.

- portfolio_records:

  Number of portfolio rows assigned to the interval.

- risk_factor_values:

  Number of distinct observed risk-factor values represented by the
  interval.

- exposure:

  Total exposure represented in a frequency or risk-premium GAM.

- claim_count:

  Total observed claim count for a frequency or severity GAM.

- frequency:

  Observed claim frequency, calculated as `claim_count / exposure`, for
  a frequency GAM.

- claim_amount:

  Total observed claim amount for a severity GAM.

- average_severity:

  Observed average severity, calculated as `claim_amount / claim_count`,
  for a severity GAM.

- risk_premium_amount:

  Total exposure-weighted risk-premium amount for a risk-premium GAM.

- risk_premium:

  Observed risk premium, calculated as `risk_premium_amount / exposure`,
  for a risk-premium GAM.

The response columns are model dependent. The returned table therefore
contains the numerator, denominator and observed y-axis measure relevant
to the model used by
[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md).

## See also

[`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md),
[`add_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/add_tariff_segments.md)

## Author

Martin Haringa
