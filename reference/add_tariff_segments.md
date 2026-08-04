# Add derived tariff segments to portfolio data

Adds the tariff segments derived by
[`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md)
as a new factor column to a portfolio data set. The stored boundaries
are applied to the original continuous risk-factor column, so the result
does not depend on the row order used when the GAM was fitted.

The helper does not re-estimate the GAM or derive new boundaries. It can
be used after filtering or reordering the original portfolio and on new
data whose risk-factor values remain within the range used to derive the
segmentation.

## Usage

``` r
add_tariff_segments(data, segments, name = NULL, overwrite = FALSE)
```

## Arguments

- data:

  A data frame to which the tariff segments should be added.

- segments:

  Object of class `"tariff_segments"`, produced by
  [`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md).
  Old `"tariff_classes"` objects are accepted for backward
  compatibility.

- name:

  Character string. Name of the new output column. If `NULL`, the name
  is based on the risk factor name, for example
  `"age_policyholder_segment"`.

- overwrite:

  Logical. If `FALSE`, the function stops when `name` already exists in
  `data`.

## Value

A data frame with the derived tariff segment column added.

## Details

The risk-factor name and optional rounding increment are taken from
`segments`. The risk-factor column in `data` must be numeric and contain
only finite, non-missing values. Values outside the original
segmentation range produce an error because their tariff treatment has
not been supported by the fitted GAM. The resulting factor can be used
in a GLM or retained as a candidate grouping for further actuarial
review.

## See also

[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md),
[`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md)

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
age_segments <- risk_factor_gam(
  MTPL,
  risk_factor = "age_policyholder",
  claim_count = "nclaims",
  exposure = "exposure"
) |>
  derive_tariff_segments()

MTPL |>
  add_tariff_segments(age_segments, name = "age_policyholder_segment")
} # }
```
