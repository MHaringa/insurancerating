# Add derived tariff segments to portfolio data

Adds the tariff segments derived by
[`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md)
as a new factor column to a portfolio data set. This is the recommended
way to attach derived tariff segments to the same portfolio rows that
were used to fit the risk factor GAM.

The helper preserves row alignment between the portfolio and the
assigned segments. It does not re-estimate the GAM or reconstruct the
segment boundaries.

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

`segments$assigned_segments` contains one segment for each observation
used to estimate the risk-factor effect. `data` must therefore contain
the same number of rows in the same order. The resulting factor can be
used in a GLM or retained as a candidate grouping for further actuarial
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
