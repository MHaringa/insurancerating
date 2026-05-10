# Add derived tariff groups to portfolio data

Adds the tariff groups derived by
[`derive_tariff_groups()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_groups.md)
as a new factor column to a portfolio data set. This is the recommended
way to attach derived tariff groups to the same portfolio rows that were
used to fit the risk factor GAM.

## Usage

``` r
add_tariff_groups(data, groups, name = NULL, overwrite = FALSE)
```

## Arguments

- data:

  A data frame to which the tariff groups should be added.

- groups:

  Object of class `"tariff_groups"`, produced by
  [`derive_tariff_groups()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_groups.md).
  Old `"tariff_classes"` objects are accepted for backward
  compatibility.

- name:

  Character string. Name of the new output column. If `NULL`, the name
  is based on the risk factor name, for example
  `"age_policyholder_group"`.

- overwrite:

  Logical. If `FALSE`, the function stops when `name` already exists in
  `data`.

## Value

A data frame with the derived tariff group column added.

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
age_groups <- risk_factor_gam(
  MTPL,
  risk_factor = "age_policyholder",
  claim_count = "nclaims",
  exposure = "exposure"
) |>
  derive_tariff_groups()

MTPL |>
  add_tariff_groups(age_groups, name = "age_policyholder_group")
} # }
```
