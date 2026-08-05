# Present a refinement audit as a gt table

Format the risk-factor and level impact calculated by
[`audit_refinement()`](https://mharinga.github.io/insurancerating/reference/audit_refinement.md)
for a technical note or actuarial review.

## Usage

``` r
# S3 method for class 'refinement_audit'
as_gt(
  x,
  locale = "nl-NL",
  value_decimals = 2,
  ratio_decimals = 1,
  title = "Refinement impact",
  subtitle = NULL,
  ...
)
```

## Arguments

- x:

  A `refinement_audit` object.

- locale:

  Character string used for number formatting.

- value_decimals:

  Non-negative whole number for fitted values and absolute changes.

- ratio_decimals:

  Non-negative whole number for percentage changes.

- title:

  Optional table title.

- subtitle:

  Optional table subtitle. If `NULL`, package version and audit date are
  used.

- ...:

  Currently unused.

## Value

A `gt_tbl` object.

## See also

[`audit_refinement()`](https://mharinga.github.io/insurancerating/reference/audit_refinement.md)
