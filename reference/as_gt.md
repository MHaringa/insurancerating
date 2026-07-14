# Convert an object to a gt table

Generic presentation helper. Methods return a `gt` table for objects
where a formatted reporting table is more useful than another plot.

Create a formatted `gt` table from an object returned by
[`assess_excess_threshold()`](https://mharinga.github.io/insurancerating/reference/assess_excess_threshold.md).
The original object remains a regular `data.frame` subclass; `as_gt()`
is only used when a presentation table is needed for a report, tariff
note or pricing review.

## Usage

``` r
as_gt(x, ...)

# S3 method for class 'threshold_assessment'
as_gt(
  x,
  claims = TRUE,
  loss = FALSE,
  premium = TRUE,
  locale = "nl-NL",
  loss_decimals = 0,
  premium_decimals = 0,
  ratio_decimals = 1,
  color_last_column = TRUE,
  title = NULL,
  subtitle = NULL,
  ...
)
```

## Arguments

- x:

  An object returned by
  [`assess_excess_threshold()`](https://mharinga.github.io/insurancerating/reference/assess_excess_threshold.md).

- ...:

  Unused.

- claims:

  Logical. If `TRUE`, include claim-count columns.

- loss:

  Logical. If `TRUE`, include loss amount columns. The default is
  `FALSE` to keep the threshold comparison compact.

- premium:

  Logical. If `TRUE`, include pure-premium and premium-reduction
  columns.

- locale:

  Character. Locale used for number formatting, for example `"nl-NL"` or
  `"en-US"`.

- loss_decimals, premium_decimals, ratio_decimals:

  Non-negative whole numbers controlling displayed decimals for loss
  amounts, premium amounts and percentage ratios.

- color_last_column:

  Logical. If `TRUE`, color the final displayed column from white to
  yellow so the highest values stand out in the presentation table.

- title:

  Optional character. Table title. If `NULL`, no table title is added.

- subtitle:

  Optional character. Table subtitle. If `NULL`, no table subtitle is
  added.

## Value

A `gt_tbl` object for supported methods.

A `gt_tbl` object.

## Author

Martin Haringa

## Examples

``` r
portfolio <- data.frame(
  policy_id = 1:10,
  sector = rep(c("Industry", "Retail"), each = 5),
  claim_count = c(
    0, 1, 1, 1, 1,
    0, 1, 1, 1, 1
  ),
  claim_amount = c(
    0, 25000, 120000, 50000, 175000,
    0, 40000, 90000, 150000, 300000
  ),
  policy_years = rep(1, 10)
)

thresholds <- assess_excess_threshold(
  data = portfolio,
  claim_amount = "claim_amount",
  thresholds = c(25000, 50000, 100000, 150000),
  exposure = "policy_years",
  group = "sector",
  claim_count = "claim_count"
)

if (requireNamespace("gt", quietly = TRUE)) {
  as_gt(thresholds)
}


  

```
