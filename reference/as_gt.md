# Convert an object to a gt table

Generic presentation helper. Methods return a `gt` table for objects
where a formatted reporting table is more useful than another plot.

Create a formatted `gt` table from an object returned by
[`assess_excess_threshold()`](https://mharinga.github.io/insurancerating/reference/assess_excess_threshold.md).
The original object remains a regular `data.frame` subclass; `as_gt()`
is only used when a presentation table is needed for a report, tariff
note or pricing review.

Create a formatted `gt` table from an object returned by
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md).
Risk factors are presented as row groups, while fitted model effects are
shown as relativities or coefficients depending on the scale selected in
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md).

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

# S3 method for class 'rating_table'
as_gt(
  x,
  significance = NULL,
  locale = "nl-NL",
  estimate_decimals = 3,
  exposure_decimals = 0,
  title = NULL,
  subtitle = NULL,
  ...
)
```

## Arguments

- x:

  A supported object to convert, such as a `threshold_assessment`
  returned by
  [`assess_excess_threshold()`](https://mharinga.github.io/insurancerating/reference/assess_excess_threshold.md)
  or a `rating_table` returned by
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md).

- ...:

  Arguments passed to methods.

- claims:

  Logical. If `TRUE`, include claim-count columns.

- loss:

  Logical. If `TRUE`, include loss amount columns. The default is
  `FALSE` to keep the threshold comparison compact.

- premium:

  Logical. If `TRUE`, include pure-premium and premium-reduction
  columns.

- locale:

  Character. Locale used to format model effects and exposure, for
  example `"nl-NL"` or `"en-US"`.

- loss_decimals, premium_decimals, ratio_decimals:

  Non-negative whole numbers controlling displayed decimals for loss
  amounts, premium amounts and percentage ratios.

- color_last_column:

  Logical. If `TRUE`, color the final displayed column from white to
  yellow so the highest values stand out in the presentation table.

- title:

  Optional character. Table title. If `NULL`, no title is added.

- subtitle:

  Optional character. Table subtitle. If `NULL`, no subtitle is added.

- significance:

  Optional logical. If `NULL`, use the significance setting stored on
  `x`. If `TRUE`, append the stored significance stars to the model
  effects and add the significance-level note. If `FALSE`, show fitted
  effects without stars.

- estimate_decimals:

  Non-negative whole number. Number of decimals shown for fitted
  coefficients or relativities.

- exposure_decimals:

  Non-negative whole number. Number of decimals shown for the exposure
  column, when available.

## Value

A `gt_tbl` object for supported methods.

## Details

The first column of a `rating_table` identifies the model risk factor.
`as_gt()` uses this column as `groupname_col` and sets
`row_group_as_column = TRUE`. Levels belonging to the same risk factor
are therefore kept together in a compact format suitable for a tariff
note, model review or technical appendix.

With `significance = TRUE`, significance stars are appended to the
fitted effects and the significance levels are shown below the table.
This requires an object originally created with
`rating_table(significance = TRUE)`, because p-value information is
deliberately not retained when significance is disabled during table
construction. Significance stars are a statistical diagnostic and should
be interpreted together with exposure, effect size, model stability and
actuarial relevance.

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
