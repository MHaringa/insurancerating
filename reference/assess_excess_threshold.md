# Assess possible excess-loss thresholds

Compare candidate thresholds for capped severity and large-loss pricing
work.

`assess_excess_threshold()` is a diagnostic helper. It does not choose a
threshold automatically. It shows how many claims, how many records
contain claim amounts above candidate thresholds, how much historical
claim cost sits above those thresholds, and how much risk premium would
remain after capping claims at each threshold.

The function is intended for portfolio-level data as well as claim-level
data. Portfolio-level data can include policies without claims, for
example rows where `claim_count = 0` and the claim amount is zero. Use
this before
[`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md)
to understand the effect of the threshold on the portfolio. The output
is useful for tariff notes, pricing reviews and governance discussions
around capped severity models.

## Usage

``` r
assess_excess_threshold(
  data,
  claim_amount,
  thresholds,
  exposure = NULL,
  group = NULL,
  claim_count = NULL
)
```

## Arguments

- data:

  A `data.frame` with portfolio-level or claim-level observations.
  Portfolio-level data can include policies without claims.

- claim_amount:

  Character string. Claim amount column.

- thresholds:

  Numeric vector of candidate thresholds.

- exposure:

  Optional character string. Exposure column. If supplied, risk premium
  before and after capping is calculated. The output column keeps this
  original name. If `NULL`, every record is counted as one exposure unit
  and the output contains an `exposure` column.

- group:

  Optional character string. Grouping column used to assess thresholds
  by segment. The output column keeps this original name. If `NULL`, no
  grouping column is added.

- claim_count:

  Optional character string. Claim-count column. If supplied, `n_claims`
  is calculated as the sum of this column. If `NULL`, records with
  `claim_amount > 0` are counted as one claim and records with
  `claim_amount == 0` as zero claims.

## Value

A `data.frame` with class `"threshold_assessment"` and columns:

- group column:

  The original grouping column, such as `sector`, if `group` is
  supplied. This is the first column when grouping is used.

- `threshold`:

  The excess threshold being assessed. Thresholds are shown in the same
  order as supplied in the `thresholds` argument.

- exposure column:

  The original exposure column, such as `policy_years`, if `exposure` is
  supplied. If `exposure = NULL`, this column is named `exposure` and
  counts records.

- `n_claims`:

  Total number of claims, calculated from `claim_count` or inferred from
  `claim_amount > 0`.

- `n_excess_records`:

  Number of records with `claim_amount > threshold`. This counts
  records, not individual claims.

- `total_loss`:

  Total claim amount before applying the threshold.

- `capped_loss`:

  Total claim amount retained below or at the threshold.

- `excess_loss`:

  Total claim amount above the threshold.

- `pure_premium_before`:

  Risk premium before capping: `total_loss / exposure`.

- `pure_premium_after`:

  Risk premium after capping: `capped_loss / exposure`.

- `premium_reduction`:

  `pure_premium_before - pure_premium_after`, equivalent to
  `excess_loss / exposure`. This is positive when applying the threshold
  reduces the retained risk premium.

- `premium_reduction_ratio`:

  `premium_reduction / pure_premium_before`. This is between 0 and 1
  when `pure_premium_before > 0`; if `pure_premium_before == 0`, it is
  defined as 0.

## Details

The output can be used for two common follow-up analyses. First,
aggregate the threshold assessment to portfolio level to calculate the
average additional risk premium required to finance the excess layer.
Second, after selecting a threshold, compare groups to see which parts
of the portfolio benefit most from the excess protection.

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

thresholds
#>     sector threshold policy_years n_claims n_excess_records total_loss
#> 1 Industry     25000            5        4                3     370000
#> 2 Industry     50000            5        4                2     370000
#> 3 Industry    100000            5        4                2     370000
#> 4 Industry    150000            5        4                1     370000
#> 5   Retail     25000            5        4                4     580000
#> 6   Retail     50000            5        4                3     580000
#> 7   Retail    100000            5        4                2     580000
#> 8   Retail    150000            5        4                1     580000
#>   capped_loss excess_loss pure_premium_before pure_premium_after
#> 1      100000      270000               74000              20000
#> 2      175000      195000               74000              35000
#> 3      275000       95000               74000              55000
#> 4      345000       25000               74000              69000
#> 5      100000      480000              116000              20000
#> 6      190000      390000              116000              38000
#> 7      330000      250000              116000              66000
#> 8      430000      150000              116000              86000
#>   premium_reduction premium_reduction_ratio
#> 1             54000              0.72972973
#> 2             39000              0.52702703
#> 3             19000              0.25675676
#> 4              5000              0.06756757
#> 5             96000              0.82758621
#> 6             78000              0.67241379
#> 7             50000              0.43103448
#> 8             30000              0.25862069
if (requireNamespace("gt", quietly = TRUE)) {
  as_gt(thresholds)
}


  

```
