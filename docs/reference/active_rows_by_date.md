# Match event dates to active portfolio periods

Match event dates, such as claim dates or inspection dates, to policy
records that were active when the event occurred. The matched result
contains the portfolio characteristics and coverage information
applicable on each event date.

## Usage

``` r
active_rows_by_date(
  portfolio,
  dates,
  period_start,
  period_end,
  date,
  by = NULL,
  unmatched = c("drop", "keep"),
  multiple_matches = c("all", "first", "last"),
  nomatch = NULL,
  mult = NULL
)
```

## Arguments

- portfolio:

  A `data.frame` or `data.table` with portfolio rows and active date
  intervals.

- dates:

  A `data.frame` or `data.table` with event or snapshot dates.

- period_start:

  Character string. Name of the portfolio column with period start
  dates.

- period_end:

  Character string. Name of the portfolio column with period end dates.

- date:

  Character string. Name of the date column in `dates`.

- by:

  Character vector with additional columns used to match `portfolio` and
  `dates`, for example policy number or claim identifier.

- unmatched:

  Character string. Use `"drop"` to omit event dates for which no active
  portfolio row is found, or `"keep"` to retain them with missing
  portfolio information. The default is `"drop"`.

- multiple_matches:

  Character string controlling events that match multiple active
  portfolio rows. Use `"all"` to retain every match, or `"first"` or
  `"last"` to retain one matching row. The default is `"all"`.

- nomatch, mult:

  Deprecated technical argument names. Use `unmatched` and
  `multiple_matches` instead.

## Value

A regular `data.frame` containing the event records and the portfolio
information active on their dates. Event order is preserved. Depending
on `multiple_matches`, one event can produce more than one output row.

## Details

Claim and event files often contain an event date and policy identifier
but not the rating factors used at that point in time. The function
performs an interval match between those events and the portfolio
history. Supplying a policy identifier through `by` prevents an event
from matching active periods belonging to another policy.

This is a temporal matching operation rather than a portfolio reduction.
See
[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
for consolidating connected coverage periods and
[`split_periods_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md)
for expanding periods into monthly records.

Multiple matches can be valid when one event relates to several
concurrently active coverages. They can also reveal overlapping or
duplicated policy periods. Use `multiple_matches = "all"` when every
active record is relevant; use `"first"` or `"last"` only when the
source system defines which record should take precedence.

With `unmatched = "drop"`, events outside every applicable coverage
period are omitted. With `unmatched = "keep"`, they remain visible with
missing portfolio fields. Retaining unmatched events is generally
preferable during data-quality review because it makes gaps in the
policy history explicit.

The interval join is performed internally with
[`data.table::foverlaps()`](https://rdrr.io/pkg/data.table/man/foverlaps.html)
on local copies. Neither input is modified by reference. The output
follows the original order of `dates` and is always returned as a
regular `data.frame`.

## See also

[`split_periods_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md),
[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md),
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)

## Author

Martin Haringa

## Examples

``` r
portfolio <- data.frame(
  policy_id = c("P001", "P001", "P002"),
  coverage_start = as.Date(c("2024-01-01", "2025-01-01", "2025-01-01")),
  coverage_end = as.Date(c("2024-12-31", "2025-12-31", "2025-12-31")),
  sector = c("Retail", "Industry", "Services"),
  insured_amount = c(500000, 750000, 300000),
  earned_premium = c(900, 1250, 650)
)

claims <- data.frame(
  claim_id = c("C001", "C002", "C003"),
  policy_id = c("P001", "P001", "P002"),
  claim_date = as.Date(c("2024-06-15", "2025-08-10", "2026-01-10")),
  claim_amount = c(12000, 45000, 8000)
)

# Attach the policy characteristics that applied on each claim date.
active_rows_by_date(
  portfolio,
  claims,
  period_start = "coverage_start",
  period_end = "coverage_end",
  date = "claim_date",
  by = "policy_id"
)
#>   policy_id coverage_start coverage_end   sector insured_amount earned_premium
#> 1      P001     2024-01-01   2024-12-31   Retail         500000            900
#> 2      P001     2025-01-01   2025-12-31 Industry         750000           1250
#>   claim_id claim_amount claim_date
#> 1     C001        12000 2024-06-15
#> 2     C002        45000 2025-08-10

# Keep claims outside the available policy history for data-quality review.
active_rows_by_date(
  portfolio,
  claims,
  period_start = "coverage_start",
  period_end = "coverage_end",
  date = "claim_date",
  by = "policy_id",
  unmatched = "keep"
)
#>   policy_id coverage_start coverage_end   sector insured_amount earned_premium
#> 1      P001     2024-01-01   2024-12-31   Retail         500000            900
#> 2      P001     2025-01-01   2025-12-31 Industry         750000           1250
#> 3      P002           <NA>         <NA>     <NA>             NA             NA
#>   claim_id claim_amount claim_date
#> 1     C001        12000 2024-06-15
#> 2     C002        45000 2025-08-10
#> 3     C003         8000 2026-01-10
```
