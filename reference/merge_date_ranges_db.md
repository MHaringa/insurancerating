# Merge connected portfolio periods in DuckDB

Construct the temporal portfolio reduction performed by
[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
as a lazy DuckDB query. Overlapping or adjacent periods are merged
inside DuckDB, so only the consolidated periods need to be copied into
R.

## Usage

``` r
merge_date_ranges_db(
  data,
  period_start,
  period_end,
  group_by,
  aggregate_cols = NULL,
  aggregate_fun = c("sum", "mean", "min", "max"),
  merge_gap_days = 1
)
```

## Arguments

- data:

  A lazy DuckDB table created with
  [`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html).

- period_start:

  Character string naming the period-start column.

- period_end:

  Character string naming the period-end column.

- group_by:

  Character vector identifying the policy, risk or segment within which
  periods may be combined.

- aggregate_cols:

  Optional character vector naming numeric columns to aggregate over
  each merged period.

- aggregate_fun:

  Character string specifying the SQL aggregation for `aggregate_cols`:
  `"sum"`, `"mean"`, `"min"`, or `"max"`.

- merge_gap_days:

  Non-negative whole number. The interpretation is the same as in
  [`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md):
  `1` merges overlapping and directly adjacent periods, while `0` merges
  overlapping periods only.

## Value

A lazy DuckDB table. Use
[`dbplyr::sql_render()`](https://dbplyr.tidyverse.org/reference/sql_build.html)
to inspect the SQL or
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
to import the consolidated periods into R.

## Details

Merging connected intervals is a SQL gaps-and-islands calculation. The
query uses ordered window functions to compare each start date with the
latest preceding end date, assigns an interval identifier, and then
aggregates each resulting interval.

Date arithmetic differs between database systems. This implementation is
therefore deliberately restricted to DuckDB. It does not download the
source table and does not create a permanent database table.

## See also

[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md),
[`rating_grid_db()`](https://mharinga.github.io/insurancerating/reference/rating_grid_db.md),
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
con <- DBI::dbConnect(duckdb::duckdb())
portfolio_db <- dplyr::tbl(con, "portfolio_periods")

periods_db <- merge_date_ranges_db(
  portfolio_db,
  period_start = "period_start",
  period_end = "period_end",
  group_by = c("policy_id", "coverage"),
  aggregate_cols = c("earned_exposure", "earned_premium")
)

periods <- dplyr::collect(periods_db)
DBI::dbDisconnect(con, shutdown = TRUE)
} # }
```
