# Split policy periods into monthly rows

Split policy or exposure periods that cross calendar-month boundaries
into monthly rows. Numeric portfolio measures, such as exposure and
premium, can be prorated over the resulting periods while preserving
their total per original record.

## Usage

``` r
split_periods_to_months(
  df,
  period_start = NULL,
  period_end = NULL,
  prorate_cols = NULL,
  begin = NULL,
  end = NULL,
  cols = NULL
)
```

## Arguments

- df:

  A `data.frame` or `data.table`.

- period_start:

  Character string. Name of the column with policy period start dates.

- period_end:

  Character string. Name of the column with policy period end dates.

- prorate_cols:

  Character vector with names of numeric columns to prorate over the
  monthly rows, for example exposure or premium.

- begin, end, cols:

  Deprecated argument names kept for backward compatibility.

## Value

A `data.frame` with the same columns as in `df`, plus an `id` column.

## Details

Rating and monitoring work often needs exposure, premium, claim counts,
or policy counts by calendar month. Source portfolios, however, usually
contain policy periods that start and end on arbitrary dates. This
helper expands those periods into monthly rows before modelling,
reporting, or joining to monthly portfolio summaries.

Prorated columns are distributed according to the part of the policy
period that falls in each monthly row. Full months receive weight 1;
partial months use a 30-day month convention. The total value of each
prorated column is preserved per original row.

Column names are supplied as character strings, for example
`period_start = "begin_date"`. The deprecated
[`period_to_months()`](https://mharinga.github.io/insurancerating/reference/period_to_months.md)
interface used unquoted column names and is retained only for backward
compatibility.

## Author

Martin Haringa

## Examples

``` r
library(lubridate)
portfolio <- data.frame(
  begin_date = ymd(c("2014-01-01", "2014-01-01")),
  end_date   = ymd(c("2014-03-14", "2014-05-10")),
  exposure   = c(0.2025, 0.3583),
  premium    = c(125, 150)
)

# Split policy records and prorate premium and exposure by month
split_periods_to_months(portfolio,
  period_start = "begin_date",
  period_end = "end_date",
  prorate_cols = c("premium", "exposure")
)
#>   id begin_date   end_date   exposure  premium
#> 1  1 2014-01-01 2014-01-31 0.08437500 52.08333
#> 2  1 2014-02-01 2014-02-28 0.07875000 48.61111
#> 3  1 2014-03-01 2014-03-14 0.03937500 24.30556
#> 4  2 2014-01-01 2014-01-31 0.08397656 35.15625
#> 5  2 2014-02-01 2014-02-28 0.07837813 32.81250
#> 6  2 2014-03-01 2014-03-31 0.08397656 35.15625
#> 7  2 2014-04-01 2014-04-30 0.08397656 35.15625
#> 8  2 2014-05-01 2014-05-10 0.02799219 11.71875

# Deprecated interface with unquoted column names
if (FALSE) { # \dontrun{
period_to_months(portfolio, begin_date, end_date, premium, exposure)
} # }
```
