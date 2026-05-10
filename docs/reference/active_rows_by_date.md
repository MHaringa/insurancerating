# Find active portfolio rows for event dates

Matches event dates, such as claim dates or portfolio snapshot dates, to
the rows that were active in the portfolio on those dates.

## Usage

``` r
active_rows_by_date(
  portfolio,
  dates,
  period_start,
  period_end,
  date,
  by = NULL,
  nomatch = NULL,
  mult = "all"
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

- nomatch:

  When a row (with interval say, `[a,b]`) in x has no match in y,
  nomatch=NA means NA is returned for y's non-by.y columns for that row
  of x. nomatch=NULL (default) means no rows will be returned for that
  row of x.

- mult:

  When multiple rows in y match to the row in x, `mult` controls which
  values are returned - "all" (default), "first" or "last".

- df, df_begin, df_end, dates_date, ...:

  Deprecated argument names kept for backward compatibility in
  [`rows_per_date()`](https://mharinga.github.io/insurancerating/reference/rows_per_date.md).

## Value

returned class is equal to class of `df`

## Details

This is useful when claim records or other dated events need the rating
factors, premium, exposure, or policy attributes that were active at the
event date. The function performs an interval join between event dates
and portfolio coverage periods, optionally within matching identifiers
such as a policy number.

## Author

Martin Haringa

## Examples

``` r
library(lubridate)
#> 
#> Attaching package: ‘lubridate’
#> The following objects are masked from ‘package:base’:
#> 
#>     date, intersect, setdiff, union
portfolio <- data.frame(
begin1 = ymd(c("2014-01-01", "2014-01-01")),
end = ymd(c("2014-03-14", "2014-05-10")),
termination = ymd(c("2014-03-14", "2014-05-10")),
exposure = c(0.2025, 0.3583),
premium =  c(125, 150),
car_type = c("BMW", "TESLA"))

## Find active rows on different dates
dates0 <- data.frame(active_date = seq(ymd("2014-01-01"), ymd("2014-05-01"),
by = "months"))
active_rows_by_date(
  portfolio,
  dates0,
  period_start = "begin1",
  period_end = "end",
  date = "active_date"
)
#>       begin1        end termination exposure premium car_type active_date
#> 1 2014-01-01 2014-03-14  2014-03-14   0.2025     125      BMW  2014-01-01
#> 2 2014-01-01 2014-05-10  2014-05-10   0.3583     150    TESLA  2014-01-01
#> 3 2014-01-01 2014-03-14  2014-03-14   0.2025     125      BMW  2014-02-01
#> 4 2014-01-01 2014-05-10  2014-05-10   0.3583     150    TESLA  2014-02-01
#> 5 2014-01-01 2014-03-14  2014-03-14   0.2025     125      BMW  2014-03-01
#> 6 2014-01-01 2014-05-10  2014-05-10   0.3583     150    TESLA  2014-03-01
#> 7 2014-01-01 2014-05-10  2014-05-10   0.3583     150    TESLA  2014-04-01
#> 8 2014-01-01 2014-05-10  2014-05-10   0.3583     150    TESLA  2014-05-01

## With extra identifiers (merge claim date with time interval in portfolio)
claim_dates <- data.frame(claim_date = ymd("2014-01-01"),
car_type = c("BMW", "VOLVO"))

### Only rows are returned that can be matched
active_rows_by_date(
  portfolio,
  claim_dates,
  period_start = "begin1",
  period_end = "end",
  date = "claim_date",
  by = "car_type"
)
#>   car_type     begin1        end termination exposure premium claim_date
#> 1      BMW 2014-01-01 2014-03-14  2014-03-14   0.2025     125 2014-01-01

### When row cannot be matched, NA is returned for that row
active_rows_by_date(
  portfolio,
  claim_dates,
  period_start = "begin1",
  period_end = "end",
  date = "claim_date",
  by = "car_type",
  nomatch = NA
)
#>   car_type     begin1        end termination exposure premium claim_date
#> 1      BMW 2014-01-01 2014-03-14  2014-03-14   0.2025     125 2014-01-01
#> 2    VOLVO       <NA>       <NA>        <NA>       NA      NA 2014-01-01
```
