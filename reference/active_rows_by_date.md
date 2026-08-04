# Find active portfolio rows for event dates

Matches event dates, such as claim dates or portfolio snapshot dates, to
the policy or risk records that were active on those dates. This allows
an event to inherit the rating factors and coverage information that
applied when it occurred.

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

  Controls event dates for which no active portfolio row is found. With
  `NULL`, unmatched events are omitted. With `NA`, they are retained
  with missing portfolio information.

- mult:

  Controls the result when an event date matches multiple active
  portfolio rows. Use `"all"` to retain every match, or `"first"` or
  `"last"` to retain one matching row. The default is `"all"`.

## Value

An object with the same class as `portfolio`.

## Details

This is useful when claim records or other dated events need the rating
factors, premium, exposure, or policy attributes that were active at the
event date. The function performs an interval join between event dates
and portfolio coverage periods, optionally within matching identifiers
such as a policy number.

Multiple matches can be valid, for example when several coverages are
active on the same date, but may also indicate overlapping portfolio
periods. The analyst should select `mult` in accordance with the
structure of the source data and the intended event-level analysis.

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
