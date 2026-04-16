# Find active rows per date

Fast overlap joins. Usually, `df` is a very large data.table (e.g.
insurance portfolio) with small interval ranges, and `dates` is much
smaller with (e.g.) claim dates.

## Usage

``` r
rows_per_date(
  df,
  dates,
  df_begin,
  df_end,
  dates_date,
  ...,
  nomatch = NULL,
  mult = "all"
)
```

## Arguments

- df:

  data.frame with portfolio (df should include time period)

- dates:

  data.frame with dates to join

- df_begin:

  column name with begin dates of time period in `df`

- df_end:

  column name with end dates of time period in `df`

- dates_date:

  column name with dates in `dates`

- ...:

  additional column names in `dates` to join by

- nomatch:

  When a row (with interval say, `[a,b]`) in x has no match in y,
  nomatch=NA means NA is returned for y's non-by.y columns for that row
  of x. nomatch=NULL (default) means no rows will be returned for that
  row of x.

- mult:

  When multiple rows in y match to the row in x, `mult` controls which
  values are returned - "all" (default), "first" or "last".

## Value

returned class is equal to class of `df`

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
rows_per_date(portfolio, dates0, df_begin = begin1, df_end = end,
dates_date = active_date)
#>       begin1        end termination exposure premium car_type index_df
#> 1 2014-01-01 2014-03-14  2014-03-14   0.2025     125      BMW        1
#> 2 2014-01-01 2014-05-10  2014-05-10   0.3583     150    TESLA        2
#> 3 2014-01-01 2014-03-14  2014-03-14   0.2025     125      BMW        1
#> 4 2014-01-01 2014-05-10  2014-05-10   0.3583     150    TESLA        2
#> 5 2014-01-01 2014-03-14  2014-03-14   0.2025     125      BMW        1
#> 6 2014-01-01 2014-05-10  2014-05-10   0.3583     150    TESLA        2
#> 7 2014-01-01 2014-05-10  2014-05-10   0.3583     150    TESLA        2
#> 8 2014-01-01 2014-05-10  2014-05-10   0.3583     150    TESLA        2
#>   active_date index_dates
#> 1  2014-01-01           1
#> 2  2014-01-01           1
#> 3  2014-02-01           2
#> 4  2014-02-01           2
#> 5  2014-03-01           3
#> 6  2014-03-01           3
#> 7  2014-04-01           4
#> 8  2014-05-01           5

## With extra identifiers (merge claim date with time interval in portfolio)
claim_dates <- data.frame(claim_date = ymd("2014-01-01"),
car_type = c("BMW", "VOLVO"))

### Only rows are returned that can be matched
rows_per_date(portfolio, claim_dates, df_begin = begin1,
   df_end = end, dates_date = claim_date, car_type)
#>        begin1        end termination exposure premium car_type index_df
#>        <Date>     <Date>      <Date>    <num>   <num>   <char>    <int>
#> 1: 2014-01-01 2014-03-14  2014-03-14   0.2025     125      BMW        1
#>    claim_date index_dates
#>        <Date>       <int>
#> 1: 2014-01-01           1

### When row cannot be matched, NA is returned for that row
rows_per_date(portfolio, claim_dates, df_begin = begin1,
   df_end = end, dates_date = claim_date, car_type, nomatch = NA)
#>        begin1        end termination exposure premium car_type index_df
#>        <Date>     <Date>      <Date>    <num>   <num>   <char>    <int>
#> 1: 2014-01-01 2014-03-14  2014-03-14   0.2025     125      BMW        1
#> 2: 2014-01-01 2014-05-10  2014-05-10   0.3583     150    TESLA        2
#>    claim_date index_dates
#>        <Date>       <int>
#> 1: 2014-01-01           1
#> 2:       <NA>          NA
```
