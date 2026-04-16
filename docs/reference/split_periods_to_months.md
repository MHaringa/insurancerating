# Split periods into monthly intervals

Splits rows where the time period is longer than one month into multiple
rows with a time period of exactly one month each. Values in numeric
columns (e.g. exposure or premium) are divided proportionally over the
months.

This function uses **standard evaluation (SE)**: column names must be
passed as **character strings** (e.g. `begin = "begin_date"`). The older
function `period_to_months()` used non-standard evaluation (NSE) and is
deprecated as of version 0.8.0.

## Usage

``` r
split_periods_to_months(df, begin, end, cols = NULL)

period_to_months(df, begin, end, ...)
```

## Arguments

- df:

  A `data.frame` or `data.table`.

- begin:

  Character string. Name of column in `df` with begin dates.

- end:

  Character string. Name of column in `df` with end dates.

- cols:

  Character vector with names of numeric columns in `df` to split.

- ...:

  Columns in `df` to split. Deprecated, use `cols` instead.

## Value

A `data.frame` with the same columns as in `df`, plus an `id` column.

## Details

In insurance portfolios it is common that rows relate to periods longer
than one month. This can be problematic when monthly exposures are
needed.

Since insurance premiums are assumed constant over months (and not
depending on the number of days per month), the function assumes each
month has 30 days.

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

# New SE interface
split_periods_to_months(portfolio,
  begin = "begin_date", end = "end_date",
  cols = c("premium", "exposure")
)
#>   id begin_date   end_date   exposure  premium id    new_end
#> 1  1 2014-01-01 2014-01-31 0.08209459 50.67568  1 2014-03-15
#> 2  1 2014-02-01 2014-02-28 0.08209459 50.67568  1 2014-03-15
#> 3  1 2014-03-01 2014-03-31 0.03831081 23.64865  1 2014-03-15
#> 4  2 2014-01-01 2014-01-31 0.08268462 34.61538  2 2014-05-11
#> 5  2 2014-02-01 2014-02-28 0.08268462 34.61538  2 2014-05-11
#> 6  2 2014-03-01 2014-03-31 0.08268462 34.61538  2 2014-05-11
#> 7  2 2014-04-01 2014-04-30 0.08268462 34.61538  2 2014-05-11
#> 8  2 2014-05-01 2014-05-31 0.02756154 11.53846  2 2014-05-11

# Old NSE interface (deprecated)
if (FALSE) { # \dontrun{
period_to_months(portfolio, begin_date, end_date, premium, exposure)
} # }
```
