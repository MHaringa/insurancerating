# Reduce a database portfolio to observed rating-grid points

Construct the same type of observed rating-factor combinations as
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md),
while leaving the calculation in the database. The function returns a
lazy query and does not copy the source portfolio into R.

This is useful when the row-level portfolio is too large for available R
memory. The database performs the grouping and aggregation; the reduced
result can subsequently be imported with
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html).

## Usage

``` r
rating_grid_db(
  x,
  group_by,
  exposure = NULL,
  aggregate_cols = NULL,
  drop_na = FALSE,
  exposure_by = NULL
)
```

## Arguments

- x:

  A lazy database table created with
  [`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html).

- group_by:

  Character vector containing the columns that define an observed
  rating-grid point.

- exposure:

  Optional character string naming an exposure column to sum. If `NULL`,
  the output contains a `count` column with the number of source records
  in each combination.

- aggregate_cols:

  Optional character vector naming additional columns to sum within each
  combination.

- drop_na:

  Logical. If `TRUE`, records with missing `group_by` values are
  excluded. If `FALSE`, missing values remain an observed group.

- exposure_by:

  Reserved for consistency with
  [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md).
  Splitting an exposure into dynamically named wide columns is not
  performed in a lazy database query. Include this variable in
  `group_by`, collect the reduced long table, and reshape it in R
  instead.

## Value

A lazy database table. Use
[`dbplyr::sql_render()`](https://dbplyr.tidyverse.org/reference/sql_build.html)
to inspect the SQL or
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
to import the reduced result into R.

## Details

`rating_grid_db()` performs only operations that translate naturally to
SQL: grouping, row counting and sums. Column names are supplied as
strings. No SQL is executed until the lazy query is printed, collected
or otherwise used by the database backend.

Database tables have no inherent row order. Apply
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
after the final database operation when a specific presentation order is
required.

The database table should already contain the rating variables required
for the intended model or portfolio analysis. Unlike
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md),
this function does not inspect a fitted R model or refinement metadata
because those objects are held in R rather than in the database.

## See also

[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md),
[`merge_date_ranges_db()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges_db.md),
[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
con <- DBI::dbConnect(duckdb::duckdb())
portfolio_db <- dplyr::tbl(con, "portfolio")

grid_db <- rating_grid_db(
  portfolio_db,
  group_by = c("sector", "region"),
  exposure = "earned_exposure",
  aggregate_cols = "earned_premium"
)

dbplyr::sql_render(grid_db)
grid <- dplyr::collect(grid_db)
DBI::dbDisconnect(con, shutdown = TRUE)
} # }
```
