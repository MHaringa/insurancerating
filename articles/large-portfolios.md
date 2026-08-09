# Working with large insurance portfolios

Insurance portfolio extracts can contain millions of policy-period
records. The practical limit is not imposed by R as a statistical
language, but by the memory needed to hold the data and temporary
objects created during a calculation. A database can perform the first
reduction when the row-level portfolio does not fit comfortably in
memory.

This vignette compares two workflows:

1.  use
    [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
    and
    [`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
    on data already held in R;
2.  use
    [`rating_grid_db()`](https://mharinga.github.io/insurancerating/reference/rating_grid_db.md)
    and
    [`merge_date_ranges_db()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges_db.md)
    to reduce data in a database and import only the result.

``` r

library(insurancerating)
```

## Memory rather than a fixed row limit

There is no generally valid maximum number of rows for R. Required
memory depends on the number and type of columns. Ten million rows
containing a few integer or factor columns are materially smaller than
ten million rows with many character fields. Reading a file and
subsequently grouping it may also require more memory than the final
object because source and intermediate objects coexist temporarily.

The following ranges are practical planning guidance rather than hard
limits:

| Portfolio size | Typical approach |
|----|----|
| Up to about 1 million rows | Usually straightforward in R |
| About 1 to 10 million rows | Often feasible for a reasonably narrow table with sufficient memory and `data.table`-based operations |
| Above about 10 million rows | Estimate memory before importing; database reduction is often preferable |
| About 50 million wide portfolio rows | Usually reduce in DuckDB or the source database before collecting into R |

A sample of the source file gives a more useful estimate than its row
count:

``` r

bytes_per_row <- as.numeric(object.size(portfolio_sample)) /
  nrow(portfolio_sample)

estimated_object_gb <- bytes_per_row * expected_rows / 1024^3
```

Allow additional working memory for reading, copying, grouping and
modelling. If the estimated object already occupies a substantial part
of available RAM, perform the initial reduction in a database.

## Local portfolio reduction

[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
combines records with the same observed rating-factor values. The
following example first rounds reconstruction value to units of EUR
1,000. This may be appropriate when individual euro values are not
relevant to the tariff structure.

``` r

set.seed(2026)
local_portfolio <- data.frame(
  policy_id = seq_len(100000),
  sector = sample(c("Industry", "Retail", "Services"), 100000, TRUE),
  region = sample(c("North", "South", "West"), 100000, TRUE),
  reconstruction_value = sample(seq(100000, 2000000, by = 500), 100000, TRUE),
  earned_exposure = runif(100000, 0.25, 1),
  earned_premium = runif(100000, 100, 2500)
)

local_portfolio$reconstruction_value_1000 <-
  round(local_portfolio$reconstruction_value / 1000) * 1000

local_grid <- rating_grid(
  local_portfolio,
  group_by = c("sector", "region", "reconstruction_value_1000"),
  exposure = "earned_exposure",
  aggregate_cols = "earned_premium"
)

data.frame(
  stage = c("Portfolio", "Rating grid"),
  rows = c(nrow(local_portfolio), nrow(local_grid))
)
#>         stage   rows
#> 1   Portfolio 100000
#> 2 Rating grid  16660
```

The result contains one row per observed combination and preserves total
exposure and premium. The policy-level table is still required when
individual policy predictions or policy-level diagnostics are needed.

[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
performs a different reduction. It combines connected coverage periods
within the same policy or risk:

``` r

periods <- data.frame(
  policy_id = c("P001", "P001", "P002"),
  coverage = c("Fire", "Fire", "Fire"),
  period_start = as.Date(c("2025-01-01", "2025-07-01", "2025-01-01")),
  period_end = as.Date(c("2025-06-30", "2025-12-31", "2025-12-31")),
  earned_exposure = c(0.5, 0.5, 1)
)

merge_date_ranges(
  periods,
  period_start = "period_start",
  period_end = "period_end",
  group_by = c("policy_id", "coverage"),
  aggregate_cols = "earned_exposure"
)
#>   policy_id coverage period_start period_end earned_exposure
#> 1      P001     Fire   2025-01-01 2025-12-31               1
#> 2      P002     Fire   2025-01-01 2025-12-31               1
```

## Lazy reduction in DuckDB

The database functions accept a lazy table created with
[`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html). They
return another lazy table. Calling the function therefore constructs SQL
but does not import the source portfolio.

The next example is executed when the suggested database packages are
available. It writes the generated portfolio to a temporary, file-backed
DuckDB database. The grouping remains lazy:
[`rating_grid_db()`](https://mharinga.github.io/insurancerating/reference/rating_grid_db.md)
constructs the query, and only the reduced grid is read back into R with
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html).

``` r

library(DBI)
library(dbplyr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:dbplyr':
#> 
#>     ident, sql, sql_escape_ident, sql_escape_string
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(duckdb)

database_path <- tempfile(fileext = ".duckdb")
con <- dbConnect(duckdb(), dbdir = database_path)
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpyLxcLk/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.

dbWriteTable(
  con,
  "portfolio",
  local_portfolio,
  overwrite = TRUE
)

portfolio_db <- tbl(con, "portfolio")

grid_db <- portfolio_db |>
  mutate(
    reconstruction_value_1000 =
      round(reconstruction_value / 1000) * 1000
  ) |>
  rating_grid_db(
    group_by = c("sector", "region", "reconstruction_value_1000"),
    exposure = "earned_exposure",
    aggregate_cols = "earned_premium"
  )

# Inspect the SQL without collecting the row-level portfolio.
sql_render(grid_db)
#> <SQL> SELECT
#>   sector,
#>   region,
#>   reconstruction_value_1000,
#>   SUM(earned_premium) AS earned_premium,
#>   SUM(earned_exposure) AS earned_exposure
#> FROM (
#>   SELECT
#>     policy_id,
#>     sector,
#>     region,
#>     reconstruction_value,
#>     earned_exposure,
#>     earned_premium,
#>     ROUND_EVEN(reconstruction_value / 1000.0, CAST(ROUND(0.0, 0) AS INTEGER)) * 1000.0 AS reconstruction_value_1000
#>   FROM portfolio
#> ) AS q01
#> GROUP BY sector, region, reconstruction_value_1000

database_row_counts <- data.frame(
  source_rows = portfolio_db |>
    summarise(n = n()) |>
    collect() |>
    pull(n),
  reduced_rows = grid_db |>
    summarise(n = n()) |>
    collect() |>
    pull(n)
)

database_grid <- collect(grid_db)
database_row_counts
#>   source_rows reduced_rows
#> 1       1e+05        16660
head(database_grid)
#> # A tibble: 6 × 5
#>   sector   region reconstruction_value_1000 earned_premium earned_exposure
#>   <chr>    <chr>                      <dbl>          <dbl>           <dbl>
#> 1 Industry North                     638000          4099.            3.09
#> 2 Industry North                    1642000          7925.            5.01
#> 3 Services North                    1478000         10346.            3.18
#> 4 Industry South                    1652000         23788.           10.5 
#> 5 Industry South                     358000         14686.            9.13
#> 6 Retail   South                     448000         14492.            7.21

dbDisconnect(con, shutdown = TRUE)
unlink(database_path)
unlink(paste0(database_path, ".wal"))
```

This example starts from an R object only to make the workflow
reproducible inside the vignette. In production, the row-level portfolio
will commonly already reside in a database or be read by DuckDB directly
from Parquet files. In that case the large source table never needs to
be materialised in R.

DuckDB can also query files without first loading them into an R object.
For example, a Parquet extract can be exposed as a database view:

``` r

library(DBI)
library(dbplyr)
library(dplyr)
library(duckdb)

con <- dbConnect(duckdb())

dbExecute(con, "
  CREATE VIEW portfolio AS
  SELECT * FROM read_parquet('portfolio/*.parquet')
")
portfolio_db <- tbl(con, "portfolio")
```

The following DuckDB example generates ten million rows inside the
database. The records are never materialised as an R data frame. The
code is not executed during the vignette build because creating ten
million rows on every package check would be disproportionate, but the
block is complete and can be run as shown.

``` r

large_database_path <- "large_portfolio.duckdb"
con <- dbConnect(duckdb(), dbdir = large_database_path)

dbExecute(con, "
  CREATE TABLE portfolio_10m AS
  SELECT
    i AS policy_id,
    'Sector ' || CAST(i % 20 AS VARCHAR) AS sector,
    'Region ' || CAST(FLOOR(i / 20) % 10 AS VARCHAR) AS region,
    100000 + (FLOOR(i / 200) % 100) * 1000 AS reconstruction_value,
    0.5 + (i % 50) / 100.0 AS earned_exposure,
    100 + (i % 2000) AS earned_premium
  FROM range(10000000) AS portfolio(i)
")

portfolio_10m <- tbl(con, "portfolio_10m")

grid_10m_db <- portfolio_10m |>
  mutate(
    reconstruction_value_1000 =
      round(reconstruction_value / 1000) * 1000
  ) |>
  rating_grid_db(
    group_by = c("sector", "region", "reconstruction_value_1000"),
    exposure = "earned_exposure",
    aggregate_cols = "earned_premium"
  )

sql_render(grid_10m_db)

row_comparison_10m <- data.frame(
  source_rows = portfolio_10m |> summarise(n = n()) |> pull(n),
  reduced_rows = grid_10m_db |> summarise(n = n()) |> pull(n)
)
row_comparison_10m$reduction <-
  1 - row_comparison_10m$reduced_rows / row_comparison_10m$source_rows

row_comparison_10m
grid_10m <- collect(grid_10m_db)
```

In this constructed portfolio, the three grouping variables define at
most 20,000 combinations. Ten million source rows are therefore reduced
to no more than 20,000 rating-grid rows before
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) is
called: a reduction of 99.8%. The exact reduction in a real portfolio
depends on the number of observed combinations.

The same pattern applies to 50 million rows. Only the range used to
generate the database table changes:

``` r

dbExecute(con, "
  CREATE TABLE portfolio_50m AS
  SELECT
    i AS policy_id,
    'Sector ' || CAST(i % 20 AS VARCHAR) AS sector,
    'Region ' || CAST(FLOOR(i / 20) % 10 AS VARCHAR) AS region,
    100000 + (FLOOR(i / 200) % 100) * 1000 AS reconstruction_value,
    0.5 + (i % 50) / 100.0 AS earned_exposure,
    100 + (i % 2000) AS earned_premium
  FROM range(50000000) AS portfolio(i)
")

portfolio_50m <- tbl(con, "portfolio_50m")

grid_50m_db <- rating_grid_db(
  portfolio_50m,
  group_by = c("sector", "region", "reconstruction_value"),
  exposure = "earned_exposure",
  aggregate_cols = "earned_premium"
)

row_comparison_50m <- data.frame(
  source_rows = portfolio_50m |> summarise(n = n()) |> pull(n),
  reduced_rows = grid_50m_db |> summarise(n = n()) |> pull(n)
)

grid_50m <- collect(grid_50m_db)
```

The 50 million source records again reduce to no more than 20,000
combinations in this example, a reduction of 99.96%. The large table
remains in DuckDB; only the compact grid enters R. A file-backed DuckDB
database can be used when the database itself should persist between
sessions.

## Reducing date ranges in DuckDB

[`merge_date_ranges_db()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges_db.md)
applies the temporal gaps-and-islands calculation in DuckDB. It returns
consolidated periods as a lazy query:

``` r

dbExecute(con, "
  CREATE TABLE portfolio_periods AS
  SELECT * FROM (VALUES
    ('P001', 'Fire', 'Industry', DATE '2025-01-01', DATE '2025-06-30', 0.5, 600.0),
    ('P001', 'Fire', 'Industry', DATE '2025-07-01', DATE '2025-12-31', 0.5, 650.0),
    ('P002', 'Fire', 'Retail',   DATE '2025-01-01', DATE '2025-12-31', 1.0, 900.0)
  ) AS periods(
    policy_id,
    coverage,
    sector,
    period_start,
    period_end,
    earned_exposure,
    earned_premium
  )
")

periods_db <- tbl(con, "portfolio_periods")

merged_periods_db <- merge_date_ranges_db(
  periods_db,
  period_start = "period_start",
  period_end = "period_end",
  group_by = c("policy_id", "coverage", "sector"),
  aggregate_cols = c("earned_exposure", "earned_premium"),
  merge_gap_days = 1
)

merged_periods <- collect(merged_periods_db)
dbDisconnect(con, shutdown = TRUE)
unlink(large_database_path)
unlink(paste0(large_database_path, ".wal"))
```

This database variant is restricted to DuckDB because date arithmetic
and window-function details differ between database systems.
[`rating_grid_db()`](https://mharinga.github.io/insurancerating/reference/rating_grid_db.md)
is based on standard grouped SQL and can be used with other `dbplyr`
backends.

## Which reduction comes first?

The two reductions answer different questions and do not always need to
be combined.

- Use
  [`rating_grid_db()`](https://mharinga.github.io/insurancerating/reference/rating_grid_db.md)
  directly when the source periods are already suitable for aggregation
  and the objective is a table of observed rating combinations.
- Use
  [`merge_date_ranges_db()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges_db.md)
  when renewals, endorsements or administrative splits first need to be
  consolidated into meaningful coverage periods.
- When both are required, merge periods first and construct the rating
  grid second. A rating grid no longer contains the row-level interval
  structure needed to merge policy periods.

The grouping columns used for temporal merging must retain every
attribute that should remain distinct. For example, include `sector`
when a policy changes sector during its history. Aggregate premium or
exposure only when the source amounts are additive; overlapping records
that describe the same covered days should be resolved before summing.

For a large workflow it can be useful to materialise an intermediate
reduction inside DuckDB with
[`dplyr::compute()`](https://dplyr.tidyverse.org/reference/compute.html).
This avoids repeating an expensive merge while still keeping the
intermediate table outside R. Call
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) only
after checking that the reduced row count and columns fit the intended R
analysis.
