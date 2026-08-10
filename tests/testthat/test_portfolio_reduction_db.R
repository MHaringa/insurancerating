test_that("rating_grid_db constructs a lazy grouped query", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("DBI")

  portfolio_db <- dbplyr::lazy_frame(
    sector = c("Industry", "Retail"),
    exposure = c(1, 2),
    premium = c(100, 200)
  )

  result <- rating_grid_db(
    portfolio_db,
    group_by = "sector",
    exposure = "exposure",
    aggregate_cols = "premium"
  )
  sql <- dbplyr::sql_render(result)

  expect_s3_class(result, "tbl_lazy")
  expect_match(sql, "GROUP BY", fixed = TRUE)
  expect_match(sql, "SUM", fixed = TRUE)
  expect_match(sql, "exposure", fixed = TRUE)
  expect_match(sql, "premium", fixed = TRUE)
})

test_that("rating_grid_db counts records without exposure", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("DBI")

  portfolio_db <- dbplyr::lazy_frame(
    sector = c("Industry", "Retail")
  )
  result <- rating_grid_db(portfolio_db, group_by = "sector")

  expect_match(dbplyr::sql_render(result), "COUNT", fixed = TRUE)
  expect_true("count" %in% colnames(result))
})

test_that("rating_grid_db validates its standard-evaluation API", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("DBI")

  portfolio_db <- dbplyr::lazy_frame(
    sector = "Industry",
    exposure = 1
  )

  expect_error(
    rating_grid_db(data.frame(sector = "Industry"), group_by = "sector"),
    "lazy database table",
    fixed = TRUE
  )
  expect_error(
    rating_grid_db(portfolio_db, group_by = "missing"),
    "not present in the database table: missing",
    fixed = TRUE
  )
  expect_error(
    rating_grid_db(
      portfolio_db,
      group_by = "sector",
      exposure = "exposure",
      exposure_by = "sector"
    ),
    "not supported",
    fixed = TRUE
  )
})

test_that("database reductions agree with local reductions in DuckDB", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  connection <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  portfolio <- data.frame(
    policy_id = c("A", "A", "B", "B"),
    sector = c("Industry", "Industry", "Retail", "Retail"),
    period_start = as.Date(c(
      "2025-01-01", "2025-02-01", "2025-01-01", "2025-03-02"
    )),
    period_end = as.Date(c(
      "2025-01-31", "2025-02-28", "2025-02-28", "2025-03-31"
    )),
    exposure = c(1, 2, 3, 4),
    premium = c(100, 200, 300, NA)
  )
  DBI::dbWriteTable(connection, "portfolio", portfolio)
  portfolio_db <- dplyr::tbl(connection, "portfolio")

  grid_db <- rating_grid_db(
    portfolio_db,
    group_by = "sector",
    exposure = "exposure",
    aggregate_cols = "premium"
  )
  grid_result <- dplyr::collect(grid_db)
  grid_local <- rating_grid(
    portfolio,
    group_by = "sector",
    exposure = "exposure",
    aggregate_cols = "premium"
  )
  grid_result <- as.data.frame(grid_result)[order(grid_result$sector), , drop = FALSE]
  grid_local <- grid_local[order(grid_local$sector), , drop = FALSE]
  row.names(grid_result) <- NULL
  row.names(grid_local) <- NULL

  expect_s3_class(grid_db, "tbl_lazy")
  expect_equal(grid_result, grid_local)

  periods_db <- merge_date_ranges_db(
    portfolio_db,
    period_start = "period_start",
    period_end = "period_end",
    group_by = c("policy_id", "sector"),
    aggregate_cols = c("exposure", "premium")
  )
  periods_result <- dplyr::collect(periods_db)
  periods_local <- merge_date_ranges(
    portfolio,
    period_start = "period_start",
    period_end = "period_end",
    group_by = c("policy_id", "sector"),
    aggregate_cols = c("exposure", "premium")
  )
  period_order_db <- order(
    periods_result$policy_id,
    periods_result$sector,
    periods_result$period_start,
    periods_result$period_end
  )
  period_order_local <- order(
    periods_local$policy_id,
    periods_local$sector,
    periods_local$period_start,
    periods_local$period_end
  )
  periods_result <- periods_result[period_order_db, , drop = FALSE]
  periods_local <- periods_local[period_order_local, , drop = FALSE]
  row.names(periods_result) <- NULL
  row.names(periods_local) <- NULL

  expect_s3_class(periods_db, "tbl_lazy")
  expect_identical(names(periods_result), names(periods_local))
  expect_equal(
    lapply(periods_result, identity),
    lapply(periods_local, identity)
  )
})

test_that("merge_date_ranges_db validates DuckDB-specific inputs", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  connection <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(connection, "periods", data.frame(
    policy_id = "A",
    period_start = as.Date("2025-01-01"),
    period_end = as.Date("2025-01-31"),
    exposure = 1
  ))
  periods_db <- dplyr::tbl(connection, "periods")

  expect_error(
    merge_date_ranges_db(
      periods_db,
      period_start = "period_start",
      period_end = "period_end",
      group_by = "policy_id",
      merge_gap_days = -1
    ),
    "non-negative whole number",
    fixed = TRUE
  )
  expect_error(
    merge_date_ranges_db(
      periods_db,
      period_start = "period_start",
      period_end = "period_end",
      group_by = "policy_id",
      aggregate_cols = "policy_id"
    ),
    "must not contain grouping or period columns",
    fixed = TRUE
  )
})
