#' @noRd
.db_check_packages <- function(duckdb = FALSE) {
  rlang::check_installed(
    c("DBI", "dbplyr"),
    reason = "to construct lazy portfolio-reduction queries."
  )
  if (duckdb) {
    rlang::check_installed(
      "duckdb",
      reason = "to merge portfolio date ranges in the database."
    )
  }
}

#' @noRd
.db_is_lazy_table <- function(x) {
  inherits(x, c("tbl_lazy", "tbl_sql"))
}

#' @noRd
.db_validate_lazy_table <- function(x, argument = "`x`") {
  if (!.db_is_lazy_table(x)) {
    stop(
      argument, " must be a lazy database table created with dplyr::tbl().",
      call. = FALSE
    )
  }
}

#' @noRd
.db_validate_columns <- function(x, columns, argument) {
  if (is.null(columns)) {
    return(invisible(NULL))
  }
  if (!is.character(columns) || anyNA(columns) || any(columns == "")) {
    stop(argument, " must contain column names as character strings.",
         call. = FALSE)
  }
  missing_columns <- setdiff(columns, colnames(x))
  if (length(missing_columns) > 0L) {
    stop(
      argument,
      " contains columns that are not present in the database table: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Reduce a database portfolio to observed rating-grid points
#'
#' @description
#' Construct the same type of observed rating-factor combinations as
#' [rating_grid()], while leaving the calculation in the database. The function
#' returns a lazy query and does not copy the source portfolio into R.
#' Each output row is a model point: one observed covariate combination together
#' with aggregated exposure and other additive quantities requested by the user.
#'
#' This is useful when the row-level portfolio is too large for available R
#' memory. The database performs the grouping and aggregation; the reduced
#' result can subsequently be imported with [dplyr::collect()].
#'
#' @param x A lazy database table created with [dplyr::tbl()].
#' @param group_by Character vector containing the columns that define an
#'   observed rating-grid point.
#' @param exposure Optional character string naming an exposure column to sum.
#'   If `NULL`, the output contains a `count` column with the number of source
#'   records in each combination.
#' @param aggregate_cols Optional character vector naming additional columns to
#'   sum within each combination.
#' @param drop_na Logical. If `TRUE`, records with missing `group_by` values are
#'   excluded. If `FALSE`, missing values remain an observed group.
#' @param exposure_by Reserved for consistency with [rating_grid()]. Splitting
#'   an exposure into dynamically named wide columns is not performed in a lazy
#'   database query. Include this variable in `group_by`, collect the reduced
#'   long table, and reshape it in R instead.
#'
#' @details
#' `rating_grid_db()` performs only operations that translate naturally to SQL:
#' grouping, row counting and sums. Column names are supplied as strings. No SQL
#' is executed until the lazy query is printed, collected or otherwise used by
#' the database backend.
#'
#' Database tables have no inherent row order. Apply [dplyr::arrange()] after
#' the final database operation when a specific presentation order is required.
#'
#' The database table should already contain the rating variables required for
#' the intended model or portfolio analysis. Unlike [rating_grid()], this
#' function does not inspect a fitted R model or refinement metadata because
#' those objects are held in R rather than in the database.
#'
#' @return A lazy database table. Use [dbplyr::sql_render()] to inspect the SQL
#'   or [dplyr::collect()] to import the reduced result into R.
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(duckdb::duckdb())
#' portfolio_db <- dplyr::tbl(con, "portfolio")
#'
#' grid_db <- rating_grid_db(
#'   portfolio_db,
#'   group_by = c("sector", "region"),
#'   exposure = "earned_exposure",
#'   aggregate_cols = "earned_premium"
#' )
#'
#' dbplyr::sql_render(grid_db)
#' grid <- dplyr::collect(grid_db)
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#'
#' @author Martin Haringa
#' @seealso [rating_grid()], [merge_date_ranges_db()], [merge_date_ranges()]
#' @export
rating_grid_db <- function(x,
                           group_by,
                           exposure = NULL,
                           aggregate_cols = NULL,
                           drop_na = FALSE,
                           exposure_by = NULL) {
  .db_check_packages()
  .db_validate_lazy_table(x)
  .db_validate_columns(x, group_by, "`group_by`")
  .db_validate_columns(x, exposure, "`exposure`")
  .db_validate_columns(x, aggregate_cols, "`aggregate_cols`")

  if (length(group_by) == 0L) {
    stop("`group_by` must contain at least one column name.", call. = FALSE)
  }
  if (!is.null(exposure) && length(exposure) != 1L) {
    stop("`exposure` must be NULL or one column name.", call. = FALSE)
  }
  if (!is.logical(drop_na) || length(drop_na) != 1L || is.na(drop_na)) {
    stop("`drop_na` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(exposure_by)) {
    stop(
      "`exposure_by` is not supported by `rating_grid_db()`. Include that ",
      "column in `group_by`, collect the reduced long table, and reshape it in R.",
      call. = FALSE
    )
  }

  overlapping <- intersect(group_by, c(exposure, aggregate_cols))
  if (length(overlapping) > 0L) {
    stop(
      "Aggregation columns must not also be used in `group_by`: ",
      paste(overlapping, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  query <- x
  if (drop_na) {
    for (column in group_by) {
      query <- dplyr::filter(query, !is.na(.data[[column]]))
    }
  }

  summaries <- list()
  if (is.null(exposure)) {
    summaries$count <- rlang::expr(dplyr::n())
  }
  for (column in setdiff(aggregate_cols, exposure)) {
    summaries[[column]] <- rlang::expr(
      sum(!!rlang::sym(column), na.rm = TRUE)
    )
  }
  if (!is.null(exposure)) {
    summaries[[exposure]] <- rlang::expr(
      sum(!!rlang::sym(exposure), na.rm = TRUE)
    )
  }

  query |>
    dplyr::summarise(
      !!!summaries,
      .by = dplyr::all_of(group_by)
    )
}

#' @noRd
.db_quote_identifier <- function(connection, x) {
  as.character(DBI::dbQuoteIdentifier(connection, x))
}

#' @noRd
.db_duckdb_connection <- function(connection) {
  any(grepl("duckdb", class(connection), ignore.case = TRUE))
}

#' Merge connected portfolio periods in DuckDB
#'
#' @description
#' Construct the temporal portfolio reduction performed by
#' [merge_date_ranges()] as a lazy DuckDB query. Overlapping or adjacent periods
#' are merged inside DuckDB, so only the consolidated periods need to be copied
#' into R.
#'
#' @param data A lazy DuckDB table created with [dplyr::tbl()].
#' @param period_start Character string naming the period-start column.
#' @param period_end Character string naming the period-end column.
#' @param group_by Character vector identifying the policy, risk or segment
#'   within which periods may be combined.
#' @param aggregate_cols Optional character vector naming numeric columns to
#'   aggregate over each merged period.
#' @param aggregate_fun Character string specifying the SQL aggregation for
#'   `aggregate_cols`: `"sum"`, `"mean"`, `"min"`, or `"max"`.
#' @param merge_gap_days Non-negative whole number. The interpretation is the
#'   same as in [merge_date_ranges()]: `1` merges overlapping and directly
#'   adjacent periods, while `0` merges overlapping periods only.
#'
#' @details
#' Merging connected intervals is a SQL gaps-and-islands calculation. The query
#' uses ordered window functions to compare each start date with the latest
#' preceding end date, assigns an interval identifier, and then aggregates each
#' resulting interval.
#'
#' Date arithmetic differs between database systems. This implementation is
#' therefore deliberately restricted to DuckDB. It does not download the
#' source table and does not create a permanent database table.
#'
#' @return A lazy DuckDB table. Use [dbplyr::sql_render()] to inspect the SQL or
#'   [dplyr::collect()] to import the consolidated periods into R.
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(duckdb::duckdb())
#' portfolio_db <- dplyr::tbl(con, "portfolio_periods")
#'
#' periods_db <- merge_date_ranges_db(
#'   portfolio_db,
#'   period_start = "period_start",
#'   period_end = "period_end",
#'   group_by = c("policy_id", "coverage"),
#'   aggregate_cols = c("earned_exposure", "earned_premium")
#' )
#'
#' periods <- dplyr::collect(periods_db)
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#'
#' @author Martin Haringa
#' @seealso [merge_date_ranges()], [rating_grid_db()], [rating_grid()]
#' @export
merge_date_ranges_db <- function(data,
                                 period_start,
                                 period_end,
                                 group_by,
                                 aggregate_cols = NULL,
                                 aggregate_fun = c("sum", "mean", "min", "max"),
                                 merge_gap_days = 1) {
  .db_check_packages(duckdb = TRUE)
  .db_validate_lazy_table(data, "`data`")
  .db_validate_columns(data, c(period_start, period_end),
                       "`period_start` and `period_end`")
  .db_validate_columns(data, group_by, "`group_by`")
  .db_validate_columns(data, aggregate_cols, "`aggregate_cols`")

  if (length(period_start) != 1L || length(period_end) != 1L) {
    stop("`period_start` and `period_end` must each name one column.",
         call. = FALSE)
  }
  if (length(group_by) == 0L) {
    stop("`group_by` must contain at least one column name.", call. = FALSE)
  }
  if (!.time_is_nonnegative_whole_number(merge_gap_days)) {
    stop("`merge_gap_days` must be a non-negative whole number.",
         call. = FALSE)
  }
  aggregate_fun <- match.arg(aggregate_fun)

  identifier_columns <- c(group_by, period_start, period_end)
  overlapping <- intersect(aggregate_cols, identifier_columns)
  if (length(overlapping) > 0L) {
    stop(
      "`aggregate_cols` must not contain grouping or period columns: ",
      paste(overlapping, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  helper_columns <- c(
    ".insurancerating_previous_end",
    ".insurancerating_new_interval",
    ".insurancerating_interval_id"
  )
  conflicts <- intersect(helper_columns, colnames(data))
  if (length(conflicts) > 0L) {
    stop(
      "The database table contains reserved temporary columns: ",
      paste(conflicts, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  connection <- dbplyr::remote_con(data)
  if (!.db_duckdb_connection(connection)) {
    stop(
      "`merge_date_ranges_db()` currently supports DuckDB connections only.",
      call. = FALSE
    )
  }

  quote_id <- function(column) .db_quote_identifier(connection, column)
  group_sql <- paste(vapply(group_by, quote_id, character(1)), collapse = ", ")
  start_sql <- quote_id(period_start)
  end_sql <- quote_id(period_end)
  previous_end <- quote_id(helper_columns[[1L]])
  new_interval <- quote_id(helper_columns[[2L]])
  interval_id <- quote_id(helper_columns[[3L]])
  partition_sql <- paste0("PARTITION BY ", group_sql, " ")
  source_sql <- as.character(dbplyr::sql_render(data))

  aggregate_sql <- character(0)
  if (length(aggregate_cols) > 0L) {
    sql_fun <- switch(
      aggregate_fun,
      sum = "SUM",
      mean = "AVG",
      min = "MIN",
      max = "MAX"
    )
    aggregate_sql <- vapply(aggregate_cols, function(column) {
      quoted <- quote_id(column)
      expression <- paste0(sql_fun, "(", quoted, ")")
      if (identical(aggregate_fun, "sum")) {
        expression <- paste0("COALESCE(", expression, ", 0)")
      }
      paste0(expression, " AS ", quoted)
    }, character(1))
  }

  select_sql <- paste(
    c(
      group_sql,
      paste0("MIN(", start_sql, ") AS ", start_sql),
      paste0("MAX(", end_sql, ") AS ", end_sql),
      aggregate_sql
    ),
    collapse = ",\n    "
  )
  group_result_sql <- paste(c(group_sql, interval_id), collapse = ", ")
  order_result_sql <- paste(c(group_sql, start_sql, end_sql), collapse = ", ")

  query_sql <- paste0(
    "WITH source_data AS (\n", source_sql, "\n),\n",
    "ordered_periods AS (\n",
    "  SELECT *,\n",
    "    MAX(", end_sql, ") OVER (", partition_sql,
    "ORDER BY ", start_sql, ", ", end_sql,
    " ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS ", previous_end, "\n",
    "  FROM source_data\n",
    "),\n",
    "interval_flags AS (\n",
    "  SELECT *,\n",
    "    CASE WHEN ", previous_end, " IS NULL OR ",
    "date_diff('day', ", previous_end, ", ", start_sql, ") - 1 >= ",
    as.integer(merge_gap_days),
    " THEN 1 ELSE 0 END AS ", new_interval, "\n",
    "  FROM ordered_periods\n",
    "),\n",
    "intervals AS (\n",
    "  SELECT *,\n",
    "    SUM(", new_interval, ") OVER (", partition_sql,
    "ORDER BY ", start_sql, ", ", end_sql,
    " ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS ", interval_id, "\n",
    "  FROM interval_flags\n",
    ")\n",
    "SELECT\n    ", select_sql, "\n",
    "FROM intervals\n",
    "GROUP BY ", group_result_sql, "\n",
    "ORDER BY ", order_result_sql
  )

  dplyr::tbl(connection, dbplyr::sql(query_sql))
}
