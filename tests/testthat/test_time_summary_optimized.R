summary_periods_fixture <- function() {
  portfolio <- data.frame(
    policy = seq_len(8L),
    coverage = factor(c("Fire", "Liability", "Fire", NA, "Fire", "Fire", "Liability", NA),
                      levels = c("Liability", "Fire", "Unused")),
    region = c("North", "South", NA, "North", "South", "North", "South", "North"),
    start_date = as.Date(c("2019-12-31", "2020-01-01", "2020-02-28", "2020-02-29",
                          "2020-03-31", "2020-12-31", "2020-01-01", "2020-02-29")),
    end_date = as.Date(c("2020-01-31", "2020-02-29", "2020-03-31", "2020-04-30",
                        "2020-12-31", "2021-01-31", "2020-02-29", "2020-04-30"))
  )
  merge_date_ranges(
    portfolio, period_start = "start_date", period_end = "end_date",
    group_by = c("policy", "coverage", "region"),
    allow_na_in = c("coverage", "region")
  )
}

# Build one event per row, independently of the date-first aggregation path.
reference_period_summary <- function(object, groups = character(), period = "days",
                                     name = "count") {
  in_date <- object$start_date
  shift <- switch(period, days = lubridate::days(1), weeks = lubridate::weeks(1),
                  months = lubridate::period(month = 1), quarters = lubridate::period(month = 3),
                  years = lubridate::days(0))
  out_date <- lubridate::`%m+%`(object$end_date, shift)
  dates <- c(in_date, out_date)
  bucket <- switch(
    period,
    days = dates,
    weeks = sprintf("%dW%02d", data.table::year(dates), data.table::week(dates)),
    months = sprintf("%dM%02d", data.table::year(dates), data.table::month(dates)),
    quarters = paste0(data.table::year(dates), "Q", data.table::quarter(dates)),
    years = data.table::year(dates)
  )
  period_column <- switch(period, days = "date", weeks = "week", months = "month",
                          quarters = "quarter", years = "year")
  # Empty week/month columns retain their intermediate Date type.
  if (!length(dates) && period %in% c("weeks", "months")) bucket <- dates
  events <- data.table::data.table(bucket = bucket, type = rep(c("in", "out"), each = nrow(object)))
  data.table::setnames(events, "bucket", period_column)
  for (group in groups) data.table::set(events, j = group, value = rep(object[[group]], 2L))
  result <- events[, list(count = .N), by = c(period_column, "type", groups)]
  if (length(groups)) {
    data.table::setcolorder(result, c(period_column, "type", "count", groups))
    data.table::setorderv(result, c(period_column, groups, "type"),
                          c(-1L, rep(1L, length(groups)), 1L))
  } else {
    # Preserve the existing count-based ordering of ungrouped summaries.
    data.table::setorderv(result, c("count", "type"), c(-1L, 1L))
  }
  data.table::setnames(result, "count", name)
  as.data.frame(result)
}

with_legacy_summary_weeks <- function(code) {
  old <- options(datatable.week = "legacy")
  on.exit(options(old), add = TRUE)
  force(code)
}

test_that("slim summaries preserve movement counts, periods and ordering", with_legacy_summary_weeks({
  object <- summary_periods_fixture()
  for (period in c("days", "weeks", "months", "quarters", "years")) {
    expect_identical(summary(object, period = period),
                     reference_period_summary(object, period = period))
    expect_identical(summary(object, coverage, period = period),
                     reference_period_summary(object, "coverage", period))
    expect_identical(summary(object, coverage, region, period = period),
                     reference_period_summary(object, c("coverage", "region"), period))
    expect_identical(summary(object, policy, period = period, name = "movements"),
                     reference_period_summary(object, "policy", period, "movements"))
    expect_identical(summary(object, period = sub("s$", "", period)),
                     summary(object, period = period))
  }
}))

test_that("unused wide columns do not affect summaries or get mutated", with_legacy_summary_weeks({
  narrow <- summary_periods_fixture()
  wide <- narrow
  for (i in seq_len(100L)) wide[[paste0("unused_", i)]] <- seq_len(nrow(wide)) + i
  wide$amount <- rep(1e6, nrow(wide))
  wide$detail <- rep(list(list(note = "unused", values = 1:3)), nrow(wide))
  wide$count <- wide$type <- wide$month <- rep("unused", nrow(wide))
  original <- unserialize(serialize(wide, NULL))

  for (period in c("days", "weeks", "months", "quarters", "years")) {
    expect_identical(summary(wide, period = period), summary(narrow, period = period))
    expect_identical(summary(wide, coverage, region, period = period),
                     summary(narrow, coverage, region, period = period))
    expect_identical(wide, original)
  }
  expect_identical(summary(wide, coverage)$coverage,
                   summary(narrow, coverage)$coverage)
}))

test_that("slim summaries retain empty result schemas", {
  object <- summary_periods_fixture()[FALSE, ]
  original <- unserialize(serialize(object, NULL))
  for (period in c("days", "weeks", "months", "quarters", "years")) {
    expect_identical(summary(object, period = period),
                     reference_period_summary(object, period = period))
    expect_identical(summary(object, coverage, region, period = period),
                     reference_period_summary(object, c("coverage", "region"), period))
  }
  expect_identical(object, original)
})

test_that("slim summaries still reject missing groups and invalid periods", {
  object <- summary_periods_fixture()
  original <- unserialize(serialize(object, NULL))
  expect_error(summary(object, missing_group))
  expect_error(summary(object, period = "decades"), "period is not valid")
  expect_identical(object, original)
})

test_that("summary grouping columns can share internal variable names", with_legacy_summary_weeks({
  object <- summary_periods_fixture()
  object$by_begin <- object$coverage
  object$by_end <- object$region
  original <- unserialize(serialize(object, NULL))
  for (period in c("days", "weeks", "months", "quarters", "years")) {
    expect_identical(summary(object, by_begin, period = period),
                     reference_period_summary(object, "by_begin", period))
    expect_identical(summary(object, by_end, period = period),
                     reference_period_summary(object, "by_end", period))
    expect_identical(summary(object, by_begin, by_end, period = period),
                     reference_period_summary(object, c("by_begin", "by_end"), period))
  }
  expect_identical(object, original)
}))
