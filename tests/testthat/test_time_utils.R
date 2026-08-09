library(insurancerating)
context("time utilities")

test_that("split_periods_to_months prorates values and does not mutate input", {
  portfolio <- data.frame(
    policy = c("A", "B"),
    begin = as.Date(c("2020-01-15", "2020-02-01")),
    end = as.Date(c("2020-03-14", "2020-02-29")),
    exposure = c(2, 1),
    premium = c(200, 100)
  )
  original <- portfolio

  result <- split_periods_to_months(
    portfolio,
    period_start = "begin",
    period_end = "end",
    prorate_cols = c("exposure", "premium")
  )

  expect_equal(portfolio, original)
  expect_s3_class(result, "data.frame")
  expect_false(data.table::is.data.table(result))
  expect_named(result, c("id", names(portfolio)))
  expect_equal(nrow(result), 4)
  expect_equal(sum(result$exposure[result$id == 1]), 2)
  expect_equal(sum(result$premium[result$id == 1]), 200)
  expect_true(all(result$begin <= result$end))
})

test_that("split_periods_to_months uses data.table internally without leaking it", {
  portfolio <- data.table::data.table(
    policy = "A",
    begin = as.Date("2020-01-15"),
    end = as.Date("2020-03-14"),
    premium = 300
  )
  original <- data.table::copy(portfolio)

  result <- split_periods_to_months(
    portfolio,
    period_start = "begin",
    period_end = "end",
    prorate_cols = "premium"
  )

  expect_identical(portfolio, original)
  expect_s3_class(result, "data.frame")
  expect_false(data.table::is.data.table(result))
  expect_equal(sum(result$premium), 300)
})

test_that("split_periods_to_months returns a valid empty data frame", {
  portfolio <- data.frame(
    policy = character(),
    begin = as.Date(character()),
    end = as.Date(character()),
    premium = numeric()
  )

  result <- split_periods_to_months(
    portfolio,
    period_start = "begin",
    period_end = "end",
    prorate_cols = "premium"
  )

  expect_s3_class(result, "data.frame")
  expect_false(data.table::is.data.table(result))
  expect_named(result, c("id", names(portfolio)))
  expect_equal(nrow(result), 0)
})

test_that("split_periods_to_months retains deprecated named df compatibility", {
  portfolio <- data.frame(
    begin = as.Date("2020-01-01"),
    end = as.Date("2020-01-31")
  )

  expect_warning(
    result <- split_periods_to_months(
      df = portfolio,
      period_start = "begin",
      period_end = "end"
    ),
    "deprecated"
  )
  expect_s3_class(result, "data.frame")
  expect_error(
    split_periods_to_months(
      data = portfolio,
      df = portfolio,
      period_start = "begin",
      period_end = "end"
    ),
    "Use only one of `data` and deprecated `df`.",
    fixed = TRUE
  )
})

test_that("split_periods_to_months validates inputs", {
  portfolio <- data.frame(
    begin = as.Date("2020-01-01"),
    end = as.Date("2020-01-31"),
    exposure = 1,
    segment = "A"
  )

  expect_error(
    split_periods_to_months(portfolio, period_start = "missing",
                            period_end = "end"),
    "`period_start` and `period_end` not found in data",
    fixed = TRUE
  )
  expect_error(
    split_periods_to_months(
      transform(portfolio, begin = as.character(begin)),
      period_start = "begin",
      period_end = "end"
    ),
    "`period_start` and `period_end` must refer to Date columns.",
    fixed = TRUE
  )
  expect_error(
    split_periods_to_months(
      transform(portfolio, begin = as.Date("2020-02-01")),
      period_start = "begin",
      period_end = "end"
    ),
    "`period_start` must be on or before `period_end` for every row.",
    fixed = TRUE
  )
  expect_error(
    split_periods_to_months(portfolio, period_start = "begin",
                            period_end = "end", prorate_cols = "segment"),
    "`prorate_cols` must be numeric: segment.",
    fixed = TRUE
  )
})

test_that("period_to_months remains available as deprecated wrapper", {
  portfolio <- data.frame(
    begin = as.Date("2020-01-01"),
    end = as.Date("2020-01-31"),
    exposure = 1
  )

  expect_warning(
    result <- period_to_months(portfolio, begin, end, exposure),
    "deprecated"
  )
  expect_equal(sum(result$exposure), 1)
})

test_that("merge_date_ranges merges ranges and aggregates without mutation", {
  portfolio <- data.frame(
    policy = c("A", "A", "A", "B"),
    begin = as.Date(c("2020-01-01", "2020-02-02", "2020-04-01", "2020-01-01")),
    end = as.Date(c("2020-01-31", "2020-02-28", "2020-04-30", "2020-01-31")),
    premium = c(100, 200, 300, 50)
  )
  original <- portfolio

  result <- merge_date_ranges(
    portfolio,
    period_start = "begin",
    period_end = "end",
    group_by = "policy",
    aggregate_cols = "premium",
    merge_gap_days = 5
  )

  expect_equal(portfolio, original)
  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "reduce")
  expect_s3_class(result, "merged_date_ranges")
  expect_false(data.table::is.data.table(result))
  expect_equal(nrow(result), 3)
  expect_equal(result$premium[result$policy == "A" & result$begin == as.Date("2020-01-01")], 300)
  expect_equal(attr(result, "begin"), "begin")
  expect_equal(attr(result, "end"), "end")
  expect_equal(attr(result, "cols"), "policy")
})

test_that("merge_date_ranges uses direct adjacency as its default boundary", {
  portfolio <- data.frame(
    policy = rep("A", 4),
    begin = as.Date(c(
      "2020-01-01", "2020-02-01", "2020-03-02", "2020-04-02"
    )),
    end = as.Date(c(
      "2020-01-31", "2020-02-29", "2020-03-31", "2020-04-30"
    ))
  )

  default_result <- merge_date_ranges(
    portfolio,
    period_start = "begin",
    period_end = "end",
    group_by = "policy"
  )
  overlap_only <- merge_date_ranges(
    portfolio,
    period_start = "begin",
    period_end = "end",
    group_by = "policy",
    merge_gap_days = 0
  )

  expect_equal(nrow(default_result), 3)
  expect_equal(default_result$begin[1], as.Date("2020-01-01"))
  expect_equal(default_result$end[1], as.Date("2020-02-29"))
  expect_equal(nrow(overlap_only), 4)
})

test_that("merge_date_ranges applies the gap boundary strictly", {
  portfolio <- data.frame(
    policy = rep("A", 3),
    begin = as.Date(c("2020-01-01", "2020-02-05", "2020-03-10")),
    end = as.Date(c("2020-01-31", "2020-02-29", "2020-03-31"))
  )

  result <- merge_date_ranges(
    portfolio,
    period_start = "begin",
    period_end = "end",
    group_by = "policy",
    merge_gap_days = 5
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$end[1], as.Date("2020-02-29"))
})

test_that("merge_date_ranges handles overlapping and nested periods", {
  portfolio <- data.frame(
    policy = rep("A", 4),
    begin = as.Date(c("2020-01-01", "2020-01-10", "2020-02-01", "2020-04-02")),
    end = as.Date(c("2020-03-31", "2020-01-20", "2020-02-28", "2020-04-30")),
    premium = c(100, 20, 50, 75)
  )

  result <- merge_date_ranges(
    portfolio,
    period_start = "begin",
    period_end = "end",
    group_by = "policy",
    aggregate_cols = "premium"
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$begin[1], as.Date("2020-01-01"))
  expect_equal(result$end[1], as.Date("2020-03-31"))
  expect_equal(result$premium, c(170, 75))
})

test_that("merge_date_ranges supports multiple groups and equal start dates", {
  portfolio <- data.frame(
    policy = c("A", "A", "B", "B"),
    coverage = c("Fire", "Fire", "Liability", "Liability"),
    begin = as.Date(c("2020-01-01", "2020-01-01", "2020-02-01", "2020-03-01")),
    end = as.Date(c("2020-01-15", "2020-01-31", "2020-02-29", "2020-03-31"))
  )

  result <- merge_date_ranges(
    portfolio,
    period_start = "begin",
    period_end = "end",
    group_by = c("policy", "coverage")
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$end[result$policy == "A"], as.Date("2020-01-31"))
  expect_equal(result$end[result$policy == "B"], as.Date("2020-03-31"))
})

test_that("merge_date_ranges returns a valid empty result", {
  portfolio <- data.frame(
    policy = character(),
    begin = as.Date(character()),
    end = as.Date(character()),
    premium = numeric()
  )

  result <- merge_date_ranges(
    portfolio,
    period_start = "begin",
    period_end = "end",
    group_by = "policy",
    aggregate_cols = "premium"
  )

  expect_s3_class(result, "merged_date_ranges")
  expect_equal(nrow(result), 0)
  expect_named(result, c("policy", "begin", "end", "premium"))
})

test_that("merge_date_ranges accepts aggregation functions without na.rm", {
  portfolio <- data.frame(
    policy = c("A", "A"),
    begin = as.Date(c("2020-01-01", "2020-02-01")),
    end = as.Date(c("2020-01-31", "2020-02-29")),
    premium = c(100, NA)
  )

  result <- merge_date_ranges(
    portfolio,
    period_start = "begin",
    period_end = "end",
    group_by = "policy",
    aggregate_cols = "premium",
    aggregate_fun = length
  )

  expect_equal(result$premium, 1)
})

test_that("merge_date_ranges validates inputs", {
  portfolio <- data.frame(
    policy = "A",
    begin = as.Date("2020-01-01"),
    end = as.Date("2020-01-31"),
    premium = 100,
    segment = "x"
  )

  expect_error(
    merge_date_ranges(portfolio, period_start = "begin", period_end = "end"),
    "`group_by` must contain at least one column name.",
    fixed = TRUE
  )
  expect_error(
    merge_date_ranges(portfolio, period_start = "begin", period_end = "end",
                      group_by = "policy", aggregate_cols = "segment"),
    "`aggregate_cols` must be numeric: segment.",
    fixed = TRUE
  )
  expect_error(
    merge_date_ranges(portfolio, period_start = "begin", period_end = "end",
                      group_by = "policy", merge_gap_days = -1),
    "`merge_gap_days` must be a non-negative whole number.",
    fixed = TRUE
  )
  expect_error(
    merge_date_ranges(
      transform(portfolio, policy = NA_character_),
      period_start = "begin",
      period_end = "end",
      group_by = "policy"
    ),
    "missing values in `group_by`: policy (1)",
    fixed = TRUE
  )
})

test_that("merge_date_ranges retains deprecated named df compatibility", {
  portfolio <- data.frame(
    policy = c("A", "A"),
    begin = as.Date(c("2020-01-01", "2020-02-01")),
    end = as.Date(c("2020-01-31", "2020-02-29"))
  )

  expect_warning(
    result <- merge_date_ranges(
      df = portfolio,
      period_start = "begin",
      period_end = "end",
      group_by = "policy"
    ),
    "deprecated"
  )
  expect_s3_class(result, "data.frame")
  expect_false(data.table::is.data.table(result))
  expect_error(
    merge_date_ranges(
      data = portfolio,
      df = portfolio,
      period_start = "begin",
      period_end = "end",
      group_by = "policy"
    ),
    "Use only one of `data` and deprecated `df`.",
    fixed = TRUE
  )
})

test_that("deprecated reduce-style merge_date_ranges call still works", {
  portfolio <- data.frame(
    policy = c("A", "A"),
    begin = as.Date(c("2020-01-01", "2020-02-01")),
    end = as.Date(c("2020-01-31", "2020-02-28")),
    premium = c(100, 200)
  )

  expect_warning(
    result <- merge_date_ranges(
      portfolio,
      begin = begin,
      end = end,
      policy,
      agg_cols = list(premium),
      min.gapwidth = 5
    ),
    "deprecated"
  )
  expect_s3_class(result, "reduce")
  expect_s3_class(result, "merged_date_ranges")
  expect_equal(result$premium, 300)
})

test_that("summary.reduce returns movement counts", {
  portfolio <- data.frame(
    policy = c("A", "B"),
    begin = as.Date(c("2020-01-01", "2020-02-01")),
    end = as.Date(c("2020-01-31", "2020-02-28"))
  )
  reduced <- merge_date_ranges(
    portfolio,
    period_start = "begin",
    period_end = "end",
    group_by = "policy"
  )

  result <- summary(reduced, period = "months", name = "policies")

  expect_s3_class(result, "data.frame")
  expect_true("policies" %in% names(result))
  expect_true(all(result$type %in% c("in", "out")))
})

test_that("active_rows_by_date matches events to active portfolio rows", {
  portfolio <- data.frame(
    policy = c("A", "A"),
    begin = as.Date(c("2020-01-01", "2020-03-01")),
    end = as.Date(c("2020-01-31", "2020-03-31")),
    premium = c(100, 300)
  )
  original <- portfolio
  claims <- data.frame(
    policy = c("A", "A"),
    claim_date = as.Date(c("2020-01-15", "2020-02-15")),
    claim_id = 1:2
  )

  result <- active_rows_by_date(
    portfolio,
    claims,
    period_start = "begin",
    period_end = "end",
    date = "claim_date",
    by = "policy",
    unmatched = "keep"
  )

  expect_equal(portfolio, original)
  expect_s3_class(result, "data.frame")
  expect_false(data.table::is.data.table(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$premium[result$claim_id == 1], 100)
  expect_true(is.na(result$premium[result$claim_id == 2]))
})

test_that("active_rows_by_date validates inputs", {
  portfolio <- data.frame(
    policy = "A",
    begin = as.Date("2020-01-01"),
    end = as.Date("2020-01-31")
  )
  dates <- data.frame(policy = "A", claim_date = as.Date("2020-01-15"))

  expect_error(
    active_rows_by_date(portfolio, dates, period_start = "begin",
                        period_end = "end", date = "missing"),
    "`date` not found in data",
    fixed = TRUE
  )
  expect_error(
    active_rows_by_date(portfolio, transform(dates, claim_date = as.Date(NA)),
                        period_start = "begin", period_end = "end",
                        date = "claim_date"),
    "`dates` must not contain missing values in `date`.",
    fixed = TRUE
  )
  expect_error(
    active_rows_by_date(portfolio, dates, period_start = "begin",
                        period_end = "end", date = "claim_date",
                        by = "missing"),
    "`by` not found in data",
    fixed = TRUE
  )
  expect_error(
    active_rows_by_date(portfolio, dates, period_start = "begin",
                        period_end = "end", date = "claim_date",
                        multiple_matches = "many"),
    "arg"
  )
})

test_that("active_rows_by_date preserves event order", {
  portfolio <- data.table::data.table(
    policy = c("A", "A"),
    begin = as.Date(c("2020-01-01", "2020-03-01")),
    end = as.Date(c("2020-01-31", "2020-03-31")),
    premium = c(100, 300)
  )
  original <- data.table::copy(portfolio)
  dates <- data.table::data.table(
    policy = c("A", "A"),
    claim_date = as.Date(c("2020-03-15", "2020-01-15")),
    claim_id = c(2L, 1L)
  )

  result <- active_rows_by_date(
    portfolio,
    dates,
    period_start = "begin",
    period_end = "end",
    date = "claim_date",
    by = "policy"
  )

  expect_identical(portfolio, original)
  expect_false(data.table::is.data.table(result))
  expect_identical(result$claim_id, c(2L, 1L))
})

test_that("active_rows_by_date validates matching identifiers and collisions", {
  portfolio <- data.frame(
    policy = "A",
    begin = as.Date("2020-01-01"),
    end = as.Date("2020-01-31"),
    status = "active"
  )
  dates <- data.frame(
    policy = "A",
    claim_date = as.Date("2020-01-15"),
    status = "reported"
  )

  expect_error(
    active_rows_by_date(
      transform(portfolio, policy = NA_character_),
      dates,
      period_start = "begin",
      period_end = "end",
      date = "claim_date",
      by = "policy"
    ),
    "must not contain missing values"
  )
  expect_error(
    active_rows_by_date(
      portfolio,
      dates,
      period_start = "begin",
      period_end = "end",
      date = "claim_date",
      by = "policy"
    ),
    "Conflicting columns: status",
    fixed = TRUE
  )
})

test_that("active_rows_by_date retains deprecated join arguments", {
  portfolio <- data.frame(
    policy = "A",
    begin = as.Date("2020-01-01"),
    end = as.Date("2020-01-31")
  )
  dates <- data.frame(
    policy = c("A", "A"),
    claim_date = as.Date(c("2020-01-15", "2020-02-15"))
  )

  expect_warning(
    result <- active_rows_by_date(
      portfolio,
      dates,
      period_start = "begin",
      period_end = "end",
      date = "claim_date",
      by = "policy",
      nomatch = NA,
      mult = "all"
    ),
    "deprecated"
  )
  expect_equal(nrow(result), 2)
  expect_error(
    active_rows_by_date(
      portfolio,
      dates,
      period_start = "begin",
      period_end = "end",
      date = "claim_date",
      by = "policy",
      unmatched = "keep",
      nomatch = NA
    ),
    "Use only one of `unmatched` and deprecated `nomatch`.",
    fixed = TRUE
  )
})

test_that("rows_per_date remains available as deprecated wrapper", {
  portfolio <- data.frame(
    policy = "A",
    begin = as.Date("2020-01-01"),
    end = as.Date("2020-01-31"),
    premium = 100
  )
  claims <- data.frame(policy = "A", claim_date = as.Date("2020-01-15"))

  expect_warning(
    result <- rows_per_date(portfolio, claims, df_begin = begin,
                            df_end = end, dates_date = claim_date, policy),
    "deprecated"
  )
  expect_equal(result$premium, 100)
})
