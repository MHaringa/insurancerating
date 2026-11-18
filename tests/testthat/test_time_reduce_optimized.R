reference_period_merge <- function(data, group_by, aggregate_cols = character(),
                                    gap = 1, fun = sum) {
  d <- data.table::as.data.table(data.table::copy(data))
  data.table::setorderv(d, c(group_by, "begin", "end"))
  groups <- split(seq_len(nrow(d)), data.table::rleidv(d, group_by))
  intervals <- list()
  for (rows in groups) {
    current <- integer()
    last_end <- -Inf
    for (i in rows) {
      if (length(current) && as.numeric(d$begin[i]) - last_end - 1 >= gap) {
        intervals[[length(intervals) + 1L]] <- current
        current <- integer()
        last_end <- -Inf
      }
      current <- c(current, i)
      last_end <- max(last_end, as.numeric(d$end[i]))
    }
    intervals[[length(intervals) + 1L]] <- current
  }
  records <- lapply(intervals, function(rows) {
    fields <- c(lapply(group_by, function(col) d[[col]][rows[1L]]),
                list(min(d$begin[rows]), max(d$end[rows])),
                lapply(aggregate_cols, function(col) {
                  x <- d[[col]][rows]
                  fun(x[!is.na(x)])
                }))
    names(fields) <- c(group_by, "begin", "end", aggregate_cols)
    fields
  })
  out <- as.data.frame(data.table::rbindlist(records))
  attr(out, "begin") <- "begin"
  attr(out, "end") <- "end"
  attr(out, "cols") <- group_by
  class(out) <- c("merged_date_ranges", "reduce", "data.frame")
  out
}

test_that("compact interval construction matches a row-wise reference", {
  set.seed(315)
  d <- data.frame(
    policy = sample(c(NA, 1:12), 250, replace = TRUE),
    coverage = ordered(sample(c("Fire", "Liability", NA), 250, replace = TRUE),
                       levels = c("Liability", "Fire")),
    begin = as.Date("2024-01-01") + sample(0:100, 250, replace = TRUE),
    amount = sample(c(NA_real_, -5:25), 250, replace = TRUE),
    count = sample(c(NA_integer_, 0:10), 250, replace = TRUE)
  )
  d$end <- d$begin + sample(0:50, nrow(d), replace = TRUE)
  for (gap in c(0, 1, 5)) {
    for (fun in list(sum, mean, function(x) sum(x^2), function(x) x[1L])) {
      # Ensure the order-sensitive aggregate has at least one value per island.
      input <- d
      if (!identical(fun, sum)) input$amount[is.na(input$amount)] <- 0
      expected <- reference_period_merge(input, c("policy", "coverage"), "amount", gap, fun)
      actual <- merge_date_ranges(input, period_start = "begin", period_end = "end",
                                  group_by = c("policy", "coverage"),
                                  allow_na_in = c("policy", "coverage"),
                                  aggregate_cols = "amount", aggregate_fun = fun,
                                  merge_gap_days = gap)
      expect_equal(actual, expected)
    }
    expect_equal(
      merge_date_ranges(d, period_start = "begin", period_end = "end",
                        group_by = c("policy", "coverage"),
                        allow_na_in = c("policy", "coverage"), merge_gap_days = gap),
      reference_period_merge(d, c("policy", "coverage"), gap = gap)
    )
  }
})

test_that("wide portfolios preserve all keys, eight sums and the original input", {
  set.seed(311)
  policy <- rep(seq_len(60), each = 4)
  d <- data.frame(policy = policy)
  for (i in seq_len(99)) {
    x <- (policy + i) %% 7
    x[x == 0] <- NA_integer_
    d[[paste0("key_", i)]] <- switch(as.character(i %% 4),
      "0" = factor(x, levels = 6:1), "1" = as.character(x),
      "2" = as.numeric(x), "3" = x > 3)
  }
  keys <- names(d)
  d$begin <- as.Date("2024-01-01") + rep(c(0, 5, 50, 55), 60)
  d$end <- d$begin + 4
  amounts <- paste0("amount_", seq_len(8))
  for (column in amounts) d[[column]] <- sample(c(NA_real_, -10:100), nrow(d), TRUE)
  d$unused <- I(rep(list(list(unused = "not part of the reduction")), nrow(d)))
  d <- d[sample.int(nrow(d)), ]
  expected <- reference_period_merge(d, keys, amounts)
  dt <- data.table::as.data.table(d)
  data.table::setkeyv(dt, "policy")
  for (input in list(d, dt)) {
    before <- data.table::copy(input)
    actual <- merge_date_ranges(input, period_start = "begin", period_end = "end",
                                group_by = keys, allow_na_in = keys,
                                aggregate_cols = amounts)
    expect_equal(actual, expected)
    expect_equal(nrow(actual), 120)
    expect_identical(names(actual), c(keys, "begin", "end", amounts))
    expect_identical(lapply(actual[keys], class), lapply(d[keys], class))
    expect_identical(input, before)
  }
})

test_that("sum handles missing values, infinities and integer promotion", {
  d <- data.frame(policy = rep(1:3, each = 2),
                  begin = as.Date("2024-01-01") + rep(0:1, 3))
  d$end <- d$begin
  d$amount <- c(Inf, -Inf, NA, NaN, 1, 2)
  d$count <- c(NA_integer_, NA_integer_, 1L, 2L, 3L, 4L)
  d$large <- rep(c(.Machine$integer.max, 1L), 3)
  for (fun in list("sum", sum)) {
    expect_no_warning(actual <- merge_date_ranges(
      d, period_start = "begin", period_end = "end", group_by = "policy",
      aggregate_cols = c("amount", "count", "large"), aggregate_fun = fun
    ))
    expect_equal(actual, reference_period_merge(d, "policy", c("amount", "count", "large")))
    expect_identical(actual$count, c(0L, 3L, 7L))
    expect_type(actual$large, "double")
  }
})

test_that("custom aggregates and numeric classes retain the general route", {
  d <- data.frame(policy = c(1, 1, 2, 2), begin = as.Date("2024-01-01") + rep(0:1, 2),
                  amount = c(NA, 3, 5, 7))
  d$end <- d$begin
  calls <- 0L
  custom <- function(x, na.rm = FALSE) {
    expect_true(na.rm)
    calls <<- calls + 1L
    sum(x, na.rm = na.rm) + 10
  }
  actual <- merge_date_ranges(d, period_start = "begin", period_end = "end",
                              group_by = "policy", aggregate_cols = "amount",
                              aggregate_fun = custom)
  expect_identical(calls, 2L)
  expect_equal(actual$amount, c(13, 22))
  actual <- merge_date_ranges(d, period_start = "begin", period_end = "end",
                              group_by = "policy", aggregate_cols = "amount",
                              aggregate_fun = length)
  expect_equal(actual$amount, c(1L, 2L))
  d$amount <- I(d$amount)
  expect_equal(
    merge_date_ranges(d, period_start = "begin", period_end = "end",
                      group_by = "policy", aggregate_cols = "amount"),
    reference_period_merge(d, "policy", "amount")
  )
  expect_error(
    merge_date_ranges(d, period_start = "begin", period_end = "end",
                      group_by = "policy", aggregate_cols = "amount",
                      aggregate_fun = function(x) c(1, 2)),
    "one value per merged interval; column `amount`"
  )
})

test_that("internal names do not conflict with user columns and Date subclasses survive", {
  d <- data.frame(group_id = c("A", "A", "B"), interval = c(NA, NA, "X"),
                  begin = data.table::as.IDate("2024-01-01") + c(0, 1, 0),
                  row_id = c(2, 3, 4), value_1 = c(5, 6, 7), check.names = FALSE)
  d$end <- d$begin
  d[["net premium"]] <- c(1, 2, 3)
  d[[".merge_previous_end"]] <- c(1, 2, 3)
  d[[".merge_interval"]] <- c(4, 5, 6)
  keys <- c("group_id", "interval")
  amounts <- c("row_id", "value_1", "net premium", ".merge_previous_end", ".merge_interval")
  actual <- merge_date_ranges(d, period_start = "begin", period_end = "end",
                              group_by = keys, allow_na_in = "interval", aggregate_cols = amounts)
  expect_equal(actual, reference_period_merge(d, keys, amounts))
  expect_identical(class(actual$begin), class(d$begin))
  expect_identical(class(actual$end), class(d$end))
  expect_identical(typeof(actual$begin), typeof(d$begin))
})

test_that("missing numeric key values retain data.table grouping semantics", {
  d <- data.frame(key = rep(c(NA_real_, NaN, 1), each = 2),
                  begin = as.Date("2024-01-01") + rep(0:1, 3), amount = 1:6)
  d$end <- d$begin
  actual <- merge_date_ranges(d, period_start = "begin", period_end = "end",
                              group_by = "key", allow_na_in = "key", aggregate_cols = "amount")
  expect_identical(nrow(actual), 3L)
  expect_equal(actual, reference_period_merge(d, "key", "amount"))
})
