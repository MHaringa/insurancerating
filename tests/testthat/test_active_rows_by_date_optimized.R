test_that("active matches preserve event and portfolio order", {
  origin <- as.Date("2024-01-01")
  portfolio <- data.table::data.table(
    begin = origin + c(10, 0, 0, 0, 4, 4),
    policy = factor(c(rep("A", 4), "B", "B"), levels = c("B", "A")),
    end = origin + c(20, 17, 15, 15, 9, 9),
    period_id = 1:6,
    details = as.list(letters[1:6])
  )
  events <- data.table::data.table(
    event_id = 1:5,
    policy = factor(c("B", "A", "C", "A", "A"), levels = c("C", "A", "B")),
    claim_day = origin + c(4, 15, 0, 0, 21)
  )
  data.table::setindexv(portfolio, "policy")
  data.table::setkeyv(events, "event_id")
  original_portfolio <- data.table::copy(portfolio)
  original_events <- data.table::copy(events)

  all <- active_rows_by_date(
    portfolio, events, "begin", "end", "claim_day", by = "policy",
    unmatched = "keep"
  )
  expect_identical(names(all),
                   c("policy", "begin", "end", "period_id", "details", "event_id", "claim_day"))
  expect_identical(all$event_id, rep(1:5, c(2, 4, 1, 3, 1)))
  expect_identical(all$period_id, c(5L, 6L, 1:4, NA_integer_, 2:4, NA_integer_))
  expect_identical(all$policy, events$policy[all$event_id])
  expect_identical(all$claim_day, events$claim_day[all$event_id])
  expect_identical(all$details, portfolio$details[all$period_id])

  first <- active_rows_by_date(
    portfolio, events, "begin", "end", "claim_day", by = "policy",
    unmatched = "keep", multiple_matches = "first"
  )
  last <- active_rows_by_date(
    portfolio, events, "begin", "end", "claim_day", by = "policy",
    multiple_matches = "last"
  )
  expect_identical(first$event_id, 1:5)
  expect_identical(first$period_id, c(5L, 3L, NA_integer_, 3L, NA_integer_))
  expect_identical(last$event_id, c(1L, 2L, 4L))
  expect_identical(last$period_id, c(6L, 1L, 2L))
  expect_identical(portfolio, original_portfolio)
  expect_identical(events, original_events)
})

test_that("date-only joins handle empty inputs and ordinary column names", {
  origin <- as.Date("2024-01-01")
  portfolio <- data.frame(
    begin = origin + c(1, 4), end = origin + c(3, 6), period_id = 1:2
  )
  events <- data.frame(date = origin + c(6, 0, 1), event_id = 1:3)

  matched <- active_rows_by_date(portfolio, events, "begin", "end", "date")
  expect_identical(matched$event_id, c(1L, 3L))
  expect_identical(matched$period_id, c(2L, 1L))
  expect_identical(matched$date, events$date[c(1L, 3L)])

  no_history <- active_rows_by_date(
    portfolio[0L, ], events, "begin", "end", "date", unmatched = "keep"
  )
  expect_identical(no_history$event_id, events$event_id)
  expect_identical(no_history$begin, as.Date(rep(NA_real_, 3L), origin = origin))
  expect_identical(
    active_rows_by_date(portfolio, events[0L, ], "begin", "end", "date"),
    matched[0L, ]
  )
})
