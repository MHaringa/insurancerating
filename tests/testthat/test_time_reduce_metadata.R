merged_periods_metadata_fixture <- function() {
  portfolio <- data.frame(
    policy = c("A", "A", "B"),
    coverage = c("Fire", "Fire", "Liability"),
    start_date = as.Date(c("2024-01-01", "2024-01-16", "2024-02-01")),
    end_date = as.Date(c("2024-01-15", "2024-01-31", "2024-02-28"))
  )
  merge_date_ranges(
    portfolio, period_start = "start_date", period_end = "end_date",
    group_by = c("policy", "coverage")
  )
}

test_that("merged periods store argument-aligned metadata without legacy copies", {
  result <- merged_periods_metadata_fixture()

  expect_identical(class(result), c("merged_date_ranges", "reduce", "data.frame"))
  expect_identical(attr(result, "period_start", exact = TRUE), "start_date")
  expect_identical(attr(result, "period_end", exact = TRUE), "end_date")
  expect_identical(attr(result, "group_by", exact = TRUE), c("policy", "coverage"))
  for (attribute in c("begin", "end", "cols")) {
    expect_null(attr(result, attribute, exact = TRUE))
  }

  original <- unserialize(serialize(result, NULL))
  daily <- summary(result)
  expect_named(daily, c("date", "type", "count"))
  expect_setequal(as.character(daily$date),
                  c("2024-01-01", "2024-02-01", "2024-02-29"))
  expect_equal(sum(daily$count[daily$type == "in"]), 2)
  expect_equal(sum(daily$count[daily$type == "out"]), 2)
  expect_identical(result, original)
})

test_that("summary grouping is explicit and independent of stored group_by", {
  result <- merged_periods_metadata_fixture()
  ungrouped <- summary(result, period = "months")
  grouped <- summary(result, coverage, period = "months")
  expect_named(ungrouped, c("month", "type", "count"))
  expect_named(grouped, c("month", "type", "count", "coverage"))
  expect_setequal(grouped$coverage, c("Fire", "Liability"))

  attr(result, "group_by") <- "missing_column"
  expect_identical(summary(result, period = "months"), ungrouped)
  expect_identical(summary(result, coverage, period = "months"), grouped)
  attr(result, "group_by") <- NULL
  expect_identical(summary(result, period = "months"), ungrouped)
})

test_that("summary supports saved legacy metadata and class names", {
  modern <- merged_periods_metadata_fixture()
  legacy <- modern
  attr(legacy, "begin") <- attr(legacy, "period_start")
  attr(legacy, "end") <- attr(legacy, "period_end")
  attr(legacy, "cols") <- attr(legacy, "group_by")
  for (attribute in c("period_start", "period_end", "group_by")) {
    attr(legacy, attribute) <- NULL
  }
  legacy <- unserialize(serialize(legacy, NULL))

  for (classes in list(class(modern), c("reduce", "data.frame"))) {
    class(legacy) <- classes
    original <- unserialize(serialize(legacy, NULL))
    for (period in c("days", "weeks", "months", "quarters", "years")) {
      expect_identical(summary(legacy, period = period),
                       summary(modern, period = period))
      expect_identical(summary(legacy, coverage, period = period),
                       summary(modern, coverage, period = period))
    }
    expect_identical(legacy, original)
  }
})

test_that("summary prefers modern metadata and falls back per date attribute", {
  result <- merged_periods_metadata_fixture()
  expected <- summary(result)
  attr(result, "begin") <- "stale_start"
  attr(result, "end") <- "stale_end"
  expect_identical(summary(result), expected)

  attr(result, "begin") <- "start_date"
  attr(result, "period_start") <- NULL
  expect_identical(summary(result), expected)

  attr(result, "period_start") <- "start_date"
  attr(result, "end") <- "end_date"
  attr(result, "period_end") <- NULL
  expect_identical(summary(result), expected)
})

test_that("summary reports missing and invalid date metadata clearly", {
  result <- merged_periods_metadata_fixture()
  for (attribute in c("period_start", "period_end")) {
    broken <- result
    attr(broken, attribute) <- NULL
    attr(broken, paste0(attribute, "_extra")) <- "start_date"
    expect_error(summary(broken), paste0("Missing `", attribute, "` metadata"),
                 fixed = TRUE)

    legacy_attribute <- if (attribute == "period_start") "begin" else "end"
    attr(broken, legacy_attribute) <- attr(result, attribute)
    for (value in list(NA_character_, "", character(), c("a", "b"), 1,
                       list("start_date"), matrix("start_date"))) {
      attr(broken, attribute) <- value
      expect_error(summary(broken),
                   paste0("The `", attribute, "` attribute must name one date column"),
                   fixed = TRUE)
    }
  }
})

test_that("summary reports missing date columns and supports updated metadata", {
  result <- merged_periods_metadata_fixture()
  for (attribute in c("period_start", "period_end")) {
    column <- attr(result, attribute)
    renamed <- result
    names(renamed)[names(renamed) == column] <- "renamed_date"
    expect_error(
      summary(renamed),
      paste0("The `", attribute, "` attribute refers to column `", column,
             "`, which is missing"),
      fixed = TRUE
    )
    attr(renamed, attribute) <- "renamed_date"
    expect_identical(summary(renamed), summary(result))

    removed <- result
    removed[[column]] <- NULL
    expect_error(summary(removed), "which is missing", fixed = TRUE)

    not_date <- result
    not_date[[column]] <- as.character(not_date[[column]])
    expect_error(summary(not_date),
                 paste0("Column `", column, "` named by `", attribute,
                        "` must be a Date column"),
                 fixed = TRUE)
  }
})
