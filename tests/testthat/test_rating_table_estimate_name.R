rating_table_name_models <- function() {
  data <- data.frame(
    claims = c(1, 2, 1, 3, 2, 4, 1, 2),
    exposure = rep(1, 8),
    sector = factor(rep(c("A", "B", "C", "D"), 2))
  )
  first <- glm(
    claims ~ sector + offset(log(exposure)),
    family = poisson(), data = data
  )
  second <- glm(
    claims ~ sector + offset(log(exposure)),
    family = quasipoisson(), data = data
  )
  list(data = data, first = first, second = second)
}


testthat::test_that("estimate_name changes one estimate column exactly", {
  fixture <- rating_table_name_models()
  first <- fixture$first
  default <- rating_table(first, exposure = FALSE)
  renamed <- rating_table(
    first,
    exposure = FALSE,
    estimate_name = "relativity"
  )

  testthat::expect_true("est_first" %in% names(default))
  testthat::expect_true("relativity" %in% names(renamed))
  testthat::expect_false(any(grepl("^est_", names(renamed))))
  testthat::expect_equal(
    renamed$relativity,
    default$est_first
  )
  testthat::expect_identical(
    attr(renamed, "estimate_columns"),
    "relativity"
  )
})


testthat::test_that("estimate_name supports multiple models and named vectors", {
  fixture <- rating_table_name_models()
  first <- fixture$first
  second <- fixture$second
  unnamed <- rating_table(
    first, second,
    exposure = FALSE,
    estimate_name = c("frequency", "frequency_alternative")
  )
  named <- rating_table(
    first, second,
    exposure = FALSE,
    estimate_name = c(
      second = "alternative",
      first = "technical"
    )
  )

  testthat::expect_true(all(
    c("frequency", "frequency_alternative") %in% names(unnamed)
  ))
  testthat::expect_identical(
    attr(named, "estimate_columns"),
    c("technical", "alternative")
  )
  testthat::expect_true(all(c("technical", "alternative") %in% names(named)))
})


testthat::test_that("custom estimate names work with significance and ordering", {
  fixture <- rating_table_name_models()
  first <- fixture$first
  second <- fixture$second
  table <- rating_table(
    first, second,
    exposure = FALSE,
    significance = TRUE,
    estimate_name = c("technical", "alternative"),
    order_model = "alternative"
  )

  testthat::expect_true(all(c(
    "technical", "alternative",
    "signif_first", "signif_second"
  ) %in% names(table)))
  testthat::expect_s3_class(autoplot(table), "ggplot")
  if (requireNamespace("gt", quietly = TRUE)) {
    testthat::expect_s3_class(as_gt(table), "gt_tbl")
  }
})


testthat::test_that("estimate_name validates names and collisions", {
  fixture <- rating_table_name_models()
  first <- fixture$first
  second <- fixture$second

  testthat::expect_error(
    rating_table(
      first, second,
      estimate_name = "only_one"
    ),
    "one non-empty name for each"
  )
  testthat::expect_error(
    rating_table(
      first, second,
      estimate_name = c("same", "same")
    ),
    "must be unique"
  )
  testthat::expect_error(
    rating_table(
      first, second,
      estimate_name = c(unknown = "one", first = "two")
    ),
    "named with the supplied model names"
  )
  testthat::expect_error(
    rating_table(first, estimate_name = "risk_factor"),
    "reserved columns"
  )
  testthat::expect_error(
    rating_table(
      first,
      significance = TRUE,
      estimate_name = "signif_first"
    ),
    "conflicts with another"
  )
})
