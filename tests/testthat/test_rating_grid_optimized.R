test_that("wide and keyed portfolios produce the same grid as selected columns", {
  portfolio <- data.frame(
    by_vars = factor(c("B", "B", "A", NA, "A"), levels = c("A", "B", "C")),
    region = c("North", "North", "South", "North", "South"),
    year = c(2024, 2025, 2024, 2025, 2024),
    exposure = c(1, 0.5, NA, 1, 0.5),
    claims = c(2L, 1L, NA_integer_, 0L, 2L)
  )
  keys <- c("by_vars", "region")
  wide <- data.table::as.data.table(portfolio)
  data.table::set(wide, j = "unused", value = list(1L, 1:2, NULL, 3L, 1:3))
  data.table::setkeyv(wide, "year")
  data.table::setindexv(wide, "region")
  original <- data.table::copy(wide)

  for (drop_na in c(FALSE, TRUE)) {
    for (split in list(NULL, "year")) {
      for (exposure in list(NULL, "exposure")) {
        expect_identical(
          rating_grid(wide, keys, exposure, split, "claims", drop_na),
          rating_grid(portfolio, keys, exposure, split, "claims", drop_na)
        )
      }
    }
  }
  expect_identical(wide, original)
  expect_identical(
    rating_grid(wide, group_by = character()),
    data.frame(count = 5L)
  )
  expect_identical(
    rating_grid(wide[0L], group_by = character()),
    data.frame(count = 0L)
  )
})

test_that("splitting exposure preserves unsplit totals and missing cells", {
  portfolio <- data.frame(
    sector = c("B", "A", "A", "B"),
    year = c(2024L, 2024L, 2025L, 2024L),
    exposure = c(0.5, NA, 1, 1),
    claims = c(1L, 2L, 3L, NA_integer_)
  )

  expect_identical(
    rating_grid(portfolio, "sector", "exposure", "year", "claims"),
    data.frame(
      sector = c("A", "B"), exposure_2024 = c(0, 1.5),
      exposure_2025 = c(1, NA_real_), claims = c(5L, 1L)
    )
  )
})

test_that("column selection retains model offsets and weights", {
  portfolio <- data.frame(
    sector = factor(rep(c("A", "B"), each = 3)),
    claims = c(1L, 0L, 2L, 0L, 3L, 2L),
    exposure = c(1, 0.5, 1, 0.5, 1, 1),
    volume = c(2, 4, 5, 1, 2, 1),
    year = rep(c(2024L, 2025L, 2024L), 2),
    amount = c(20, 0, 70, 0, 50, 40)
  )
  model <- glm(claims ~ sector + offset(log(exposure)), weights = volume,
               family = poisson(), data = portfolio)
  expect_identical(
    rating_grid(model, aggregate_cols = "amount"),
    rating_grid(portfolio, "sector", aggregate_cols = c("amount", "volume", "exposure"))
  )

  model <- glm(claims ~ sector + offset(log(exposure)),
               family = poisson(), data = portfolio)
  expect_identical(
    rating_grid(model, exposure = "exposure", exposure_by = "year"),
    rating_grid(portfolio, "sector", "exposure", "year", "exposure")
  )
})

test_that("matrix columns keep their expanded grouping names", {
  portfolio <- data.frame(exposure = c(1, 2, 3))
  portfolio$bands <- cbind(area = c(2L, 2L, 1L), size = c(1L, 1L, 2L))
  expect_identical(
    rating_grid(portfolio, exposure = "exposure"),
    data.frame(bands.area = 1:2, bands.size = 2:1, exposure = c(3, 3))
  )
})
