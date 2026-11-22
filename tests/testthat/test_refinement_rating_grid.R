split_grid_fixture <- function(output_variable = "tariff_segment") {
  data <- data.frame(
    claims = c(8, 10, 12, 14, 18, 16, 9, 11, 15, 20, 22, 18),
    exposure = c(10, 20, 10, 20, 10, 20, 10, 20, 10, 20, 10, 20),
    parent = factor(rep(c("A", "B"), each = 6)),
    detail = rep(c("a1", "a2", "b1", "b2"), each = 3),
    region = factor(rep(c("north", "south"), 6))
  )
  model <- glm(claims ~ parent + region + offset(log(exposure)),
               family = poisson(), data = data)
  fixed <- prepare_refinement(model, data) |>
    add_restriction(c(A = 1, B = 1.6), model_variable = "parent")
  split <- fixed |>
    add_restriction(c(B = 1.1), model_variable = "parent",
                    restriction_type = "multiplier") |>
    add_relativities(
      model_variable = "parent", split_variable = "detail",
      output_variable = output_variable,
      relativities = relativities(split_level("B", c(b1 = 0.8, b2 = 1.2))),
      exposure = "exposure", normalize = FALSE
    )
  shrunk <- add_shrinkage(split, output_variable, credibility = 0.8)
  rebased <- add_rebasing(shrunk, output_variable, reference_level = "b1")
  list(data = data, model = model, fixed = fixed, split = split,
       shrunk = shrunk, rebased = rebased)
}

testthat::test_that("one-to-one restrictions retain their original rating grid", {
  fixture <- split_grid_fixture()
  model <- refit(fixture$fixed)
  before <- predict(model, type = "response")
  testthat::expect_warning(grid <- rating_grid(model, split = "exposure"), NA)
  testthat::expect_warning(table <- rating_table(model, exposure = "exposure"), NA)
  testthat::expect_setequal(attr(extract_model_data(model), "rf"),
                           c("parent", "region"))
  testthat::expect_equal(grid$parent_restricted,
                         unname(c(A = 1, B = 1.6)[grid$parent]))
  testthat::expect_equal(sum(grid$exposure), sum(fixture$data$exposure))
  rows <- table$df[table$df$risk_factor == "parent_restricted", ]
  testthat::expect_equal(rows[[3]], unname(c(A = 1, B = 1.6)[rows$level]))
  testthat::expect_identical(predict(model, type = "response"), before)
})

testthat::test_that("split factors own their grid through shrinkage and rebasing", {
  fixture <- split_grid_fixture()
  split_values <- c(A = 1, b1 = 1.6 * 1.1 * 0.8, b2 = 1.6 * 1.1 * 1.2)
  weights <- c(A = 90, b1 = 40, b2 = 50)
  shrunk <- split_values ^ 0.8
  shrunk <- shrunk * weighted.mean(split_values, weights) /
    weighted.mean(shrunk, weights)
  expected <- list(split = split_values, shrunk = shrunk,
                   rebased = shrunk / shrunk[["b1"]])

  for (kind in names(expected)) {
    for (intercept_only in c(FALSE, TRUE)) {
      model <- refit(fixture[[kind]], intercept_only = intercept_only)
      before <- predict(model, type = "response")
      testthat::expect_warning(table <- rating_table(model, exposure = "exposure"), NA)
      testthat::expect_warning(grid <- rating_grid(model, split = "exposure"), NA)
      testthat::expect_setequal(attr(model, "rf"), c("tariff_segment", "region"))
      testthat::expect_setequal(attr(extract_model_data(model), "rf"),
                               c("tariff_segment", "region"))
      testthat::expect_false("parent" %in% names(grid))
      testthat::expect_setequal(grid$tariff_segment, names(split_values))
      testthat::expect_equal(sum(grid$exposure), sum(fixture$data$exposure))
      testthat::expect_equal(
        grid$parent_rel, unname(split_values[grid$tariff_segment])
      )
      if (kind != "split") {
        testthat::expect_equal(grid$tariff_segment_shrunk,
                               unname(shrunk[grid$tariff_segment]))
      }
      if (kind == "rebased") {
        testthat::expect_equal(grid$tariff_segment_rebased,
                               unname(expected$rebased[grid$tariff_segment]))
        testthat::expect_identical(
          .rating_table_reference_level(model, "tariff_segment"), "b1"
        )
      }
      rows <- table$df[table$df$risk_factor == "tariff_segment", ]
      testthat::expect_setequal(rows$level, names(split_values))
      testthat::expect_equal(nrow(rows), 3L)
      testthat::expect_equal(rows[[3]], unname(expected[[kind]][rows$level]))
      testthat::expect_equal(rows$exposure, unname(weights[rows$level]))
      testthat::expect_false(any(table$df$risk_factor %in%
                                 c("parent", "parent_rel", "parent_restricted")))
      grid_prediction <- predict(model, newdata = grid, type = "response")
      testthat::expect_equal(sum(grid_prediction), sum(before))
      testthat::expect_warning(audit <- audit_refinement(model), NA)
      testthat::expect_equal(nrow(audit$steps), length(fixture[[kind]]$steps))
      impact <- audit$impact
      testthat::expect_setequal(
        impact$level[impact$risk_factor == "tariff_segment"], names(split_values)
      )
      testthat::expect_equal(audit$portfolio$after,
                             sum(before) / sum(fixture$data$exposure))
      testthat::expect_identical(predict(model, type = "response"), before)
    }
  }
})

testthat::test_that("split grid metadata supports saved models and custom output names", {
  fixture <- split_grid_fixture("business_segment")
  model <- refit(fixture$rebased, intercept_only = TRUE)
  prediction <- predict(model, type = "response")
  # Simulate metadata written before split factors had their own grid key.
  attr(model, "rf") <- c("parent", "region")
  pairs <- attr(model, "mgd_rst")
  for (i in seq_along(pairs)) {
    if (length(pairs[[i]]) >= 2L && pairs[[i]][[2]] == "parent_rel") {
      pairs[[i]][[1]] <- "parent"
    }
  }
  attr(model, "mgd_rst") <- pairs
  testthat::expect_warning(grid <- rating_grid(model, split = "exposure"), NA)
  testthat::expect_true("business_segment" %in% names(grid))
  testthat::expect_false("parent" %in% names(grid))
  testthat::expect_equal(sum(predict(model, newdata = grid, type = "response")),
                         sum(prediction))
  testthat::expect_warning(audit_refinement(model), NA)
  testthat::expect_identical(predict(model, type = "response"), prediction)
})

testthat::test_that("splits without prior restrictions use the default output factor", {
  fixture <- split_grid_fixture()
  refinement <- prepare_refinement(fixture$model, fixture$data) |>
    add_relativities(
      model_variable = "parent", split_variable = "detail",
      relativities = relativities(split_level("B", c(b1 = 0.8, b2 = 1.2))),
      exposure = "exposure", normalize = FALSE
    )
  model <- refit(refinement)
  parent <- exp(coef(fixture$model)[["parentB"]])
  expected <- c(A = 1, b1 = parent * 0.8, b2 = parent * 1.2)
  testthat::expect_warning(grid <- rating_grid(model, split = "exposure"), NA)
  testthat::expect_setequal(grid$parent_refined, names(expected))
  testthat::expect_equal(grid$parent_rel, unname(expected[grid$parent_refined]))
  testthat::expect_identical(attr(model, "mgd_rst")[[1]],
                             c("parent_refined", "parent_rel"))
  testthat::expect_warning(table <- rating_table(model, exposure = "exposure"), NA)
  rows <- table$df[table$df$risk_factor == "parent_refined", ]
  testthat::expect_equal(rows[[3]], unname(expected[rows$level]))
  testthat::expect_warning(audit_refinement(model), NA)
})

testthat::test_that("genuinely non-unique refinement mappings still warn", {
  data <- data.frame(parent = c("A", "A"), value = c(1, 2))
  grid <- data.frame(parent = "A", count = 2L)
  testthat::expect_warning(
    .rating_grid_add_refinement(grid, data, list(c("parent", "value"))),
    "Refinement column `value` has multiple values per `parent`"
  )
})
