test_that("individual audit compares matched rows, without storing them by default", {
  d <- data.frame(count = c(1, 2, 4, 2, 3, 7), exposure = c(0.5, 2, 1, 1, 3, 1),
                  zone = factor(rep(c("A", "B", "C"), 2)))
  m <- glm(count ~ zone + offset(log(exposure)), poisson(), d)
  refined <- prepare_refinement(m, data = d) |>
    add_restriction(c(B = 1.3, C = 2.1), model_variable = "zone") |> refit()
  expect_null(audit_refinement(refined)$individual_impact)
  a <- audit_refinement(refined, individual = TRUE, keep_individual = TRUE)
  rows <- extract_model_data(refined)
  old <- predict(m, rows, type = "response")
  new <- predict(refined, rows, type = "response")
  change <- unname(new / old - 1)
  expect_equal(a$individual_impact$records$change, change)
  expect_equal(a$individual_impact$summary$weighted_mean,
               weighted.mean(change, rows$exposure))
  expect_equal(sum(a$individual_impact$bands$share), 1)
  expect_s3_class(autoplot.refinement_audit(a), "ggplot")
  b <- audit_refinement(refined, individual = TRUE, impact_weights = "equal")
  expect_equal(b$individual_impact$summary$weighted_mean, mean(change))
  expect_null(b$individual_impact$records)
})

test_that("individual impact excludes invalid pairs and counts exact band endpoints", {
  a <- .audit_individual_impact(
    c(1, 1, 1, 0, -1, NA), c(0.5, 1, 2, 3, 3, 2), c(1, 3, 2, 1, 1, 1),
    "equal", c(-Inf, -0.5, 0, Inf), 0.4, 0, TRUE
  )
  expect_equal(a$summary$invalid_records, 3L)
  expect_equal(a$bands$records, rep(1, 3))
  expect_equal(a$summary$median, 0)
  expect_equal(a$thresholds$either, 0.5)
  expect_true(all(is.na(a$records$change[4:6])))
})
