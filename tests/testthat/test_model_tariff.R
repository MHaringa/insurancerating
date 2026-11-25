tariff_test_portfolio <- function() {
  d <- data.frame(
    herbouwwaarde = rep(c(20, 60, 100, 150, 200, 220, 260, 300), 2),
    zip = factor(rep(c("A", "B"), each = 8)),
    exposure = rep(c(0.5, 1, 0.75, 1), 4),
    risk_premium = c(80, 100, 110, 140, 150, 155, 175, 180,
                     95, 112, 126, 175, 180, 200, 215, 230)
  )
  d$herbouwwaarde_cat <- cut(d$herbouwwaarde, c(0, 100, 200, Inf))
  d
}

test_that("tariff prediction uses the selected estimates and original units", {
  d <- tariff_test_portfolio()
  before <- glm(risk_premium ~ herbouwwaarde_cat + zip, Gamma(link = "log"),
                weights = exposure, data = d)
  refined <- prepare_refinement(before, data = d) |>
    add_restriction(c(B = 1.1), model_variable = "zip") |>
    refit(intercept_only = TRUE)
  rt <- rating_table(before, refined, exposure = FALSE,
                     estimate_name = c("before", "after"))
  expect_error(as_tariff(rt), "Choose one estimate column")
  expect_error(as_tariff(rt, estimate = "observed_relativity"), "Choose one estimate column")

  before_tariff <- as_tariff(rt, estimate = "before")
  after_tariff <- as_tariff(rt, estimate = "after")
  expect_s3_class(before_tariff, "rating_tariff")
  newdata <- d[c("herbouwwaarde_cat", "zip")]
  newdata$herbouwwaarde_cat <- factor(
    newdata$herbouwwaarde_cat, levels = rev(levels(newdata$herbouwwaarde_cat))
  )
  result <- add_prediction(newdata, before_tariff, after_tariff,
                           predictions = c("before", "after"))
  expect_identical(result[names(newdata)], newdata)
  expect_equal(result$before, unname(predict(before, newdata, type = "response")))
  expect_equal(
    result$after,
    unname(predict(refined, extract_model_data(refined), type = "response"))
  )
  mixed <- add_prediction(newdata, before, before_tariff)
  expect_equal(mixed$pred_risk_premium_before, mixed$pred_risk_premium_before_tariff)
  expect_error(add_prediction(newdata, before_tariff, confidence = TRUE),
               "only available for fitted GLMs")
})

test_that("numeric sources respect interval boundaries and existing categories", {
  d <- tariff_test_portfolio()
  model <- glm(risk_premium ~ herbouwwaarde_cat, Gamma(link = "log"), data = d)
  rt <- rating_table(model, exposure = FALSE)
  tariff <- as_tariff(rt,
                      segment_sources = c(herbouwwaarde_cat = "herbouwwaarde"))
  insured <- c(100, 101, 200, 1200)
  raw <- data.frame(herbouwwaarde = insured)
  band <- cut(insured, c(0, 100, 200, Inf))
  expect_equal(
    add_prediction(raw, tariff, predictions = "premium")$premium,
    unname(predict(model, data.frame(herbouwwaarde_cat = band), type = "response"))
  )
  raw$herbouwwaarde_cat <- rev(band)
  expect_equal(predict(tariff, raw), predict(as_tariff(rt), raw))
  expect_error(predict(as_tariff(rt), raw["herbouwwaarde"]),
               'segment_sources = c(herbouwwaarde_cat = "herbouwwaarde")', fixed = TRUE)
  expect_error(predict(tariff, data.frame(other = 1)), "numeric segment source")
  expect_error(predict(tariff, data.frame(herbouwwaarde = "100")), "numeric column")

  d$herbouwwaarde_cat <- factor(rep(c("[0,100)", "[100,200)", "[200,Inf)"),
                                  length.out = nrow(d)))
  left <- as_tariff(rating_table(update(model, data = d), exposure = FALSE),
                    segment_sources = c(herbouwwaarde_cat = "herbouwwaarde"))
  expect_equal(predict(left, data.frame(herbouwwaarde = c(0, 100, 200))),
               predict(left, data.frame(herbouwwaarde_cat = levels(d$herbouwwaarde_cat))))
})

test_that("unmatched values report each factor once and leave usable predictions", {
  d <- tariff_test_portfolio()
  model <- glm(risk_premium ~ herbouwwaarde_cat + zip, Gamma(link = "log"), data = d)
  rt <- rating_table(model, exposure = FALSE)
  sources <- c(herbouwwaarde_cat = "herbouwwaarde")
  tariff <- as_tariff(rt, segment_sources = sources)
  newdata <- data.frame(herbouwwaarde = c(100, 0, 150, NA, 180, -5),
                        zip = c("A", "C", "invalid", "B", "B", "B"))
  warnings <- list()
  result <- withCallingHandlers(
    add_prediction(newdata, tariff, predictions = "premium"),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1L)
  expect_s3_class(warnings[[1L]], "tariff_unmatched_warning")
  expect_match(conditionMessage(warnings[[1L]]), '`zip` from `zip`: "C", "invalid"',
               fixed = TRUE)
  expect_match(conditionMessage(warnings[[1L]]),
               '`herbouwwaarde_cat` from `herbouwwaarde`: "0", <NA>, "-5"', fixed = TRUE)
  expect_equal(warnings[[1L]]$unmatched$herbouwwaarde_cat$values, c(0, NA, -5))
  expect_identical(which(is.na(result$premium)), c(2L, 3L, 4L, 6L))
  expect_equal(result$premium[c(1, 5)], predict(tariff, newdata[c(1, 5), ]))

  strict <- as_tariff(rt, segment_sources = sources, unmatched = "error")
  expect_error(add_prediction(newdata, strict), "Unmatched tariff values",
               class = "tariff_unmatched_error")
  newdata$herbouwwaarde_cat <- c("(0,100]", "unknown", "(100,200]", NA,
                                "(100,200]", "(0,100]")
  expect_error(predict(strict, newdata), '"unknown", <NA>', fixed = TRUE)
})

test_that("offsets stay on the original prediction scale", {
  d <- tariff_test_portfolio()
  d$claims <- rep(c(1, 2, 3, 2, 4, 3, 5, 4), 2)
  for (model in list(
    glm(claims ~ zip + offset(log(exposure)), poisson(), d),
    glm(claims ~ zip, poisson(), d, offset = log(exposure))
  )) {
    tariff <- as_tariff(rating_table(model, exposure = FALSE))
    expect_equal(predict(tariff, d), unname(predict(model, d, type = "response")))
    expect_error(predict(tariff, d["zip"]), "Tariff offset column `exposure`")
  }
})

test_that("smoothed tariff bands can be reconstructed without the fitted model", {
  d <- tariff_test_portfolio()
  d$record_id <- seq_len(nrow(d))
  d$herbouwwaarde_cat <- cut(d$herbouwwaarde, c(0, 100, 200, 400))
  model <- glm(risk_premium ~ herbouwwaarde_cat + zip, Gamma(link = "log"), data = d)
  refined <- prepare_refinement(model, data = d) |>
    add_smoothing(
      model_variable = "herbouwwaarde_cat", source_variable = "herbouwwaarde",
      breaks = c(0, 80, 150, 400), smoothing = "poly", degree = 1
    ) |>
    refit(intercept_only = TRUE)
  rt <- rating_table(refined, exposure = FALSE)
  tariff <- as_tariff(rt, segment_sources = c(herbouwwaarde_smooth = "herbouwwaarde"))
  refined_data <- extract_model_data(refined)
  expected <- unname(predict(refined, refined_data, type = "response"))
  expected <- expected[match(d$record_id, refined_data$record_id)]
  expect_equal(predict(tariff, d), expected)
})

test_that("tariffs reject ambiguous intervals and unsupported or incomplete tables", {
  d <- tariff_test_portfolio()
  model <- glm(risk_premium ~ herbouwwaarde_cat + zip, Gamma(link = "log"), data = d)
  rt <- rating_table(model, exposure = FALSE)
  expect_error(as_tariff(rt[-1L, ]), "complete tariff")
  expect_error(as_tariff(rating_table(model, exponentiate = FALSE, exposure = FALSE)),
               "exponentiate = TRUE", fixed = TRUE)
  expect_error(as_tariff(rt, segment_sources = c(foo = "herbouwwaarde")),
               "Unknown tariff factor")
  expect_error(as_tariff(rt, segment_sources = c(zip = "herbouwwaarde")),
               "exact numeric interval labels")

  unsupported <- list(
    update(model, . ~ . + herbouwwaarde),
    update(model, . ~ herbouwwaarde_cat * zip),
    update(model, contrasts = list(zip = "contr.sum")),
    update(model, offset = 2 * log(exposure)),
    update(model, family = gaussian())
  )
  for (fit in unsupported) {
    expect_error(as_tariff(rating_table(fit, exposure = FALSE)), "fitted model instead")
  }

  levels(d$herbouwwaarde_cat) <- c("(0,100]", "[100,200]", "(200,Inf]")
  overlap <- rating_table(update(model, data = d), exposure = FALSE)
  expect_error(as_tariff(overlap, segment_sources = c(herbouwwaarde_cat = "herbouwwaarde")),
               "overlapping intervals")
})
