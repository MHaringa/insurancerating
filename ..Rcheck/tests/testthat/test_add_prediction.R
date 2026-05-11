context("add_prediction")

test_that("adds predictions with automatic names", {
  freq <- glm(nclaims ~ bm + zip, offset = log(exposure), family = poisson(),
              data = MTPL)
  sev <- glm(amount ~ bm + zip, weights = nclaims,
             family = Gamma(link = "log"),
             data = MTPL[MTPL$amount > 0, ])

  result <- add_prediction(MTPL[1:5, ], freq, sev)

  expect_true(all(c("pred_nclaims_freq", "pred_amount_sev") %in%
                    names(result)))
  expect_equal(nrow(result), 5)
})

test_that("custom prediction names and prefix work", {
  freq <- glm(nclaims ~ bm + zip, offset = log(exposure), family = poisson(),
              data = MTPL)
  sev <- glm(amount ~ bm + zip, weights = nclaims,
             family = Gamma(link = "log"),
             data = MTPL[MTPL$amount > 0, ])

  custom <- add_prediction(MTPL[1:5, ], freq, sev,
                           predictions = c("freq_hat", "sev_hat"))
  prefixed <- add_prediction(MTPL[1:5, ], freq, prefix = "expected")

  expect_true(all(c("freq_hat", "sev_hat") %in% names(custom)))
  expect_true("expected_nclaims_freq" %in% names(prefixed))
})

test_that("confidence intervals use lower and upper names", {
  freq <- glm(nclaims ~ bm + zip, offset = log(exposure), family = poisson(),
              data = MTPL)

  result <- add_prediction(MTPL[1:5, ], freq, confidence = TRUE)

  expect_true(all(c(
    "pred_nclaims_freq",
    "pred_nclaims_freq_lower",
    "pred_nclaims_freq_upper"
  ) %in% names(result)))
})

test_that("deprecated var and conf_int still work", {
  freq <- glm(nclaims ~ bm + zip, offset = log(exposure), family = poisson(),
              data = MTPL)

  result <- expect_warning(
    add_prediction(MTPL[1:5, ], freq, var = "freq_hat"),
    "deprecated"
  )
  result_ci <- expect_warning(
    add_prediction(MTPL[1:5, ], freq, conf_int = TRUE),
    "deprecated"
  )

  expect_true("freq_hat" %in% names(result))
  expect_true("pred_nclaims_freq_lower" %in% names(result_ci))
})

test_that("invalid arguments fail clearly", {
  freq <- glm(nclaims ~ bm + zip, offset = log(exposure), family = poisson(),
              data = MTPL)

  expect_error(add_prediction(MTPL[1:5, ]), "at least one")
  expect_error(add_prediction(MTPL[1:5, ], lm(nclaims ~ bm, data = MTPL)),
               "glm")
  expect_error(add_prediction(MTPL[1:5, ], freq, predictions = c("a", "b")),
               "one name per model")
  expect_error(add_prediction(MTPL[1:5, ], freq, confidence = "yes"),
               "`confidence`")
  expect_error(add_prediction(MTPL[1:5, ], freq, alpha = 2), "`alpha`")
  expect_error(add_prediction(MTPL[1:5, ], freq, interval_names = "lower"),
               "`interval_names`")
})

test_that("name collisions and duplicates fail clearly", {
  freq <- glm(nclaims ~ bm + zip, offset = log(exposure), family = poisson(),
              data = MTPL)

  expect_error(add_prediction(MTPL[1:5, ], freq, freq), "duplicated")
  expect_error(add_prediction(MTPL[1:5, ], freq, predictions = "nclaims"),
               "already exist")
})

test_that("inline models require explicit prediction names", {
  expect_error(
    add_prediction(
      MTPL[1:5, ],
      glm(nclaims ~ bm + zip, offset = log(exposure), family = poisson(),
          data = MTPL)
    ),
    "inline model"
  )

  result <- add_prediction(
    MTPL[1:5, ],
    glm(nclaims ~ bm + zip, offset = log(exposure), family = poisson(),
        data = MTPL),
    predictions = "freq_hat"
  )

  expect_true("freq_hat" %in% names(result))
})
