monotone_fixture <- function(count = c(100, 115, 110, 135, 130), base = 1L) {
  lv <- c("low", "modest", "average", "strong", "very strong")
  d <- data.frame(
    grade = ordered(rep(lv, 2), levels = lv),
    band = factor(rep(c("A", "B"), each = 5)),
    exposure = rep(c(1, 2), each = 5),
    volume = rep(c(1, 3, 1, 2, 2), 2),
    claims = c(count, 4 * count)
  )
  coding <- if (is.null(base)) NULL else list(grade = stats::contr.treatment(lv, base))
  m <- glm(claims ~ grade + band + offset(log(exposure)),
           data = d, family = poisson(), contrasts = coding)
  prepare_refinement(m, d)
}

test_that("ordinal projections use the requested direction and one-sided method", {
  ref <- monotone_fixture()
  a <- c(1, 1.15, 1.10, 1.35, 1.30)
  values <- function(direction, method, weights = NULL) {
    add_monotonicity(ref, "grade", direction, method, weights)$steps[[1]]$values
  }
  for (direction in c("increasing", "decreasing")) {
    up <- values(direction, "increase_only")
    down <- values(direction, "decrease_only")
    inc <- direction == "increasing"
    expect_equal(up$original_relativity, a)
    expect_equal(up$adjusted_relativity, if (inc) cummax(a) else rev(cummax(rev(a))))
    expect_equal(down$adjusted_relativity, if (inc) rev(cummin(rev(a))) else cummin(a))
    expect_equal(values(direction, "increase_only", "volume")$adjusted_relativity,
                 up$adjusted_relativity)
    expect_equal(values(direction, "decrease_only", "volume")$adjusted_relativity,
                 down$adjusted_relativity)
  }
  b <- sqrt(1.15 * 1.10)
  upper <- sqrt(1.35 * 1.30)
  equal <- values("increasing", "isotonic")
  expect_equal(equal$weight, rep(1, 5))
  expect_equal(equal$adjusted_relativity, c(1, b, b, upper, upper))
  weighted <- values("increasing", "isotonic", "volume")
  expect_equal(weighted$weight, c(2, 6, 2, 4, 4))
  b <- exp((3 * log(1.15) + log(1.10)) / 4)
  expect_equal(weighted$adjusted_relativity, c(1, b, b, upper, upper))
  expect_equal(values("decreasing", "isotonic", "volume")$adjusted_relativity,
               rep(exp(weighted.mean(log(a), weighted$weight)), 5))
})

test_that("monotone effects are unchanged, including polynomial contrasts", {
  for (base in list(3L, NULL)) {
    ref <- monotone_fixture(count = (1:5) * 100, base = base)
    before <- serialize(ref, NULL)
    spec <- add_monotonicity(ref, "grade")
    vals <- spec$steps[[1]]$values
    expect_identical(vals$level, levels(ref$base$data$grade))
    expect_equal(vals$original_relativity / vals$original_relativity[1], 1:5)
    expect_identical(vals$adjusted_relativity, vals$original_relativity)
    for (io in c(TRUE, FALSE)) {
      fit <- refit(spec, intercept_only = io)
      expect_equal(fitted(fit), fitted(ref$base$model))
    }
    expect_identical(serialize(ref, NULL), before)
  }
})

test_that("the reference level can move and only projected ratios are fixed", {
  ref <- monotone_fixture(base = 3L)
  spec <- add_monotonicity(ref, "grade", weights = "volume", output_variable = "tariff")
  vals <- spec$steps[[1]]$values
  expect_equal(vals$original_relativity[3], 1)
  expect_gt(vals$adjusted_relativity[3], 1)
  fit <- refit(spec)
  expect_named(coef(fit), c("(Intercept)", "bandB"))
  d <- ref$base$data
  d$tariff <- vals$adjusted_relativity[match(d$grade, vals$level)]
  independent <- glm(claims ~ band + offset(log(exposure * tariff)),
                     data = d, family = poisson())
  expect_equal(fitted(fit), fitted(independent))
  tab <- rating_table(fit, estimate_name = "est")
  rows <- tab$risk_factor == "tariff"
  expect_identical(tab$level[rows], vals$level)
  expect_equal(tab$est[rows], vals$adjusted_relativity[match(tab$level[rows], vals$level)])
  expect_identical(audit_refinement(fit)$steps$type, "monotonicity")
  expect_match(summary(spec)$steps$details, "increasing; method = isotonic; weights = volume")
  expect_equal(autoplot(spec)$data$Coef, c(vals$original_relativity, vals$adjusted_relativity))
})

test_that("projection follows current offsets and subsequent steps replace it once", {
  ref <- monotone_fixture(base = NULL)
  before <- add_restriction(ref, model_variable = "grade", restrictions = c(low = 1.8))
  spec <- add_monotonicity(before, "grade", method = "increase_only")
  vals <- spec$steps[[2]]$values
  original <- ref$base$rating_factors
  expected <- original$estimate[original$risk_factor == "grade"]
  expected[1] <- 1.8
  expect_equal(vals$original_relativity, expected)
  expect_equal(vals$adjusted_relativity, cummax(expected))
  fixed <- add_restriction(spec, model_variable = "grade", output_variable = "final",
                           restrictions = c(low = 1.2))
  expect_identical(length(fixed$steps), 3L)
  fit <- refit(fixed)
  expect_setequal(unique(rating_table(fit)$risk_factor), c("(Intercept)", "band", "final"))
  expect_equal(fit$data$final, rep(c(1.2, vals$adjusted_relativity[-1]), 2))
  expect_equal(fit$data$final, exp(fit$offset) / fit$data$exposure)
  rebase <- add_rebasing(spec, "grade", reference_level = "modest")
  tab <- rating_table(refit(rebase))
  expect_setequal(unique(tab$risk_factor), c("(Intercept)", "band", "grade"))
  multi <- add_restriction(ref, model_variable = "grade", restriction_type = "multiplier",
                           restrictions = c(low = 2))
  expect_equal(add_monotonicity(multi, "grade")$steps[[2]]$values$original_relativity[1],
               2 * original$estimate[original$risk_factor == "grade" & original$level == "low"])
  twice <- spec |>
    add_restriction(model_variable = "grade", restrictions = c(low = 2),
                    restriction_type = "multiplier") |>
    add_restriction(model_variable = "grade", restrictions = c(low = 3),
                    restriction_type = "multiplier") |>
    refit()
  tab <- rating_table(twice, estimate_name = "est")
  expect_setequal(unique(tab$risk_factor), c("(Intercept)", "band", "grade_monotone"))
  expect_equal(tab$est[tab$risk_factor == "grade_monotone"],
               vals$adjusted_relativity * c(6, 1, 1, 1, 1))
})

test_that("monotonicity rejects ambiguous effects, orders, outputs and weights", {
  ref <- monotone_fixture()
  d <- ref$base$data
  expect_error(add_monotonicity(ref$base$model, "grade"), "prepare_refinement")
  expect_error(add_monotonicity(ref, "band"), "ordered factor")
  expect_error(add_monotonicity(ref, "volume"), "standalone")
  expect_error(add_monotonicity(ref, "grade", weights = "absent"), "not found")
  expect_error(add_monotonicity(ref, "grade", output_variable = "band"), "new, syntactically")
  for (invalid in list(NA_real_, Inf, -2, "none", 0)) {
    bad <- ref
    bad$base$data$volume[bad$base$data$grade == "low"] <- invalid
    expect_error(add_monotonicity(bad, "grade", weights = "volume"), "weights")
  }
  interaction <- glm(claims ~ grade * band, data = d, family = poisson())
  expect_error(add_monotonicity(prepare_refinement(interaction), "grade"), "interaction")
  slope <- glm(claims ~ volume, data = d, family = poisson())
  expect_error(add_monotonicity(prepare_refinement(slope), "volume"), "ordered factor")
  linear <- glm(claims ~ grade, data = d)
  expect_error(add_monotonicity(prepare_refinement(linear), "grade"), "log-link")
  d$grade_score <- as.integer(d$grade)
  aliased <- glm(claims ~ grade_score + grade, data = d, family = poisson())
  expect_error(add_monotonicity(prepare_refinement(aliased), "grade"), "estimable")
  ref$base$data$grade[1] <- NA
  expect_error(add_monotonicity(ref, "grade"), "missing levels")
})

numeric_monotone_fixture <- function(labels = c("10", "20", "100", "200", "1000"),
                                      ordered = FALSE) {
  d <- monotone_fixture()$base$data
  d$grade <- factor(rep(labels, 2), levels = labels[c(3, 5, 2, 1, 4)], ordered = ordered)
  m <- glm(claims ~ grade + band + offset(log(exposure)), data = d, family = poisson())
  prepare_refinement(m, d)
}

test_that("numeric-labelled factors use numeric order and correctly matched weights", {
  ref <- numeric_monotone_fixture()
  original <- serialize(ref, NULL)
  lv <- c("10", "20", "100", "200", "1000")
  a <- c(1, 1.15, 1.10, 1.35, 1.30) / 1.10
  w <- c(2, 6, 2, 4, 4)
  for (direction in c("increasing", "decreasing")) {
    for (method in c("isotonic", "increase_only", "decrease_only")) {
      spec <- add_monotonicity(ref, "grade", direction, method, weights = "volume")
      vals <- spec$steps[[1]]$values
      idx <- if (direction == "increasing") seq_along(a) else rev(seq_along(a))
      expected <- switch(method,
        isotonic = stats::isoreg(rep(log(a[idx]), w[idx]))$yf[cumsum(w[idx])],
        increase_only = cummax(log(a[idx])),
        decrease_only = rev(cummin(rev(log(a[idx])))))
      expect_identical(vals$level, lv)
      expect_identical(spec$steps[[1]]$level_order, lv)
      expect_equal(vals$weight, w)
      expect_equal(vals$original_relativity, a)
      expect_equal(vals$adjusted_relativity[idx], exp(expected))
      fit <- refit(spec)
      d <- ref$base$data
      d$tariff <- vals$adjusted_relativity[match(d$grade, vals$level)]
      independent <- glm(claims ~ band + offset(log(exposure * tariff)),
                         data = d, family = poisson())
      expect_equal(fitted(fit), fitted(independent))
      tab <- rating_table(fit, estimate_name = "est")
      rows <- tab$risk_factor == "grade_monotone"
      expect_identical(tab$level[rows], lv)
      expect_equal(tab$est[rows], vals$adjusted_relativity)
      expect_identical(levels(fit$data$grade), levels(ref$base$data$grade))
    }
  }
  expect_identical(serialize(ref, NULL), original)
})

test_that("ordinary cut intervals retain their categories without smoothing", {
  lv <- levels(cut(c(10, 20, 100, 200, 1000), c(0, 15, 50, 150, 500, Inf)))
  ref <- numeric_monotone_fixture(lv)
  spec <- add_monotonicity(ref, "grade", weights = "volume")
  vals <- spec$steps[[1]]$values
  expect_false(is.ordered(ref$base$data$grade))
  expect_identical(vals$level, lv)
  expect_equal(vals$original_relativity, c(1, 1.15, 1.10, 1.35, 1.30) / 1.10)
  expect_equal(vals$weight, c(2, 6, 2, 4, 4))
  p <- autoplot(spec)
  expect_identical(p$data$level, rep(lv, 2))
  expect_equal(p$data$x_num, rep(seq_along(lv), 2))
  expect_identical(unique(spec$steps[[1]]$restrictions[[1]]), lv)
  expect_equal(preview_refinement(spec)$state$monotonicity_df, vals)

  before <- add_restriction(ref, model_variable = "grade",
                            restrictions = stats::setNames(2, lv[2]),
                            restriction_type = "multiplier")
  after <- add_monotonicity(before, "grade", method = "increase_only")
  current <- after$steps[[2]]$values
  expect_equal(current$original_relativity, vals$original_relativity * c(1, 2, 1, 1, 1))
  expect_equal(current$adjusted_relativity, cummax(current$original_relativity))
  expect_equal(refit(after)$data$grade_monotone,
               rep(current$adjusted_relativity, 2))
})

test_that("numeric ordering handles boundaries without guessing ambiguous labels", {
  lv <- c("(-Inf,-100]", "(-100,0]", "(0,1e2]", "(100,200]", "(200,Inf)")
  spec <- add_monotonicity(numeric_monotone_fixture(lv), "grade")
  expect_identical(spec$steps[[1]]$values$level, lv)
  numeric_lv <- c("-1e3", "-20", "0", "100", "2e3")
  spec <- add_monotonicity(numeric_monotone_fixture(numeric_lv), "grade")
  expect_identical(spec$steps[[1]]$values$level, numeric_lv)
  for (ordered in c(FALSE, TRUE)) {
    for (labels in list(c("1", "1.0", "2", "3", "4"),
                        c("1", "(0,2]", "3", "4", "5"))) {
      expect_error(add_monotonicity(numeric_monotone_fixture(labels, ordered), "grade"),
                    "distinct values.*do not mix")
    }
    for (labels in list(c("(0,100]", "(50,200]", "(200,300]", "(300,400]", "(400,500]"),
                        c("[0,100]", "[100,200]", "(200,300]", "(300,400]", "(400,500]"),
                        c("(0,0]", "(0,200]", "(200,300]", "(300,400]", "(400,500]"))) {
      expect_error(add_monotonicity(numeric_monotone_fixture(labels, ordered), "grade"),
                    "non-overlapping")
    }
  }
  expect_error(add_monotonicity(numeric_monotone_fixture(c("1", "2", "3", "4", "unknown")),
                                "grade"), "ordered factor with explicit levels")
})

test_that("ordered numeric factors use the same order in projection, plots and tables", {
  for (lv in list(c("10", "20", "100", "200", "1000"),
                  c("(-Inf,15]", "(15,50]", "(50,150]", "(150,500]", "(500,Inf)"))) {
    ref <- numeric_monotone_fixture(lv, ordered = TRUE)
    before <- serialize(ref, NULL)
    for (direction in c("increasing", "decreasing")) {
      expect_warning(
        spec <- add_monotonicity(ref, "grade", direction = direction, weights = "volume"),
        "ascending numeric order for `grade`.*ordered-factor levels.*`direction`"
      )
      vals <- spec$steps[[1]]$values
      expect_identical(vals$level, lv)
      expect_identical(spec$steps[[1]]$level_order, lv)
      expect_equal(vals$weight, c(2, 6, 2, 4, 4))
      change <- diff(vals$adjusted_relativity)
      expect_true(if (direction == "increasing") all(change >= 0) else all(change <= 0))
      expect_no_warning(p <- autoplot(spec))
      expect_identical(p$data$level, rep(lv, 2))
      expect_no_warning(fit <- refit(spec))
      tab <- rating_table(fit, estimate_name = "est")
      rows <- tab$risk_factor == "grade_monotone"
      expect_identical(tab$level[rows], lv)
      expect_equal(tab$est[rows], vals$adjusted_relativity)
      expect_identical(levels(fit$data$grade), levels(ref$base$data$grade))
    }
    expect_identical(serialize(ref, NULL), before)
    d <- ref$base$data
    d$grade <- ordered(d$grade, levels = lv)
    m <- glm(claims ~ grade + band + offset(log(exposure)), data = d, family = poisson())
    expect_no_warning(add_monotonicity(prepare_refinement(m, d), "grade"))
  }
  # Business order is still retained for non-numeric categories.
  ref <- monotone_fixture()
  expect_no_warning(spec <- add_monotonicity(ref, "grade"))
  expect_identical(spec$steps[[1]]$level_order, levels(ref$base$data$grade))
})

test_that("repeated projections keep history and apply the effective effect only once", {
  for (ref in list(monotone_fixture(), numeric_monotone_fixture())) {
    first <- add_monotonicity(ref, "grade", weights = "volume")
    first_copy <- serialize(first, NULL)
    second <- add_monotonicity(first, "grade", weights = "volume")
    expect_length(second$steps, 2L)
    expect_identical(second$steps[[1]], first$steps[[1]])
    expect_identical(second$steps[[2]]$values$original_relativity,
                      first$steps[[1]]$values$adjusted_relativity)
    expect_identical(second$steps[[2]]$values$adjusted_relativity,
                      first$steps[[1]]$values$adjusted_relativity)
    third <- add_monotonicity(second, "grade", direction = "decreasing", weights = "volume")
    expect_identical(vapply(third$steps, `[[`, character(1), "output_variable"),
                      c("grade_monotone", "grade_monotone_1", "grade_monotone_2"))
    expect_identical(third$steps[1:2], second$steps)
    vals <- third$steps[[3]]$values
    d <- ref$base$data
    d$tariff <- vals$adjusted_relativity[match(d$grade, vals$level)]
    independent <- glm(claims ~ band + offset(log(exposure * tariff)),
                       data = d, family = poisson())
    fit <- refit(third)
    expect_equal(fitted(fit), fitted(independent))
    expect_equal(fit$data$grade_monotone_2, d$tariff)
    expect_setequal(unique(rating_table(fit)$risk_factor),
                     c("(Intercept)", "band", "grade_monotone_2"))
    expect_identical(audit_refinement(fit)$steps$type, rep("monotonicity", 3))
    expect_length(summary(third)$steps$step, 3L)
    expect_equal(autoplot(third, step = 1)$data$Coef,
                  c(first$steps[[1]]$values$original_relativity,
                    first$steps[[1]]$values$adjusted_relativity))
    expect_equal(autoplot(third, step = 3)$data$Coef,
                  c(vals$original_relativity, vals$adjusted_relativity))
    expect_identical(serialize(first, NULL), first_copy)

    level <- first$steps[[1]]$values$level[1]
    restricted <- add_restriction(first, model_variable = "grade",
                                  restrictions = stats::setNames(2, level),
                                  restriction_type = "multiplier")
    again <- add_monotonicity(restricted, "grade", method = "increase_only")
    vals <- again$steps[[3]]$values
    expect_equal(vals$original_relativity,
                  first$steps[[1]]$values$adjusted_relativity * c(2, 1, 1, 1, 1))
    expect_equal(vals$adjusted_relativity, cummax(vals$original_relativity))
    fit <- refit(again)
    expect_equal(exp(fit$offset) / fit$data$exposure,
                  vals$adjusted_relativity[match(fit$data$grade, vals$level)])
    expect_identical(again$steps[[3]]$output_variable, "grade_monotone_1")
  }
})

test_that("automatic output names avoid existing columns but explicit collisions fail", {
  ref <- monotone_fixture()
  ref$base$data$grade_monotone <- 99
  ref$base$data$grade_monotone_1 <- 101
  spec <- add_monotonicity(ref, "grade")
  expect_identical(spec$steps[[1]]$output_variable, "grade_monotone_2")
  expect_identical(preview_refinement(spec)$state$data$grade_monotone,
                    ref$base$data$grade_monotone)
  expect_identical(preview_refinement(spec)$state$data$grade_monotone_1,
                    ref$base$data$grade_monotone_1)
  expect_error(add_monotonicity(spec, "grade", output_variable = "grade_monotone_2"),
                "must be a new")
  explicit <- add_monotonicity(spec, "grade", output_variable = "final_tariff")
  expect_identical(explicit$steps[[2]]$output_variable, "final_tariff")
})

test_that("numeric factor projections use level contributions with non-treatment contrasts", {
  d <- numeric_monotone_fixture()$base$data
  for (coding in list(stats::contr.sum(5), stats::contr.helmert(5))) {
    m <- glm(claims ~ grade + band + offset(log(exposure)), data = d,
             family = poisson(), contrasts = list(grade = coding))
    ref <- prepare_refinement(m, d)
    spec <- add_monotonicity(ref, "grade", weights = "volume")
    vals <- spec$steps[[1]]$values
    effect <- exp(predict(m, type = "terms")[1:5, "grade"])
    expect_equal(vals$original_relativity, unname(effect))
    d$tariff <- vals$adjusted_relativity[match(d$grade, vals$level)]
    independent <- glm(claims ~ band + offset(log(exposure * tariff)),
                       data = d, family = poisson())
    expect_equal(fitted(refit(spec)), fitted(independent))
    before <- add_restriction(ref, model_variable = "grade", restrictions = c("20" = 2),
                              restriction_type = "multiplier")
    after <- add_monotonicity(before, "grade")
    expect_equal(after$steps[[2]]$values$original_relativity,
                 vals$original_relativity * c(1, 2, 1, 1, 1))
  }
})
