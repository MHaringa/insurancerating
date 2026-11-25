test_that("cluster draws duplicate complete clusters and keep holdout disjoint", {
  ids <- c("B", "A", "A", "C")
  rows <- split(seq_along(ids), match(ids, unique(ids)))
  draw <- .cluster_resample_rows(rows, c(2, 2, 1), TRUE)
  expect_equal(draw$train, c(2, 3, 2, 3, 1))
  expect_equal(draw$evaluation, 4L)
  expect_length(intersect(ids[draw$train], ids[draw$evaluation]), 0L)
})

test_that("cluster scores use whole weighted trains and match singleton draws", {
  d <- data.frame(policy = rep(1:8, each = 2), id = 1:16,
                  nclaims = rep(c(1, 3, 1, 2), 4), exposure = rep(c(0.5, 1), 8),
                  weight = rep(1:4, 4))
  model <- glm(nclaims ~ 1 + offset(log(exposure)), poisson(), d, weights = weight)
  set.seed(5)
  selected <- sample.int(8, 4, replace = FALSE)
  train <- d[d$policy %in% selected, ]
  test <- d[!d$policy %in% selected, ]
  rate <- with(train, sum(nclaims * weight) / sum(exposure * weight))
  set.seed(5)
  result <- validate_performance(model, d, n_resamples = 1, sample_fraction = 0.5,
                                 sampling = "split", cluster = "policy",
                                 show_progress = FALSE)
  expect_equal(unname(result$rmse_bs),
               with(test, sqrt(mean((nclaims - exposure * rate)^2))))
  expect_equal(result$diagnostics$evaluation_clusters, 4)
  set.seed(6)
  one <- validate_performance(model, d, n_resamples = 4, show_progress = FALSE)
  set.seed(6)
  two <- validate_performance(model, d, n_resamples = 4, cluster = "id", show_progress = FALSE)
  expect_equal(two$rmse_bs, one$rmse_bs)
  set.seed(6)
  expect_equal(validate_performance(model, d, n_resamples = 4, cluster = "id",
                                    show_progress = FALSE), two)
  d$id[1] <- NA
  expect_error(validate_performance(model, d, cluster = "id"), "missing values")
  expect_error(validate_performance(model, d, sampling = "rolling", cluster = "policy"),
               "bootstrap and split")
})
