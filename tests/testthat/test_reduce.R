library(insurancerating)
context("reduce")

test_that("check if object is of reduce class", {
  portfolio <- structure(list(policy_nr = c("12345", "12345", "12345", "12345",
                                            "12345", "12345", "12345", "12345", "12345", "12345", "12345"),
                              productgroup = c("fire", "fire", "fire", "fire", "fire", "fire",
                                               "fire", "fire", "fire", "fire", "fire"),
                              product = c("contents",
                                          "contents", "contents", "contents", "contents", "contents", "contents",
                                          "contents", "contents", "contents", "contents"),
                              begin_dat = structure(c(16709,
                                                      16740, 16801, 17410, 17440, 17805, 17897, 17956, 17987, 18017,
                                                      18262), class = "Date"),
                              end_dat = structure(c(16739, 16800,
                                                    16831, 17439, 17531, 17896, 17955, 17986, 18016, 18261, 18292),
                                                  class = "Date"),
                              premium = c(89L, 58L, 83L, 73L, 69L, 94L,
                                          91L, 97L, 57L, 65L, 55L)), row.names = c(NA, -11L), class = "data.frame")
  x <- reduce(portfolio, begin = begin_dat, end = end_dat, policy_nr,
              productgroup, product, min.gapwidth = 5)
  expect_is(x, "reduce")
})
