# Create a tariff from a rating table

Select one model's base rate and relativities from
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
for prediction on a portfolio. The result can be used with
[`add_prediction()`](https://mharinga.github.io/insurancerating/reference/add_prediction.md)
or [`stats::predict()`](https://rdrr.io/r/stats/predict.html) without
retaining the fitted model.

## Usage

``` r
as_tariff(
  x,
  estimate = NULL,
  segment_sources = NULL,
  unmatched = c("warn", "error")
)

# S3 method for class 'rating_tariff'
predict(object, newdata, type = "response", ...)
```

## Arguments

- x:

  A complete `rating_table` object with exponentiated estimates.

- estimate:

  Name of the estimate column to use. Required when the table compares
  several models; otherwise defaults to its only estimate.

- segment_sources:

  Named character vector. Names are tariff factors with numeric interval
  levels, and values are the corresponding numeric columns in the
  portfolio. For example, use `c(amount_band = "insured_amount")`. If
  the portfolio already contains the band column, it takes precedence
  and no mapping is needed.

- unmatched:

  How to handle portfolio values without a matching relativity: `"warn"`
  (default) warns and returns `NA` for affected rows; `"error"` stops.
  The diagnostic identifies the factors and values.

- object:

  A `rating_tariff` returned by `as_tariff()`.

- newdata:

  Data frame with the tariff factor columns, or their mapped numeric
  sources, and any required numeric offset columns.

- type:

  Prediction scale. Only `"response"` is supported.

- ...:

  Not used.

## Value

`as_tariff()` returns a `rating_tariff` object. Its
[`predict()`](https://rdrr.io/r/stats/predict.html) method returns a
numeric vector, in portfolio row order.

## Details

Predictions are the base rate multiplied by one relativity per factor.
Only log-link GLMs with an intercept and additive, treatment-coded
categorical effects are supported. Refinements represented by
categorical tariff factors are supported when their offset mappings can
be resolved. Simple offsets such as `log(exposure)` retain their
original role as multipliers. For example, a Poisson model with that
offset predicts expected claim counts for the supplied exposure, not a
rate per exposure unit. Predictions use the original response scale and
do not add commercial loadings or calculate confidence intervals. Use
the fitted model directly for continuous effects, interactions or more
complicated offsets.

Numeric sources are matched against the exact interval labels in the
table. For example, `(0,100]` includes 100 but not 0. Infinite bounds
such as `(250000,Inf]` are supported. Do not use rounded labels if they
differ from the tariff boundaries. Unknown categories, missing values in
a factor column and values outside the intervals follow `unmatched`.
Missing columns, non-numeric segment sources and overlapping intervals
are errors. The warning or error lists up to ten values per factor; its
`unmatched` field holds all unmatched values for use in a condition
handler.

## See also

[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md),
[`add_prediction()`](https://mharinga.github.io/insurancerating/reference/add_prediction.md)

## Examples

``` r
training <- data.frame(
  amount_band = factor(rep(c("(0,100]", "(100,200]"), each = 3)),
  risk_premium = c(90, 100, 110, 120, 135, 150)
)
model <- glm(risk_premium ~ amount_band, Gamma(link = "log"), data = training)
rt <- rating_table(model, exposure = FALSE)

# The portfolio already contains the band.
add_prediction(training["amount_band"], as_tariff(rt),
               predictions = "risk_premium")
#>   amount_band risk_premium
#> 1     (0,100]          100
#> 2     (0,100]          100
#> 3     (0,100]          100
#> 4   (100,200]          135
#> 5   (100,200]          135
#> 6   (100,200]          135

# Derive the band from the numeric amount using the table's interval labels.
tariff <- as_tariff(rt, segment_sources = c(amount_band = "insured_amount"))
portfolio <- data.frame(insured_amount = c(100, 150))
add_prediction(portfolio, tariff, predictions = "risk_premium")
#>   insured_amount risk_premium
#> 1            100          100
#> 2            150          135
```
