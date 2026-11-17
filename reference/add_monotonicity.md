# Enforce monotone tariff relativities for numeric bands or ordered categories

Project the current level effects of a standalone categorical factor
onto a non-decreasing or non-increasing sequence. This is a
post-estimation adjustment, not likelihood-constrained GLM estimation.
Unlike
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
it needs no continuous source variable, draws no curve between levels
and does not impose smoothness.

## Usage

``` r
add_monotonicity(
  model,
  model_variable,
  direction = c("increasing", "decreasing"),
  method = c("isotonic", "increase_only", "decrease_only"),
  weights = NULL,
  output_variable = NULL
)
```

## Arguments

- model:

  A refinement specification created by
  [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md).

- model_variable:

  Name of a standalone factor in a log-link GLM with an intercept.
  Numeric labels (e.g. `"100"`, `"200"`, `"1000"`) and numeric intervals
  (e.g. levels created by [`cut()`](https://rdrr.io/r/base/cut.html))
  are ordered numerically, not alphabetically. Intervals must not
  overlap. An ordered factor instead uses its explicitly declared level
  order. For non-numeric labels, declare that order with
  `ordered(x, levels = ...)` before fitting.

- direction:

  Desired order of relativities in the numeric or declared level order.
  Both directions permit ties.

- method:

  `"isotonic"` minimizes the weighted sum of squared changes to
  log-relativities by pooling adjacent violating levels. Alternatively,
  `"increase_only"` raises only levels that must rise; `"decrease_only"`
  lowers only levels that must fall.

- weights:

  Optional name of a non-negative numeric volume column. Weights are
  summed by level; each total must be finite and positive. `NULL` gives
  each level equal weight, irrespective of its row count. Weights are
  recorded but do not affect the two one-sided methods.

- output_variable:

  Name of the new numeric tariff column used as a fixed offset at refit
  and displayed in
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md).
  Defaults to the model variable name followed by `_monotone`.

## Value

A `rating_refinement` with an appended step; no GLM is fitted. The
step's `values` table contains the ordered levels, input and adjusted
relativities and level weights. Use
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
for a preview.

## Details

For grouped insured amount, reconstruction value or similar variables,
pass the existing factor used in the GLM, just as the `model_variable`
of
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md).
No `source_variable` or new `breaks` are needed: existing categories are
retained and only their relativities are adjusted. A continuous numeric
slope is not supported.

A treatment-coded reference level enters the projection with its current
relativity (initially one); it can move. For polynomial contrasts the
input is the exponentiated fitted level contribution, not an individual
contrast coefficient. Projected values are not renormalized.
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
fixes their ratios and estimates the intercept and remaining free
coefficients. Later explicit refinements can change these ratios.

## See also

[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`audit_refinement()`](https://mharinga.github.io/insurancerating/reference/audit_refinement.md)

## Examples

``` r
portfolio <- data.frame(
  insured_amount = c(50000, 150000, 750000, 1500000, 3000000),
  claims = c(100, 115, 110, 135, 130), exposure = rep(1000, 5)
)
portfolio$amount_band <- cut(portfolio$insured_amount,
  breaks = c(0, 100000, 500000, 1000000, 2000000, Inf))
# cut() creates an ordinary factor; its numeric intervals provide the order.
fit <- glm(claims ~ amount_band + offset(log(exposure)), poisson(), portfolio)
spec <- prepare_refinement(fit) |>
  add_monotonicity("amount_band", weights = "exposure")
spec$steps[[1]]$values
#>           level original_relativity weight adjusted_relativity
#> 1     (0,1e+05]                1.00   1000            1.000000
#> 2 (1e+05,5e+05]                1.15   1000            1.124722
#> 3 (5e+05,1e+06]                1.10   1000            1.124722
#> 4 (1e+06,2e+06]                1.35   1000            1.324764
#> 5   (2e+06,Inf]                1.30   1000            1.324764
rating_table(refit(spec))
#>            risk_factor         level est_model
#> 1          (Intercept)   (Intercept) 0.1000174
#> 2 amount_band_monotone     (0,1e+05] 1.0000000
#> 3 amount_band_monotone (1e+05,5e+05] 1.1247222
#> 4 amount_band_monotone (5e+05,1e+06] 1.1247222
#> 5 amount_band_monotone (1e+06,2e+06] 1.3247641
#> 6 amount_band_monotone   (2e+06,Inf] 1.3247641
```
