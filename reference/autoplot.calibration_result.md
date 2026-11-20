# Plot observed-versus-expected calibration

Plot overall O/E, observed versus expected outcomes by predicted-risk
group, or O/E across periods or rating-factor levels.

## Usage

``` r
# S3 method for class 'calibration_result'
autoplot(
  object,
  ...,
  type = c("auto", "overall", "risk", "temporal", "factor")
)
```

## Arguments

- object:

  Result from
  [`check_calibration()`](https://mharinga.github.io/insurancerating/reference/check_calibration.md).

- ...:

  Unused.

- type:

  Plot type. The default, `"auto"`, uses the calibration grouping. For a
  result grouped with `by`, it uses a temporal plot for `Date`,
  date-time or whole-number years from 1800 to 2200, and a factor plot
  otherwise. Set `"temporal"` or `"factor"` to override that choice (for
  example, when years are stored as ordered factors).

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Details

Overall, temporal and factor plots use a reference line at O/E = 1.
Factor plots preserve the result's order and label missing groups.
Temporal plots omit missing periods; undefined O/E values are not drawn.

The predicted-risk plot shows observed against expected with a 45-degree
reference line and the same scale on both axes. If exposure is
available, reported totals are divided by it and groups with zero
exposure are not plotted. If no group has positive exposure, the plot
uses totals instead. With case weights, these are weighted totals
divided by the reported exposure, not necessarily unweighted portfolio
rates.

## See also

[`check_calibration()`](https://mharinga.github.io/insurancerating/reference/check_calibration.md)

## Examples

``` r
portfolio <- as.data.frame(MTPL2)
portfolio$area <- factor(portfolio$area)
# Illustrative years; the source data have no calendar-year field.
portfolio$year <- rep(2023:2024, length.out = nrow(portfolio))
model <- glm(nclaims ~ area + offset(log(exposure)),
             family = poisson(), data = portfolio)

autoplot(check_calibration(model, portfolio))

autoplot(check_calibration(model, portfolio, groups = 10))

autoplot(check_calibration(model, portfolio, by = "year"))

autoplot(check_calibration(model, portfolio, by = "area"))

```
