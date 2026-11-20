# Check observed versus expected insurance experience

Compare the observed and predicted totals from a fitted GLM, overall or
by calendar year, rating factor or predicted risk. The model is not
changed.

## Usage

``` r
check_calibration(model, data, by = NULL, groups = NULL)
```

## Arguments

- model:

  A fitted Poisson, quasi-Poisson, Gamma or Tweedie GLM.

- data:

  A non-empty data frame with the response and variables used in the
  model, including any weight and offset variables. Responses,
  predictions and weights must be finite and non-negative.

- by:

  Optional name of one grouping column. Factor levels follow their
  declared order; other values are sorted. Missing values form a
  separate group at the end.

- groups:

  Optional number of groups of increasing predicted risk. Supply a
  positive whole number no larger than the number of rows. Use either
  `groups` or `by`, not both.

## Value

A data frame with `Observed`, `Expected`, `OE` (observed divided by
expected) and `N` (number of observations). If expected is zero, `OE` is
`NA`. The grouping column or integer `Group` is included when
applicable. `Exposure` is the sum of the identified exposure column,
without applying any case weights.

## Details

Overall totals show calibration-in-the-large. Risk groups check whether
the model over- or under-predicts at different predicted risk levels. A
calendar-year grouping checks temporal calibration. Changing O/E over
time does not necessarily mean the model has become worse at ranking
risks.

Predictions are on the response scale and use the offset in `data`. For
Poisson claim counts with a log-exposure offset, predictions are
expected counts and are not multiplied by exposure again. Other prior
weights are used for weighted totals, for example claim counts in a
weighted average-severity GLM or exposure in a claim-rate GLM. If a
recognised offset already uses the same column as the prior weights,
that column is not applied again; weights that use it in a more complex
expression are rejected as ambiguous. Specify weights using columns in
`data`; weights stored as a fixed vector cannot be matched to new rows.

A simple `log(column)` offset in a log-link GLM identifies an exposure
column. It may be supplied in the formula, as `offset = log(column)`, or
as one term of an additive offset. Exposure must be positive. With
several candidate terms, a single column named `exposure` takes
precedence. Without an offset, explicit `weights = exposure`, or weights
on the denominator column of a response such as `I(claims / duration)`,
identify exposure used as prior weights. Other weight variables and
unused columns are not assumed to represent exposure.

Risk groups use expected outcomes per unit exposure for models with a
recognised exposure offset, and response-scale predictions otherwise.
For models with an offset that cannot be identified as exposure, use
`by` or overall calibration rather than risk groups. Groups approximate
equal exposure by cutting at the nearest cumulative-exposure targets
without splitting records. If exposure is unavailable or totals zero,
groups approximate equal row counts. Ties are broken in input row order.

## See also

[`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md),
[`resample_performance()`](https://mharinga.github.io/insurancerating/reference/resample_performance.md),
[`adjust_intercept()`](https://mharinga.github.io/insurancerating/reference/adjust_intercept.md)

## Examples

``` r
portfolio <- as.data.frame(MTPL2)
portfolio$area <- factor(portfolio$area)
# Illustrative calendar years; MTPL2 does not contain actual policy dates.
portfolio$year <- rep(2023:2024, length.out = nrow(portfolio))
model <- glm(nclaims ~ area + offset(log(exposure)),
             family = poisson(), data = portfolio)

check_calibration(model, portfolio)
#>   Observed Expected OE    N Exposure
#> 1      358      358  1 3000 2662.586
check_calibration(model, portfolio, by = "year")
#>   year Observed Expected        OE    N Exposure
#> 1 2023      177 179.3463 0.9869173 1500 1332.923
#> 2 2024      181 178.6537 1.0131334 1500 1329.663
check_calibration(model, portfolio, by = "area")
#>   area Observed Expected OE    N   Exposure
#> 1    0        1        1  1   15   13.30685
#> 2    1      146      146  1 1194 1065.74795
#> 3    2       98       98  1  921  818.53973
#> 4    3      113      113  1  870  764.99178
check_calibration(model, portfolio, groups = 10)
#>    Group Observed Expected        OE   N Exposure
#> 1      1       23 31.26429 0.7356637 293 266.0877
#> 2      2       35 31.95487 1.0952946 306 266.9014
#> 3      3       35 31.83351 1.0994704 300 265.8877
#> 4      4       41 35.85394 1.1435286 298 265.8767
#> 5      5       38 36.54710 1.0397541 296 266.7808
#> 6      6       32 36.39960 0.8791305 297 265.7041
#> 7      7       40 36.57263 1.0937142 303 266.9671
#> 8      8       38 38.92222 0.9763060 305 265.9205
#> 9      9       39 39.32916 0.9916307 301 266.2521
#> 10    10       37 39.32268 0.9409328 301 266.2082
```
