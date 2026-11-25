# Check observed versus expected insurance experience

Compare outcomes observed in the supplied data with outcomes expected
under the fitted GLM. The ratio O/E is calculated as
`Observed / Expected`. Compare totals overall or by calendar year,
rating factor, combinations of columns or predicted risk. The model is
not changed.

## Usage

``` r
calibration_analysis(
  model,
  data,
  by = NULL,
  groups = NULL,
  conf_level = NULL,
  interval_method = c("auto", "poisson", "wald")
)
```

## Arguments

- model:

  A fitted Poisson, quasi-Poisson, Gamma or Tweedie GLM.

- data:

  A non-empty data frame with the response and variables used in the
  model, including any weight and offset variables. Responses,
  predictions and weights must be finite and non-negative. For a refined
  model's own portfolio, use
  [`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md)
  to include the columns created by the refinement.

- by:

  Optional character vector of distinct grouping column names, such as
  `c("year", "area")`. Only combinations present in the data are
  returned. Results are sorted in the order of the supplied columns.
  Factor levels follow their declared order, and missing values come
  last within each column.

- groups:

  Optional number of groups formed by sorting records from lowest to
  highest predicted risk. Supply a positive whole number no larger than
  the number of rows. Use either `groups` or `by`, not both.

- conf_level:

  Optional confidence level. The default `NULL` leaves the result
  unchanged and does not compute intervals.

- interval_method:

  `"auto"`, `"poisson"` (exact count interval) or `"wald"`
  (variance-based log-Wald approximation).

## Value

A data frame of class `calibration_result` with the totals `Observed`
and `Expected`, their ratio `OE` and the number of observations `N`. If
`Expected` is zero, `OE` is `NA`. The grouping columns or integer
`Group` are included when applicable. `Exposure` is the sum of the
identified exposure column, without applying case weights. When
intervals are requested, `Lower`, `Upper`, `Conf_level` and
`Interval_method` describe the O/E interval.

## Details

Overall totals show calibration-in-the-large. Risk groups check whether
the model over- or under-predicts at different predicted risk levels. A
calendar-year grouping checks temporal calibration. Changing O/E over
time does not necessarily mean the model has become worse at ranking
risks.

### Conditional uncertainty

Intervals estimate an underlying observed-to-expected ratio, treating
the predictions as fixed and observations as independent. They are not
prediction intervals and do not include parameter uncertainty or
clustering. For unweighted Poisson counts, the exact limits for the
total observed count are divided by the expected total. An observed zero
has lower limit zero. For Gamma, Tweedie and quasi-Poisson models, the
log-Wald interval uses the fitted dispersion and sums the GLM variances
at the means scaled by the group's estimated ratio. Prior weights are
assumed to be precision weights: a response with weight `w` contributes
`w` times its mean to the total and `w` times the unweighted variance.
Zero observed or expected totals have no log-Wald interval. Weighted
Poisson models and ambiguous exposure weights are not supported for
intervals.

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

### Calibration by predicted risk

With `groups = 5`, group 1 contains the lowest predicted risks and group
5 the highest. With a recognised exposure offset, the ranking uses
predictions per unit of exposure. For a claim-count model this means
predicted claims per policy-year rather than predicted claims for the
observed period. A longer policy period therefore does not by itself put
a policy in a higher risk group. Without an offset, the function ranks
the response-scale predictions. If an offset cannot be identified as
exposure, use `by` or overall calibration instead.

The sorted records are divided so each group has roughly the same total
exposure (about 20% each for five groups), without splitting any record.
If exposure is unavailable or totals zero, the aim is equal row counts.
Ties follow the input row order, so policies with the same prediction on
the ranking scale can appear in different groups.

Compare the observed and expected totals within each group. For example,
24 observed claims versus 20 expected means O/E is 1.2: the model
underpredicts for that group. Look for patterns across groups and
consider exposure and claim volume before interpreting a gap. This is a
calibration diagnostic, not a hypothesis test or a measure of how well
the model ranks individual risks. Use separate evaluation data to assess
calibration outside the estimation sample.

### Calibration after refinement

Use the fitted model returned by
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
not the refinement plan.
`calibration_analysis(smoothed_model, extract_model_data(smoothed_model))`
evaluates the smoothed model on its own portfolio. To review the new
tariff bands, use the band column as `by`: for example,
`by = "age_smooth"` when smoothing from `source_variable = "age"`. The
numeric relativity column used by the model is a separate column. For a
separate evaluation portfolio, create both columns using the same
refinement mappings. When a grouping column is unavailable, the error
can suggest the related smoothing band or a similar column name. A valid
`by` is never replaced: `by = "age"` groups by the original ages, not
the tariff bands.

To compare observed outcomes with the expected outcomes before and after
a restriction, evaluate both fitted models on the same records and group
them by the same `by` column. Do not match `groups` by group number
across models: each model can rank the records differently.

## See also

[`autoplot.calibration_result()`](https://mharinga.github.io/insurancerating/reference/autoplot.calibration_result.md),
[`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md),
[`validate_performance()`](https://mharinga.github.io/insurancerating/reference/validate_performance.md),
[`adjust_intercept()`](https://mharinga.github.io/insurancerating/reference/adjust_intercept.md),
[`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md),
[`audit_refinement()`](https://mharinga.github.io/insurancerating/reference/audit_refinement.md)

## Examples

``` r
portfolio <- as.data.frame(MTPL2)
portfolio$area <- factor(portfolio$area)
# Illustrative calendar years; MTPL2 does not contain actual policy dates.
portfolio$year <- rep(2023:2024, length.out = nrow(portfolio))
model <- glm(nclaims ~ area + offset(log(exposure)),
             family = poisson(), data = portfolio)

calibration_analysis(model, portfolio)
#>   Observed Expected OE    N Exposure
#> 1      358      358  1 3000 2662.586
calibration_analysis(model, portfolio, by = "year")
#>   year Observed Expected        OE    N Exposure
#> 1 2023      177 179.3463 0.9869173 1500 1332.923
#> 2 2024      181 178.6537 1.0131334 1500 1329.663
# The area-only model matches each area's total on its estimation data.
calibration_analysis(model, portfolio, by = "area")
#>   area Observed Expected OE    N   Exposure
#> 1    0        1        1  1   15   13.30685
#> 2    1      146      146  1 1194 1065.74795
#> 3    2       98       98  1  921  818.53973
#> 4    3      113      113  1  870  764.99178
calibration_analysis(model, portfolio, by = c("year", "area"))
#>   year area Observed   Expected        OE   N   Exposure
#> 1 2023    0        1  0.6483426 1.5423944  10   8.627397
#> 2 2023    1       72 70.0086736 1.0284440 574 511.038356
#> 3 2023    2       46 48.9371852 0.9399805 460 408.745205
#> 4 2023    3       58 59.7521363 0.9706766 456 404.512329
#> 5 2024    0        0  0.3516574 0.0000000   5   4.679452
#> 6 2024    1       74 75.9913264 0.9737953 620 554.709589
#> 7 2024    2       52 49.0628148 1.0598658 461 409.794521
#> 8 2024    3       55 53.2478637 1.0329053 414 360.479452

# Compare observed and expected rates in roughly equal-exposure groups
# on records not used to fit the model.
risk_portfolio <- as.data.frame(MTPL)
set.seed(123)
train <- sample.int(nrow(risk_portfolio), floor(0.7 * nrow(risk_portfolio)))
risk_model <- glm(
  nclaims ~ age_policyholder + offset(log(exposure)),
  family = poisson(), data = risk_portfolio[train, ]
)
calibration_analysis(risk_model, risk_portfolio[-train, ], groups = 5)
#>   Group Observed Expected        OE    N Exposure
#> 1     1      150 148.5634 1.0096701 1722 1598.134
#> 2     2      194 187.1246 1.0367421 1773 1597.397
#> 3     3      203 219.1066 0.9264898 1801 1597.888
#> 4     4      216 254.3768 0.8491341 1842 1597.666
#> 5     5      314 299.0661 1.0499351 1862 1598.115

# Same O/E, but much less uncertainty in the larger group.
evaluation <- data.frame(claims = c(10, 1000), exposure = c(10, 1000),
                         size = c("Small", "Large"))
count_model <- glm(claims ~ 1 + offset(log(exposure)), poisson(), evaluation)
calibration_analysis(count_model, evaluation, by = "size", conf_level = 0.95)
#>    size Observed Expected OE N Exposure    Upper     Lower Conf_level
#> 1 Large     1000     1000  1 1     1000 1.063952 0.9389730       0.95
#> 2 Small       10       10  1 1       10 1.839036 0.4795389       0.95
#>   Interval_method
#> 1         poisson
#> 2         poisson
```
