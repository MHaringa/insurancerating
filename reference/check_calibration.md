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
  predictions and weights must be finite and non-negative. For a refined
  model's own portfolio, use
  [`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md)
  to include the columns created by the refinement.

- by:

  Optional name of one grouping column. Factor levels follow their
  declared order; other values are sorted. Missing values form a
  separate group at the end.

- groups:

  Optional number of groups formed by sorting records from lowest to
  highest predicted risk. Supply a positive whole number no larger than
  the number of rows. Use either `groups` or `by`, not both.

## Value

A data frame of class `calibration_result` with `Observed`, `Expected`,
`OE` (observed divided by expected) and `N` (number of observations). If
expected is zero, `OE` is `NA`. The grouping column or integer `Group`
is included when applicable. `Exposure` is the sum of the identified
exposure column, without applying any case weights.

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
`check_calibration(smoothed_model, extract_model_data(smoothed_model))`
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
[`resample_performance()`](https://mharinga.github.io/insurancerating/reference/resample_performance.md),
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
# Compare the lowest to highest predicted claim frequencies.
check_calibration(model, portfolio, groups = 5)
#>   Group Observed Expected        OE   N Exposure
#> 1     1       58 63.21916 0.9174434 599 532.9890
#> 2     2       76 67.68745 1.1228079 598 531.7644
#> 3     3       70 72.94670 0.9596047 593 532.4849
#> 4     4       78 75.49485 1.0331831 608 532.8877
#> 5     5       76 78.65184 0.9662838 602 532.4603
```
