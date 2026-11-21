# Evaluate model performance across resamples or time windows

Refit a pricing model and score its predictions across resamples or time
windows. By default, the model is fitted and scored on the same
bootstrap sample. Evaluation on rows outside that fit depends on the
sampling design.

## Usage

``` r
validate_performance(
  model,
  data,
  n_resamples = 50,
  sample_fraction = 1,
  metric = "rmse",
  sampling = c("bootstrap", "split"),
  show_progress = TRUE,
  rmse_model = NULL,
  n = NULL,
  frac = NULL,
  metrics = "rmse",
  method = NULL,
  time = NULL,
  initial = NULL,
  horizon = 1
)
```

## Arguments

- model:

  A fitted model object that can be updated on resampled data.

- data:

  Data frame containing the model response and predictors.

- n_resamples:

  Positive whole number. Number of resampling replicates. Default is 50.

- sample_fraction:

  Fraction of input rows to draw for each random training sample, before
  adding any rows for missing factor levels. Must be in `(0, 1]`.
  Default is 1. Not used for rolling validation.

- metric:

  Legacy argument that accepts only `"rmse"`. Use `metrics` to select
  metrics.

- sampling:

  Character string. Use `"bootstrap"` (default) to sample training rows
  with replacement, `"split"` to sample without replacement, or
  `"rolling"` for expanding-window temporal validation.

- show_progress:

  Logical. Show a progress bar during resampling. Default is `TRUE`.

- rmse_model:

  Optional finite numeric RMSE for the original fitted model. If `NULL`
  and RMSE is selected, it is calculated from `model` and `data`.

- n, frac:

  Deprecated argument names. Use `n_resamples` and `sample_fraction`
  instead.

- metrics:

  Character vector of distinct metrics in the desired output order:
  `"rmse"` (default), `"mae"`, or `"deviance"`.

- method:

  Alternative name for `sampling`. Supply only one of the two.

- time:

  For rolling validation, name of a column containing numeric periods,
  dates, date-times or an ordered factor. Missing periods are not
  allowed.

- initial:

  For rolling validation, number of distinct periods in the first
  training window.

- horizon:

  For rolling validation, number of subsequent distinct periods
  evaluated at each origin. Default is 1.

## Value

For rolling validation, a data frame with one row per origin:
`iteration`, `train_start`, `train_end`, `test_start`, `test_end`,
`n_train`, `n_test` and the requested metric columns. Counts refer to
the input rows in each window. Scores are calculated on the test rows;
binomial prediction errors use proportions for grouped responses.

For random resampling, a list of class `"bootstrap_performance"`. For
RMSE alone, the original structure is unchanged: `rmse_bs` contains the
scores, `rmse_mod` contains the original model's RMSE, and `metric`,
`sampling`, `n_resamples`, `sample_fraction`, `n` and `frac` describe
the run. When another metric is selected, `scores` is a data frame with
one row per replicate and a column for each requested metric, and
`original_scores` is a named numeric vector for the original model on
`data`. `metric` records the selected metric names; `rmse_bs` and
`rmse_mod` are `NULL` if RMSE was not selected. For these results,
[`as.vector()`](https://rdrr.io/r/base/vector.html) returns scores for
the first requested metric.

## Details

### Resampling design

|  |  |  |
|----|----|----|
| Design | Training rows | Evaluation rows |
| Bootstrap, fraction 1 | Full-size draw with replacement | Same sample (in sample) |
| Bootstrap, fraction \< 1 | Draw with replacement | Out-of-bag rows |
| Split, fraction \< 1 | Draw without replacement | Remaining rows |
| Split, fraction 1 | All input rows | Same rows (in sample) |
| Rolling | Expanding window | Subsequent periods |

In a bootstrap, `sample_fraction = 0.8` means 80 draws per 100 input
rows, with replacement. Some rows can be drawn more than once, so the
out-of-bag set is not a fixed 20% of the data.

For random resampling, factor levels come from the supplied data. For
factor columns used directly in the model, rows may be added to the
training sample to cover levels that were not drawn. These rows are then
excluded from held-out evaluation. If no evaluation rows remain, try a
smaller `sample_fraction`.

### Rolling-origin validation

With `sampling = "rolling"`, fit the GLM to the earliest `initial`
periods, score the next `horizon` periods, then advance the origin by
one observed period at a time. Training and test periods never overlap
within a window, but test windows can overlap across origins when
`horizon > 1`. The number of windows follows from the available periods,
not `n_resamples` or `sample_fraction`.

Supply an unrestricted
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) as the model
specification. It is fitted on each training window. Transformations
such as [`scale()`](https://rdrr.io/r/base/scale.html) learn their
parameters from the training rows. Precomputed predictors must not use
future data. Supply weights and offsets using columns in `data`, and
filter any intended subset before calling the function.
Observation-level starting values are not supported. A factor level
unseen in training produces an error; future observations are not added
to the training window.

Rolling validation assesses a specification repeatedly fitted to
history. For temporal calibration of one fixed fitted model, use
[`calibration_analysis()`](https://mharinga.github.io/insurancerating/reference/calibration_analysis.md)
with `by` set to the period column.

### Metrics and interpretation

RMSE (root mean squared error) and MAE (mean absolute error) are
unweighted summaries of response-scale errors. For an exposure-offset
Poisson model these are errors in claim counts, not claim frequency per
unit exposure. For a binomial GLM, the errors are on the probability
scale: two-column responses are converted to proportions, and factor
responses use the fitted model's reference level to define zero.
Predictive deviance sums the fitted GLM family's deviance contributions
on the evaluation rows, using those rows' response-scale predictions
(including offsets) and observation weights. Specify weights and offsets
using columns in `data`, rather than separate row-aligned vectors, so
the model can be refitted and evaluated on each sample. Total deviance
can vary with the number and weight of evaluation rows as well as with
prediction quality. On the original estimation rows with the same
predictions and weights, it agrees with the residual deviance reported
by
[`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md).

Under random resampling, the score distribution reflects variation in
both the refitted models and the rows used to score them. It does not
isolate coefficient stability; use
[`coefficient_stability()`](https://mharinga.github.io/insurancerating/reference/coefficient_stability.md)
for that. Even with held-out rows, resampling the observed portfolio is
not independent validation on a future or external portfolio.

## See also

[`rmse()`](https://mharinga.github.io/insurancerating/reference/rmse.md),
[`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md),
[`autoplot.bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/autoplot.bootstrap_performance.md)

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
mod1 <- glm(nclaims ~ age_policyholder, data = MTPL,
            offset = log(exposure), family = poisson())

# Draw with replacement and score on the resampled training rows.
x <- validate_performance(mod1, MTPL, n_resamples = 80,
                          show_progress = FALSE)
print(x)
autoplot(x)

# Draw 80% of the rows without replacement; score on the held-out rows.
x_frac <- validate_performance(mod1, MTPL, n_resamples = 50,
                               sample_fraction = .8, sampling = "split",
                               show_progress = FALSE,
                               metrics = c("rmse", "mae", "deviance"))
head(x_frac$scores)
autoplot(x_frac, metric = "rmse")
} # }

# Illustrative years; MTPL2 does not contain real policy dates.
portfolio <- as.data.frame(MTPL2)
portfolio$year <- rep(2020:2025, length.out = nrow(portfolio))
model <- glm(nclaims ~ factor(area) + offset(log(exposure)),
             family = poisson(), data = portfolio)
validate_performance(
  model, portfolio, sampling = "rolling", time = "year", initial = 3,
  metrics = c("rmse", "mae", "deviance"), show_progress = FALSE
)
#>   iteration train_start train_end test_start test_end n_train n_test      rmse
#> 1         1        2020      2022       2023     2023    1500    500 0.3148918
#> 2         2        2020      2023       2024     2024    2000    500 0.3402196
#> 3         3        2020      2024       2025     2025    2500    500 0.3490226
#>         mae deviance
#> 1 0.2056950 237.7274
#> 2 0.2084982 259.1681
#> 3 0.2086374 255.4882
```
