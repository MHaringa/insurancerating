# Assess predictive stability by repeated resampling

Refit a pricing model on repeated samples and record the resulting
response-scale prediction error. The distribution of RMSE values
describes how sensitive model performance is to changes in the observed
portfolio sample.

## Usage

``` r
bootstrap_performance(
  model,
  data,
  n_resamples = 50,
  sample_fraction = 1,
  metric = "rmse",
  sampling = c("bootstrap", "split"),
  show_progress = TRUE,
  rmse_model = NULL,
  n = NULL,
  frac = NULL
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

  Fraction of the data used in the training sample. Must be in `(0, 1]`.
  Default is 1.

- metric:

  Character string. Performance metric to compute. Currently only
  `"rmse"` is supported.

- sampling:

  Character string. Sampling scheme. `"bootstrap"` samples training rows
  with replacement and evaluates on out-of-bag rows when
  `sample_fraction < 1`. `"split"` samples training rows without
  replacement and evaluates on the remaining rows when
  `sample_fraction < 1`.

- show_progress:

  Logical. Show a progress bar during resampling. Default is `TRUE`.

- rmse_model:

  Optional finite numeric RMSE for the original fitted model. If `NULL`,
  it is calculated from `model` and `data`.

- n, frac:

  Deprecated argument names. Use `n_resamples` and `sample_fraction`
  instead.

## Value

An object of class `"bootstrap_performance"`, which is a list with
components:

- rmse_bs:

  Numeric vector with `n_resamples` bootstrap RMSE values.

- rmse_mod:

  Root mean squared error for the original fitted model.

- metric:

  Metric name.

- sampling:

  Sampling scheme.

## Details

### Resampling design

With `sampling = "bootstrap"`, training rows are sampled with
replacement. With `sampling = "split"`, they are sampled without
replacement. When `sample_fraction < 1`, performance is evaluated on
records not used for fitting. When `sample_fraction = 1`, performance is
evaluated on the sampled training data and should be interpreted as an
in-sample stability measure.

Character columns and factor columns are converted to factors with
levels taken from the full input data before resampling. For factor
variables used in the model, the training sample is augmented when
needed so every observed level is represented at least once. This
prevents prediction failures when a level is present in the evaluation
data but absent from a particular training sample.

### Actuarial interpretation

The resampled RMSE distribution is useful for comparing the stability of
alternative frequency, severity or risk-premium specifications under
repeated portfolio sampling. A narrow distribution indicates that the
measured error is relatively insensitive to the sampled records; a wide
distribution indicates greater sampling sensitivity.

This is an experience-based diagnostic and does not by itself represent
the full uncertainty in future claims, trend, portfolio mix or model
specification. Sparse factor levels are retained in training samples
where necessary to avoid new-level prediction failures. That protection
is useful operationally, but should be considered when interpreting the
resampling design.

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

# Use all records
x <- bootstrap_performance(mod1, MTPL, n_resamples = 80,
                           show_progress = FALSE)
print(x)
autoplot(x)

# Use 80% of records and evaluate on the remaining records
x_frac <- bootstrap_performance(mod1, MTPL, n_resamples = 50,
                                sample_fraction = .8, sampling = "split",
                                show_progress = FALSE)
autoplot(x_frac)
} # }
```
