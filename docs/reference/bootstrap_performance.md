# Bootstrapped model performance

Generate repeated train/evaluation samples to compute model performance.
Currently, the supported metric is root mean squared error (RMSE).

`bootstrap_rmse()` is deprecated in favour of `bootstrap_performance()`.
Objects returned by `bootstrap_rmse()` keep class `"bootstrap_rmse"` for
backward compatibility and also inherit from `"bootstrap_performance"`.

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

bootstrap_rmse(
  model,
  data,
  n = 50,
  frac = 1,
  metric = "rmse",
  sampling = c("bootstrap", "split"),
  show_progress = TRUE,
  rmse_model = NULL
)
```

## Arguments

- model:

  A fitted model object.

- data:

  Data used to fit the model object.

- n_resamples:

  Integer. Number of resampling replicates. Default = 50.

- sample_fraction:

  Fraction of the data used in the training sample. Must be in `(0, 1]`.
  Default = 1.

- metric:

  Character. Performance metric to compute. Currently only `"rmse"` is
  supported.

- sampling:

  Character. Sampling scheme. `"bootstrap"` samples training rows with
  replacement and evaluates on out-of-bag rows when
  `sample_fraction < 1`. `"split"` samples training rows without
  replacement and evaluates on the remaining rows when
  `sample_fraction < 1`.

- show_progress:

  Logical. Show progress bar during bootstrap iterations. Default =
  TRUE.

- rmse_model:

  Optional numeric RMSE of the fitted (original) model. If NULL
  (default), it is computed automatically.

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

To test the predictive stability of a fitted model it can be helpful to
assess the variation in a performance metric. The variation is
calculated by refitting the model on repeated samples and storing the
resulting metric values.

- If `sample_fraction = 1`, the metric is evaluated on the sampled
  training data.

- If `sample_fraction < 1`, the metric is evaluated on rows that were
  not used for training.

Character columns and factor columns are converted to factors with
levels taken from the full input data before resampling. For factor
variables used in the model, the training sample is augmented when
needed so every observed level is represented at least once. This
prevents prediction failures when a level is present in the evaluation
data but absent from a particular training sample.

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
