# Bootstrapped model performance

Generate `n` bootstrap replicates to compute `n` root mean squared
errors (RMSE). This can be used to evaluate the predictive stability of
a fitted model.

`bootstrap_rmse()` is deprecated in favour of `bootstrap_performance()`.

## Usage

``` r
bootstrap_performance(
  model,
  data,
  n = 50,
  frac = 1,
  show_progress = TRUE,
  rmse_model = NULL
)

bootstrap_rmse(
  model,
  data,
  n = 50,
  frac = 1,
  show_progress = TRUE,
  rmse_model = NULL
)
```

## Arguments

- model:

  A fitted model object.

- data:

  Data used to fit the model object.

- n:

  Integer. Number of bootstrap replicates. Default = 50.

- frac:

  Fraction of the data used in the training set if cross-validation is
  applied. Must be in (0, 1\]. Default = 1 (use all data).

- show_progress:

  Logical. Show progress bar during bootstrap iterations. Default =
  TRUE.

- rmse_model:

  Optional numeric RMSE of the fitted (original) model. If NULL
  (default), it is computed automatically.

## Value

An object of class `"bootstrap_performance"`, which is a list with
components:

- rmse_bs:

  Numeric vector with `n` bootstrap RMSE values.

- rmse_mod:

  Root mean squared error for the original fitted model.

## Details

To test the predictive ability of a fitted model it can be helpful to
assess the variation in the computed RMSE. The variation is calculated
by refitting the model on `n` bootstrap replicates and storing the
resulting RMSE values.

- If `frac = 1`, each bootstrap sample has the same size as the dataset.

- If `frac < 1`, a subset of the data is used as training, and the
  remainder as test set (cross-validation).

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
mod1 <- glm(nclaims ~ age_policyholder, data = MTPL,
            offset = log(exposure), family = poisson())

# Use all records
x <- bootstrap_performance(mod1, MTPL, n = 80, show_progress = FALSE)
print(x)
autoplot(x)

# Use 80% of records (cross-validation style)
x_frac <- bootstrap_performance(mod1, MTPL, n = 50, frac = .8, show_progress = FALSE)
autoplot(x_frac)
} # }
```
