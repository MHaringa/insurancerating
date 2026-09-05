# Assess GLM coefficient stability by portfolio-row bootstrap

Refit a GLM on repeated bootstrap samples of the estimation portfolio
and retain the coefficient estimates from every successful refit. The
resulting distribution describes how sensitive individual model
coefficients are to sampling variation in the observed portfolio.

## Usage

``` r
bootstrap_coefficients(
  object,
  n_resamples = 500,
  seed = NULL,
  show_progress = interactive()
)
```

## Arguments

- object:

  A fitted `glm` object. Refined GLMs are accepted when their estimation
  data can be recovered from the model object.

- n_resamples:

  Positive whole number. Number of bootstrap samples. Default is 500.

- seed:

  Optional single numeric seed for reproducible resampling.

- show_progress:

  Logical. If `TRUE`, display a text progress bar.

## Value

An object of class `"bootstrap_coefficients"`. It contains the original
coefficients, a coefficient matrix with one row per requested resample,
indicators for successful model fits, recorded failure messages, and the
resampling settings. Use
[`summary.bootstrap_coefficients()`](https://mharinga.github.io/insurancerating/reference/summary.bootstrap_coefficients.md)
for a coefficient-level data frame and
[`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)
for a formatted table.

## Details

Each resample contains the same number of portfolio rows as the original
estimation data and is drawn with replacement. The function recovers
these data from `object`; a separate `data` argument is deliberately not
required. Rows omitted during the original model fit are excluded so the
resampling population remains aligned with the fitted GLM.

Original factor levels, the model formula, offsets and model weights are
retained during refitting. A factor level may nevertheless be absent
from a particular bootstrap sample. Its coefficient can then be
non-estimable and is stored as `NA` for that replicate.

A failed or non-converged GLM refit does not stop the procedure. The
failed replicate is recorded and the function continues. After
resampling, an informative message reports how many requested refits
produced usable model objects.
[`summary.bootstrap_coefficients()`](https://mharinga.github.io/insurancerating/reference/summary.bootstrap_coefficients.md)
reports the number of finite estimates separately for each coefficient.

### Actuarial interpretation

The bootstrap distribution can identify tariff effects that are
sensitive to the particular portfolio sample. Wide intervals, material
bootstrap bias or a low number of estimable replicates often indicate
sparse levels, correlated model terms or limited claim information.
These diagnostics should be considered alongside exposure, claim counts,
coefficient interpretation and stability across calendar periods.

The row bootstrap represents sampling variation in the observed
estimation portfolio. It does not include future trend, parameter
uncertainty caused by model selection, structural changes in portfolio
composition or dependence between repeated records for the same policy.
Where such dependence is material, a cluster-level bootstrap would
require a different resampling design.

## See also

[`summary.bootstrap_coefficients()`](https://mharinga.github.io/insurancerating/reference/summary.bootstrap_coefficients.md),
[`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md),
[`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md),
[`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)

## Author

Martin Haringa

## Examples

``` r
model <- glm(
  nclaims ~ age_policyholder + zip + offset(log(exposure)),
  family = poisson(),
  data = MTPL
)

if (FALSE) { # \dontrun{
boot <- bootstrap_coefficients(
  model,
  n_resamples = 25,
  seed = 123,
  show_progress = FALSE
)

summary(boot, scale = "link")
summary(boot, scale = "exponentiated")
summary(boot, scale = "relativity")

if (requireNamespace("gt", quietly = TRUE)) {
  as_gt(boot, scale = "relativity")
}
} # }
```
