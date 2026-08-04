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

boot <- bootstrap_coefficients(
  model,
  n_resamples = 25,
  seed = 123,
  show_progress = FALSE
)

summary(boot, scale = "link")
#>               term      estimate bootstrap_mean          bias bootstrap_se
#> 1      (Intercept) -1.1636200937   -1.220009055 -0.0563889610  0.249137624
#> 2 age_policyholder -0.0170418702   -0.016712389  0.0003294813  0.001285304
#> 3             zip1 -0.0006505428    0.039395119  0.0400456614  0.232329635
#> 4             zip2 -0.1037738083   -0.067240901  0.0365329070  0.243555770
#> 5             zip3 -0.0456536315   -0.007005003  0.0386486282  0.225874270
#>         lower       upper n_successful n_requested success_rate
#> 1 -1.68483701 -0.82875913           25          25            1
#> 2 -0.01939017 -0.01443457           25          25            1
#> 3 -0.32506741  0.50862020           25          25            1
#> 4 -0.41445244  0.40491816           25          25            1
#> 5 -0.35596929  0.40830170           25          25            1
summary(boot, scale = "exponentiated")
#>               term  estimate bootstrap_mean         bias bootstrap_se     lower
#> 1      (Intercept) 0.3123534      0.3039502 -0.008403155  0.073652441 0.1856046
#> 2 age_policyholder 0.9831025      0.9834273  0.000324747  0.001263773 0.9807967
#> 3             zip1 0.9993497      1.0684033  0.069053585  0.263559137 0.7225523
#> 4             zip2 0.9014292      0.9629808  0.061551629  0.249736638 0.6609599
#> 5             zip3 0.9553728      1.0182805  0.062907708  0.241018199 0.7004985
#>       upper n_successful n_requested success_rate
#> 1 0.4365920           25          25            1
#> 2 0.9856691           25          25            1
#> 3 1.6686660           25          25            1
#> 4 1.5047469           25          25            1
#> 5 1.5113859           25          25            1
summary(boot, scale = "relativity")
#>               term  estimate bootstrap_mean         bias bootstrap_se     lower
#> 1      (Intercept) 0.3123534      0.3039502 -0.008403155  0.073652441 0.1856046
#> 2 age_policyholder 0.9831025      0.9834273  0.000324747  0.001263773 0.9807967
#> 3             zip1 0.9993497      1.0684033  0.069053585  0.263559137 0.7225523
#> 4             zip2 0.9014292      0.9629808  0.061551629  0.249736638 0.6609599
#> 5             zip3 0.9553728      1.0182805  0.062907708  0.241018199 0.7004985
#>       upper n_successful n_requested success_rate
#> 1 0.4365920           25          25            1
#> 2 0.9856691           25          25            1
#> 3 1.6686660           25          25            1
#> 4 1.5047469           25          25            1
#> 5 1.5113859           25          25            1

if (requireNamespace("gt", quietly = TRUE)) {
  as_gt(boot, scale = "relativity")
}


  


Term
```
