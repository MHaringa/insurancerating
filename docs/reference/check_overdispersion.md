# Check overdispersion of a Poisson GLM

Tests whether a fitted Poisson regression model is overdispersed using
Pearson's chi-squared statistic.

## Usage

``` r
check_overdispersion(object)
```

## Arguments

- object:

  A fitted model of class `"glm"` with family Poisson.

## Value

An object of class `"overdispersion"`, which is a list with elements:

- chisq:

  Pearson's chi-squared statistic.

- ratio:

  Dispersion ratio (chisq / residual df).

- rdf:

  Residual degrees of freedom.

- p:

  P-value from chi-squared test.

## Details

- A dispersion ratio close to 1 indicates a good Poisson fit.

- A dispersion ratio \> 1 suggests overdispersion.

- A p-value \< 0.05 indicates significant overdispersion.

- A dispersion ratio \> 2 usually means a more serious lack of fit (e.g.
  outliers or misspecified model).

## References

Bolker B. et al. (2017). [GLMM
FAQ](http://bbolker.github.io/mixedmodels-misc/glmmFAQ.md) See also:
`performance::check_overdispersion()`.

## Author

Martin Haringa

## Examples

``` r
x <- glm(nclaims ~ area, offset = log(exposure),
         family = poisson(), data = MTPL2)
check_overdispersion(x)
#> Dispersion ratio =    1.229
#> Pearson's Chi-squared = 3684.679
#> p-value =  < 0.001
#> 
#> Overdispersion detected.
```
