# Check overdispersion of a Poisson claim frequency model

Tests whether a fitted Poisson GLM shows overdispersion using Pearson's
chi-squared statistic.

## Usage

``` r
check_overdispersion(object)
```

## Arguments

- object:

  A fitted model of class `"glm"` with family Poisson.

## Value

An object of class `"overdispersion_check"` and `"overdispersion"`,
which is a list with elements:

- pearson_chisq:

  Pearson's chi-squared statistic.

- dispersion_ratio:

  Dispersion ratio, calculated as Pearson's chi-squared statistic
  divided by residual degrees of freedom.

- residual_df:

  Residual degrees of freedom.

- p_value:

  P-value from the chi-squared test.

For backwards compatibility the object also contains the aliases
`chisq`, `ratio`, `rdf`, and `p`.

## Details

In Poisson claim frequency models, the variance is assumed to be equal
to the mean. A dispersion ratio above 1 indicates that the observed
variation is larger than expected under that assumption. In pricing work
this can be a useful diagnostic signal for omitted heterogeneity,
clustering, outliers, or model misspecification. It does not
automatically mean that the model is unusable.

- A dispersion ratio close to 1 is broadly consistent with the Poisson
  variance assumption.

- A dispersion ratio above 1 suggests overdispersion.

- A p-value below 0.05 indicates statistically significant
  overdispersion.

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
