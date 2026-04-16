# Check model residuals

Detect overall deviations of residuals from the expected distribution
using a simulation-based approach. Provides standardized residuals that
are more interpretable for GLMs than classical residual plots.

## Usage

``` r
check_residuals(object, n_simulations = 30)
```

## Arguments

- object:

  A fitted model object (e.g. of class `"glm"`).

- n_simulations:

  Number of simulations to generate residuals. Default = 30.

## Value

An object of class `"check_residuals"`, which is a list with:

- df:

  Data frame with theoretical quantiles (`x`) and observed residuals
  (`y`).

- p.val:

  P-value from Kolmogorov-Smirnov test against uniform(0,1).

Invisibly returns the object.

## Details

Misspecifications in GLMs cannot reliably be diagnosed with standard
residual plots, because the expected distribution of the data changes
with fitted values. `check_residuals()` uses a simulation-based approach
(similar to a parametric bootstrap or Bayesian p-value) to generate
standardized residuals between 0 and 1, which can be interpreted
intuitively like residuals from linear models.

This function wraps
[`DHARMa::simulateResiduals()`](https://rdrr.io/pkg/DHARMa/man/simulateResiduals.html),
adapted for convenience.

Note: If all simulations for a data point have the same value (e.g. all
zeros), an error may occur
(`Error in approxfun: need at least two non-NA values`). Increasing
`n_simulations` can help in such cases.

## References

Dunn, K. P., & Smyth, G. K. (1996). Randomized quantile residuals.
*JCGS*, 5, 1–10.

Gelman, A., & Hill, J. (2006). *Data analysis using regression and
multilevel/hierarchical models*. Cambridge University Press.

Hartig, F. (2020). DHARMa: Residual Diagnostics for Hierarchical
(Multi-Level / Mixed) Regression Models. R package version 0.3.0.
<https://CRAN.R-project.org/package=DHARMa>

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
m1 <- glm(nclaims ~ area, offset = log(exposure),
          family = poisson(), data = MTPL2)
cr <- check_residuals(m1, n_simulations = 50)
autoplot(cr)
} # }
```
