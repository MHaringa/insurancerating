# Check simulation-based model residuals

Checks whether a fitted model shows systematic residual deviations from
the distribution implied by the model. The function uses
simulation-based residuals from
[`DHARMa::simulateResiduals()`](https://rdrr.io/pkg/DHARMa/man/simulateResiduals.html),
which are especially useful for GLMs where classical residual plots can
be hard to interpret.

## Usage

``` r
check_residuals(object, n_simulations = 30)
```

## Arguments

- object:

  A fitted `"glm"` object supported by
  [`DHARMa::simulateResiduals()`](https://rdrr.io/pkg/DHARMa/man/simulateResiduals.html).

- n_simulations:

  Number of simulations used to generate residuals. Must be a positive
  whole number. Default is 30.

## Value

An object of class `"residual_check"` and `"check_residuals"`, which is
a list with:

- qq_data:

  Data frame with theoretical quantiles (`x`) and observed scaled
  residuals (`y`).

- scaled_residuals:

  Numeric vector of DHARMa scaled residuals.

- p_value:

  P-value from a Kolmogorov-Smirnov test against `uniform(0, 1)`.

For backwards compatibility the object also contains the aliases `df`
and `p.val`.

## Details

In insurance pricing, residual checks are used to assess whether a model
is behaving consistently across the portfolio. For example, a Poisson
frequency model may fit the average claim count well but still show
structure in the residuals because of omitted rating factors, unmodelled
heterogeneity, clustering, outliers, or an unsuitable distributional
assumption.

DHARMa simulates new responses from the fitted model and compares the
observed response with those simulations. The resulting scaled residuals
are approximately uniformly distributed on `[0, 1]` when the model is
correctly specified. This gives a common diagnostic scale for GLMs and
related models, where raw residuals are otherwise difficult to compare
across different fitted values, exposures, or expected claim amounts.

`check_residuals()` returns the scaled residuals, QQ-plot data, and a
Kolmogorov-Smirnov p-value for a simple uniformity check. The p-value
should be read as a diagnostic signal, not as a pricing decision rule. A
low p-value indicates that the residual distribution differs from what
the fitted model implies and that the model specification may need
review.

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
