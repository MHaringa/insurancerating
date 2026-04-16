# Construct insurance tariff classes

Constructs insurance tariff classes for objects of class `"fitgam"`
produced by
[`riskfactor_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md)
(formerly
[`fit_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md)).
The goal is to bin continuous risk factors into categorical tariff
classes that capture the effect of the covariate on the response in an
accurate way, while remaining easy to use in a generalized linear model
(GLM).

## Usage

``` r
construct_tariff_classes(
  object,
  alpha = 0,
  niterations = 10000,
  ntrees = 200,
  seed = 1
)
```

## Arguments

- object:

  An object of class `"fitgam"`, produced by
  [`riskfactor_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md).

- alpha:

  Complexity parameter passed to
  [`evtree::evtree.control()`](https://rdrr.io/pkg/evtree/man/evtree.control.html).
  Higher values yield fewer tariff classes. Default = 0.

- niterations:

  Maximum number of iterations before termination. Passed to
  [`evtree::evtree.control()`](https://rdrr.io/pkg/evtree/man/evtree.control.html).
  Default = 10000.

- ntrees:

  Number of trees in the population. Passed to
  [`evtree::evtree.control()`](https://rdrr.io/pkg/evtree/man/evtree.control.html).
  Default = 200.

- seed:

  Integer, seed for the random number generator (for reproducibility).

## Value

A `list` of class `"constructtariffclasses"` with components:

- prediction:

  Data frame with predicted values.

- x:

  Name of the continuous risk factor for which tariff classes are
  constructed.

- model:

  Model type: `"frequency"`, `"severity"`, or `"burning"`.

- data:

  Data frame with predicted and observed values.

- x_obs:

  Observed values of the continuous risk factor.

- splits:

  Numeric vector with boundaries of the constructed tariff classes.

- tariff_classes:

  Factor with the tariff class each observation falls into.

## Details

Evolutionary trees (via
[`evtree::evtree()`](https://rdrr.io/pkg/evtree/man/evtree.html)) are
used as a technique to bin the fitted GAM object into risk-homogeneous
categories. This method is based on the work by Henckaerts et al.
(2018). See Grubinger et al. (2014) for details on the parameters
controlling the evtree fit.

## References

Antonio, K. and Valdez, E. A. (2012). Statistical concepts of a priori
and a posteriori risk classification in insurance. *Advances in
Statistical Analysis*, 96(2), 187–224.
[doi:10.1007/s10182-011-0152-7](https://doi.org/10.1007/s10182-011-0152-7)

Grubinger, T., Zeileis, A., and Pfeiffer, K.-P. (2014). *evtree:
Evolutionary learning of globally optimal classification and regression
trees in R*. Journal of Statistical Software, 61(1), 1–29.
[doi:10.18637/jss.v061.i01](https://doi.org/10.18637/jss.v061.i01)

Henckaerts, R., Antonio, K., Clijsters, M., & Verbelen, R. (2018). A
data driven binning strategy for the construction of insurance tariff
classes. *Scandinavian Actuarial Journal*, 2018(8), 681–705.
[doi:10.1080/03461238.2018.1429300](https://doi.org/10.1080/03461238.2018.1429300)

Wood, S.N. (2011). Fast stable restricted maximum likelihood and
marginal likelihood estimation of semiparametric generalized linear
models. *JRSS B*, 73(1), 3–36.
[doi:10.1111/j.1467-9868.2010.00749.x](https://doi.org/10.1111/j.1467-9868.2010.00749.x)

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

# Recommended new usage (SE)
riskfactor_gam(MTPL,
               nclaims = "nclaims",
               x = "age_policyholder",
               exposure = "exposure") |>
  construct_tariff_classes()

# Deprecated usage (NSE, still works with warning)
fit_gam(MTPL, nclaims = nclaims, x = age_policyholder, exposure = exposure) |>
  construct_tariff_classes()
} # }
```
