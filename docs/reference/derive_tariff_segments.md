# Derive insurance tariff segments

Derives data-driven tariff segments for a continuous risk factor from a
fitted `"riskfactor_gam"` object produced by
[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md).
The segments help translate a smooth GAM response pattern into practical
categorical rating factors for a GLM tariff.

## Usage

``` r
derive_tariff_segments(
  object,
  complexity = 0,
  max_iterations = 10000,
  population_size = 200,
  seed = 1,
  alpha = NULL,
  niterations = NULL,
  ntrees = NULL
)
```

## Arguments

- object:

  An object of class `"riskfactor_gam"`, produced by
  [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md).
  Objects with the old `"fitgam"` class are still supported for backward
  compatibility.

- complexity:

  Numeric. Controls the complexity penalty used when deriving segments.
  Higher values generally yield fewer tariff segments. Default = 0.

- max_iterations:

  Integer. Maximum number of search iterations used by the underlying
  grouping algorithm. Default = 10000.

- population_size:

  Integer. Number of candidate trees used by the underlying grouping
  algorithm. Default = 200.

- seed:

  Integer, seed for the random number generator (for reproducibility).

- alpha:

  Deprecated. Use `complexity` instead.

- niterations:

  Deprecated. Use `max_iterations` instead.

- ntrees:

  Deprecated. Use `population_size` instead.

## Value

A `list` of class `"tariff_segments"` with components:

- gam_prediction:

  Data frame with the fitted GAM curve.

- risk_factor:

  Name of the continuous risk factor.

- model_type:

  Model type: `"frequency"`, `"severity"`, or `"pure_premium"`.

- classification_data:

  Data frame used to derive the segments.

- risk_factor_values:

  Observed risk factor values in portfolio row order.

- segment_boundaries:

  Numeric vector with segment boundaries.

- assigned_segments:

  Factor with the tariff segment assigned to each observed risk factor
  value.

For backward compatibility, the old components `prediction`, `x`,
`model`, `data`, `x_obs`, `splits`, `class_boundaries`,
`assigned_groups`, and `tariff_classes` are also returned.

## Details

Evolutionary trees (via
[`evtree::evtree()`](https://rdrr.io/pkg/evtree/man/evtree.html)) are
used as a technique to bin the fitted GAM object into candidate tariff
segments. This method is based on the work by Henckaerts et al. (2018).
See Grubinger et al. (2014) for details on the parameters controlling
the evtree fit.

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
age_segments <- risk_factor_gam(MTPL,
                                risk_factor = "age_policyholder",
                                claim_count = "nclaims",
                                exposure = "exposure") |>
  derive_tariff_segments()

MTPL |>
  add_tariff_segments(age_segments, name = "age_policyholder_segment")
} # }
```
