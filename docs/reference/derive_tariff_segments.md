# Derive candidate tariff segments from a smooth risk-factor effect

Approximate the smooth effect estimated by
[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
with intervals for a continuous risk factor. The resulting boundaries
provide a candidate categorical representation that can be inspected
before inclusion in a pricing GLM or tariff structure.

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

  A `"risk_factor_gam"` object returned by
  [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md).
  Legacy `"riskfactor_gam"` and `"fitgam"` classes are accepted for
  compatibility.

- complexity:

  Non-negative numeric complexity penalty for the evolutionary tree.
  Larger values generally favour fewer internal boundaries.

- max_iterations:

  Positive integer. Maximum number of evolutionary search iterations.

- population_size:

  Positive integer. Number of candidate trees maintained during the
  evolutionary search.

- seed:

  Numeric random seed used by the grouping algorithm.

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

### Method

An evolutionary regression tree from
[`evtree::evtree()`](https://rdrr.io/pkg/evtree/man/evtree.html) is
fitted to the GAM response over the observed risk-factor values.
Internal tree split points are translated into interval boundaries. If
no internal split is supported by the fitted search, one interval
spanning the observed range is returned.

The method follows the data-driven binning approach described by
Henckaerts et al. (2018). `complexity`, `population_size`,
`max_iterations` and `seed` control the search rather than an actuarial
minimum-volume rule.

### Actuarial interpretation

The returned segments approximate the shape of the fitted univariate
GAM; they are not automatically a final tariff classification. Before
use in a multivariate model, the boundaries should be assessed against
exposure and claim volume, stability across periods, operational
rounding and the interaction with other risk factors. Particular care is
required for boundaries in sparsely populated tails.

Use
[`autoplot.tariff_segments()`](https://mharinga.github.io/insurancerating/reference/autoplot.tariff_segments.md)
to compare the smooth curve and boundaries. Use
[`add_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/add_tariff_segments.md)
to attach the resulting factor to the portfolio rows used for the GAM.

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

## See also

[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md),
[`autoplot.tariff_segments()`](https://mharinga.github.io/insurancerating/reference/autoplot.tariff_segments.md),
[`add_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/add_tariff_segments.md)

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
age_segments <- risk_factor_gam(
  MTPL,
  risk_factor = "age_policyholder",
  claim_count = "nclaims",
  exposure = "exposure"
) |>
  derive_tariff_segments()

autoplot(age_segments, show_observations = TRUE)

MTPL |>
  add_tariff_segments(age_segments, name = "age_policyholder_segment")
} # }
```
