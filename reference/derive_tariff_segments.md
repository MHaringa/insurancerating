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
  segmentation_penalty = 0,
  seed = 1,
  max_iterations = 10000,
  population_size = 200,
  complexity = NULL,
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

- segmentation_penalty:

  Non-negative numeric penalty on additional tree splits. Larger values
  generally favour fewer tariff segments. The default `0` retains the
  historical behaviour and applies no explicit split penalty; it can
  therefore produce a relatively detailed candidate segmentation. There
  is no universal actuarial value: compare candidate penalties and
  assess the resulting volume and stability by segment.

- seed:

  Single finite whole number used to reproduce the evolutionary search.

- max_iterations:

  Positive integer. Maximum number of evolutionary search iterations.
  This is an advanced algorithm-control parameter.

- population_size:

  Positive integer. Number of candidate trees maintained during the
  evolutionary search. This is an advanced algorithm-control parameter.

- complexity:

  Deprecated. Use `segmentation_penalty` instead.

- alpha:

  Deprecated. Use `segmentation_penalty` instead.

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

- segment_summary:

  Data frame with portfolio counts, distinct risk-factor values and the
  observed response components for each candidate segment. Use
  [`summary()`](https://rdrr.io/r/base/summary.html) as the public
  interface for this table.

- segmentation_penalty:

  Penalty applied to additional tree splits.

For backward compatibility, the old components `prediction`, `x`,
`model`, `data`, `x_obs`, `splits`, `class_boundaries`,
`assigned_groups`, and `tariff_classes` are also returned.

## Details

### Method

An evolutionary regression tree from
[`evtree::evtree()`](https://rdrr.io/pkg/evtree/man/evtree.html) is
fitted to the predicted GAM effect over the distinct observed
risk-factor values. The tree therefore approximates the estimated
univariate curve; it is not fitted directly to individual claim outcomes
or portfolio loss. Internal tree split points are translated into
interval boundaries. If no internal split is supported by the fitted
search, one interval spanning the observed range is returned.

The method follows the data-driven binning approach described by
Henckaerts et al. (2018). `segmentation_penalty`, `population_size`,
`max_iterations` and `seed` control the stochastic search rather than an
actuarial minimum-volume rule. Reusing the same inputs and `seed` makes
the result reproducible.

Each distinct observed risk-factor value has equal influence when the
tree approximates the fitted curve. Exposure, claim count or another
actuarial weight is deliberately not applied again in this step. The
relevant portfolio information has already influenced the curve through
the statistical specification used by
[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md),
such as the exposure offset in a frequency model, claim-count weights in
a severity model or exposure weights in a risk-premium model. Applying a
second weight during segmentation would introduce an additional
portfolio-distribution choice after the GAM has been estimated.

Exposure and claim count remain available through
[`summary()`](https://rdrr.io/r/base/summary.html). They are diagnostics
for assessing the support and practical stability of candidate segments,
but they do not influence the estimated boundaries.

### Actuarial interpretation

The returned segments approximate the shape of the fitted univariate
GAM; they are not automatically a final tariff classification. Before
use in a multivariate model, the boundaries should be assessed against
exposure and claim volume, stability across periods, operational
rounding and the interaction with other risk factors. Particular care is
required for boundaries in sparsely populated tails.

[`summary()`](https://rdrr.io/r/base/summary.html) reports the number of
portfolio records, number of distinct risk-factor values and available
exposure and claim volume within each proposed segment. These
diagnostics support actuarial review but do not constitute an automatic
acceptance rule. Minimum-volume requirements and operational rounding
should be selected with reference to portfolio size, model purpose and
governance standards.

### A staged GLM refinement workflow

In practical pricing work, the candidate boundaries are often used to
form an initial set of relatively broad model groups. The actuary
reviews [`summary()`](https://rdrr.io/r/base/summary.html) and, where
necessary, combines thinly populated segments or increases
`segmentation_penalty` until the groups have sufficient exposure and
claim information for stable estimation. The resulting factor can then
be included in an unrestricted GLM.

This broad first-stage grouping avoids estimating a separate free GLM
coefficient for every fine tariff interval when observations are
unevenly distributed over the continuous risk factor. After fitting the
GLM,
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
can use the broad model effect together with the original continuous
variable to construct a regularised pattern over finer breaks. These
finer breaks may reflect operational or commercial tariff boundaries,
while their relativities remain linked through the smoothing
specification rather than being estimated independently for every small
segment.

The staged approach therefore separates statistical support from final
tariff granularity: broad groups provide the information used by the
GLM, while smoothing can translate that information into a finer and
more regular tariff structure. Smoothing does not create additional
observations, so the resulting classes should still be assessed for
stability, extrapolation and commercial suitability.

The first and last boundaries equal the observed range used by the GAM.
Applying the segmentation to new data outside that range results in an
informative error rather than silent extrapolation.

Use
[`autoplot.tariff_segments()`](https://mharinga.github.io/insurancerating/reference/autoplot.tariff_effect.md)
to compare the smooth curve and boundaries. Use
[`add_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/add_tariff_segments.md)
to apply the resulting boundaries to portfolio data using the original
continuous risk factor.

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
[`autoplot.tariff_segments()`](https://mharinga.github.io/insurancerating/reference/autoplot.tariff_effect.md),
[`add_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/add_tariff_segments.md),
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)

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
  derive_tariff_segments(
    segmentation_penalty = 10,
    seed = 1
  )

autoplot(age_segments, show_observations = TRUE)
summary(age_segments)

MTPL |>
  add_tariff_segments(age_segments, name = "age_policyholder_segment")
} # }
```
