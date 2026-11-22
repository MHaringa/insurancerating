# Plot bootstrap coefficient stability

Show original GLM estimates as points and bootstrap intervals as
horizontal lines. Row labels show the number of comparable estimates
over the number of requested resamples. A row without an interval has
insufficient comparable estimates for the selected interval method.

## Usage

``` r
# S3 method for class 'bootstrap_coefficients'
autoplot(
  object,
  ...,
  scale = c("link", "exponentiated", "relativity"),
  terms = NULL,
  confidence = 0.95,
  interval = c("percentile", "normal")
)
```

## Arguments

- object:

  A `bootstrap_coefficients` object.

- ...:

  Additional arguments are not used.

- scale:

  Character string. `"link"` reports coefficients on their fitted GLM
  scale. `"exponentiated"` applies
  [`exp()`](https://rdrr.io/r/base/Log.html) to every original and
  bootstrap coefficient. `"relativity"` is an alias for
  `"exponentiated"`; this interpretation is most direct for a log-link
  GLM. For treatment-coded factors, these are ratios to the reference
  level. For an untransformed numeric predictor without interactions,
  the ratio is for a one-unit increase. The exponentiated intercept is a
  baseline level, not a factor relativity. For a logit-link model,
  exponentiated coefficients are odds ratios rather than response
  probabilities.

- terms:

  Optional character vector of coefficient names, in display order from
  top to bottom. By default show all terms except the intercept, or the
  intercept alone for an intercept-only model.

- confidence:

  Numeric scalar between 0 and 1 giving the confidence level.

- interval:

  Character string. `"percentile"` uses empirical bootstrap quantiles.
  `"normal"` uses the original estimate plus or minus a normal quantile
  times the bootstrap standard error.

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Details

The reference line marks no effect (0 on the link scale, 1 after
exponentiation). It is omitted if the plot includes the intercept, which
represents a baseline level. Intervals describe sampling stability, not
uncertainty about future claims.

## See also

[`coefficient_stability()`](https://mharinga.github.io/insurancerating/reference/coefficient_stability.md),
[`summary.bootstrap_coefficients()`](https://mharinga.github.io/insurancerating/reference/summary.bootstrap_coefficients.md)
