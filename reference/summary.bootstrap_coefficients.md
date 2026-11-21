# Summarise bootstrap coefficient stability

Summarise the coefficient distributions returned by
[`coefficient_stability()`](https://mharinga.github.io/insurancerating/reference/coefficient_stability.md)
on the GLM link scale or after exponentiation.

## Usage

``` r
# S3 method for class 'bootstrap_coefficients'
summary(
  object,
  scale = c("link", "exponentiated", "relativity"),
  confidence = 0.95,
  interval = c("percentile", "normal"),
  ...
)
```

## Arguments

- object:

  A `bootstrap_coefficients` object.

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

- confidence:

  Numeric scalar between 0 and 1 giving the confidence level.

- interval:

  Character string. `"percentile"` uses empirical bootstrap quantiles.
  `"normal"` uses the original estimate plus or minus a normal quantile
  times the bootstrap standard error.

- ...:

  Additional arguments are not used.

## Value

A data frame with one row per original coefficient and columns:

- term:

  Coefficient name.

- estimate:

  Estimate from the original GLM.

- bootstrap_mean:

  Mean of the finite bootstrap estimates.

- bias:

  Bootstrap mean minus the original estimate.

- bootstrap_se:

  Standard deviation of the bootstrap estimates.

- lower, upper:

  Requested bootstrap interval.

- n_successful:

  Number of finite, comparable estimates for the term. This can be lower
  than the number of converged refits.

- n_requested:

  Number of requested bootstrap samples.

- success_rate:

  `n_successful / n_requested`.

## See also

[`coefficient_stability()`](https://mharinga.github.io/insurancerating/reference/coefficient_stability.md),
[`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)

## Author

Martin Haringa
