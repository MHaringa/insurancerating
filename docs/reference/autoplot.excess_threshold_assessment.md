# Plot an excess threshold assessment

Visualise one diagnostic from an object returned by
[`assess_excess_threshold()`](https://mharinga.github.io/insurancerating/reference/assess_excess_threshold.md).
The plot helps compare how candidate thresholds affect excess loss,
excess claim counts or pure-premium impact.

## Usage

``` r
# S3 method for class 'excess_threshold_assessment'
autoplot(
  object,
  y = c("premium_impact", "excess_loss", "n_excess_claims", "excess_loss_ratio"),
  ...
)
```

## Arguments

- object:

  An object returned by
  [`assess_excess_threshold()`](https://mharinga.github.io/insurancerating/reference/assess_excess_threshold.md).

- y:

  Character. Measure to plot on the y-axis.

- ...:

  Unused.

## Value

A `ggplot` object.

## Author

Martin Haringa
