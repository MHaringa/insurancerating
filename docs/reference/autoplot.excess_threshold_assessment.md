# Plot an excess threshold assessment

Visualise how excess-loss diagnostics move across candidate thresholds.

## Usage

``` r
# S3 method for class 'excess_threshold_assessment'
autoplot(
  object,
  y = c("excess_loss", "excess_per_exposure", "claims_above", "excess_loss_share"),
  ...
)
```

## Arguments

- object:

  An object returned by
  [`assess_excess_thresholds()`](https://mharinga.github.io/insurancerating/reference/assess_excess_thresholds.md).

- y:

  Character. Diagnostic to plot on the y-axis.

- ...:

  Reserved for future extensions.

## Value

A ggplot object.

## Author

Martin Haringa
