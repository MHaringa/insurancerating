# Plot an excess-loss allocation

Visualise the allocated excess loading, allocated excess loss or
credibility by allocation group.

## Usage

``` r
# S3 method for class 'excess_loss_allocation'
autoplot(
  object,
  y = c("allocated_loading", "allocated_excess_loss", "credibility"),
  ...
)
```

## Arguments

- object:

  An object returned by
  [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md).

- y:

  Character. Measure to plot on the y-axis.

- ...:

  Unused.

## Value

A `ggplot` object.

## Author

Martin Haringa
