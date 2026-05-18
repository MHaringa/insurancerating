# Plot an excess-loss vector

Visualise the allocation stored on an excess-loss vector. Without `by`,
the plot shows allocated versus not allocated. With `by`, it shows the
selected allocation metric by group. `type = "histogram"` shows the
bootstrap distribution of total excess-loss estimates when available.

## Usage

``` r
# S3 method for class 'excess_loss_vector'
autoplot(
  object,
  by = NULL,
  y = c("allocated_excess", "allocated_share", "allocation_factor", "allocation_base"),
  type = c("bar", "histogram"),
  ...
)
```

## Arguments

- object:

  An object returned by
  [`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md).

- by:

  Optional character string. Grouping column available in the stored
  allocation data.

- y:

  Character. Allocation metric to show for `type = "bar"`.

- type:

  Character. `"bar"` or `"histogram"`.

- ...:

  Reserved for future extensions.

## Value

A ggplot object.

## Author

Martin Haringa
