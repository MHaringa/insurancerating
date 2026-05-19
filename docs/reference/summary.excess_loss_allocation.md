# Summarise an excess-loss allocation

Return the allocation audit table from an object produced by
[`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md).

## Usage

``` r
# S3 method for class 'excess_loss_allocation'
summary(object, compare_to_empirical = FALSE, ...)
```

## Arguments

- object:

  An object returned by
  [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md).

- compare_to_empirical:

  Logical. If `TRUE`, keep columns with the empirical loss and empirical
  excess loss used for comparison.

- ...:

  Unused.

## Value

A `data.frame`.

## Author

Martin Haringa
