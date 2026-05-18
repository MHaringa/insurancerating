# Extract allocation factors from an excess-loss vector

Extract allocation diagnostics from an object returned by
[`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md).
Use `type = "vector"` for the row-level factor, `type = "data"` for
row-level allocation data, and `type = "summary"` for grouped allocation
diagnostics.

## Usage

``` r
allocation_factor(x, type = c("data", "vector", "summary"), ...)
```

## Arguments

- x:

  An object returned by
  [`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md).

- type:

  Character. Output type.

- ...:

  Reserved for future extensions.

## Value

A vector or `data.frame`.

## Author

Martin Haringa
