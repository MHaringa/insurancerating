# Add calculated excess-loss columns to data

`add_excess_loss()` adds output from
[`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md)
to a `data.frame`. It does not calculate anything itself and only copies
stored vectors from the attributes of `x`. This keeps the workflow
auditable: the excess-loss calculation is done once in
[`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md),
while `add_excess_loss()` is only a data-preparation step for modelling,
reporting or later tariff refinement.

With the default `include = c("amount", "share", "factor")`, the
function adds the allocated excess-loss amount, the row-level share of
the total excess-loss amount and the allocation factor used to
distribute the excess. Use `include = "base"` when you also want to
inspect the allocation base (`allocation_weights * allocation_factor`).

## Usage

``` r
add_excess_loss(
  data,
  x,
  name = "excess_loss",
  include = c("amount", "share", "factor"),
  overwrite = FALSE
)
```

## Arguments

- data:

  A `data.frame`.

- x:

  An `"excess_loss_vector"` returned by
  [`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md).

- name:

  Character string. Base output column name.

- include:

  Character vector with any of `"amount"`, `"share"`, `"factor"` and
  `"base"`. `"amount"` refers to the allocated excess-loss amount, not
  the original claim amount column.

- overwrite:

  Logical. If `FALSE`, existing output columns cause an error.

## Value

A `data.frame` with added columns.

## Author

Martin Haringa

## Examples

``` r
claims <- data.frame(
  segment = rep(c("A", "B"), each = 4),
  claim_amount = c(1000, 120000, 30000, 8000, 2000, 150000, 40000, 6000),
  exposure = rep(1, 8)
)
x <- calculate_excess_loss(
  claims,
  claim_amount = "claim_amount",
  exposure = "exposure",
  fit_threshold = 20000,
  excess_threshold = 100000,
  method = "empirical",
  allocation_method = "exposure",
  allocation_by = "segment",
  allocation_levels = c("A", "B")
)
add_excess_loss(claims, x, name = "large_loss")
#>   segment claim_amount exposure large_loss large_loss_share large_loss_factor
#> 1       A         1000        1       8750            0.125                 1
#> 2       A       120000        1       8750            0.125                 1
#> 3       A        30000        1       8750            0.125                 1
#> 4       A         8000        1       8750            0.125                 1
#> 5       B         2000        1       8750            0.125                 1
#> 6       B       150000        1       8750            0.125                 1
#> 7       B        40000        1       8750            0.125                 1
#> 8       B         6000        1       8750            0.125                 1
```
