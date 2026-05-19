# Add excess loading to a pricing portfolio

Add an allocated excess loading to a portfolio data set.

`add_excess_loading()` is the final workflow step. It does not estimate
or allocate excess loss. It takes an object produced by
[`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md)
and adds row-level loading columns to `data`.

The default workflow is premium based:
`loaded_premium = base_premium + allocated_excess_loss`.
`allocated_excess_loss` represents the allocated excess burden in
absolute monetary terms. `allocated_loading` represents the excess
loading per unit of weight.

Use `output = "rate"` when `base_premium` should be interpreted as a
premium amount that first needs to be converted to a rate with
`base_rate = base_premium / weight`.

## Usage

``` r
add_excess_loading(
  data,
  allocation,
  base_premium = "base_premium",
  allocated_excess_loss = NULL,
  allocated_loading = NULL,
  weight = NULL,
  output = c("premium", "rate")
)
```

## Arguments

- data:

  A `data.frame`.

- allocation:

  An object returned by
  [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md).

- base_premium:

  Character string. Base premium amount before excess loading.

- allocated_excess_loss:

  Optional character string. Column in `allocation$data` with the
  allocated excess burden in absolute monetary terms. If `NULL`,
  `allocated_excess_loss` is used.

- allocated_loading:

  Optional character string. Column in `allocation$data` with the excess
  loading per unit of `weight`. If `NULL`, `allocated_loading` is used.

- weight:

  Optional character string. Weight column used to convert between
  premium amounts and rates when `output = "rate"`.

- output:

  Character. Use `"premium"` to return premium amounts or `"rate"` to
  return rates per unit of `weight`.

## Value

A `data.frame`. With `output = "premium"`, the result contains
`base_premium`, `allocated_excess_loss`, `allocated_loading`,
`excess_loading` and `loaded_premium`. With `output = "rate"`, it
contains `base_rate`, `allocated_loading` and `loaded_rate`.

## Author

Martin Haringa

## Examples

``` r
claims <- data.frame(
  sector = rep(c("Industry", "Retail"), each = 4),
  claim_amount = c(1000, 120000, 30000, 8000, 2000, 150000, 40000, 6000),
  earned_exposure = rep(1, 8)
)
decomposed <- calculate_excess_loss(claims, "claim_amount", threshold = 100000)
allocation <- allocate_excess_loss(
  decomposed,
  excess_amount = "excess_claim_amount",
  weight = "earned_exposure"
)
add_excess_loading(decomposed, allocation)
#> Error: Column not found in `data`: base_premium
```
