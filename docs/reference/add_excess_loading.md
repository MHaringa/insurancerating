# Add excess loading to a pricing portfolio

Add an allocated excess loading to a portfolio data set.

`add_excess_loading()` is the final workflow step. It does not estimate
or allocate excess loss. It takes an object produced by
[`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md)
and adds row-level loading columns to `data`.

## Usage

``` r
add_excess_loading(data, allocation)
```

## Arguments

- data:

  A `data.frame`.

- allocation:

  An object returned by
  [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md).

## Value

A `data.frame` with `base_premium`, `excess_loading` and
`loaded_premium`. If `data` already contains `base_premium`, that column
is used as the starting premium. Otherwise `base_premium` is set to
zero.

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
#>     sector claim_amount earned_exposure capped_claim_amount excess_claim_amount
#> 1 Industry         1000               1               1e+03                   0
#> 2 Industry       120000               1               1e+05               20000
#> 3 Industry        30000               1               3e+04                   0
#> 4 Industry         8000               1               8e+03                   0
#> 5   Retail         2000               1               2e+03                   0
#> 6   Retail       150000               1               1e+05               50000
#> 7   Retail        40000               1               4e+04                   0
#> 8   Retail         6000               1               6e+03                   0
#>   is_excess_claim base_premium excess_loading loaded_premium
#> 1           FALSE            0           8750           8750
#> 2            TRUE            0           8750           8750
#> 3           FALSE            0           8750           8750
#> 4           FALSE            0           8750           8750
#> 5           FALSE            0           8750           8750
#> 6            TRUE            0           8750           8750
#> 7           FALSE            0           8750           8750
#> 8           FALSE            0           8750           8750
```
