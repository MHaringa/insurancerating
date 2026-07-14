# Apply excess loading to a pricing portfolio

Apply an allocated excess-loss loading to a portfolio data set.

## Usage

``` r
apply_excess_loading(
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

  A data.frame containing the base premium or base rate.

- allocation:

  An object returned by
  [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md).

- base_premium:

  Character string. Column containing the base premium amount or base
  rate before the excess loading is added.

- allocated_excess_loss:

  Optional character string. Column in `allocation$data` containing the
  allocated excess-loss amount in monetary terms. If `NULL`,
  `allocated_excess_loss` is used.

- allocated_loading:

  Optional character string. Column in `allocation$data` containing the
  allocated excess loading per unit of allocation weight. If `NULL`,
  `allocated_loading` is used.

- weight:

  Optional character string. Weight column used to convert between
  premium amounts and rates when `output = "rate"`.

- output:

  Character string. Use `"premium"` to return premium amounts or
  `"rate"` to return rates per unit of weight.

## Value

A data.frame. With `output = "premium"`, the result contains
`base_premium`, `allocated_excess_loss`, `allocated_loading`,
`excess_loading` and `loaded_premium`. With `output = "rate"`, the
result contains `base_rate`, `allocated_loading` and `loaded_rate`.

## Details

`apply_excess_loading()` is the final step in the excess-loss pricing
workflow. It does not cap claims, estimate excess losses or allocate the
excess burden. Instead, it takes the output of
[`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md)
and adds the allocated excess component back to the base premium or base
rate.

The function is typically used after the base premium has been modelled
on capped claim amounts. The excess loading then ensures that the cost
of claims above the selected threshold is still reflected in the final
technical premium.

### Premium output

With `output = "premium"`, the function adds the allocated excess loss
in monetary terms to the base premium:

\$\$ loaded\\premium = base\\premium + allocated\\excess\\loss \$\$

`allocated_excess_loss` is the row-level monetary amount of excess loss
allocated to each risk.

### Rate output

With `output = "rate"`, the function adds the allocated excess loading
per unit of weight to the base rate:

\$\$ loaded\\rate = base\\rate + allocated\\loading \$\$

Use this option when the base value represents a rate per exposure,
premium unit, insured value or other allocation weight.

If the input column supplied through `base_premium` contains premium
amounts rather than rates, the function first converts the base premium
to a rate:

\$\$ base\\rate = \frac{base\\premium}{weight} \$\$

### Interpretation of allocation columns

`allocated_excess_loss` represents the monetary excess-loss burden
allocated to a row.

`allocated_loading` represents the excess loading per unit of allocation
weight.

In other words:

\$\$ allocated\\excess\\loss = allocated\\loading \cdot weight \$\$

This distinction is important when moving between premium amounts and
rates.

### Typical pricing workflow

A common workflow is:

1.  Use
    [`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md)
    to separate capped and excess losses.

2.  Model the base premium using capped claim amounts.

3.  Allocate the excess-loss burden using
    [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md).

4.  Use `apply_excess_loading()` to add the allocated excess component
    back to the base premium or base rate.

This produces a final technical premium that reflects both the modelled
capped loss cost and the separately allocated excess-loss burden.

## Author

Martin Haringa

## Examples

``` r
claims <- data.frame(
  sector = rep(c("Industry", "Retail"), each = 4),
  claim_amount = c(
    1000, 120000, 30000, 8000,
    2000, 150000, 40000, 6000
  ),
  earned_exposure = rep(1, 8)
)

decomposed <- calculate_excess_loss(
  claims,
  claim_amount = "claim_amount",
  threshold = 100000
)

decomposed$base_premium <- 500

allocation <- allocate_excess_loss(
  decomposed,
  excess_amount = "claim_amount_excess",
  allocation_weight = "earned_exposure"
)

apply_excess_loading(
  decomposed,
  allocation,
  base_premium = "base_premium"
)
#>     sector claim_amount earned_exposure claim_amount_capped claim_amount_excess
#> 1 Industry         1000               1               1e+03                   0
#> 2 Industry       120000               1               1e+05               20000
#> 3 Industry        30000               1               3e+04                   0
#> 4 Industry         8000               1               8e+03                   0
#> 5   Retail         2000               1               2e+03                   0
#> 6   Retail       150000               1               1e+05               50000
#> 7   Retail        40000               1               4e+04                   0
#> 8   Retail         6000               1               6e+03                   0
#>   claim_amount_is_excess base_premium allocated_excess_loss allocated_loading
#> 1                  FALSE          500                  8750              8750
#> 2                   TRUE          500                  8750              8750
#> 3                  FALSE          500                  8750              8750
#> 4                  FALSE          500                  8750              8750
#> 5                  FALSE          500                  8750              8750
#> 6                   TRUE          500                  8750              8750
#> 7                  FALSE          500                  8750              8750
#> 8                  FALSE          500                  8750              8750
#>   excess_loading loaded_premium
#> 1           8750           9250
#> 2           8750           9250
#> 3           8750           9250
#> 4           8750           9250
#> 5           8750           9250
#> 6           8750           9250
#> 7           8750           9250
#> 8           8750           9250

apply_excess_loading(
  decomposed,
  allocation,
  base_premium = "base_premium",
  weight = "earned_exposure",
  output = "rate"
)
#>     sector claim_amount earned_exposure claim_amount_capped claim_amount_excess
#> 1 Industry         1000               1               1e+03                   0
#> 2 Industry       120000               1               1e+05               20000
#> 3 Industry        30000               1               3e+04                   0
#> 4 Industry         8000               1               8e+03                   0
#> 5   Retail         2000               1               2e+03                   0
#> 6   Retail       150000               1               1e+05               50000
#> 7   Retail        40000               1               4e+04                   0
#> 8   Retail         6000               1               6e+03                   0
#>   claim_amount_is_excess base_premium base_rate allocated_loading loaded_rate
#> 1                  FALSE          500       500              8750        9250
#> 2                   TRUE          500       500              8750        9250
#> 3                  FALSE          500       500              8750        9250
#> 4                  FALSE          500       500              8750        9250
#> 5                  FALSE          500       500              8750        9250
#> 6                   TRUE          500       500              8750        9250
#> 7                  FALSE          500       500              8750        9250
#> 8                  FALSE          500       500              8750        9250
```
