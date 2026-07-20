# Apply allocated excess losses or loadings to a pricing portfolio

Add an allocated expected excess loss or excess loading to an existing
base premium or base rate.

## Usage

``` r
apply_excess_loading(
  data,
  allocation,
  output = c("premium", "rate"),
  base_value = "base_value",
  allocation_weight = NULL
)
```

## Arguments

- data:

  A data.frame containing the existing base premium amounts or rates.
  Its rows must correspond to the rows in `allocation`.

- allocation:

  An `excess_allocation` object returned by
  [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md).
  It supplies the row-level amount and loading that are applied to
  `data`.

- output:

  Character string. Use `"premium"` to add the allocated monetary amount
  or `"rate"` to add the loading per unit of allocation weight.

- base_value:

  Character string. Column containing the existing value to which the
  excess component is added. This is typically a premium amount when
  `output = "premium"` and a rate when `output = "rate"`. When
  `output = "rate"` and `allocation_weight` is supplied, `base_value` is
  interpreted as a monetary amount and divided by `allocation_weight`
  before the loading is added.

- allocation_weight:

  Optional character string. Positive numeric column used to optionally
  convert `base_value` from a monetary amount to a rate when
  `output = "rate"`. If `NULL`, `base_value` is treated as an existing
  rate. When supplied, it should be the same column used as
  `allocation_weight` in
  [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md).
  The standard `expected_excess_loss` and `blended_excess_loading`
  columns are read automatically from `allocation` and do not need to be
  specified.

## Value

A data.frame. With `output = "premium"`, the result contains
`base_premium`, `expected_excess_loss`, `blended_excess_loading`,
`excess_loading` and `loaded_premium`. With `output = "rate"`, the
result contains `base_rate`, `blended_excess_loading` and `loaded_rate`.

## Details

### Relationship with allocation

[`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md)
first distributes the portfolio's excess losses across individual
observations. It returns both `expected_excess_loss`, the monetary
amount allocated to each observation, and `blended_excess_loading`, the
corresponding loading per unit of allocation weight.
`apply_excess_loading()` then applies one of these results to an
existing base premium or base rate.

The distinction between the functions is deliberate:

- [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md)
  determines and allocates the expected excess-loss burden.

- `apply_excess_loading()` adds the resulting amount or rate to the
  pricing portfolio. It does not estimate or reallocate excess loss.

### Premium output

With `output = "premium"`, the row-level `expected_excess_loss` is added
to the column selected by `base_value`:

\$\$ loaded\\premium = base\\value + expected\\excess\\loss \$\$

### Rate output

With `output = "rate"`, `blended_excess_loading` is added to the base
rate:

\$\$ loaded\\rate = base\\rate + blended\\excess\\loading \$\$

By default, the column selected by `base_value` is treated as an
existing base rate. If `allocation_weight` is supplied, `base_value` is
instead treated as a monetary amount and converted to a rate before the
excess loading is added:

\$\$ base\\rate = \frac{base\\value}{allocation\\weight} \$\$

`allocation_weight` should refer to the same quantity used in
[`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md),
such as earned exposure or insured amount times earned exposure. Where a
row-level expected excess amount needs to be interpreted as a rate, the
equivalent relationship is `expected_excess_loss / allocation_weight`.
The allocation object already provides this rate as
`blended_excess_loading`.

### Interpretation of allocation columns

`expected_excess_loss` represents the monetary excess-loss burden
allocated to a row.

`blended_excess_loading` represents the excess loading per unit of
allocation weight.

In other words:

\$\$ expected\\excess\\loss = blended\\excess\\loading \cdot
allocation\\weight \$\$

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

## See also

[`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md),
[`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md)

## Author

Martin Haringa

## Examples

``` r
portfolio <- data.frame(
  policy_id = 1:10,
  sector = rep(c("Industry", "Retail"), each = 5),
  claim_count = c(0, 1, 1, 1, 1, 0, 1, 1, 1, 1),
  claim_amount = c(
    0, 25000, 120000, 50000, 175000,
    0, 40000, 90000, 150000, 750000
  ),
  earned_exposure = rep(1, 10)
)

decomposed <- calculate_excess_loss(
  portfolio,
  claim_amount = "claim_amount",
  threshold = 100000
)

decomposed$base_premium <- 500

allocation <- allocate_excess_loss(
  decomposed,
  allocation_weight = "earned_exposure",
  claim_count = "claim_count"
)

# Add the allocated monetary amount to the base premium.
premium_result <- apply_excess_loading(
  decomposed,
  allocation,
  base_value = "base_premium"
)

# Add the excess loading per exposure unit to the base premium rate.
decomposed$base_rate <- decomposed$base_premium / decomposed$earned_exposure
rate_result <- apply_excess_loading(
  decomposed,
  allocation,
  output = "rate",
  base_value = "base_rate"
)
```
