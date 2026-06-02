# Decompose claim amounts into capped and excess parts

Large claims can distort risk-factor relativities and make pricing
models unstable. `calculate_excess_loss()` separates each claim into a
capped part and an excess part above a selected threshold.

## Usage

``` r
calculate_excess_loss(data, claim_amount, threshold)
```

## Arguments

- data:

  A data.frame with claim-level observations.

- claim_amount:

  Character string. Claim amount column.

- threshold:

  Positive numeric scalar. Claims above this value contribute to the
  excess component. Claims below the threshold remain fully included in
  the capped claim amount.

## Value

A data.frame with the original data and the columns `claim_amount`,
`capped_claim_amount`, `excess_claim_amount` and `is_excess_claim`.

## Details

The capped claim amount can be used to model the base premium, while the
excess component can be analysed, pooled or allocated separately. This
allows the impact of large individual claims to be controlled without
ignoring the associated cost.

The function is deliberately deterministic. It does not perform
smoothing, credibility weighting, allocation or simulation. It simply
decomposes each observed claim into:

\$\$ claim\\amount = capped\\claim\\amount + excess\\claim\\amount \$\$

where:

\$\$ excess\\claim\\amount = max(claim\\amount - threshold, 0) \$\$

and:

\$\$ capped\\claim\\amount = min(claim\\amount, threshold) \$\$

The resulting excess component can subsequently be allocated using
[`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md)
and added back to the technical premium using
[`apply_excess_loading()`](https://mharinga.github.io/insurancerating/reference/apply_excess_loading.md).

### Typical pricing workflow

A common workflow is:

1.  Select an excess threshold.

2.  Split claims into capped and excess components.

3.  Model frequency and severity using capped claim amounts.

4.  Allocate the excess-loss burden separately.

5.  Add the resulting excess loading back to the technical premium.

This approach reduces the influence of a small number of large claims on
risk-factor relativities while ensuring that the total cost of excess
losses remains reflected in the final premium.

## Author

Martin Haringa

## Examples

``` r
claims <- data.frame(
  claim_amount = c(1000, 120000, 30000)
)

calculate_excess_loss(
  claims,
  claim_amount = "claim_amount",
  threshold = 100000
)
#>   claim_amount capped_claim_amount excess_claim_amount is_excess_claim
#> 1         1000               1e+03                   0           FALSE
#> 2       120000               1e+05               20000            TRUE
#> 3        30000               3e+04                   0           FALSE
```
