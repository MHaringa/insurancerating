# Decompose claim amounts into capped and excess parts

Large claims can distort risk-factor relativities and make pricing
models unstable. `calculate_excess_loss()` separates each row in a
portfolio into a capped claim amount and an excess part above a selected
threshold.

## Usage

``` r
calculate_excess_loss(data, claim_amount, threshold)
```

## Arguments

- data:

  A data.frame with portfolio-level or claim-level observations.
  Portfolio-level data can include policies without claims, for example
  rows where `n_claims = 0` and the claim amount is zero.

- claim_amount:

  Character string. Claim amount column.

- threshold:

  Positive numeric scalar. Claims above this value contribute to the
  excess component. Claims below the threshold remain fully included in
  the capped claim amount.

## Value

A data.frame with the original data and three added columns. The names
are derived from `claim_amount`: `<claim_amount>_capped`,
`<claim_amount>_excess` and `<claim_amount>_is_excess`.

## Details

The capped claim amount can be used to model the base premium, while the
excess component can be analysed, pooled or allocated separately. This
allows the impact of large individual claims to be controlled without
ignoring the associated cost.

The function is deliberately deterministic. It does not perform
smoothing, credibility weighting, allocation or simulation. It simply
decomposes each observed claim into:

\$\$ claim\\amount = claim\\amount\\capped + claim\\amount\\excess \$\$

where:

\$\$ claim\\amount\\excess = max(claim\\amount - threshold, 0) \$\$

and:

\$\$ claim\\amount\\capped = min(claim\\amount, threshold) \$\$

The output column names are derived from the column supplied through
`claim_amount`. For example, if `claim_amount = "incurred_loss"`, the
added columns are `incurred_loss_capped`, `incurred_loss_excess` and
`incurred_loss_is_excess`.

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
portfolio <- data.frame(
  policy_id = 1:4,
  n_claims = c(0, 1, 1, 0),
  claim_amount = c(0, 120000, 30000, 0)
)

calculate_excess_loss(
  portfolio,
  claim_amount = "claim_amount",
  threshold = 100000
)
#>   policy_id n_claims claim_amount claim_amount_capped claim_amount_excess
#> 1         1        0            0               0e+00                   0
#> 2         2        1       120000               1e+05               20000
#> 3         3        1        30000               3e+04                   0
#> 4         4        0            0               0e+00                   0
#>   claim_amount_is_excess
#> 1                  FALSE
#> 2                   TRUE
#> 3                  FALSE
#> 4                  FALSE
```
