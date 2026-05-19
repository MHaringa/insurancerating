# Decompose claim amounts into capped and excess parts

Calculate the historical excess amount above a selected threshold.

`calculate_excess_loss()` is deliberately deterministic. It does not
perform bootstrap simulation, smoothing or allocation. The function
simply decomposes each observed claim into a capped part and an excess
part: `excess_claim_amount = pmax(claim_amount - threshold, 0)`.

Use the output as input for
[`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md)
when the historical excess burden needs to be pooled,
credibility-weighted or bootstrapped.

## Usage

``` r
calculate_excess_loss(data, claim_amount, threshold)
```

## Arguments

- data:

  A `data.frame` with claim-level observations.

- claim_amount:

  Character string. Claim amount column.

- threshold:

  Positive numeric scalar. Claims above this value are treated as excess
  claims.

## Value

A `data.frame` with the original data and the columns `claim_amount`,
`capped_claim_amount`, `excess_claim_amount` and `is_excess_claim`.

## Author

Martin Haringa

## Examples

``` r
claims <- data.frame(claim_amount = c(1000, 120000, 30000))
calculate_excess_loss(claims, claim_amount = "claim_amount", threshold = 100000)
#>   claim_amount capped_claim_amount excess_claim_amount is_excess_claim
#> 1         1000               1e+03                   0           FALSE
#> 2       120000               1e+05               20000            TRUE
#> 3        30000               3e+04                   0           FALSE
```
