# Allocate excess loss to a pricing portfolio

Large claims can distort risk-factor relativities and create unstable
premiums. `allocate_excess_loss()` redistributes historical excess
losses across a portfolio in a controlled and transparent way.

The function is typically used after
[`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md).
The base premium can be modelled on capped claim amounts, while the
excess part of large claims is allocated back to the portfolio as an
additional loading.

## Usage

``` r
allocate_excess_loss(
  data,
  excess_amount = NULL,
  allocation_weight,
  risk_factor = NULL,
  receives_allocation = NULL,
  claim_count = NULL,
  allocation = c("portfolio", "risk_factor", "partial"),
  credibility = NULL,
  credibility_basis = c("claims", "excess_claims", "allocation_weight"),
  credibility_threshold = 50,
  credibility_scale = 1,
  method = c("observed", "bootstrap"),
  n_bootstrap = 1000,
  bootstrap_seed = NULL,
  severity_noise = c("none", "lognormal", "normal"),
  severity_noise_sd = 0.25,
  preserve_total_excess = TRUE
)
```

## Arguments

- data:

  A data.frame, typically the output of
  [`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md).

- excess_amount:

  Optional character string. Column containing the excess claim amount
  to allocate. If `NULL`, the function uses the
  `claim_amount_excess_column` metadata created by
  [`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md).

- allocation_weight:

  Character string. Column used as allocation weight, typically
  exposure, premium, insured value or another earned unit.

- risk_factor:

  Optional character string. Risk-factor column used for
  `allocation = "risk_factor"` or `allocation = "partial"`.

- receives_allocation:

  Optional character string. Name of a logical column indicating which
  rows receive a share of the total excess loss. Rows with `FALSE`
  receive no allocation, but their excess losses still contribute to the
  total amount being allocated. If `NULL`, all rows receive an
  allocation.

- claim_count:

  Optional character string. Claim-count column. If supplied, claim
  counts in the allocation summary are calculated as the sum of this
  column. If `NULL`, claim counts are inferred from the original claim
  amount column when available: rows with a positive claim amount count
  as one claim and rows with zero claim amount count as zero claims.

- allocation:

  Character string. One of `"portfolio"`, `"risk_factor"` or
  `"partial"`.

- credibility:

  Optional numeric scalar between 0 and 1. Used directly when
  `allocation = "partial"`. If `NULL`, credibility is calculated from
  `credibility_basis` and `credibility_threshold`.

- credibility_basis:

  Character string. Experience basis used when `credibility = NULL`:
  `"claims"`, `"excess_claims"` or `"allocation_weight"`.

- credibility_threshold:

  Positive numeric scalar. Amount of experience required to reach 50
  percent credibility.

- credibility_scale:

  Positive numeric scalar. Multiplies the derived or supplied
  credibility before it is capped between 0 and 1.

- method:

  Character string. Either `"observed"` or `"bootstrap"`.

- n_bootstrap:

  Positive whole number. Number of bootstrap samples.

- bootstrap_seed:

  Optional integer seed for reproducible bootstrap allocation.

- severity_noise:

  Character string. One of `"none"`, `"lognormal"` or `"normal"`.

- severity_noise_sd:

  Non-negative numeric scalar controlling severity variation in
  bootstrap samples.

- preserve_total_excess:

  Logical. If `TRUE`, the final allocation is rescaled so that the total
  allocated excess loss equals the total excess loss being allocated.

## Value

The input `data` enriched with allocation columns and class
`"excess_allocation"`. The object prints as an ordinary data frame and
has a custom
[`summary.excess_allocation()`](https://mharinga.github.io/insurancerating/reference/summary.excess_allocation.md)
method for aggregated allocation statistics. Original rows, columns, row
order and metadata from
[`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md)
are preserved. The added columns are:

- `receives_allocation`:

  Logical indicator showing whether the row receives a share of the
  total excess loss. This does not indicate whether the row contributed
  to the observed excess-loss amount: all excess losses can contribute
  to the total excess loss, while `receives_allocation` controls the
  target rows over which that total is redistributed. When the
  `receives_allocation` argument is supplied, rows receive allocation
  when the referenced logical column is `TRUE` and the allocation weight
  is positive. Rows with `FALSE` receive no allocation, but their excess
  losses still contribute to the total amount being allocated.

- `<risk_factor>_excess_loading`:

  Excess loading estimated from the experience of the risk-factor level.
  For example, `risk_factor = "sector"` creates `sector_excess_loading`.
  When no risk factor is supplied, the column is named
  `risk_factor_excess_loading`.

- `<risk_factor>_credibility`:

  Credibility weight assigned to the risk-factor-level estimate. For
  example, `risk_factor = "sector"` creates `sector_credibility`.

- `portfolio_excess_loading`:

  Portfolio-level excess loading per unit of allocation weight.

- `blended_excess_loading`:

  Credibility-weighted blend of the risk-factor and portfolio excess
  loadings. For `risk_factor = "sector"`, this is calculated as
  `sector_excess_loading * sector_credibility + portfolio_excess_loading * (1 - sector_credibility)`.

- `expected_excess_loss`:

  Row-level expected excess-loss amount, calculated as
  `blended_excess_loading * allocation_weight`.

## Details

### Allocation methods

The `allocation` argument determines how the excess burden is shared.

- `"portfolio"`: excess losses are pooled across the entire portfolio
  and redistributed using the specified allocation weight. The excess
  burden is shared by all included risks regardless of their risk-factor
  level.

  This provides the most stable excess loading and is often appropriate
  when excess losses are infrequent, highly volatile or considered a
  portfolio- wide risk rather than a risk-factor-specific
  characteristic.

  For portfolio allocation, no risk-factor-level experience is used.
  Therefore, `risk_factor_credibility` equals zero and
  `blended_excess_loading` equals `portfolio_excess_loading`. The same
  output columns are kept for all allocation methods so downstream
  reporting code can use one consistent structure.

- `"risk_factor"`: excess losses are allocated separately for each
  risk-factor level. The excess burden observed within a group is spread
  across all risks in that group and is not shared with other groups.

  This produces the strongest link between excess loadings and observed
  group experience, but can lead to volatile results when excess losses
  are rare.

  For risk-factor allocation, only risk-factor-level experience is used.
  Therefore, `sector_credibility` equals one and
  `blended_excess_loading` equals `sector_excess_loading` when
  `risk_factor = "sector"`.

- `"partial"`: excess losses are allocated using a credibility-weighted
  combination of portfolio and risk-factor experience. Risk-factor
  levels with more credible experience receive excess loadings that more
  closely reflect their own observed excess-loss burden, while less
  credible groups are pooled more strongly towards the portfolio
  average.

  This approach typically provides a good balance between pricing
  stability and risk differentiation and is therefore often the
  preferred choice in practical rating applications.

### Credibility weighting

For `allocation = "partial"`, excess losses are allocated using a
credibility-weighted blend of portfolio and risk-factor experience.

The blended excess loading is calculated as:

\$\$ blended\\excess\\loading_g = sector\\credibility_g \cdot
sector\\excess\\loading_g + (1 - sector\\credibility_g) \cdot
portfolio\\excess\\loading \$\$

In the output, `sector_excess_loading` is replaced by
`<risk_factor>_excess_loading` and `sector_credibility` by
`<risk_factor>_credibility` when another risk-factor column is supplied.
`expected_excess_loss` is then calculated as
`blended_excess_loading * allocation_weight`.

If `credibility` is supplied, the same credibility is applied to all
risk-factor levels.

If `credibility = NULL`, credibility is determined separately for each
risk-factor level based on the selected `credibility_basis`:

\$\$ Z_g = \frac{n_g}{n_g + credibility\\threshold} \$\$

where `n_g` is determined by `credibility_basis`:

- `"claims"`: total number of claims in the risk-factor level.

- `"excess_claims"`: number of claims with positive excess loss.

- `"allocation_weight"`: total allocation weight in the risk-factor
  level.

`credibility_threshold` represents the amount of experience required to
reach 50 percent credibility.

Example:

A sector with 20 claims and `credibility_threshold = 50` receives:

\$\$ Z = \frac{20}{20 + 50} = 0.29 \$\$

Therefore 29% of the excess loading is based on the sector's own
excess-loss experience and 71% is based on the portfolio-wide excess
loading.

For example, with `credibility_threshold = 50`, a group with:

- 10 claims receives 17% credibility;

- 50 claims receives 50% credibility;

- 100 claims receives 67% credibility;

- 200 claims receives 80% credibility.

The final credibility is scaled using `credibility_scale` and then
capped between 0 and 1:

\$\$ Z_g = min(1, max(0, Z_g \cdot credibility\\scale)) \$\$

Higher values of `credibility_threshold` or lower values of
`credibility_scale` pool more strongly towards the portfolio loading.

### Bootstrap allocation

With `method = "observed"`, the function allocates the historically
observed excess loss.

With `method = "bootstrap"`, the function repeatedly resamples observed
positive excess claim amounts. This provides a pragmatic estimate of
excess-loss volatility and the resulting uncertainty in excess loadings.

The approach is intended as a practical pricing approximation rather
than a formal extreme value model.

The bootstrap affects both the total excess burden and the distribution
of excess loss across risk-factor levels. Use `bootstrap_seed` to make
bootstrap results reproducible.

### Severity noise

`severity_noise` can only be used with `method = "bootstrap"`.

If `severity_noise = "none"`, bootstrap samples reuse the observed
excess claim amounts.

If `severity_noise = "lognormal"`, sampled excess claims are multiplied
by lognormal noise. This is usually the most natural option for large
claims, because claim amounts remain positive and variation is
multiplicative.

If `severity_noise = "normal"`, additive normal noise is applied. This
may be useful for experimentation, but is generally less natural for
large positive claim amounts.

`severity_noise_sd` controls the amount of additional severity
variation. As a rough guide:

- `0.10` provides limited variation;

- `0.25` provides moderate variation;

- `0.50` provides substantial variation.

### Preserving the total excess loss

If `preserve_total_excess = TRUE`, the final allocation is rescaled so
that the sum of allocated excess loss equals the total excess loss being
allocated.

This ensures that credibility blending, bootstrap sampling or other
allocation choices do not unintentionally increase or decrease the total
excess burden.

### Typical pricing workflow

A common workflow is:

1.  Use
    [`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md)
    to separate capped and excess losses.

2.  Model the base premium using capped claim amounts.

3.  Allocate the excess-loss burden using `allocate_excess_loss()`.

4.  Add the resulting excess loading back to the technical premium using
    [`apply_excess_loading()`](https://mharinga.github.io/insurancerating/reference/apply_excess_loading.md).

This approach prevents a small number of large claims from distorting
risk-factor relativities while still ensuring that the excess-loss
burden is reflected in the final premium.

## See also

[`summary.excess_allocation()`](https://mharinga.github.io/insurancerating/reference/summary.excess_allocation.md)

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

# Pool all excess losses across the portfolio
portfolio_allocation <- allocate_excess_loss(
  decomposed,
  allocation_weight = "earned_exposure",
  claim_count = "claim_count",
  allocation = "portfolio"
)
# No sector-level experience is used here: risk_factor_credibility is zero
# and blended_excess_loading equals portfolio_excess_loading.

# Allocate excess losses separately by sector
sector_allocation <- allocate_excess_loss(
  decomposed,
  allocation_weight = "earned_exposure",
  claim_count = "claim_count",
  risk_factor = "sector",
  allocation = "risk_factor"
)
# Only sector-level experience is used here: sector_credibility is one
# and blended_excess_loading equals sector_excess_loading.

# Blend sector and portfolio experience using credibility
partial_allocation <- allocate_excess_loss(
  decomposed,
  allocation_weight = "earned_exposure",
  claim_count = "claim_count",
  risk_factor = "sector",
  allocation = "partial",
  credibility_basis = "claims",
  credibility_threshold = 50
)

summary(partial_allocation)
#>     sector earned_exposure claim_count excess_claim_count observed_excess_loss
#> 1 Industry               5           4                  2                95000
#> 2   Retail               5           4                  2               700000
#>   observed_excess_loading sector_excess_loading sector_credibility
#> 1                   19000                 19000         0.07407407
#> 2                  140000                140000         0.07407407
#>   portfolio_excess_loading blended_excess_loading expected_excess_loss
#> 1                    79500               75018.52             375092.6
#> 2                    79500               83981.48             419907.4

# Allocate excess loss by insured amount, restricted to policies with an
# insured amount of at least 500,000.
portfolio <- portfolio |>
  dplyr::mutate(
    insured_amount = c(
      100000, 250000, 500000, 750000, 1000000,
      1500000, 2500000, 5000000, 7500000, 10000000
    ),
    receives_allocation = insured_amount >= 500000,
    allocation_weight = insured_amount * earned_exposure
  )

decomposed <- calculate_excess_loss(
  portfolio,
  claim_amount = "claim_amount",
  threshold = 500000
)
insured_amount_allocation <- allocate_excess_loss(
  decomposed,
  allocation = "portfolio",
  allocation_weight = "allocation_weight",
  receives_allocation = "receives_allocation"
)
insured_amount_allocation
#>    policy_id   sector claim_count claim_amount earned_exposure insured_amount
#> 1          1 Industry           0            0               1        1.0e+05
#> 2          2 Industry           1        25000               1        2.5e+05
#> 3          3 Industry           1       120000               1        5.0e+05
#> 4          4 Industry           1        50000               1        7.5e+05
#> 5          5 Industry           1       175000               1        1.0e+06
#> 6          6   Retail           0            0               1        1.5e+06
#> 7          7   Retail           1        40000               1        2.5e+06
#> 8          8   Retail           1        90000               1        5.0e+06
#> 9          9   Retail           1       150000               1        7.5e+06
#> 10        10   Retail           1       750000               1        1.0e+07
#>    receives_allocation allocation_weight claim_amount_capped
#> 1                FALSE           1.0e+05                   0
#> 2                FALSE           2.5e+05               25000
#> 3                 TRUE           5.0e+05              120000
#> 4                 TRUE           7.5e+05               50000
#> 5                 TRUE           1.0e+06              175000
#> 6                 TRUE           1.5e+06                   0
#> 7                 TRUE           2.5e+06               40000
#> 8                 TRUE           5.0e+06               90000
#> 9                 TRUE           7.5e+06              150000
#> 10                TRUE           1.0e+07              500000
#>    claim_amount_excess claim_amount_is_excess risk_factor_excess_loading
#> 1                    0                  FALSE                0.008695652
#> 2                    0                  FALSE                0.008695652
#> 3                    0                  FALSE                0.008695652
#> 4                    0                  FALSE                0.008695652
#> 5                    0                  FALSE                0.008695652
#> 6                    0                  FALSE                0.008695652
#> 7                    0                  FALSE                0.008695652
#> 8                    0                  FALSE                0.008695652
#> 9                    0                  FALSE                0.008695652
#> 10              250000                   TRUE                0.008695652
#>    risk_factor_credibility portfolio_excess_loading blended_excess_loading
#> 1                        0              0.008695652            0.000000000
#> 2                        0              0.008695652            0.000000000
#> 3                        0              0.008695652            0.008695652
#> 4                        0              0.008695652            0.008695652
#> 5                        0              0.008695652            0.008695652
#> 6                        0              0.008695652            0.008695652
#> 7                        0              0.008695652            0.008695652
#> 8                        0              0.008695652            0.008695652
#> 9                        0              0.008695652            0.008695652
#> 10                       0              0.008695652            0.008695652
#>    expected_excess_loss
#> 1                 0.000
#> 2                 0.000
#> 3              4347.826
#> 4              6521.739
#> 5              8695.652
#> 6             13043.478
#> 7             21739.130
#> 8             43478.261
#> 9             65217.391
#> 10            86956.522

# All losses above the threshold contribute to the total excess loss, but
# only rows with receives_allocation = TRUE receive a share. The share is
# proportional to insured_amount * earned_exposure. A larger insured amount
# therefore produces a larger absolute expected_excess_loss, while a policy
# with the same insured amount but lower earned exposure receives
# proportionally less. For receiving policies this gives one constant excess
# loading as a percentage of insured amount, adjusted for earned exposure.
# No risk-factor-level experience is used in this portfolio allocation.
```
