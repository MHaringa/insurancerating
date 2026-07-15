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
  allocation_subset = NULL,
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

- allocation_subset:

  Optional character string. Logical column indicating which rows
  participate in the allocation. If `NULL`, all rows are included.

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

- `allocation_included`:

  Logical indicator showing whether the row was included in the
  allocation basis. Rows are included when they satisfy
  `allocation_subset` if supplied; included rows must also have a
  positive allocation weight.

- `<risk_factor>_excess_loading`:

  Excess loading estimated from the experience of the risk-factor level.
  For example, `risk_factor = "sector"` creates `sector_excess_loading`.
  When no risk factor is supplied, the column is named
  `risk_factor_excess_loading`.

- `portfolio_excess_loading`:

  Excess loading estimated from the full portfolio.

- `credibility`:

  Credibility weight assigned to the risk-factor-level experience.

- `allocated_excess_loading`:

  Final excess loading per unit of `allocation_weight`. For partial
  allocation this is the credibility blend of the risk-factor and
  portfolio excess loadings.

- `allocated_excess_loss`:

  Row-level allocated excess-loss amount, calculated as
  `allocated_excess_loading * allocation_weight`.

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

- `"risk_factor"`: excess losses are allocated separately for each
  risk-factor level. The excess burden observed within a group is spread
  across all risks in that group and is not shared with other groups.

  This produces the strongest link between excess loadings and observed
  group experience, but can lead to volatile results when excess losses
  are rare.

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

The allocated loading is calculated as:

\$\$ loading_g = Z_g \cdot loading_g^{risk\\ factor} + (1 - Z_g) \cdot
loading^{portfolio} \$\$

where `Z_g` represents the credibility assigned to the risk-factor-level
experience.

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
claims <- data.frame(
  sector = rep(c("Industry", "Retail"), each = 4),
  claim_count = c(1, 1, 1, 1, 1, 1, 1, 1),
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

# Pool all excess losses across the portfolio
portfolio_allocation <- allocate_excess_loss(
  decomposed,
  allocation_weight = "earned_exposure",
  claim_count = "claim_count",
  allocation = "portfolio"
)

# Allocate excess losses separately by sector
sector_allocation <- allocate_excess_loss(
  decomposed,
  allocation_weight = "earned_exposure",
  claim_count = "claim_count",
  risk_factor = "sector",
  allocation = "risk_factor"
)

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
#> 1 Industry               4           4                  1                20000
#> 2   Retail               4           4                  1                50000
#>   observed_excess_loading credibility sector_excess_loading
#> 1                    5000  0.07407407                  5000
#> 2                   12500  0.07407407                 12500
#>   portfolio_excess_loading allocated_excess_loading allocated_excess_loss
#> 1                     8750                 8472.222              33888.89
#> 2                     8750                 9027.778              36111.11
```
