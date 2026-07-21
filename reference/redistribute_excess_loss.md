# Redistribute large losses before fitting a severity model

Reduce the influence of individual large losses without removing their
cost from the portfolio. `redistribute_excess_loss()` caps observed
claim amounts at a selected threshold and redistributes the excess
amount across the claims used for severity modelling.

In practical terms, the function shifts part of the observed loss cost
from claims above the threshold to other claims before the severity
model is fitted. Large claims therefore have less direct influence on
the fitted severity relativities, while the total portfolio loss remains
unchanged.

The redistributed large-loss cost is already included in the adjusted
claim amounts. It should not be added to the risk premium again as a
separate large-loss loading. This provides a practical alternative to
modelling and adding a separate excess-loss premium component after the
severity model has been fitted.

The function combines decomposition and redistribution in one step. It
is intended for a workflow in which a severity GLM is fitted to adjusted
claim amounts. The resulting adjusted average claim amount can be used
directly as the response in a claim-count-weighted severity GLM.

## Usage

``` r
redistribute_excess_loss(
  data,
  claim_amount,
  threshold,
  claim_count = NULL,
  redistribution_weight = NULL,
  receives_redistribution = NULL,
  redistribute_excess = NULL,
  risk_factor = NULL,
  redistribution_method = c("portfolio", "risk_factor", "partial"),
  credibility = NULL,
  credibility_basis = c("claims", "excess_records"),
  credibility_threshold = 50,
  credibility_scale = 1,
  calculation_details = TRUE
)
```

## Arguments

- data:

  A data.frame with portfolio-level or claim-level observations.

- claim_amount:

  Character string. Numeric column containing observed claim amounts or
  aggregate claim loss per row.

- threshold:

  Positive numeric scalar. Claim amounts above this value are capped
  before the excess amount is redistributed.

- claim_count:

  Optional character string. Numeric claim-count column. Claim count
  determines which rows contain claims and is the denominator for
  adjusted average claim amount. It is also the default redistribution
  weight. If `NULL`, every row with `claim_amount > 0` is treated as one
  claim.

- redistribution_weight:

  Optional character string. Numeric non-negative column determining the
  relative redistribution shares. If `NULL`, claim count is used. Values
  must be positive for rows that receive redistribution.

- receives_redistribution:

  Optional character string. Logical column indicating which rows may
  receive redistributed excess loss. A row only receives redistribution
  when this column is `TRUE` and its claim count is positive. Rows with
  `FALSE` receive zero, while their observed excess loss remains part of
  the total amount being redistributed. If `NULL`, all rows with a
  positive claim count receive redistribution.

- redistribute_excess:

  Optional character string. Logical column indicating which large
  losses have their excess part redistributed. Rows with `FALSE` are not
  capped: their full observed claim amount remains in the adjusted claim
  amount and their excess does not enter the redistribution pool. If
  `NULL`, all rows above `threshold` contribute their excess.

- risk_factor:

  Optional character string. Risk-factor column used for
  `redistribution_method = "risk_factor"` or
  `redistribution_method = "partial"`.

- redistribution_method:

  Character string. Method used to redistribute excess loss:
  `"portfolio"`, `"risk_factor"` or `"partial"`.

- credibility:

  Optional numeric scalar between zero and one. For partial
  redistribution, use this credibility for every risk-factor level. If
  `NULL`, credibility is derived from `credibility_basis`.

- credibility_basis:

  Character string. Experience measure used for automatic credibility:
  `"claims"` or `"excess_records"`.

- credibility_threshold:

  Positive numeric scalar. Amount of experience required to reach 50
  percent credibility.

- credibility_scale:

  Non-negative numeric scalar. Multiplier applied to credibility before
  it is bounded between zero and one.

- calculation_details:

  Logical. If `TRUE`, append the risk-factor loading, credibility,
  portfolio loading, blended loading, preservation factor and final
  redistribution loading used in the row-level calculation. Set to
  `FALSE` for a compact modelling data set. The same information remains
  available through
  [`summary.excess_redistribution()`](https://mharinga.github.io/insurancerating/reference/summary.excess_redistribution.md).

## Value

The original data.frame with class `"excess_redistribution"` and six
dynamically named claim-amount columns. The object prints as an ordinary
data.frame;
[`summary.excess_redistribution()`](https://mharinga.github.io/insurancerating/reference/summary.excess_redistribution.md)
provides an audit of contributed, received and shifted loss. The added
columns are:

- `<claim_amount>_capped`:

  Observed claim amount capped at `threshold` when its excess is
  redistributed. An unselected row retains its full observed amount.

- `<claim_amount>_excess`:

  Observed claim amount above `threshold`.

- `<claim_amount>_is_excess`:

  Whether the row's claim amount exceeds `threshold`.

- `<claim_amount>_redistributed_excess`:

  Excess loss redistributed to the row. Rows without claims receive
  zero.

- `<claim_amount>_adjusted`:

  Capped claim amount plus redistributed excess loss.

- `<claim_amount>_adjusted_average`:

  Adjusted claim amount divided by the row's claim count. This is the
  response that can be used in a claim-count-weighted severity GLM. Rows
  without claims contain zero.

With `calculation_details = TRUE`, the result also contains
`<risk_factor>_excess_loading`, `<risk_factor>_credibility`,
`portfolio_excess_loading`, `blended_excess_loading`,
`redistribution_scaling_factor` and `final_redistribution_loading`. The
scaling factor preserves the total loss being redistributed;
`final_redistribution_loading` equals `blended_excess_loading` times
this factor for receiving rows.

## Details

For each row selected through `redistribute_excess`, the observed claim
amount is decomposed as:

\$\$ claim\\amount = capped\\claim\\amount + excess\\claim\\amount \$\$

An unselected row retains its full observed amount as its capped
component; its observed excess is reported for diagnostics but is not
redistributed.

The total excess amount is then redistributed over eligible rows with a
positive claim count. Rows without claims remain zero and do not receive
redistributed loss. By default, redistribution is proportional to the
number of claims in each row. A separate `redistribution_weight` can be
used when large-loss exposure also depends on another measure, such as
insured amount.

The adjusted loss is:

\$\$ adjusted\\claim\\amount = capped\\claim\\amount +
redistributed\\excess \$\$

The function always rescales the redistribution so that the total
adjusted claim amount equals the total observed claim amount, subject to
numerical tolerance.

### Reproducing the redistribution

The calculation columns make the redistribution directly auditable. For
partial redistribution, the loading before preservation is:

\$\$ blended\\loading = Z_g \cdot risk\\factor\\loading_g + (1 - Z_g)
\cdot portfolio\\loading \$\$

where `Z_g` is the credibility of risk-factor level `g`. Because
blending can change the total amount implied by these loadings, a single
preservation factor is applied:

\$\$ preservation\\factor = \frac{total\\ excess\\ to\\ redistribute}
{\sum_i blended\\loading_i \cdot redistribution\\weight_i} \$\$

The final amount received by row `i` is:

\$\$ redistributed\\excess_i = blended\\loading_i \cdot
preservation\\factor \cdot redistribution\\weight_i \$\$

For example, suppose a sector loading is 30 per unit of weight, the
portfolio loading is 20 and sector credibility is 40 percent. The
blended loading is `0.40 * 30 + 0.60 * 20 = 24`. If the preservation
factor is 1.10, the final loading is 26.4. A receiving row with
redistribution weight 2 is assigned `26.4 * 2 = 52.8`. Its adjusted
claim amount equals its capped claim amount plus 52.8.

With portfolio redistribution, credibility is zero and the blend equals
the portfolio loading. With risk-factor redistribution, credibility is
one and the blend equals the risk-factor loading.

### Eligibility and redistribution weights

`receives_redistribution` controls which claim-bearing rows receive part
of the excess loss. Rows for which this logical column is `FALSE`
receive zero, but their observed excess loss still contributes to the
total amount being redistributed. If the argument is `NULL`, all rows
with a positive claim count receive redistribution.

`redistribute_excess` controls which large losses contribute their
excess part to the redistribution. If it is `NULL`, every row with
`claim_amount > threshold` contributes. For a row marked `FALSE`, the
claim is not capped: its full observed amount remains in the adjusted
claim amount and none of its excess enters the redistribution pool. This
keeps excluded large losses in the severity experience without spreading
them over other claims.

`redistribution_weight` controls the relative shares among receiving
rows. If it is `NULL`, claim count is used. For example, a user can
create `claim_count * insured_amount` as a weight when claims on
policies with a higher insured amount should receive a larger part of
the large-loss burden. The function does not automatically link the
claim threshold to an insured- amount threshold; these are separate
actuarial choices.

### Redistribution methods

The `redistribution_method` argument controls how excess experience is
shared:

- `"portfolio"` gives every claim the same excess amount per claim. No
  risk-factor-level experience is used.

- `"risk_factor"` redistributes excess loss within each risk-factor
  level. Only the experience of that level is used.

- `"partial"` blends the risk-factor loading with the portfolio loading
  using credibility. This balances group responsiveness with portfolio
  stability.

For partial redistribution, credibility is either supplied directly
through `credibility` or calculated as:

\$\$ Z_g = \frac{n_g}{n_g + credibility\\threshold} \$\$

With `credibility_basis = "claims"`, `n_g` is the number of claims in
the risk-factor level. With `credibility_basis = "excess_records"`, it
is the number of records containing a positive excess amount. The
resulting value is multiplied by `credibility_scale` and bounded between
zero and one.

### Aggregated portfolio rows

When a row contains multiple claims, `claim_amount` is treated as the
total loss for that row and `threshold` is applied to that row total.
The function cannot determine which individual claim crossed the
threshold from aggregated data. Use claim-level data when the threshold
must be applied to each individual claim.

## See also

[`summary.excess_redistribution()`](https://mharinga.github.io/insurancerating/reference/summary.excess_redistribution.md)

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
    0, 40000, 90000, 150000, 300000
  )
)

adjusted <- redistribute_excess_loss(
  portfolio,
  claim_amount = "claim_amount",
  threshold = 100000,
  claim_count = "claim_count",
  risk_factor = "sector",
  redistribution_method = "partial"
)
summary(adjusted)
#>     sector n_records claim_count redistribution_weight n_excess_records
#> 1 Industry         5           4                     4                2
#> 2   Retail         5           4                     4                2
#>   n_redistributed_excess_records observed_loss observed_excess_loss
#> 1                              2        370000                95000
#> 2                              2        580000               250000
#>   redistributed_excess_contributed sector_excess_loading sector_credibility
#> 1                            95000                 23750         0.07407407
#> 2                           250000                 62500         0.07407407
#>   portfolio_excess_loading blended_excess_loading redistribution_scaling_factor
#> 1                    43125               41689.81                             1
#> 2                    43125               44560.19                             1
#>   final_redistribution_loading redistributed_excess_received net_loss_shift
#> 1                     41689.81                      166759.3       71759.26
#> 2                     44560.19                      178240.7      -71759.26
#>   adjusted_loss observed_average_claim adjusted_average_claim
#> 1      441759.3                  92500               110439.8
#> 2      508240.7                 145000               127060.2

# Inspect the complete row-level calculation. The last amount equals the
# final loading multiplied by the row's redistribution weight.
adjusted[c(
  "sector_excess_loading", "sector_credibility",
  "portfolio_excess_loading", "blended_excess_loading",
  "redistribution_scaling_factor", "final_redistribution_loading",
  "claim_amount_redistributed_excess"
)]
#>    sector_excess_loading sector_credibility portfolio_excess_loading
#> 1                  23750         0.07407407                    43125
#> 2                  23750         0.07407407                    43125
#> 3                  23750         0.07407407                    43125
#> 4                  23750         0.07407407                    43125
#> 5                  23750         0.07407407                    43125
#> 6                  62500         0.07407407                    43125
#> 7                  62500         0.07407407                    43125
#> 8                  62500         0.07407407                    43125
#> 9                  62500         0.07407407                    43125
#> 10                 62500         0.07407407                    43125
#>    blended_excess_loading redistribution_scaling_factor
#> 1                41689.81                             1
#> 2                41689.81                             1
#> 3                41689.81                             1
#> 4                41689.81                             1
#> 5                41689.81                             1
#> 6                44560.19                             1
#> 7                44560.19                             1
#> 8                44560.19                             1
#> 9                44560.19                             1
#> 10               44560.19                             1
#>    final_redistribution_loading claim_amount_redistributed_excess
#> 1                          0.00                              0.00
#> 2                      41689.81                          41689.81
#> 3                      41689.81                          41689.81
#> 4                      41689.81                          41689.81
#> 5                      41689.81                          41689.81
#> 6                          0.00                              0.00
#> 7                      44560.19                          44560.19
#> 8                      44560.19                          44560.19
#> 9                      44560.19                          44560.19
#> 10                     44560.19                          44560.19

# Keep the modelling data compact when row-level calculation details are not
# needed. summary() still provides the complete allocation audit.
compact_adjusted <- redistribute_excess_loss(
  portfolio,
  claim_amount = "claim_amount",
  threshold = 100000,
  claim_count = "claim_count",
  risk_factor = "sector",
  redistribution_method = "partial",
  calculation_details = FALSE
)
summary(compact_adjusted)
#>     sector n_records claim_count redistribution_weight n_excess_records
#> 1 Industry         5           4                     4                2
#> 2   Retail         5           4                     4                2
#>   n_redistributed_excess_records observed_loss observed_excess_loss
#> 1                              2        370000                95000
#> 2                              2        580000               250000
#>   redistributed_excess_contributed sector_excess_loading sector_credibility
#> 1                            95000                 23750         0.07407407
#> 2                           250000                 62500         0.07407407
#>   portfolio_excess_loading blended_excess_loading redistribution_scaling_factor
#> 1                    43125               41689.81                             1
#> 2                    43125               44560.19                             1
#>   final_redistribution_loading redistributed_excess_received net_loss_shift
#> 1                     41689.81                      166759.3       71759.26
#> 2                     44560.19                      178240.7      -71759.26
#>   adjusted_loss observed_average_claim adjusted_average_claim
#> 1      441759.3                  92500               110439.8
#> 2      508240.7                 145000               127060.2

# Fit a severity model to the adjusted average claim amount.
severity_data <- adjusted[adjusted$claim_count > 0, ]
stats::glm(
  claim_amount_adjusted_average ~ sector,
  weights = claim_count,
  family = stats::Gamma(link = "log"),
  data = severity_data
)
#> 
#> Call:  stats::glm(formula = claim_amount_adjusted_average ~ sector, 
#>     family = stats::Gamma(link = "log"), data = severity_data, 
#>     weights = claim_count)
#> 
#> Coefficients:
#>  (Intercept)  sectorRetail  
#>      11.6122        0.1402  
#> 
#> Degrees of Freedom: 7 Total (i.e. Null);  6 Residual
#> Null Deviance:       0.6071 
#> Residual Deviance: 0.5678    AIC: 194

# Portfolio redistribution pools the excess loss across all sectors. Every
# receiving claim gets the same redistributed amount per unit of weight.
# A sector's own historical large-loss experience does not determine its
# share; sector differences are subsequently estimated by the severity GLM.
portfolio_adjusted <- redistribute_excess_loss(
  portfolio,
  claim_amount = "claim_amount",
  threshold = 100000,
  claim_count = "claim_count",
  redistribution_method = "portfolio"
)
summary(portfolio_adjusted, by = "sector")
#>     sector n_records claim_count redistribution_weight n_excess_records
#> 1 Industry         5           4                     4                2
#> 2   Retail         5           4                     4                2
#>   n_redistributed_excess_records observed_loss observed_excess_loss
#> 1                              2        370000                95000
#> 2                              2        580000               250000
#>   redistributed_excess_contributed risk_factor_excess_loading
#> 1                            95000                      43125
#> 2                           250000                      43125
#>   risk_factor_credibility portfolio_excess_loading blended_excess_loading
#> 1                       0                    43125                  43125
#> 2                       0                    43125                  43125
#>   redistribution_scaling_factor final_redistribution_loading
#> 1                             1                        43125
#> 2                             1                        43125
#>   redistributed_excess_received net_loss_shift adjusted_loss
#> 1                        172500          77500        447500
#> 2                        172500         -77500        502500
#>   observed_average_claim adjusted_average_claim
#> 1                  92500                 111875
#> 2                 145000                 125625

# Risk-factor redistribution keeps each sector's excess burden within that
# sector. Claims in Industry receive only redistributed excess originating
# from Industry, and the same applies to Retail. This preserves sector-level
# large-loss experience, but can be less stable for sectors with few claims.
sector_adjusted <- redistribute_excess_loss(
  portfolio,
  claim_amount = "claim_amount",
  threshold = 100000,
  claim_count = "claim_count",
  risk_factor = "sector",
  redistribution_method = "risk_factor"
)

# Give claims on policies with a higher insured amount a larger share, while
# restricting redistribution to policies insured for at least 100,000.
weighted_portfolio <- transform(
  portfolio,
  insured_amount = rep(c(50000, 250000), each = 5)
)
weighted_portfolio$receives_redistribution <-
  weighted_portfolio$insured_amount >= 100000
weighted_portfolio$redistribution_weight <-
  weighted_portfolio$claim_count * weighted_portfolio$insured_amount

weighted_adjusted <- redistribute_excess_loss(
  weighted_portfolio,
  claim_amount = "claim_amount",
  threshold = 100000,
  claim_count = "claim_count",
  redistribution_weight = "redistribution_weight",
  receives_redistribution = "receives_redistribution"
)

# Exclude catastrophe events and unsettled claims from the excess amounts
# that are redistributed. Their full observed claim amounts remain in the
# adjusted severity data.
portfolio$is_catastrophe <- c(
  FALSE, FALSE, FALSE, FALSE, TRUE,
  FALSE, FALSE, FALSE, FALSE, FALSE
)
portfolio$claim_status <- c(
  "settled", "settled", "settled", "settled", "settled",
  "settled", "settled", "open", "settled", "settled"
)
portfolio$redistribute_excess <-
  !portfolio$is_catastrophe &
  portfolio$claim_status == "settled"

selected_adjusted <- redistribute_excess_loss(
  portfolio,
  claim_amount = "claim_amount",
  threshold = 100000,
  claim_count = "claim_count",
  redistribute_excess = "redistribute_excess"
)
```
