# Redistribute large losses for severity or risk-premium modelling

Large claims can have a disproportionate influence on observed severity
and on estimated risk-factor effects. `redistribute_excess_loss()`
decomposes each selected claim amount into a retained component up to a
specified threshold and an excess component above that threshold. The
excess component is subsequently allocated using portfolio-wide,
risk-factor-level or partially pooled experience. The allocation
preserves the total excess loss, subject to numerical tolerance.

The allocated excess can be incorporated in the pricing analysis in two
ways:

- `output = "redistributed_claim"` adds the allocated excess loss to the
  retained claim amount. The resulting variable contains both components
  and can be used as the response in a single severity model.

- `output = "excess_loading"` keeps retained claim severity and excess
  loss as separate quantities. The function returns an excess loading
  per unit of `redistribution_weight`. This loading can be added to the
  risk premium based on predicted frequency and retained severity.

Both output forms use the same threshold, credibility and allocation
calculations. They therefore differ only in how the allocated excess
loss is represented in subsequent modelling; the total amount allocated
is the same.

The default is `output = "redistributed_claim"`.

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
  calculation_details = TRUE,
  output = c("redistributed_claim", "excess_loading")
)
```

## Arguments

- data:

  A data.frame containing portfolio-level or claim-level observations.

- claim_amount:

  Character string naming a finite, non-negative numeric column with
  observed claim amounts or aggregate claim loss per row.

- threshold:

  Positive numeric scalar defining the boundary between retained and
  excess loss. For selected rows, the amount above this value is
  allocated.

- claim_count:

  Optional character string naming a non-negative, whole-number
  claim-count column. Claim count identifies claim-bearing rows and is
  the denominator of the adjusted average claim amount. It is also the
  default redistribution weight. If `NULL`, each row with
  `claim_amount > 0` is treated as one claim.

- redistribution_weight:

  Optional character string naming a finite, non-negative numeric
  column. The column determines the relative allocation shares and the
  unit of the resulting loading. If `NULL`, claim count is used. Claim
  count or expected claim count expresses the allocation per claim;
  earned exposure expresses it per exposure unit. Rows with zero weight
  receive no allocation. At least one eligible row must have positive
  weight.

- receives_redistribution:

  Optional character string. Logical column indicating which rows may
  receive allocated excess loss. Rows with `FALSE` receive zero, while
  their observed excess remains in the total allocation unless excluded
  by `redistribute_excess`. If `NULL`, all otherwise eligible rows are
  included. Eligibility additionally requires positive claim count for
  redistributed claims and positive redistribution weight for excess
  loadings.

- redistribute_excess:

  Optional character string. Logical column indicating which rows
  contribute their excess component to the allocation. Rows with `FALSE`
  retain their full observed amount and contribute no excess to the
  allocation pool. If `NULL`, every row above `threshold` contributes.

- risk_factor:

  Optional character string naming the risk-factor column for
  `redistribution_method = "risk_factor"` or
  `redistribution_method = "partial"`.

- redistribution_method:

  Character string specifying the experience level used in the
  allocation: `"portfolio"`, `"risk_factor"` or `"partial"`.

- credibility:

  Optional numeric scalar in `[0, 1]`. For partial redistribution, the
  supplied value is applied to every risk-factor level. If `NULL`,
  credibility is calculated from `credibility_basis`.

- credibility_basis:

  Character string specifying the experience measure used in automatic
  credibility: `"claims"` or `"excess_records"`.

- credibility_threshold:

  Positive numeric scalar representing the amount of credibility-basis
  experience at which automatic credibility equals 0.5, before applying
  `credibility_scale`.

- credibility_scale:

  Non-negative numeric scalar multiplying automatic credibility before
  truncation to `[0, 1]`.

- calculation_details:

  Logical. If `TRUE`, append the risk-factor loading, credibility,
  portfolio loading, blended loading, scaling factor and final loading
  used in the row-level calculation. If `FALSE`, these columns are
  omitted from `data` but remain available through
  [`summary.excess_redistribution()`](https://mharinga.github.io/insurancerating/reference/summary.excess_redistribution.md).

- output:

  Character string specifying the representation of allocated excess.
  `"redistributed_claim"` adds it to retained claim amounts;
  `"excess_loading"` returns it separately per unit of
  `redistribution_weight`.

## Value

The input data.frame with additional columns and class
`"excess_redistribution"`. The object uses standard data.frame printing.
[`summary.excess_redistribution()`](https://mharinga.github.io/insurancerating/reference/summary.excess_redistribution.md)
aggregates contributed, allocated and shifted loss. Both output forms
add:

- `<claim_amount>_capped`:

  Observed claim amount capped at `threshold` when its excess is
  allocated. Rows excluded through `redistribute_excess` retain their
  full observed amount.

- `<claim_amount>_excess`:

  Observed amount above `threshold`, whether or not that amount is
  selected for allocation.

- `<claim_amount>_is_excess`:

  Logical indicator that the observed claim amount exceeds `threshold`.

With `output = "redistributed_claim"`, the result additionally contains:

- `<claim_amount>_redistributed_excess`:

  Row-level allocated excess loss. Rows without claims receive zero.

- `<claim_amount>_adjusted`:

  Retained claim amount plus row-level allocated excess loss.

- `<claim_amount>_adjusted_average`:

  Redistributed claim amount divided by claim count. Rows without claims
  contain zero.

With `output = "excess_loading"`, the result additionally contains:

- `allocated_excess_loss`:

  Absolute excess-loss amount allocated to the row.

- `excess_loading`:

  Allocated excess loss per unit of `redistribution_weight`.

With `calculation_details = TRUE`, the result also contains
`<risk_factor>_excess_loading`, `<risk_factor>_credibility`,
`portfolio_excess_loading`, `blended_excess_loading`,
`redistribution_scaling_factor` and `final_redistribution_loading`. For
receiving rows, `final_redistribution_loading` equals
`blended_excess_loading` multiplied by `redistribution_scaling_factor`.
The selected output and effective redistribution-weight label are stored
in the `"output"` and `"redistribution_weight_label"` attributes.

## Details

For each row selected through `redistribute_excess`, the observed claim
amount is decomposed as:

\$\$ claim\\amount = capped\\claim\\amount + excess\\claim\\amount \$\$

If a row is not selected through `redistribute_excess`, its full
observed amount is retained. Any amount above the threshold remains
available as a diagnostic quantity but is excluded from the amount to be
allocated.

The excess amount is allocated over eligible rows in proportion to
`redistribution_weight`. If no weight column is supplied, claim count is
used. For redistributed-claim output, only rows with a positive claim
count are eligible. For excess-loading output, eligibility is determined
by a positive redistribution weight, so policy rows without observed
claims may receive a loading when exposure is used.

For `output = "redistributed_claim"`, the redistributed claim amount is:

\$\$ adjusted\\claim\\amount = capped\\claim\\amount +
redistributed\\excess \$\$

This redistributed claim amount is returned as
`<claim_amount>_adjusted`. The corresponding average per claim is
suitable as the response in a claim-count-weighted severity GLM. Because
this response includes allocated excess loss, the same excess component
should not subsequently be added to the estimated risk premium.

For `output = "excess_loading"`, capped claim severity remains separate
and:

\$\$ excess\\loading_i =
\frac{allocated\\excess\\loss_i}{redistribution\\weight_i} \$\$

The loading can be added to the risk premium derived from predicted
frequency and retained severity. When earned exposure is used as
`redistribution_weight`, the loading is expressed per unit of earned
exposure. The total allocated excess loss is preserved, subject to
numerical tolerance.

### Interpretation of the output forms

Redistributed-claim output combines retained and allocated loss in one
model response. It requires one severity model and assigns the complete
historical loss burden to that response. Its interpretation is most
direct when the modelled risk-factor levels contain sufficient claim
experience and the estimated effects are stable across observation
periods.

The allocated component is not an observed loss for the receiving row.
For example, an observed claim of 10,000 may receive an allocation of
20,000, resulting in a redistributed amount of 30,000. If the row
belongs to a risk-factor level with few claims, the fitted model may
attribute a material part of this allocated portfolio experience to that
individual level. This can increase the sampling variability of its
estimated effect.

Excess-loading output estimates retained severity from observed loss up
to the threshold and represents allocated excess as a separate
risk-premium component:

\$\$ retained\\ risk\\ premium = predicted\\ frequency \cdot predicted\\
retained\\ severity \$\$

\$\$ total\\ risk\\ premium = retained\\ risk\\ premium + excess\\
loading \$\$

The two output forms therefore imply different model interpretations
rather than different total loss amounts. The selection should reflect
claim volume, the stability of risk-factor effects and the intended
construction of the technical risk premium.

### Sparse risk-factor levels

Before fitting a redistributed-claim severity model, claim volume should
be assessed by risk-factor level. Levels with limited information may be
combined using an economically or actuarially meaningful hierarchy.
Coefficient stability across periods and agreement between observed and
predicted severity provide additional diagnostics. Excess-loading output
is an alternative when separate level estimates remain weakly supported.

For example, a model-preparation rule may map sectors with fewer than 20
claims to `"Other"`. The value 20 is illustrative and should not be
treated as a general minimum. An appropriate threshold depends on
portfolio size, heterogeneity and validation results. Grouping is
therefore outside the scope of this function.

### Reproducing the redistribution

The optional calculation columns allow the allocation to be reproduced.
For partial redistribution, the loading before total-preservation
scaling is:

\$\$ blended\\loading = Z_g \cdot risk\\factor\\loading_g + (1 - Z_g)
\cdot portfolio\\loading \$\$

where `Z_g` denotes the credibility assigned to risk-factor level `g`.
Blending may change the total amount implied by the unscaled loadings. A
common scaling factor is therefore applied:

\$\$ preservation\\factor = \frac{total\\ excess\\ to\\ redistribute}
{\sum_i blended\\loading_i \cdot redistribution\\weight_i} \$\$

The final amount received by row `i` is:

\$\$ redistributed\\excess_i = blended\\loading_i \cdot
preservation\\factor \cdot redistribution\\weight_i \$\$

For example, let the sector loading be 30 per unit of weight, the
portfolio loading 20 and sector credibility 0.40. The blended loading
equals `0.40 * 30 + 0.60 * 20 = 24`. With a scaling factor of 1.10, the
final loading equals 26.4. A receiving row with redistribution weight 2
is then allocated `26.4 * 2 = 52.8`. In redistributed-claim output, 52.8
is added to the retained claim amount; in excess-loading output, 26.4 is
retained as the loading per unit of weight.

With portfolio redistribution, credibility is zero and the blend equals
the portfolio loading. With risk-factor redistribution, credibility is
one and the blend equals the risk-factor loading.

### Eligibility and redistribution weights

`receives_redistribution` identifies the rows to which excess loss may
be allocated. Rows with value `FALSE` receive zero. Their observed
excess loss is nevertheless included in the total amount to be allocated
unless excluded through `redistribute_excess`. For redistributed-claim
output, a receiving row must also have a positive claim count. For
excess-loading output, a receiving row must have a positive
redistribution weight.

`redistribute_excess` controls which large losses contribute their
excess part to the redistribution. If it is `NULL`, every row with
`claim_amount > threshold` contributes. For a row marked `FALSE`, the
claim is not capped: its full observed amount is retained and its excess
does not enter the allocation pool. This permits specific loss types,
such as events treated outside the regular large-loss procedure, to
remain unchanged.

`redistribution_weight` controls both the relative shares among
receiving rows and the unit of the resulting loading. Claim count
produces an amount per claim, expected claim count an amount per
expected claim, earned exposure an amount per exposure unit, and insured
amount an amount per unit insured. If it is `NULL`, claim count is used.
For an excess loading that is added to an annual risk premium, earned
exposure is usually the corresponding unit.

Rows with zero redistribution weight remain in the output but receive
zero. For example, `claim_count * insured_amount` assigns a larger share
to claim observations with a higher insured amount. The excess threshold
and an insured-amount criterion for receiving allocations are separate
model specifications.

### Redistribution methods

The `redistribution_method` argument determines the level at which
excess experience is estimated:

- `"portfolio"` derives one loading from the complete allocation
  portfolio. Risk-factor-level excess experience is not used.

- `"risk_factor"` derives a separate loading for each risk-factor level.
  Each level is based only on its own excess experience and allocation
  weight.

- `"partial"` blends the risk-factor loading with the portfolio loading
  using credibility.

For partial redistribution, credibility is either supplied directly
through `credibility` or calculated as:

\$\$ Z_g = \frac{n_g}{n_g + credibility\\threshold} \$\$

With `credibility_basis = "claims"`, `n_g` is the number of claims in
the risk-factor level. With `credibility_basis = "excess_records"`, it
is the number of records containing a positive excess amount. The
resulting value is multiplied by `credibility_scale` and bounded between
zero and one.

Credibility and redistribution weight have distinct roles. Credibility
determines the contribution of risk-factor-level experience to a partial
loading. Redistribution weight determines the row-level allocation
shares and the unit of the final loading. Thus, with
`credibility_basis = "claims"` and earned exposure as
`redistribution_weight`, claim volume determines credibility while the
resulting loading is expressed per unit of exposure.

### Aggregated portfolio rows

When a row contains multiple claims, `claim_amount` is treated as the
total loss for that row and `threshold` is applied to that row total.
The function cannot identify which individual claims exceeded the
threshold from an aggregated row. Claim-level input is required when the
threshold is intended to apply separately to each claim.

## See also

[`summary.excess_redistribution()`](https://mharinga.github.io/insurancerating/reference/summary.excess_redistribution.md)

## Author

Martin Haringa

## Examples

``` r
portfolio <- data.frame(
  policy_id = 1:10,
  sector = c(rep("Industry", 5), rep("Retail", 4), "Office"),
  claim_count = c(0, 1, 1, 1, 1, 0, 1, 1, 1, 1),
  claim_amount = c(
    0, 25000, 120000, 50000, 175000,
    0, 40000, 90000, 150000, 300000
  ),
  policy_years = rep(1, 10)
)

# Output form 1: include allocated excess in the severity response.
adjusted <- redistribute_excess_loss(
  portfolio,
  claim_amount = "claim_amount",
  threshold = 100000,
  claim_count = "claim_count",
  risk_factor = "sector",
  redistribution_method = "partial",
  output = "redistributed_claim"
)
summary(adjusted)
#>     sector n_records claim_count redistribution_weight n_excess_records
#> 1 Industry         5           4                     4                2
#> 2   Office         1           1                     1                1
#> 3   Retail         4           3                     3                1
#>   n_redistributed_excess_records observed_loss retained_loss
#> 1                              2        370000        275000
#> 2                              1        300000        100000
#> 3                              1        280000        230000
#>   observed_excess_loss redistributed_excess_contributed sector_excess_loading
#> 1                95000                            95000              23750.00
#> 2               200000                           200000             200000.00
#> 3                50000                            50000              16666.67
#>   sector_credibility portfolio_excess_loading blended_excess_loading
#> 1         0.07407407                    43125               41689.81
#> 2         0.01960784                    43125               46200.98
#> 3         0.05660377                    43125               41627.36
#>   redistribution_scaling_factor final_redistribution_loading
#> 1                      1.021186                     42573.07
#> 2                      1.021186                     47179.82
#> 3                      1.021186                     42509.30
#>   allocated_excess_loss average_excess_loading redistributed_excess_received
#> 1             170292.30               42573.07                     170292.30
#> 2              47179.82               47179.82                      47179.82
#> 3             127527.89               42509.30                     127527.89
#>   net_loss_shift adjusted_loss observed_average_claim adjusted_average_claim
#> 1       75292.30      445292.3               92500.00               111323.1
#> 2     -152820.18      147179.8              300000.00               147179.8
#> 3       77527.89      357527.9               93333.33               119176.0

# Inspect the row-level calculation. The allocated amount in the final column
# equals final_redistribution_loading times redistribution weight.
adjusted[c(
  "sector_excess_loading", "sector_credibility",
  "portfolio_excess_loading", "blended_excess_loading",
  "redistribution_scaling_factor", "final_redistribution_loading",
  "claim_amount_redistributed_excess"
)]
#>    sector_excess_loading sector_credibility portfolio_excess_loading
#> 1               23750.00         0.07407407                    43125
#> 2               23750.00         0.07407407                    43125
#> 3               23750.00         0.07407407                    43125
#> 4               23750.00         0.07407407                    43125
#> 5               23750.00         0.07407407                    43125
#> 6               16666.67         0.05660377                    43125
#> 7               16666.67         0.05660377                    43125
#> 8               16666.67         0.05660377                    43125
#> 9               16666.67         0.05660377                    43125
#> 10             200000.00         0.01960784                    43125
#>    blended_excess_loading redistribution_scaling_factor
#> 1                41689.81                      1.021186
#> 2                41689.81                      1.021186
#> 3                41689.81                      1.021186
#> 4                41689.81                      1.021186
#> 5                41689.81                      1.021186
#> 6                41627.36                      1.021186
#> 7                41627.36                      1.021186
#> 8                41627.36                      1.021186
#> 9                41627.36                      1.021186
#> 10               46200.98                      1.021186
#>    final_redistribution_loading claim_amount_redistributed_excess
#> 1                          0.00                              0.00
#> 2                      42573.07                          42573.07
#> 3                      42573.07                          42573.07
#> 4                      42573.07                          42573.07
#> 5                      42573.07                          42573.07
#> 6                          0.00                              0.00
#> 7                      42509.30                          42509.30
#> 8                      42509.30                          42509.30
#> 9                      42509.30                          42509.30
#> 10                     47179.82                          47179.82

# Omit row-level calculation columns while retaining them in summary().
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
#> 2   Office         1           1                     1                1
#> 3   Retail         4           3                     3                1
#>   n_redistributed_excess_records observed_loss retained_loss
#> 1                              2        370000        275000
#> 2                              1        300000        100000
#> 3                              1        280000        230000
#>   observed_excess_loss redistributed_excess_contributed sector_excess_loading
#> 1                95000                            95000              23750.00
#> 2               200000                           200000             200000.00
#> 3                50000                            50000              16666.67
#>   sector_credibility portfolio_excess_loading blended_excess_loading
#> 1         0.07407407                    43125               41689.81
#> 2         0.01960784                    43125               46200.98
#> 3         0.05660377                    43125               41627.36
#>   redistribution_scaling_factor final_redistribution_loading
#> 1                      1.021186                     42573.07
#> 2                      1.021186                     47179.82
#> 3                      1.021186                     42509.30
#>   allocated_excess_loss average_excess_loading redistributed_excess_received
#> 1             170292.30               42573.07                     170292.30
#> 2              47179.82               47179.82                      47179.82
#> 3             127527.89               42509.30                     127527.89
#>   net_loss_shift adjusted_loss observed_average_claim adjusted_average_claim
#> 1       75292.30      445292.3               92500.00               111323.1
#> 2     -152820.18      147179.8              300000.00               147179.8
#> 3       77527.89      357527.9               93333.33               119176.0

# Combine levels with limited claim experience before model estimation.
# Three claims is used for this small example; it is not a general minimum.
adjusted$sector_claim_count <- ave(
  adjusted$claim_count, adjusted$sector, FUN = sum
)
adjusted$sector_model <- ifelse(
  adjusted$sector_claim_count >= 3,
  adjusted$sector,
  "Other"
)

# Fit a severity model to redistributed average claim amount. For aggregated
# rows, claim count represents the number of observations underlying each
# average. With one row per claim, this additional weight is unnecessary.
severity_data <- adjusted[adjusted$claim_count > 0, ]
stats::glm(
  claim_amount_adjusted_average ~ sector_model,
  weights = claim_count,
  family = stats::Gamma(link = "log"),
  data = severity_data
)
#> 
#> Call:  stats::glm(formula = claim_amount_adjusted_average ~ sector_model, 
#>     family = stats::Gamma(link = "log"), data = severity_data, 
#>     weights = claim_count)
#> 
#> Coefficients:
#>        (Intercept)   sector_modelOther  sector_modelRetail  
#>           11.62019             0.27922             0.06816  
#> 
#> Degrees of Freedom: 7 Total (i.e. Null);  5 Residual
#> Null Deviance:       0.6092 
#> Residual Deviance: 0.5433    AIC: 195.6

# Output form 2: estimate retained severity and excess loading separately.
# Using policy years expresses excess_loading per policy year.
loading_result <- redistribute_excess_loss(
  portfolio,
  claim_amount = "claim_amount",
  threshold = 100000,
  claim_count = "claim_count",
  redistribution_weight = "policy_years",
  risk_factor = "sector",
  redistribution_method = "partial",
  output = "excess_loading"
)

frequency_model <- stats::glm(
  claim_count ~ sector + offset(log(policy_years)),
  family = stats::poisson(link = "log"),
  data = loading_result
)
retained_severity_model <- stats::glm(
  claim_amount_capped ~ sector,
  weights = claim_count,
  family = stats::Gamma(link = "log"),
  data = loading_result[loading_result$claim_count > 0, ]
)

loading_result$predicted_claim_frequency <- stats::predict(
  frequency_model,
  newdata = loading_result,
  type = "response"
) / loading_result$policy_years
loading_result$predicted_retained_severity <- stats::predict(
  retained_severity_model,
  newdata = loading_result,
  type = "response"
)
loading_result$predicted_retained_risk_premium <-
  loading_result$predicted_claim_frequency *
  loading_result$predicted_retained_severity
loading_result$predicted_total_risk_premium <-
  loading_result$predicted_retained_risk_premium +
  loading_result$excess_loading

# Portfolio redistribution estimates one loading across all sectors. Sector-
# specific excess experience does not enter the allocation loading.
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
#> 2   Office         1           1                     1                1
#> 3   Retail         4           3                     3                1
#>   n_redistributed_excess_records observed_loss retained_loss
#> 1                              2        370000        275000
#> 2                              1        300000        100000
#> 3                              1        280000        230000
#>   observed_excess_loss redistributed_excess_contributed
#> 1                95000                            95000
#> 2               200000                           200000
#> 3                50000                            50000
#>   risk_factor_excess_loading risk_factor_credibility portfolio_excess_loading
#> 1                      43125                       0                    43125
#> 2                      43125                       0                    43125
#> 3                      43125                       0                    43125
#>   blended_excess_loading redistribution_scaling_factor
#> 1                  43125                             1
#> 2                  43125                             1
#> 3                  43125                             1
#>   final_redistribution_loading allocated_excess_loss average_excess_loading
#> 1                        43125                172500                  43125
#> 2                        43125                 43125                  43125
#> 3                        43125                129375                  43125
#>   redistributed_excess_received net_loss_shift adjusted_loss
#> 1                        172500          77500        447500
#> 2                         43125        -156875        143125
#> 3                        129375          79375        359375
#>   observed_average_claim adjusted_average_claim
#> 1               92500.00               111875.0
#> 2              300000.00               143125.0
#> 3               93333.33               119791.7

# Risk-factor redistribution estimates a separate loading for each sector.
# The estimate for a sector uses only that sector's excess loss and weight.
sector_adjusted <- redistribute_excess_loss(
  portfolio,
  claim_amount = "claim_amount",
  threshold = 100000,
  claim_count = "claim_count",
  risk_factor = "sector",
  redistribution_method = "risk_factor"
)

# Allocate in proportion to claim count times insured amount, restricted to
# policies with an insured amount of at least 100,000.
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

# Exclude catastrophe events and unsettled claims from the allocation pool.
# Their full observed claim amounts remain retained in the model data.
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
