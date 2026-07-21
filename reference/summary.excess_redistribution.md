# Summarise redistributed large-loss experience

Audit how much excess loss was contributed and received across portfolio
segments after
[`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md).
The summary shows whether a segment receives more large-loss cost than
it contributes, or transfers part of its observed excess burden to other
segments.

## Usage

``` r
# S3 method for class 'excess_redistribution'
summary(object, by = NULL, ...)
```

## Arguments

- object:

  An object returned by
  [`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md).

- by:

  Optional character string. Column used to group the audit. If `NULL`,
  use the original risk factor or return a portfolio-level summary.

- ...:

  Unused.

## Value

A data.frame with one row per audit group. The grouping column keeps its
original name and type when `by` is used. The remaining columns are:

- `n_records`:

  Number of portfolio records in the group.

- `claim_count`:

  Number of claims in the group.

- `redistribution_weight`:

  Total redistribution weight of rows that receive redistributed loss.

- `n_excess_records`:

  Number of records with an observed amount above the threshold.

- `n_redistributed_excess_records`:

  Number of records whose excess amount was actually contributed to the
  redistribution pool.

- `observed_loss`:

  Observed claim cost before redistribution.

- `observed_excess_loss`:

  Observed claim cost above the threshold, including excess from rows
  excluded through `redistribute_excess`.

- `redistributed_excess_contributed`:

  Excess removed from selected large losses and contributed to the
  redistribution pool.

- `<risk_factor>_excess_loading`:

  Excess loading estimated from the risk-factor-level experience, per
  unit of redistribution weight.

- `<risk_factor>_credibility`:

  Weight assigned to the risk-factor loading. It is zero for portfolio
  redistribution, one for risk-factor redistribution and between zero
  and one for partial redistribution.

- `portfolio_excess_loading`:

  Portfolio-wide excess loading per unit of redistribution weight.

- `blended_excess_loading`:

  Risk-factor loading times credibility plus portfolio loading times one
  minus credibility.

- `redistribution_scaling_factor`:

  Factor that preserves the total amount being redistributed after
  blending.

- `final_redistribution_loading`:

  Blended loading multiplied by the redistribution scaling factor.

- `redistributed_excess_received`:

  Excess assigned to receiving claim-bearing rows.

- `net_loss_shift`:

  Received minus contributed redistributed excess.

- `adjusted_loss`:

  Claim cost after redistribution.

- `observed_average_claim`:

  Observed loss divided by claim count.

- `adjusted_average_claim`:

  Adjusted loss divided by claim count.

## Details

`redistributed_excess_contributed` is the excess amount removed from
selected large losses in a segment. `redistributed_excess_received` is
the amount assigned back to claim-bearing rows in that segment. Their
difference is:

\$\$ net\\loss\\shift = received - contributed \$\$

A positive value means that the segment receives more redistributed loss
than it contributed. A negative value means that it transfers loss to
other segments. Across the full portfolio, `net_loss_shift` sums to
zero, subject to numerical tolerance.

When `by = NULL`, the summary uses the `risk_factor` supplied to
[`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md).
If no risk factor was supplied, one portfolio row is returned. Supply
`by` to inspect a portfolio redistribution by another portfolio
characteristic, for example `summary(x, by = "sector")`.

The summary also exposes the loading calculation. For partial
redistribution, `<risk_factor>_excess_loading` is blended with
`portfolio_excess_loading` using `<risk_factor>_credibility`. The
resulting `blended_excess_loading` is multiplied by
`redistribution_scaling_factor` to obtain
`final_redistribution_loading`. Multiplying the final loading by the
total receiving `redistribution_weight` gives
`redistributed_excess_received`.

If `by` differs from the risk factor used in the redistribution, loading
and credibility columns are receiving-weighted averages within each
audit group.

## See also

[`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md)

## Author

Martin Haringa
