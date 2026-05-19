# Allocate excess loss to a pricing portfolio

Allocate a historical excess burden over a portfolio and optionally
model uncertainty around that burden.

`allocate_excess_loss()` is the core allocation step in the excess-loss
workflow. It starts from a deterministic excess amount, usually
`excess_claim_amount` produced by
[`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md),
and converts it into an excess loading per row.

`pooling` controls how much excess risk is shared between groups:

- `"portfolio"`:

  All included rows share one portfolio-wide excess loading: total
  excess divided by total weight.

- `"group"`:

  Each group carries only its own excess burden. This is responsive to
  group experience, but can be volatile for sparse large-loss data.

- `"partial"`:

  Blend the group-specific loading with the portfolio loading using
  credibility. This is useful when some groups have enough large-loss
  experience to be partly credible while smaller groups should pool back
  toward the portfolio.

When `pooling = "partial"` and `credibility = NULL`, credibility is
determined automatically from the amount and quality of group-specific
experience. The automatic credibility factor reflects the size of the
group, the number of observed claims, the number of excess claims and
the share of loss above the threshold. Groups with more stable and
repeated excess loss experience receive more credibility. Groups with
limited or incidental excess loss experience are pooled more strongly
towards the portfolio-wide loading.

`severity_noise` is only available with `method = "bootstrap"`. With
`"lognormal"`, sampled excess claims are multiplied by lognormal noise,
which is usually more natural for large claims because claim amounts
remain positive and variation is multiplicative.
`severity_noise_sd = 0.25` is a practical starting point; `0.10` gives
limited variation and `0.50` strong variation.

## Usage

``` r
allocate_excess_loss(
  data,
  excess_amount,
  weight,
  include = NULL,
  group = NULL,
  threshold = NULL,
  tail_fit_threshold = NULL,
  method = c("observed", "bootstrap"),
  pooling = c("portfolio", "group", "partial"),
  credibility = NULL,
  n_boot = 1000,
  severity_noise = c("none", "lognormal", "normal"),
  severity_noise_sd = 0.25
)
```

## Arguments

- data:

  A `data.frame`, typically the output of
  [`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md).

- excess_amount:

  Character string. Excess amount column.

- weight:

  Character string. Allocation weight column, usually exposure.

- include:

  Optional character string. Logical column indicating which rows
  participate in the allocation. If `NULL`, all rows are included.

- group:

  Optional character string. Grouping column for group or partial
  pooling.

- threshold:

  Optional numeric scalar. Main excess threshold, stored for audit
  output.

- tail_fit_threshold:

  Optional numeric scalar. Lower threshold used as tail information.
  Only allowed when `method = "bootstrap"` and must be smaller than or
  equal to `threshold`.

- method:

  Character. `"observed"` or `"bootstrap"`.

- pooling:

  Character. `"portfolio"`, `"group"` or `"partial"`.

- credibility:

  Optional numeric scalar between 0 and 1. Used for
  `pooling = "partial"`. If `NULL`, credibility is estimated by group.

- n_boot:

  Positive whole number. Number of bootstrap samples.

- severity_noise:

  Character. `"none"`, `"lognormal"` or `"normal"`.

- severity_noise_sd:

  Non-negative numeric scalar controlling severity noise in bootstrap
  samples.

## Value

An object of class `"excess_loss_allocation"`.

## Details

`method = "observed"` allocates the historically observed excess loss.

`method = "bootstrap"` resamples only the positive excess claim amounts:

`excess_amount[excess_amount > 0]`

The full claim distribution is not bootstrapped.

Bootstrap allocation introduces uncertainty in both:

- the total amount of excess loss;

- the distribution of excess loss across allocation groups.

This means that the bootstrap not only changes the total excess burden
that needs to be redistributed, but also how that burden is distributed
across portfolio segments.

When `tail_fit_threshold` is supplied, claims above the lower threshold
are used to better approximate the tail of the claim distribution. This
can be useful when only a limited number of claims exceed the main
excess threshold.

Severity noise can optionally be added to bootstrapped excess claims.
This is useful when the number of excess claims is small and a regular
bootstrap would otherwise repeatedly reproduce only the same observed
large losses.

When severity noise is used, additional variability is introduced in the
size of individual excess claims.

Lognormal severity noise is usually more natural for excess claims
because claim amounts are positive and large losses are typically
right-skewed.

For lognormal severity noise, the approximate multiplicative p10/p50/p90
factors can be inspected with:

`exp(qnorm(c(0.10, 0.50, 0.90)) * severity_noise_sd)`

For example, with `severity_noise_sd = 0.25`, this gives approximately
`0.73x`, `0.97x` and `1.38x`. An excess claim of 100,000 would therefore
typically vary between roughly 73,000 and 138,000.

The following validation rules apply:

- `tail_fit_threshold` can only be used when `method = "bootstrap"`.

- `tail_fit_threshold` must be smaller than or equal to `threshold`.

- `severity_noise != "none"` can only be used when
  `method = "bootstrap"`.

## Author

Martin Haringa

## Examples

``` r
claims <- data.frame(
  sector = rep(c("Industry", "Retail"), each = 4),
  claim_amount = c(1000, 120000, 30000, 8000, 2000, 150000, 40000, 6000),
  earned_exposure = rep(1, 8)
)
decomposed <- calculate_excess_loss(claims, "claim_amount", threshold = 100000)
allocation <- allocate_excess_loss(
  decomposed,
  excess_amount = "excess_claim_amount",
  weight = "earned_exposure",
  group = "sector",
  pooling = "partial"
)
summary(allocation)
#>      group group_weight n_claims n_excess_claims historical_excess_loss
#> 1 Industry            4        4               1                  20000
#> 2   Retail            4        4               1                  50000
#>   excess_loss_ratio group_loading portfolio_loading credibility
#> 1                 1          5000              8750        0.88
#> 2                 1         12500              8750        0.88
#>   allocated_loading allocated_excess_loss
#> 1              5450                 21800
#> 2             12050                 48200
```
