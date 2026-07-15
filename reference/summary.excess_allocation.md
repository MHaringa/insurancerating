# Summarise an excess-loss allocation

Return the allocation audit table from an object produced by
[`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md).

## Usage

``` r
# S3 method for class 'excess_allocation'
summary(object, compare_to_empirical = FALSE, ...)
```

## Arguments

- object:

  An object returned by
  [`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md).

- compare_to_empirical:

  Logical. If `TRUE`, keep columns with the empirical loss and empirical
  excess loss used for comparison.

- ...:

  Unused.

## Value

A `data.frame` with aggregated allocation statistics. The returned table
has one row per risk-factor level. The original risk-factor and
allocation-weight names are preserved. "Observed" refers to historical
excess loss in the input data; "allocated" refers to the
credibility-weighted allocation. Loadings are amounts per unit of
allocation weight and losses are total amounts for the level. The
returned columns are:

- `<risk_factor>`:

  Risk-factor level. For example, `risk_factor = "sector"` returns a
  `sector` column. When no risk factor is supplied, this column is named
  `risk_factor` and contains `"portfolio"`.

- `<allocation_weight>`:

  Total allocation weight for the level. For example,
  `allocation_weight = "earned_exposure"` returns an `earned_exposure`
  column.

- `claim_count`:

  Number of claims in the level, based on the supplied `claim_count`
  column or inferred from positive claim amounts.

- `excess_claim_count`:

  Number of claim records with excess loss.

- `observed_excess_loss`:

  Historically observed excess loss.

- `observed_excess_loading`:

  Observed excess loss per unit of allocation weight.

- `credibility`:

  Credibility assigned to the risk-factor experience.

- `<risk_factor>_excess_loading`:

  Risk-factor-specific excess loading before pooling. When no risk
  factor is supplied, this column is named `risk_factor_excess_loading`.

- `portfolio_excess_loading`:

  Portfolio-wide excess loading.

- `allocated_excess_loading`:

  Credibility-weighted excess loading.

- `allocated_excess_loss`:

  Total excess loss allocated to the level.

- `allocation_difference`:

  Allocated minus observed excess loss, included when
  `compare_to_empirical = TRUE`. A positive value means the level
  receives more allocated excess loss than it generated historically.

- `allocation_difference_ratio`:

  Allocation difference relative to observed excess loss, included when
  `compare_to_empirical = TRUE`. When observed excess loss is zero this
  value is `NA_real_`, because a relative comparison with zero observed
  excess loss is undefined.

## See also

[`allocate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/allocate_excess_loss.md)

## Author

Martin Haringa
