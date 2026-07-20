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

  Logical. If `TRUE`, append `allocation_difference` and
  `allocation_difference_ratio` to compare the credibility-weighted
  allocation with historically observed excess loss.

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

- `<claim_count>`:

  Number of claims in the level. When `claim_count` is supplied, its
  original column name is preserved. When `claim_count = NULL`, inferred
  counts are returned as `claim_count`.

- `excess_claim_count`:

  Number of claim records with excess loss.

- `observed_excess_loss`:

  Historically observed excess loss.

- `observed_excess_loading`:

  Observed excess loss per unit of allocation weight.

- `<risk_factor>_excess_loading`:

  Risk-factor-specific excess loading before pooling. When no risk
  factor is supplied, this column is named `risk_factor_excess_loading`.

- `<risk_factor>_credibility`:

  Credibility weight assigned to the risk-factor-level estimate.

- `portfolio_excess_loading`:

  Portfolio-level excess loading per unit of allocation weight.

- `blended_excess_loading`:

  Credibility-weighted blend of the risk-factor and portfolio estimates.
  For `risk_factor = "sector"`, this is calculated as
  `sector_excess_loading * sector_credibility + portfolio_excess_loading * (1 - sector_credibility)`.

- `expected_excess_loss`:

  Total expected excess loss allocated to the level, calculated as
  `blended_excess_loading * <allocation_weight>`.

- `allocation_difference`:

  Expected minus observed excess loss, included when
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
