# Assess possible excess-loss thresholds

`assess_excess_thresholds()` is a diagnostic helper for capped severity
and pure premium modelling. It does not choose the best threshold
automatically. Instead, it shows how much claim cost and claim count
sits above candidate thresholds, so actuaries and governance
stakeholders can make an informed threshold choice before using
[`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md).

This is useful when the regular severity GLM is fitted on capped claim
amounts, for example `pmin(claim_amount, 100000)`, and the expected part
above the cap is assessed separately as a technical risk premium
component.

## Usage

``` r
assess_excess_thresholds(
  data,
  claim_amount,
  exposure,
  thresholds,
  by = NULL,
  premium = NULL
)
```

## Arguments

- data:

  A `data.frame` with claim-level observations.

- claim_amount:

  Character string. Claim amount column.

- exposure:

  Character string. Exposure column.

- thresholds:

  Numeric vector of candidate thresholds.

- by:

  Optional character string. Grouping column.

- premium:

  Optional character string. Premium column.

## Value

A `data.frame` with class `"excess_threshold_assessment"`.

## Author

Martin Haringa

## Examples

``` r
claims <- data.frame(
  sector = rep(c("Industry", "Retail"), each = 5),
  claim_amount = c(1000, 25000, 120000, 50000, 175000,
                   2000, 40000, 90000, 150000, 300000),
  earned_exposure = rep(1, 10),
  earned_premium = rep(10000, 10)
)

thresholds <- assess_excess_thresholds(
  data = claims,
  claim_amount = "claim_amount",
  exposure = "earned_exposure",
  thresholds = c(25000, 50000, 100000, 150000),
  by = "sector",
  premium = "earned_premium"
)

autoplot(thresholds, y = "excess_per_exposure")

```
