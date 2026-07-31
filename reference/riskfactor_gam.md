# Deprecated alias for `risk_factor_gam()`

`riskfactor_gam()` is deprecated in favour of
[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md).

## Usage

``` r
riskfactor_gam(
  data,
  nclaims = NULL,
  x = NULL,
  exposure = NULL,
  amount = NULL,
  pure_premium = NULL,
  model = "frequency",
  round_x = NULL,
  risk_factor = NULL,
  claim_count = NULL,
  claim_amount = NULL,
  round_risk_factor = NULL
)
```

## Arguments

- data:

  A data frame containing portfolio observations.

- nclaims:

  Deprecated. Use `claim_count` instead.

- x:

  Deprecated. Use `risk_factor` instead.

- exposure:

  Character string. Exposure column used as an offset or aggregation
  weight.

- amount:

  Deprecated. Use `claim_amount` instead.

- pure_premium:

  Optional character string. Row-level risk-premium column. Required for
  `model = "pure_premium"` and aggregated using exposure weights.

- model:

  Character string. Response context: `"frequency"`, `"severity"` or
  `"pure_premium"`. The deprecated value `"burning"` maps to
  `"pure_premium"`.

- round_x:

  Deprecated. Use `round_risk_factor` instead.

- risk_factor:

  Character string. Numeric continuous risk-factor column in `data`.

- claim_count:

  Character string. Claim-count column. Required for
  `model = "frequency"` and `model = "severity"`.

- claim_amount:

  Optional character string. Total claim-amount column. Required for
  `model = "severity"`.

- round_risk_factor:

  Optional positive numeric value. The continuous risk factor is rounded
  to multiples of this value before aggregation and model fitting. This
  can reduce computation and local volatility when the variable has many
  distinct values, but it also removes detail.

## Value

See
[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md).
