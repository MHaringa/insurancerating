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

  A data.frame containing the insurance portfolio.

- nclaims:

  Deprecated. Use `claim_count` instead.

- x:

  Deprecated. Use `risk_factor` instead.

- exposure:

  Character, name of column in `data` with the exposure.

- amount:

  Deprecated. Use `claim_amount` instead.

- pure_premium:

  (Optional) Character, column name in `data` with the pure premium.
  Required for `model = "pure_premium"`.

- model:

  Character string specifying the model type. One of `"frequency"`,
  `"severity"`, or `"pure_premium"`. Default is `"frequency"`. The old
  value `"burning"` is deprecated and maps to `"pure_premium"`.

- round_x:

  Deprecated. Use `round_risk_factor` instead.

- risk_factor:

  Character, name of column in `data` with the continuous risk factor.

- claim_count:

  Character, name of column in `data` with the number of claims.

- claim_amount:

  (Optional) Character, column name in `data` with the claim amount.
  Required for `model = "severity"`.

- round_risk_factor:

  (Optional) Numeric value to round the risk factor to a multiple of
  `round_risk_factor`. Can speed up fitting for factors with many
  distinct values.

## Value

See
[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md).
