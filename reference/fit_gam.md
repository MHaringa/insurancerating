# Deprecated NSE wrapper for `risk_factor_gam()`

`fit_gam()` is deprecated as of version 0.8.0. Please use
[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
instead.

In addition, note that column arguments must now be passed as
**strings** (standard evaluation).

## Usage

``` r
fit_gam(
  data,
  nclaims,
  x,
  exposure,
  amount = NULL,
  pure_premium = NULL,
  model = "frequency",
  round_x = NULL
)
```

## Arguments

- data:

  A data.frame containing the insurance portfolio.

- nclaims:

  Deprecated NSE argument for claim counts.

- x:

  Deprecated NSE argument for the continuous risk factor.

- exposure:

  Character, name of column in `data` with the exposure.

- amount:

  Deprecated NSE argument for claim amounts.

- pure_premium:

  (Optional) Character, column name in `data` with the pure premium.
  Required for `model = "pure_premium"`.

- model:

  Character string specifying the model type. One of `"frequency"`,
  `"severity"`, or `"pure_premium"`. Default is `"frequency"`. The old
  value `"burning"` is deprecated and maps to `"pure_premium"`.

- round_x:

  Deprecated. Use `round_risk_factor` instead.

## Value

See
[`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md).
