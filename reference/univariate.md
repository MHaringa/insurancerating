# Deprecated alias for `factor_analysis()`

`univariate()` is deprecated as of version 0.8.0. Use
[`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
instead.

## Usage

``` r
univariate(
  df,
  x,
  severity = NULL,
  nclaims = NULL,
  exposure = NULL,
  premium = NULL,
  by = NULL
)
```

## Arguments

- df:

  A `data.frame` with the insurance portfolio.

- x:

  Column name or expression with the risk factor.

- severity:

  Column name or expression with claim amounts.

- nclaims:

  Column name or expression with claim counts.

- exposure:

  Column name or expression with exposures.

- premium:

  Column name or expression with premiums.

- by:

  Optional grouping column name or expression.

## Value

See
[`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md).
