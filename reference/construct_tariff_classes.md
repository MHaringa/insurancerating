# Deprecated alias for `derive_tariff_segments()`

`construct_tariff_classes()` is deprecated as of version 0.9.0. Use
[`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md)
instead.

## Usage

``` r
construct_tariff_classes(
  object,
  complexity = 0,
  max_iterations = 10000,
  population_size = 200,
  seed = 1,
  alpha = NULL,
  niterations = NULL,
  ntrees = NULL
)
```

## Arguments

- object:

  An object of class `"riskfactor_gam"`, produced by
  [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md).
  Objects with the old `"fitgam"` class are still supported for backward
  compatibility.

- complexity:

  Numeric. Controls the complexity penalty used when deriving segments.
  Higher values generally yield fewer tariff segments. Default = 0.

- max_iterations:

  Integer. Maximum number of search iterations used by the underlying
  grouping algorithm. Default = 10000.

- population_size:

  Integer. Number of candidate trees used by the underlying grouping
  algorithm. Default = 200.

- seed:

  Integer, seed for the random number generator (for reproducibility).

- alpha:

  Deprecated. Use `complexity` instead.

- niterations:

  Deprecated. Use `max_iterations` instead.

- ntrees:

  Deprecated. Use `population_size` instead.

## Value

See
[`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md).
