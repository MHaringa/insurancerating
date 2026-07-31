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

  A `"risk_factor_gam"` object returned by
  [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md).
  Legacy `"riskfactor_gam"` and `"fitgam"` classes are accepted for
  compatibility.

- complexity:

  Non-negative numeric complexity penalty for the evolutionary tree.
  Larger values generally favour fewer internal boundaries.

- max_iterations:

  Positive integer. Maximum number of evolutionary search iterations.

- population_size:

  Positive integer. Number of candidate trees maintained during the
  evolutionary search.

- seed:

  Numeric random seed used by the grouping algorithm.

- alpha:

  Deprecated. Use `complexity` instead.

- niterations:

  Deprecated. Use `max_iterations` instead.

- ntrees:

  Deprecated. Use `population_size` instead.

## Value

See
[`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md).
