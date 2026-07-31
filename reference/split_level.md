# Define a sublevel split for a model level

Define how one level of a GLM risk factor is divided into more detailed
portfolio levels with specified multiplicative relativities. The
resulting object is intended to be combined with
[`relativities()`](https://mharinga.github.io/insurancerating/reference/relativities.md)
and supplied to
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

## Usage

``` r
split_level(level, new_levels, relativities)
```

## Arguments

- level:

  Character string. Existing level of the risk factor to split.

- new_levels:

  Character vector. Levels of the more detailed portfolio variable
  within `level`.

- relativities:

  Numeric vector. Multiplicative relativities corresponding to
  `new_levels`. Must have the same length as `new_levels`.

## Value

A named list of length one. Its name is `level`; its value is a data
frame with columns `new_level` and `relativity`.

## Details

`level` identifies the existing parent level in `model_variable`.
`new_levels` identifies the corresponding levels of `split_variable`.
`relativities` gives their relative tariff effects before any optional
exposure normalisation by
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

This helper defines a tariff assumption; it does not estimate
relativities from claim experience and does not alter a fitted GLM.

## See also

[`relativities()`](https://mharinga.github.io/insurancerating/reference/relativities.md),
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)

## Author

Martin Haringa

## Examples

``` r
split_level(
  level = "construction",
  new_levels = c("residential", "commercial", "civil"),
  relativities = c(1.00, 1.10, 1.25)
)
#> $construction
#>     new_level relativity
#> 1 residential       1.00
#> 2  commercial       1.10
#> 3       civil       1.25
#> 
```
