# Define sublevel relativity specifications

Use `split_level()` to describe how one existing GLM factor level is
divided into more detailed portfolio levels with specified
multiplicative relativities. Use `relativities()` to combine one or more
of these definitions into the specification supplied to
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

## Usage

``` r
split_level(level, new_levels, relativities)

relativities(...)
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

- ...:

  One or more objects created by `split_level()`.

## Value

`split_level()` returns a named list of length one. Its name is `level`;
its value is a data frame with columns `new_level` and `relativity`.
`relativities()` returns the combined named list expected by the
`relativities` argument of
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

## Details

`level` identifies the existing parent level in `model_variable`.
`new_levels` identifies the corresponding levels of `split_variable`.
`relativities` gives their relative tariff effects before any optional
exposure normalisation by
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

Each call to `split_level()` represents one parent level. Several parent
levels can be refined in one step by passing their definitions to
`relativities()`. Parent levels must be unique within the combined
specification. Levels of the original model variable that are not
included remain unsplit.

These helpers assemble and validate explicit tariff assumptions. They do
not estimate, normalise or apply the supplied relativities and do not
alter a fitted GLM. Exposure normalisation, when requested, is performed
by
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

## See also

[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)

## Author

Martin Haringa

## Examples

``` r
construction_split <- split_level(
  level = "residential",
  new_levels = c("flat", "house"),
  relativities = c(0.95, 1.05)
)

relativities(
  construction_split,
  split_level(
    "commercial",
    new_levels = c("shop", "office"),
    relativities = c(1.10, 0.90)
  )
)
#> $residential
#>   new_level relativity
#> 1      flat       0.95
#> 2     house       1.05
#> 
#> $commercial
#>   new_level relativity
#> 1      shop        1.1
#> 2    office        0.9
#> 
```
