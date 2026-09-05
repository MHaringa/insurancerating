# Define sublevel relativity specifications

Use `split_level()` to describe how one existing GLM factor level is
divided into more detailed portfolio levels with specified
multiplicative relativities. Use `relativities()` to combine one or more
of these definitions into the specification supplied to
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

## Usage

``` r
split_level(level, new_levels, relativities = NULL)

relativities(...)
```

## Arguments

- level:

  Character string. Existing level of the risk factor to split.

- new_levels:

  Named numeric vector whose names identify levels of the more detailed
  portfolio variable and whose values give their multiplicative
  relativities. When `relativities` is supplied separately, this may
  instead be a character vector containing the level names.

- relativities:

  Optional numeric vector of multiplicative relativities corresponding
  to a character `new_levels` vector. It must have the same length as
  `new_levels`. Use this argument for the alternative two-vector syntax.

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
The preferred and most concise syntax is a named numeric vector: its
names are the new levels and its values are their relative tariff
effects. Alternatively, supply the level names as a character vector and
their effects through the separate `relativities` argument. Both forms
are supported.

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
  new_levels = c(flat = 0.95, house = 1.05)
)

relativities(
  construction_split,
  split_level(
    "commercial",
    new_levels = c(shop = 1.10, office = 0.90)
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

# The same split can also be defined with separate vectors.
split_level(
  level = "commercial",
  new_levels = c("retail shop", "office / services"),
  relativities = c(1.10, 0.90)
)
#> $commercial
#>           new_level relativity
#> 1       retail shop        1.1
#> 2 office / services        0.9
#> 
```
