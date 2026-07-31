# Combine sublevel splits into a relativity specification

Combine one or more definitions created by
[`split_level()`](https://mharinga.github.io/insurancerating/reference/split_level.md)
into the named relativity specification expected by
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

## Usage

``` r
relativities(...)
```

## Arguments

- ...:

  One or more objects created by
  [`split_level()`](https://mharinga.github.io/insurancerating/reference/split_level.md).

## Value

A named list of data frames suitable for the `relativities` argument of
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

## Details

Each input represents one existing level of `model_variable` and the
detailed `split_variable` levels that replace it. Parent levels must be
unique within the combined specification. Levels of the original model
variable that are not included remain unsplit.

`relativities()` only assembles and validates the specification. It does
not estimate, normalise or apply the supplied relativities. Exposure
normalisation, when requested, is performed by
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

## See also

[`split_level()`](https://mharinga.github.io/insurancerating/reference/split_level.md),
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)

## Author

Martin Haringa

## Examples

``` r
relativities(
  split_level(
    "residential",
    new_levels = c("flat", "house"),
    relativities = c(0.95, 1.05)
  ),
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
