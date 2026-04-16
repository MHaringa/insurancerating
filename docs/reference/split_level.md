# Define a level split with relativities

Helper function to define how one level of a risk factor should be split
into sublevels with corresponding relativities. Intended for use inside
[`relativities_list()`](https://mharinga.github.io/insurancerating/reference/relativities_list.md)
and
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

## Usage

``` r
split_level(level, new_levels, relativities)
```

## Arguments

- level:

  Character string. Existing level of the risk factor to split.

- new_levels:

  Character vector. Names of the new sublevels.

- relativities:

  Numeric vector. Relativities corresponding to each sublevel. Must have
  the same length as `new_levels`.

## Value

A named list of length 1, where the name is `level` and the value is a
data.frame with columns `new_level` and `relativity`.

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
