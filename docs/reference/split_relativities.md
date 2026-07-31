# Deprecated low-level relativity constructor

`split_relativities()` is deprecated. Use
[`split_level()`](https://mharinga.github.io/insurancerating/reference/split_level.md)
to define a named parent-level split and combine multiple splits with
[`relativities()`](https://mharinga.github.io/insurancerating/reference/relativities.md).

## Usage

``` r
split_relativities(new_levels, relativities)
```

## Arguments

- new_levels:

  Character vector. Names of the new sublevels.

- relativities:

  Numeric vector. Relativities corresponding to each sublevel. Must have
  the same length as `new_levels`.

## Value

A data frame with columns `new_level` and `relativity`. New code should
use
[`split_level()`](https://mharinga.github.io/insurancerating/reference/split_level.md),
which also records the parent model level required by
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

## See also

[`split_level()`](https://mharinga.github.io/insurancerating/reference/split_level.md),
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
