# Combine multiple level splits into a relativities list

Helper function to combine multiple level split definitions into a
single named list suitable for use in
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

## Usage

``` r
relativities_list(...)
```

## Arguments

- ...:

  One or more objects created by
  [`split_level()`](https://mharinga.github.io/insurancerating/reference/split_level.md).

## Value

A named list of data.frames suitable for the `relativities` argument in
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

## Examples

``` r
relativities_list(
  split_level("construction",
              c("residential", "commercial", "civil"),
              c(1.00, 1.10, 1.25))
)
#> $construction
#>     new_level relativity
#> 1 residential       1.00
#> 2  commercial       1.10
#> 3       civil       1.25
#> 
```
