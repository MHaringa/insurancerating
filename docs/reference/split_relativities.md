# Construct a relativities mapping for level splitting

Helper function to create a standardized data.frame defining
relativities for sublevels within a risk factor level. This function is
intended to be used as input for
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).

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

A data.frame with columns:

- new_level:

  Character. Name of the new sublevel.

- relativity:

  Numeric. Multiplicative factor relative to the base level.

## Details

This function provides a convenient and safe way to construct the
required input structure for
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).
Each call defines how a single level of a risk factor is split into
multiple sublevels with corresponding relativities.

## Examples

``` r
split_relativities(
  new_levels = c("residential", "commercial", "civil"),
  relativities = c(1.00, 1.10, 1.25)
)
#>     new_level relativity
#> 1 residential       1.00
#> 2  commercial       1.10
#> 3       civil       1.25
```
