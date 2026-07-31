# Set the reference level of a factor

Relevels a factor so that the selected category becomes the reference
(first) level. By default, the reference level is chosen as the level
with the largest total weight, for example the largest exposure in an
insurance portfolio. Use `method = "manual"` with `reference_level` when
a specific business category should be the reference level.

Choosing a reference level does not change fitted values or the overall
model fit. It changes the coefficient parameterisation and therefore the
level against which the remaining factor relativities are expressed.

## Usage

``` r
set_reference_level(
  x,
  weight = NULL,
  method = "largest_weight",
  reference_level = NULL
)
```

## Arguments

- x:

  A factor (unordered). Character vectors should be converted to factor
  before use.

- weight:

  A numeric vector of the same length as `x`, typically representing
  exposure or frequency weights. Required when
  `method = "largest_weight"`.

- method:

  Character. Method used to choose the reference level. Supported
  methods are `"largest_weight"` and `"manual"`.

- reference_level:

  Character string with the level to use as reference when
  `method = "manual"`.

## Value

A factor of the same length as `x`, with the selected reference level
set as the first level.

## Details

`method = "largest_weight"` is useful when the reference category should
represent a substantial and relatively stable part of the portfolio. The
supplied `weight` is commonly earned exposure, but another actuarially
meaningful volume measure may be used.

`method = "manual"` is appropriate when the reference category is
determined by tariff interpretation, governance or an established
pricing convention. The selected category must already be an observed
factor level.

## References

Kaas, Rob & Goovaerts, Marc & Dhaene, Jan & Denuit, Michel. (2008).
Modern Actuarial Risk Theory: Using R.
[doi:10.1007/978-3-540-70998-5](https://doi.org/10.1007/978-3-540-70998-5)

## Author

Martin Haringa

## Examples

``` r
portfolio <- data.frame(
  region = factor(c("North", "North", "South", "West")),
  exposure = c(120, 80, 60, 40)
)

set_reference_level(portfolio$region, portfolio$exposure)
#> [1] North North South West 
#> attr(,"xoriginal")
#> [1] North South West 
#> Levels: North South West
set_reference_level(
  portfolio$region,
  method = "manual",
  reference_level = "South"
)
#> [1] North North South West 
#> attr(,"xoriginal")
#> [1] North South West 
#> Levels: South North West

# Apply the largest-weight reference rule to every factor in a data frame
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
df <- chickwts |>
  mutate(across(where(is.character), as.factor)) |>
  mutate(across(where(is.factor), ~set_reference_level(., weight)))
```
