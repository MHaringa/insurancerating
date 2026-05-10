# Set the reference level of a factor

Relevels a factor so that the selected category becomes the reference
(first) level. By default, the reference level is chosen as the level
with the largest total weight, for example the largest exposure in an
insurance portfolio. Use `method = "manual"` with `reference_level` when
a specific business category should be the reference level.

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

## References

Kaas, Rob & Goovaerts, Marc & Dhaene, Jan & Denuit, Michel. (2008).
Modern Actuarial Risk Theory: Using R.
[doi:10.1007/978-3-540-70998-5.](https://doi.org/10.1007/978-3-540-70998-5.)

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
df <- chickwts |>
mutate(across(where(is.character), as.factor)) |>
mutate(across(where(is.factor), ~set_reference_level(., weight)))

set_reference_level(df$feed, method = "manual", reference_level = "casein")
} # }
```
