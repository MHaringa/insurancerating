# Set reference group to the group with largest exposure

Relevels a factor so that the category with the highest total weight
(e.g., exposure) becomes the reference (first) level. This is useful in
regression settings, where the first level of a factor is taken as the
baseline. In insurance applications, the group with the largest exposure
is often chosen as reference.

## Usage

``` r
biggest_reference(x, weight)
```

## Arguments

- x:

  A factor (unordered). Character vectors should be converted to factor
  before use.

- weight:

  A numeric vector of the same length as `x`, typically representing
  exposure or frequency weights.

## Value

A factor of the same length as `x`, with the reference level set to the
group with the largest weight.

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
mutate(across(where(is.factor), ~biggest_reference(., weight)))
} # }
```
