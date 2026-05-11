# Add coefficient restrictions to a refinement workflow

Fixes selected model levels to user-supplied relativities in a
refinement workflow. This is useful when the fitted GLM coefficients
need to be adjusted before the final tariff is refitted, for example to
apply expert judgement, enforce a business rule, remove an implausible
local effect, or make a tariff structure easier to explain.

## Usage

``` r
add_restriction(model, restrictions)
```

## Arguments

- model:

  Object of class `rating_refinement`, usually created with
  [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md).

- restrictions:

  Data frame with exactly two columns. The first column must have the
  same name as the model variable to restrict and contains the levels to
  adjust. The second column contains the replacement relativities.
  Levels that are not supplied are filled with the currently fitted GLM
  relativities.

## Value

Object of class `rating_refinement`.

## Details

`add_restriction()` stores a restriction step on a `rating_refinement`
object. It does not refit the GLM immediately. The restrictions are
applied when
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

The `restrictions` data frame identifies the model variable to restrict
by its first column. The second column contains the relativities that
should be used for those levels in the refined model. New code should
use this function after
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md);
the deprecated
[`restrict_coef()`](https://mharinga.github.io/insurancerating/reference/restrict_coef.md)
wrapper is only kept for backwards compatibility.

The restriction table may contain all levels of the model variable, or
only the levels that need a manual adjustment. If only a subset is
supplied, the missing levels are automatically filled with their current
fitted GLM relativities. This makes it possible to fix one level
explicitly while keeping the other levels at their already estimated
values.

## Author

Martin Haringa

## Examples

``` r
portfolio <- data.frame(
  claims = c(1, 2, 1, 3, 2, 4),
  exposure = rep(1, 6),
  postal_area = factor(c("A", "B", "C", "A", "B", "C"))
)

model <- glm(
  claims ~ postal_area + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)

restrictions <- data.frame(
  postal_area = "C",
  relativity = 1.10
)

refined <- prepare_refinement(model, data = portfolio) |>
  add_restriction(restrictions)
```
