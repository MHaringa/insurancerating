# Deprecated restriction helper

`restrict_coef()` is deprecated as of version 0.9.0. Use
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
instead.


    prepare_refinement(model) |>
      add_restriction(...) |>
      refit()

## Usage

``` r
restrict_coef(
  model,
  restrictions,
  allow_new_levels = TRUE,
  allow_new_risk_factors = TRUE
)
```

## Arguments

- model:

  A fitted model object.

- restrictions:

  data.frame with exactly two columns.

- allow_new_levels:

  Logical. If `TRUE` (default), restrictions may include tariff levels
  that were not observed when the model was fitted. See
  [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md).

- allow_new_risk_factors:

  Logical. Whether a fixed tariff factor that is available in the model
  data but absent from the fitted model may be added. The default is
  `TRUE` to preserve the historical behaviour of `restrict_coef()`. New
  code using
  [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  requires an explicit opt-in because its default is `FALSE`.

## Value

A `rating_refinement` object containing the restriction step. Call
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
to apply the restriction and return the refined GLM. New code should use
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md)
followed by
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
directly.

## See also

[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
