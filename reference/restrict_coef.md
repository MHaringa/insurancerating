# Deprecated restriction helper

`restrict_coef()` is deprecated as of version 0.9.0. Use
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
instead.


    prepare_refinement(model) |>
      add_restriction(...) |>
      refit()

## Usage

``` r
restrict_coef(model, restrictions)
```

## Arguments

- model:

  A fitted model object.

- restrictions:

  data.frame with exactly two columns.

## Value

A legacy restricted object. New code should use
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
and
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).

## See also

[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
