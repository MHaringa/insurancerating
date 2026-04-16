# Add coefficient restrictions to a refinement workflow

Adds a restriction step to a `rating_refinement` object.

## Usage

``` r
add_restriction(model, restrictions)

restrict_coef(model, restrictions)
```

## Arguments

- model:

  Object of class `rating_refinement`.

- restrictions:

  data.frame with two columns containing restricted data.

## Value

Object of class `rating_refinement`.

## Details

`add_restriction()` is part of the new refinement workflow and is
intended to be used in combination with
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md)
and
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).

The legacy function `restrict_coef()` remains the only function that
should be applied directly to a model object.
