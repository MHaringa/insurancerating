# Summarise a prepared refinement specification

Describe the original GLM and the ordered actuarial adjustments stored
in a `rating_refinement` object before
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called. The summary records what will be applied; it does not compare
fitted predictions because the refined GLM has not yet been estimated.

## Usage

``` r
# S3 method for class 'rating_refinement'
summary(object, ...)
```

## Arguments

- object:

  A `rating_refinement` object.

- ...:

  Currently unused.

## Value

An object of class `summary.rating_refinement` containing model and
package metadata together with a data frame describing the refinement
steps in their evaluation order.

## See also

[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
[`audit_refinement()`](https://mharinga.github.io/insurancerating/reference/audit_refinement.md)
