# Summarise a refinement audit

Return and print the provenance, ordered refinement steps, total
portfolio effect and the largest absolute level changes from
[`audit_refinement()`](https://mharinga.github.io/insurancerating/reference/audit_refinement.md).

## Usage

``` r
# S3 method for class 'refinement_audit'
summary(object, top_n = 10, ...)
```

## Arguments

- object:

  A `refinement_audit` object.

- top_n:

  Non-negative whole number controlling how many level changes are
  included in the printed summary.

- ...:

  Currently unused.

## Value

An object of class `summary.refinement_audit` containing the audit
metadata, formulas, steps, portfolio result and selected level impacts.

## See also

[`audit_refinement()`](https://mharinga.github.io/insurancerating/reference/audit_refinement.md)
