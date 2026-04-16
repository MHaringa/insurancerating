# Refit a GLM model or refinement workflow

Backwards-compatible wrapper. Prefer using
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
for the new refinement workflow.

## Usage

``` r
refit_glm(x, intercept_only = FALSE, ...)

update_glm(x, intercept_only = FALSE, ...)
```

## Arguments

- x:

  Object of class `rating_refinement`, `restricted` or `smooth`.

- intercept_only:

  Logical.

- ...:

  Other arguments.

## Value

Object of class `glm`.
