# Deprecated refit wrapper

`refit_glm()` is deprecated as of version 0.9.0. Use
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
instead.

## Usage

``` r
refit_glm(x, intercept_only = FALSE, ...)
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
