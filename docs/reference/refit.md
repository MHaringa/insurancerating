# Refit a prepared refinement workflow

Applies all collected refinement steps and refits the underlying GLM in
one call.

## Usage

``` r
refit(x, intercept_only = FALSE, ...)
```

## Arguments

- x:

  Object of class `rating_refinement`.

- intercept_only:

  Logical. Default `FALSE`.

- ...:

  Other arguments.

## Value

Object of class `glm`.
