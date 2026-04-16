# Coerce bootstrap_performance objects to a vector

Extracts the bootstrap RMSE values as a numeric vector.

## Usage

``` r
# S3 method for class 'bootstrap_performance'
as.vector(x, ...)
```

## Arguments

- x:

  An object of class `"bootstrap_performance"`.

- ...:

  Further arguments passed to
  [`as.vector()`](https://rdrr.io/r/base/vector.html).

## Value

A numeric vector with the bootstrap RMSE values.
