# Coerce fitgam objects to a data frame

Extracts the prediction component of a `fitgam` object and returns it as
a data frame.

## Usage

``` r
# S3 method for class 'fitgam'
as.data.frame(x, ...)
```

## Arguments

- x:

  An object of class `"fitgam"`.

- ...:

  Further arguments passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Value

A `data.frame` containing the predictions.
