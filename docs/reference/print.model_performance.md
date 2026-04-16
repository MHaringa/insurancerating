# Print method for model_performance objects

Nicely formats and prints the results of
[`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md),
including rounded numeric values, without requiring external packages.

## Usage

``` r
# S3 method for class 'model_performance'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"model_performance"`.

- digits:

  Number of digits to round numeric columns. Default = 3.

- ...:

  Further arguments passed to or from other methods (ignored).

## Value

Invisibly returns `x`.
