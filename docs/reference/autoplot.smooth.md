# Automatically create a ggplot for objects obtained from smooth_coef()

**\[experimental\]** Takes an object produced by
[`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md),
and produces a plot with a comparison between the smoothed coefficients
and estimated coefficients obtained from the model.

## Usage

``` r
# S3 method for class 'smooth'
autoplot(object, ...)
```

## Arguments

- object:

  object produced by
  [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)

- ...:

  other plotting parameters to affect the plot

## Value

Object of class ggplot2

## Author

Martin Haringa
