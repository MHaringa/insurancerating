# Summarise an excess-loss vector

Return the grouped allocation summary stored on an object created by
[`calculate_excess_loss()`](https://mharinga.github.io/insurancerating/reference/calculate_excess_loss.md).

## Usage

``` r
# S3 method for class 'excess_loss_vector'
summary(object, ...)
```

## Arguments

- object:

  An object of class `"excess_loss_vector"`.

- ...:

  Reserved for future extensions.

## Value

A `data.frame` with allocation summary columns.

## Author

Martin Haringa
