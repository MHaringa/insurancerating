# Automatically create a ggplot for objects obtained from bootstrap_rmse()

Takes an object produced by
[`bootstrap_rmse()`](https://mharinga.github.io/insurancerating/reference/bootstrap_rmse.md),
and plots the simulated RMSE

## Usage

``` r
# S3 method for class 'bootstrap_rmse'
autoplot(object, fill = NULL, color = NULL, ...)
```

## Arguments

- object:

  bootstrap_rmse object produced by
  [`bootstrap_rmse()`](https://mharinga.github.io/insurancerating/reference/bootstrap_rmse.md)

- fill:

  color to fill histogram (default is "steelblue")

- color:

  color to plot line colors of histogram

- ...:

  other plotting parameters to affect the plot

## Value

a ggplot object

## Author

Martin Haringa
