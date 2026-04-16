# Autoplot for bootstrap_performance objects

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
method for objects created by
[`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md).
Produces a histogram and density plot of the bootstrapped RMSE values,
with the RMSE of the original fitted model shown as a dashed vertical
line. Optionally, 95% quantile bounds are shown as dotted vertical
lines.

## Usage

``` r
# S3 method for class 'bootstrap_performance'
autoplot(object, fill = "steelblue", color = "black", ...)
```

## Arguments

- object:

  An object of class `"bootstrap_performance"`, produced by
  [`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md).

- fill:

  Fill color of the histogram bars. Default = `"steelblue"`.

- color:

  Border color of the histogram bars. Default = `"black"`.

- ...:

  Additional arguments passed to
  [`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
mod1 <- glm(nclaims ~ age_policyholder, data = MTPL,
            offset = log(exposure), family = poisson())
x <- bootstrap_performance(mod1, MTPL, n = 100, show_progress = FALSE)
autoplot(x)
} # }
```
