# Plot the resampled performance distribution

Display the empirical distribution of resampled RMSE values from
[`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md).
The histogram shows the observed resampling distribution, while the
density curve provides a smooth visual summary.

## Usage

``` r
# S3 method for class 'bootstrap_performance'
autoplot(object, fill = "#E6E6E6", color = NA, ...)
```

## Arguments

- object:

  An object of class `"bootstrap_performance"`, produced by
  [`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md).

- fill:

  Fill colour of the histogram bars. Default is `"#E6E6E6"`.

- color:

  Border colour of the histogram bars. Default is `NA`, which removes
  bar borders.

- ...:

  Currently unused.

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Details

The dashed orange line marks the RMSE of the original fitted model. The
dotted grey lines mark the 2.5 and 97.5 percent empirical quantiles of
the resampled values when these can be calculated. Their distance
provides a practical indication of sampling variability; it is not a
formal prediction interval for future portfolio performance.

## See also

[`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md),
[`rmse()`](https://mharinga.github.io/insurancerating/reference/rmse.md)

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
mod1 <- glm(nclaims ~ age_policyholder, data = MTPL,
            offset = log(exposure), family = poisson())
x <- bootstrap_performance(mod1, MTPL, n_resamples = 100,
                           show_progress = FALSE)
autoplot(x)
} # }
```
