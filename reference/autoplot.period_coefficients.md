# Plot coefficients fitted separately by period

Plot coefficients fitted separately by period

## Usage

``` r
# S3 method for class 'period_coefficients'
autoplot(object, ..., scale = c("link", "relativity"), terms = NULL)
```

## Arguments

- object:

  A result from `coefficient_stability(method = "period")`.

- ...:

  Not used.

- scale:

  Coefficients on the link scale or exponentiated effects for a log
  link.

- terms:

  Optional coefficient or model-term names to plot.

## Value

A ggplot object.
