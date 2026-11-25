# Plot discrimination and lift

Plot discrimination and lift

## Usage

``` r
# S3 method for class 'discrimination_result'
autoplot(
  object,
  ...,
  type = c("concentration", "lift", "ordered_lorenz", "lorenz"),
  benchmark = TRUE
)
```

## Arguments

- object:

  A
  [`discrimination_analysis()`](https://mharinga.github.io/insurancerating/reference/discrimination_analysis.md)
  result.

- ...:

  Not used.

- type:

  Plot of concentration curves, lift or ordered Lorenz curves.
  `"lorenz"` is an alias for `"concentration"`.

- benchmark:

  Show the classical Lorenz benchmark on concentration plots.

## Value

A ggplot object.
