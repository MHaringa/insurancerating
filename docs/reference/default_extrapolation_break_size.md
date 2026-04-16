# Default extrapolation break size based on existing tariff breaks

Uses the median width of existing break intervals as a robust
scale-aware default for extrapolation discretisation.

## Usage

``` r
default_extrapolation_break_size(borders_model, factor = 1)
```

## Arguments

- borders_model:

  A data.frame with columns `breaks_min` and `breaks_max`.

- factor:

  Numeric scalar \> 0. Multiplier applied to the median break width.

## Value

Numeric scalar (\> 0).
