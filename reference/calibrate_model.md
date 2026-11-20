# Deprecated alias for `adjust_intercept()`

`calibrate_model()` is deprecated. Use
[`adjust_intercept()`](https://mharinga.github.io/insurancerating/reference/adjust_intercept.md)
instead.

## Usage

``` r
calibrate_model(model, factor)
```

## Arguments

- model:

  A fitted refined GLM returned by
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).
  It must inherit from `refitrestricted` or `refitsmooth` and use a log
  link.

- factor:

  Positive finite numeric scalar. `1` retains the prediction level,
  values above 1 increase it, and values below 1 decrease it.

## Value

See
[`adjust_intercept()`](https://mharinga.github.io/insurancerating/reference/adjust_intercept.md).
