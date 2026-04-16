# Summary method for fitgam objects

Provides a concise summary of a `fitgam` object created by
[`riskfactor_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md).
Shows the fitted model type, the risk factor, and basic information
about the prediction data.

## Usage

``` r
# S3 method for class 'fitgam'
summary(object, ...)
```

## Arguments

- object:

  An object of class `"fitgam"`.

- ...:

  Further arguments passed to or from other methods (ignored).

## Value

Invisibly returns `object`.

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- riskfactor_gam(MTPL,
                      nclaims = "nclaims",
                      x = "age_policyholder",
                      exposure = "exposure")
summary(fit)
} # }
```
