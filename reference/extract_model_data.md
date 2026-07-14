# Extract model data

**\[experimental\]**

`extract_model_data()` retrieves the modelling data and metadata from
fitted models. It works for objects of class `"glm"`, as well as objects
produced by refitting procedures (`"refitsmooth"` or
`"refitrestricted"`).

[`model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md)
is kept as a deprecated compatibility wrapper.

## Usage

``` r
extract_model_data(x)
```

## Arguments

- x:

  An object of class `"glm"`, `"refitsmooth"`, or `"refitrestricted"`.

## Value

A `data.frame` of class `"model_data"` with additional attributes:

- `response` — response variable in the model;

- `rf` — names of risk factors in the model;

- `offweights` — weight and offset variables if present;

- `terms` — model terms object for plain GLMs;

- `mgd_rst`, `mgd_smt` — merged restrictions/smooths for refit objects;

- `new_nm`, `old_nm` — new and old column names for refit objects.

## Details

For GLM objects, the function returns the model data and attaches
attributes with the response, rating factors, terms object, and any
weights or offsets.

For refit objects, the function removes auxiliary columns used during
smoothing or restriction and attaches attributes with rating factors,
merged smooths, restrictions, and offsets.

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
library(insurancerating)

pmodel <- glm(
  breaks ~ wool + tension,
  data = warpbreaks,
  family = poisson(link = "log")
)

extract_model_data(pmodel)
} # }
```
