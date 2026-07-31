# Recover the portfolio data used by a fitted model

**\[experimental\]**

Recover the estimation data and pricing metadata stored with a fitted
GLM or a model produced by the refinement workflow. The result provides
a reproducible basis for rating grids, coefficient tables and
portfolio-level model diagnostics.

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

- `response`: response variable in the model;

- `rf`: names of risk factors in the model;

- `offweights`: weight and offset variables if present;

- `terms`: model terms object for plain GLMs;

- `mgd_rst`, `mgd_smt`: merged restrictions and smooths for refit
  objects;

- `new_nm`, `old_nm`: new and old column names for refit objects.

## Details

### Data represented by the result

For an ordinary GLM, the function recovers the data stored with the
model or its model frame and records the response, model terms, risk
factors, weights and offsets. The recovered data represent the
observations available to the fitted model. Rows omitted during fitting,
for example because of missing model variables, may therefore not be
present.

For a refined model, technical columns used to construct smoothing and
restriction terms are removed from the returned data. The mappings
required to interpret the refined coefficients are retained as
attributes.

### Actuarial use

The extracted object is intended for downstream calculations that must
remain consistent with the fitted pricing model, such as
[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
and
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md).
It should not be interpreted as a replacement for the original raw
portfolio extract: preprocessing, filtering and missing-value handling
applied before or during model fitting remain part of the data
provenance.

## See also

[`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md),
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md),
[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md)

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
