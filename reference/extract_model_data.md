# Recover the portfolio data used by a fitted model

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

- `rf`: risk factors used for default rating-grid grouping, including
  the output factors of split refinements;

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
attributes. A factor created by
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
replaces its parent in the default rating-grid grouping. Its numeric
relativity columns are mapped to the new segment levels using the stored
refinement specification; the original portfolio columns remain
available for explicit grouping and audits.

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
portfolio <- as.data.frame(MTPL2)
portfolio$area <- factor(portfolio$area)
model <- glm(
  nclaims ~ area + offset(log(exposure)),
  family = poisson(), data = portfolio
)

refined <- prepare_refinement(model, data = portfolio) |>
  add_restriction(c("3" = 1.10), model_variable = "area") |>
  refit()

# Includes exposure and the column added by the restriction.
evaluation <- extract_model_data(refined)
names(evaluation)
#> [1] "customer_id"     "area"            "nclaims"         "amount"         
#> [5] "exposure"        "premium"         "area_restricted"
calibration_analysis(refined, evaluation, by = "area")
#>   area Observed   Expected        OE    N   Exposure
#> 1    0        1   1.161442 0.8609984   15   13.30685
#> 2    1      146 169.570580 0.8609984 1194 1065.74795
#> 3    2       98 113.821349 0.8609984  921  818.53973
#> 4    3      113  73.446629 1.5385322  870  764.99178
```
