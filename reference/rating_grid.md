# Construct observed rating-grid points

Collapse portfolio records with identical risk-factor combinations into
observed rating-grid points. Exposure and other numeric measures can be
aggregated alongside the combinations for prediction, tariff comparison
and portfolio diagnostics.

Together with
[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md),
this function belongs to the portfolio reduction workflow. Both
functions reduce row-level portfolio data while retaining selected
totals. `rating_grid()` reduces across identical risk-factor
combinations;
[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
reduces temporally connected records within the same policy, risk or
portfolio segment.

The function returns only combinations that are actually observed in the
input data. It does **not** create the full Cartesian product of all
unique values. This keeps the output compact and suitable for model
diagnostics, portfolio summaries, and prediction analysis.

When `x` is an object returned by
[`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md),
the function uses the extracted model metadata to determine the grouping
variables if `group_by` is not supplied. When `x` is a plain
`data.frame`, it is recommended to supply `group_by` explicitly.

## Usage

``` r
rating_grid(
  x,
  group_by = NULL,
  exposure = NULL,
  exposure_by = NULL,
  aggregate_cols = NULL,
  drop_na = FALSE,
  group_vars = NULL,
  agg_cols = NULL
)
```

## Arguments

- x:

  A `data.frame`, an object of class `"model_data"` returned by
  [`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md),
  or a fitted model that can be passed to
  [`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md).

- group_by:

  Optional character vector with the variables that define the
  rating-grid points. If `NULL` and `x` is a `"model_data"` object, the
  risk-factor variables stored in the object are used. If `NULL` and `x`
  is a plain `data.frame`, all columns except those listed in
  `exposure`, `exposure_by`, and `aggregate_cols` are used.

- exposure:

  Optional character; name of the exposure column to aggregate.

- exposure_by:

  Optional character; name of a column used to split exposure or counts,
  for example a year variable.

- aggregate_cols:

  Optional character vector with additional numeric columns to aggregate
  using `sum(na.rm = TRUE)`.

- drop_na:

  Logical; if `TRUE`, rows with missing values in `group_by` are removed
  before aggregation. If `FALSE`, missing values define an explicit
  observed group and are retained. Default is `FALSE`.

- group_vars, agg_cols:

  Deprecated argument names. Use `group_by` and `aggregate_cols`
  instead.

## Value

A `data.frame` with one row per observed rating-grid point.

## Details

### Portfolio reduction

`rating_grid()` performs categorical portfolio reduction. It combines
rows with the same observed `group_by` values and retains the
corresponding totals. Use
[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
for the complementary temporal reduction of connected coverage periods.

### Observed combinations

The grid represents the combinations present in the supplied portfolio
or model data. It deliberately does not construct combinations that were
not observed. This avoids creating artificial model points and is
particularly relevant when risk factors are structurally related, such
as product, coverage and distribution channel.

Each output row therefore represents one observed combination of the
variables in `group_by`. Exposure and `aggregate_cols` are summed over
the source records belonging to that combination. Such a row is a model
point: one observed covariate combination together with its aggregated
additive portfolio quantities. This is a categorical reduction of the
portfolio; no date intervals are combined.

### Estimating a GLM on aggregated data

For a standard Poisson frequency GLM, aggregation before model fitting
can preserve the coefficient estimates exactly. This applies when
records are grouped by every predictor used in the model, claim counts
are summed, earned exposure is summed, and the aggregated model uses
`offset(log(exposure))`. Within such a group all records have the same
linear predictor. Their contribution to the coefficient estimation
therefore depends on total claims and total exposure, which are retained
by the rating grid.

This equivalence is conditional, not a general property of every GLM.
Aggregating over a model variable changes the model, and row-level
weights, interactions, offsets or non-additive quantities must be
retained correctly. Binomial, severity, quasi-likelihood and dispersion
analyses require their own sufficient totals and weights. Row-level
residuals and influence diagnostics are also no longer available after
aggregation, even when the fitted Poisson coefficients are unchanged.

In practice, a rating grid is particularly useful before estimating a
frequency model on a large portfolio. It can reduce repeated policy
records to a much smaller table, lowering memory use and fitting time.
Keep the unaggregated data when policy-level predictions, sampling,
validation or diagnostics are required, and verify on a representative
sample that the selected aggregation retains all inputs needed by the
intended model.

### Estimating a severity GLM on aggregated data

Claim count and claim amount are additive portfolio measures and can be
supplied through `aggregate_cols`. Frequency is then calculated as total
claim count divided by total exposure. Average severity is total claim
amount divided by total claim count.

A Gamma severity GLM fitted to grouped average severities can produce
the same coefficient estimates as a model fitted to the underlying
individual claims. This requires grouping by every predictor in the
severity model, calculating
`average_severity = claim_amount / claim_count`, using `claim_count` as
the model weight, and applying the same family and link. Within each
grid row all underlying claims then have the same linear predictor,
while the weight retains the number of claims represented by the
average.

This equivalence concerns the coefficient estimates, not the complete
model output. Aggregation removes claim-level residuals and outlier
information. Deviance, residual degrees of freedom, estimated
dispersion, standard errors and significance tests can therefore differ
from a claim-level fit. Claim- level data should remain available for
severity-distribution checks, influential-claim analysis and model
validation. The equivalence is also lost if a severity predictor varies
within a grid row or if the totals and weights do not represent the
underlying claims correctly.

If `exposure_by` is supplied, exposure or row counts are split across
levels of that variable and returned in wide format, for example
`"exposure_2020"` or `"count_2020"`.

For objects returned by
[`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md),
refinement mappings are joined by their original factor column. They are
not cross-joined onto every row.

Aggregation, reshaping and refinement joins are performed internally
with
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
to support large pricing portfolios. A local copy is used, so the
supplied object is not modified by reference. The output is a regular
`data.frame`, irrespective of the class of the input data.

When the row-level portfolio does not fit comfortably in R memory, use
[`rating_grid_db()`](https://mharinga.github.io/insurancerating/reference/rating_grid_db.md)
to perform the grouped reduction in a database and collect only the
resulting grid.

## See also

[`rating_grid_db()`](https://mharinga.github.io/insurancerating/reference/rating_grid_db.md),
[`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md),
[`merge_date_ranges_db()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges_db.md),
[`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md),
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)

## Author

Martin Haringa

## Examples

``` r
portfolio <- data.frame(
  policy_id = 1:10,
  sector = rep(c("Industry", "Retail"), each = 5),
  region = rep(c("North", "South"), 5),
  underwriting_year = rep(c(2024, 2025), each = 5),
  earned_exposure = c(1, 0.8, 1, 0.5, 1, 1, 0.7, 1, 0.9, 1),
  claim_count = c(0, 1, 2, 0, 1, 0, 1, 0, 2, 1),
  claim_amount = c(0, 2500, 18000, 0, 6000, 0, 4500, 0, 22000, 9000)
)

# Aggregate policy records into observed combinations of sector and region.
# The resulting exposure is the total earned exposure in each combination.
rating_grid(
  portfolio,
  group_by = c("sector", "region"),
  exposure = "earned_exposure"
)
#>     sector region earned_exposure
#> 1 Industry  North             3.0
#> 2 Industry  South             1.3
#> 3   Retail  North             1.6
#> 4   Retail  South             3.0

# Split earned exposure by underwriting year. This is useful when reviewing
# whether the portfolio mix within each rating combination changes over time.
rating_grid(
  portfolio,
  group_by = c("sector", "region"),
  exposure = "earned_exposure",
  exposure_by = "underwriting_year"
)
#>     sector region earned_exposure_2024 earned_exposure_2025
#> 1 Industry  North                  3.0                   NA
#> 2 Industry  South                  1.3                   NA
#> 3   Retail  North                   NA                  1.6
#> 4   Retail  South                   NA                  3.0

# Claim count and claim amount remain additive totals in the rating grid.
# Frequency and average severity can subsequently be derived from them.
claims_grid <- rating_grid(
  portfolio,
  group_by = c("sector", "region"),
  exposure = "earned_exposure",
  aggregate_cols = c("claim_count", "claim_amount")
)

claims_grid$frequency <-
  claims_grid$claim_count / claims_grid$earned_exposure
claims_grid$average_severity <- ifelse(
  claims_grid$claim_count > 0,
  claims_grid$claim_amount / claims_grid$claim_count,
  NA_real_
)
claims_grid
#>     sector region claim_count claim_amount earned_exposure frequency
#> 1 Industry  North           3        24000             3.0 1.0000000
#> 2 Industry  South           1         2500             1.3 0.7692308
#> 3   Retail  North           3        26500             1.6 1.8750000
#> 4   Retail  South           1         9000             3.0 0.3333333
#>   average_severity
#> 1         8000.000
#> 2         2500.000
#> 3         8833.333
#> 4         9000.000

# Fit a severity model to grouped average claim amounts. Grid rows without
# claims are excluded because average severity is undefined for those rows.
severity_model_grid <- glm(
  average_severity ~ sector + region,
  weights = claim_count,
  family = Gamma(link = "log"),
  data = subset(claims_grid, claim_count > 0)
)
coef(severity_model_grid)
#>  (Intercept) sectorRetail  regionSouth 
#>    8.8557334    0.3819535   -0.4844045 

# For a fitted GLM, extract_model_data() retains the model variables and
# exposure information required to construct the observed rating grid.
mtpl_portfolio <- MTPL
mtpl_portfolio$zip <- factor(mtpl_portfolio$zip)

frequency_model <- glm(
  nclaims ~ bm + zip + offset(log(exposure)),
  family = poisson(link = "log"),
  data = mtpl_portfolio
)

frequency_model |>
  extract_model_data() |>
  rating_grid()
#>    bm zip count     exposure
#> 1   1   0    88 7.426301e+01
#> 2   1   1  4760 4.208589e+03
#> 3   1   2  3291 2.943959e+03
#> 4   1   3  3213 2.841123e+03
#> 5   2   0    35 3.186027e+01
#> 6   2   1  2083 1.835279e+03
#> 7   2   2  1422 1.270674e+03
#> 8   2   3  1436 1.281537e+03
#> 9   3   0    17 1.365753e+01
#> 10  3   1   695 6.139808e+02
#> 11  3   2   478 4.148548e+02
#> 12  3   3   541 4.764192e+02
#> 13  4   0     7 6.208219e+00
#> 14  4   1   320 2.809178e+02
#> 15  4   2   205 1.800904e+02
#> 16  4   3   206 1.834274e+02
#> 17  5   0    13 1.267945e+01
#> 18  5   1   615 5.496986e+02
#> 19  5   2   445 3.963425e+02
#> 20  5   3   430 3.875151e+02
#> 21  6   0    26 2.312877e+01
#> 22  6   1   933 8.312466e+02
#> 23  6   2   667 5.949534e+02
#> 24  6   3   599 5.386904e+02
#> 25  7   0     2 2.000000e+00
#> 26  7   1   407 3.587753e+02
#> 27  7   2   288 2.563014e+02
#> 28  7   3   264 2.335315e+02
#> 29  8   0     8 6.260274e+00
#> 30  8   1   424 3.791479e+02
#> 31  8   2   276 2.491890e+02
#> 32  8   3   256 2.344466e+02
#> 33  9   0     8 5.764384e+00
#> 34  9   1   345 3.112082e+02
#> 35  9   2   261 2.366575e+02
#> 36  9   3   239 2.166712e+02
#> 37 10   0     9 7.926027e+00
#> 38 10   1   508 4.514219e+02
#> 39 10   2   298 2.709918e+02
#> 40 10   3   314 2.741397e+02
#> 41 11   0     7 7.000000e+00
#> 42 11   1   510 4.450082e+02
#> 43 11   2   386 3.522411e+02
#> 44 11   3   351 3.108137e+02
#> 45 12   0    13 1.166027e+01
#> 46 12   1   600 5.301863e+02
#> 47 12   2   462 4.113178e+02
#> 48 12   3   453 4.029890e+02
#> 49 13   0     1 1.000000e+00
#> 50 13   1    86 7.600000e+01
#> 51 13   2    69 6.207397e+01
#> 52 13   3    60 5.586027e+01
#> 53 14   0     1 5.479452e-02
#> 54 14   1    54 4.708767e+01
#> 55 14   2    53 4.833973e+01
#> 56 14   3    55 4.981096e+01
#> 57 15   0     1 6.301370e-02
#> 58 15   1    95 8.560000e+01
#> 59 15   2    50 4.660822e+01
#> 60 15   3    51 4.481370e+01
#> 61 16   0     4 2.317808e+00
#> 62 16   1    36 3.359452e+01
#> 63 16   2    28 2.256986e+01
#> 64 16   3    33 2.939726e+01
#> 65 17   1    15 1.374247e+01
#> 66 17   2     8 5.693151e+00
#> 67 17   3     6 5.739726e+00
#> 68 18   1     8 5.865753e+00
#> 69 18   2     6 5.093151e+00
#> 70 18   3     6 5.586301e+00
#> 71 19   1     9 8.550685e+00
#> 72 19   2     7 6.843836e+00
#> 73 19   3     8 7.449315e+00
#> 74 20   1     8 6.693151e+00
#> 75 20   2     5 5.000000e+00
#> 76 20   3     6 4.602740e+00
#> 77 21   0     1 1.000000e+00
#> 78 21   1     4 3.068493e+00
#> 79 21   2     1 1.000000e+00
#> 80 21   3     3 3.000000e+00
#> 81 22   1     3 3.000000e+00
#> 82 22   2     1 5.315068e-01
#> 83 23   1     2 1.964384e+00
#> 84 23   2     2 1.304110e+00

# For this Poisson frequency model, fitting on the corresponding aggregated
# grid gives the same coefficient estimates as fitting on the policy rows.
frequency_grid <- rating_grid(
  mtpl_portfolio,
  group_by = c("bm", "zip"),
  exposure = "exposure",
  aggregate_cols = "nclaims"
)

frequency_model_grid <- glm(
  nclaims ~ bm + zip + offset(log(exposure)),
  family = poisson(link = "log"),
  data = frequency_grid
)

isTRUE(all.equal(
  unname(coef(frequency_model)),
  unname(coef(frequency_model_grid)),
  tolerance = 1e-8
))
#> [1] TRUE
```
