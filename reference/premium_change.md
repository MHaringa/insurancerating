# Interpret the premium effect of a smoothing curve

Translate an effective smoothing curve in a refinement specification
into the modelled premium change when its continuous source variable
doubles. For a multiplicative relativity curve \\R(x)\\, the reported
change is \\R(2x) / R(x) - 1\\.

Format an object returned by `premium_change()`. One refinement state is
shown as a three-column table. Multiple states are shown side by side;
when exactly two are selected, their difference is added in percentage
points.

## Usage

``` r
premium_change(
  x,
  variable = NULL,
  at = NULL,
  steps = "current",
  basis = c("curve", "segments"),
  ...
)

# S3 method for class 'premium_change'
as_gt(x, locale = "en-US", decimals = 1, title = NULL, subtitle = NULL, ...)
```

## Arguments

- x:

  For `premium_change()`, a `rating_refinement` object containing at
  least one smoothing step. For
  [`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md),
  an object returned by `premium_change()`.

- variable:

  Optional character string identifying the smoothed model variable or
  its continuous source variable. This may be omitted when the
  refinement contains exactly one smoothing lineage.

- at:

  Optional positive numeric vector of starting values. Each value and
  its doubled value must lie inside the supported smoothing range of
  every selected refinement state. If `NULL`, six representative values
  are selected automatically.

- steps:

  Refinement states to evaluate. Use `"current"` for the latest state,
  `"all"` for every state from the selected smoothing onwards, or a
  numeric vector such as `c(1, 6)` for stored refinement positions.

- basis:

  Character string determining the interpretation basis. `"curve"`, the
  default, evaluates the continuous effective smoothing at the exact
  values. `"segments"` compares the effective relativities of the tariff
  intervals containing those values.

- ...:

  Additional arguments are not accepted.

- locale:

  Character string passed to `gt` for numeric formatting.

- decimals:

  Non-negative integer. Number of decimal places for changes.

- title:

  Optional table title. The source-variable name is used by default.

- subtitle:

  Optional table subtitle.

## Value

A tibble with class `premium_change` in long format, containing the
variable, refinement state, starting and doubled values, evaluated
relativities, and premium change as a decimal.

A `gt_tbl` object.

## Details

`premium_change()` is an interpretation helper for smoothing created
with
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
and subsequently modified with
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md).
It is not a smoothing method and does not change the refinement
specification.

The effective curve is reconstructed from the stored refinement history.
Consequently, `steps = "current"` reflects all smoothing edits recorded
up to the current state. Numeric step identifiers refer to positions in
the complete refinement sequence. If another type of refinement occurs
after a smoothing step, the previously effective smoothing is carried
forward.

With the default `basis = "curve"`, evaluation uses the continuous
effective smoothing line retained by the refinement system. It therefore
describes the shape and steepness of the estimated or edited curve at
exactly \\x\\ and \\2x\\; it does not use neighbouring tariff-segment
relativities.

With `basis = "segments"`, both values are assigned to the effective
tariff intervals created by the smoothing. Their current segment
relativities are compared. This describes the premium effect of the
implementable segmented tariff. The result can be zero when both values
fall in the same segment and can change discretely when doubling crosses
a segment boundary.

Values are never extrapolated. When `at = NULL`, six representative
starting values are selected from the common range for which both \\x\\
and \\2x\\ are supported in every selected refinement state.

Multiplying an entire curve by a common rebasing constant does not alter
the result because that constant cancels in \\R(2x) / R(x)\\.

## See also

[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md),
[`autoplot.rating_refinement()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_refinement.md),
[`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)

## Examples

``` r
age <- rep(seq(20, 70, by = 5), each = 5)
portfolio <- data.frame(
  claims = rep(c(0, 1, 0, 2, 1), length(age) / 5),
  exposure = 1,
  age = age
)
portfolio$age_band <- cut(
  portfolio$age,
  breaks = c(15, 30, 45, 60, 75),
  include.lowest = TRUE
)
model <- glm(
  claims ~ age_band + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)
refinement <- prepare_refinement(model, data = portfolio) |>
  add_smoothing(
    model_variable = "age_band",
    source_variable = "age",
    breaks = seq(15, 75, by = 5),
    smoothing = "poly",
    degree = 2,
    weights = "exposure"
  )
premium_change(refinement, at = c(20, 25, 30))
#> Premium change for age
#> 
#> Basis: Effective smoothing curve
#> 
#>  From To Premium change
#>    20 40          -0.0%
#>    25 50          -0.0%
#>    30 60           0.0%
premium_change(refinement, at = c(20, 25, 30), basis = "segments")
#> Premium change for age
#> 
#> Basis: Tariff segments
#> 
#>  From To Premium change
#>    20 40          -0.0%
#>    25 50          -0.0%
#>    30 60           0.0%

edited <- refinement |>
  edit_smoothing(
    model_variable = "age_band",
    from = 20,
    to = 60,
    adjustment = 1.05,
    transition = "linear"
  )
premium_change(edited, at = c(20, 25, 30), steps = c(1, 2))
#> Premium change for age
#> 
#> Basis: Effective smoothing curve
#> 
#>  From To Step 1 Step 2 Difference
#>    20 40  -0.0%  +4.4%    +4.4 pp
#>    25 50  -0.0%  +0.8%    +0.8 pp
#>    30 60   0.0%  -2.8%    -2.8 pp
```
