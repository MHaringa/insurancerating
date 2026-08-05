# Add sublevel relativities to a refinement workflow

Divide one or more levels of an existing GLM risk factor into more
detailed tariff levels using supplied relativities. This can be
appropriate when the GLM is estimated on a coarser factor for
statistical stability, while a documented actuarial segmentation is
required within sufficiently homogeneous model levels.

## Usage

``` r
add_relativities(
  model,
  model_variable,
  split_variable,
  relativities,
  exposure,
  normalize = TRUE,
  output_variable = paste0(model_variable, "_refined")
)
```

## Arguments

- model:

  Object of class `rating_refinement`, created with
  [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md).
  A fitted GLM, including a model returned by
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
  is not accepted directly; retain and modify the corresponding
  refinement specification instead.

- model_variable:

  Character string. Existing variable in the GLM, or a restricted
  version created by an earlier
  [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  step. Levels of the underlying model variable can be split into more
  detailed tariff segments. When an earlier restriction exists, its
  coefficients are used automatically.

- split_variable:

  Character string. More granular portfolio variable that defines the
  detailed groups inside `model_variable`.

- relativities:

  Named list of data frames, usually created with
  [`relativities()`](https://mharinga.github.io/insurancerating/reference/relativity_specification.md)
  and
  [`split_level()`](https://mharinga.github.io/insurancerating/reference/relativity_specification.md).

- exposure:

  Character string. Exposure column used for weighting and, when
  requested, normalisation.

- normalize:

  Logical. If `TRUE`, normalise the supplied relativities by exposure
  within each split model level.

- output_variable:

  Character string naming the resulting hybrid tariff factor. The
  default appends `_refined` to `model_variable`. A more
  application-specific name, such as `sbi_tariff_segment`, can make the
  intended tariff use clearer in model output and reporting. The name
  must not overwrite an existing column in the refinement data.

## Value

A `rating_refinement` object containing the stored relativity
specification. The pricing GLM is not fitted again until
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

## Details

`add_relativities()` stores a relativity step on a `rating_refinement`
object. It does not alter the fitted GLM immediately. The split is
evaluated in the recorded step order and applied when
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

`model_variable` is the variable already used in the GLM.
`split_variable` is the more detailed variable in the portfolio data
that will be used to split one or more levels of `model_variable`. The
`relativities` argument should be a named list describing those splits,
usually built with
[`relativities()`](https://mharinga.github.io/insurancerating/reference/relativity_specification.md)
and
[`split_level()`](https://mharinga.github.io/insurancerating/reference/relativity_specification.md).
`output_variable` names the resulting hybrid tariff factor: levels
included in `relativities` are represented by their detailed
`split_variable` level, while all other levels retain their
`model_variable` level.

Levels of `model_variable` that are not included in `relativities`
retain their existing model coefficient. In
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md),
exposure for these retained levels is aggregated from `model_variable`,
while exposure for the newly split levels is aggregated from
`split_variable` within the specified parent model level. Omitting a
model level from `relativities` therefore means that the level remains
unsplit; it is not treated as an incomplete specification.

`add_relativities()` validates the supplied sublevel names against the
observed values of `split_variable` before storing the refinement step.
A misspelled or incorrectly spaced category or sublevel therefore
produces an immediate error, with a suggestion when a closely matching
observed value is available. It also verifies that each sublevel occurs
within its specified parent category of `model_variable`.

When `normalize = TRUE`, the supplied relativities are normalised using
exposure so that their exposure-weighted mean equals one within the
split model level. They then redistribute the existing model coefficient
across the sublevels without changing its exposure-weighted average.
With `normalize = FALSE`, the supplied relativities are applied
directly.

### Step order and restrictions

If `model_variable` was restricted in an earlier
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
step, the restricted coefficients are automatically used as the basis
for the derived relativities. The user can continue to supply the
original model variable; no additional argument is needed. Supplying the
restricted variable explicitly gives the same coefficient basis and does
not apply the restriction a second time. Refinement steps are
order-dependent, so a restriction added after `add_relativities()` does
not affect an earlier relativity step. Once the restricted coefficients
have been used to derive the final split,
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
reports `output_variable` as the tariff factor and does not also show
the intermediate restricted variable.

Conversely,
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
can be called after `add_relativities()` to adjust selected levels of
the derived `output_variable`. The output variable is then recognised as
an existing refinement factor; users do not need to set
`allow_new_risk_factors = TRUE`. Levels omitted from the restriction
table are fixed at the relativities calculated by this step.

### Appropriate use

`add_relativities()` is intended for refinement within an already
reasonably homogeneous GLM segment. It redistributes an existing
coefficient across sublevels using exposure-weighted relativities, while
preserving the overall level of the original coefficient when
normalisation is used. Appropriate applications include mild residual
heterogeneity, monotonic tariff differentiation and expert-based
segmentation within a stable risk group where the original GLM
coefficient remains broadly representative.

### Limitations

The method is not a substitute for creating a separate risk segment when
the original GLM coefficient is itself distorted. For example, suppose a
broad industry segment contains many relatively stable businesses, but a
few chemical companies drive most of the losses while representing
little exposure. The fitted industry coefficient may then be dominated
by the chemical companies' experience. Applying exposure-weighted
relativities inside that segment may barely reduce the coefficient for
the large exposure group, because the original coefficient is already
pulled upward by the outlier subgroup.

In that situation it is often better to create a separate GLM factor
level, derive a separate tariff segment, or apply explicit segmentation
or acceptation rules, instead of relying only on `add_relativities()`.

## See also

[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md),
[`relativities()`](https://mharinga.github.io/insurancerating/reference/relativity_specification.md),
[`split_level()`](https://mharinga.github.io/insurancerating/reference/relativity_specification.md),
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)

## Author

Martin Haringa

## Examples

``` r
portfolio <- data.frame(
  claims = c(1, 2, 1, 3, 2, 4),
  exposure = rep(1, 6),
  construction = factor(c("residential", "commercial", "residential",
                          "commercial", "residential", "commercial")),
  construction_detail = factor(c("flat", "shop", "house",
                                 "office", "flat", "shop"))
)

model <- glm(
  claims ~ construction + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)

relativities <- relativities(
  split_level(
    "residential",
    new_levels = c("flat", "house"),
    relativities = c(0.95, 1.05)
  ),
  split_level(
    "commercial",
    new_levels = c("shop", "office"),
    relativities = c(1.10, 0.90)
  )
)

refined <- prepare_refinement(model, data = portfolio) |>
  add_relativities(
    model_variable = "construction",
    split_variable = "construction_detail",
    output_variable = "construction_tariff_segment",
    relativities = relativities,
    exposure = "exposure"
  )

# A subsequent restriction can revise one derived level. The remaining
# tariff-segment levels are fixed at the relativities calculated above.
refined <- refined |>
  add_restriction(data.frame(
    construction_tariff_segment = "flat",
    construction_tariff_segment_restricted = 1.00
  ))

refined_model <- refit(refined)
rating_table(refined_model, exposure = FALSE)
#>                              risk_factor       level est_refined_model
#> 1                            (Intercept) (Intercept)         2.3746130
#> 2 construction_tariff_segment_restricted        shop         1.0645161
#> 3 construction_tariff_segment_restricted      office         0.8709677
#> 4 construction_tariff_segment_restricted        flat         1.0000000
#> 5 construction_tariff_segment_restricted       house         0.4745763
```
