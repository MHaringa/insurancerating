# Add expert-based relativities to a refinement workflow

Splits an existing model variable into more detailed tariff groups using
supplied relativities. This is useful when the GLM is fitted on a
coarser rating factor for credibility or stability, but the final tariff
needs a more detailed split that is based on portfolio exposure, expert
judgement or externally agreed relativities.

## Usage

``` r
add_relativities(
  model,
  model_variable,
  split_variable,
  relativities,
  exposure,
  normalize = TRUE
)
```

## Arguments

- model:

  Object of class `rating_refinement`, usually created with
  [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md).

- model_variable:

  Character string. Existing variable in the GLM. Levels of this
  variable can be split into more detailed tariff groups.

- split_variable:

  Character string. More granular portfolio variable that defines the
  detailed groups inside `model_variable`.

- relativities:

  Named list of data frames, usually created with
  [`relativities_list()`](https://mharinga.github.io/insurancerating/reference/relativities_list.md)
  and
  [`split_level()`](https://mharinga.github.io/insurancerating/reference/split_level.md).

- exposure:

  Character string. Exposure column used for weighting and, when
  requested, normalisation.

- normalize:

  Logical. If `TRUE`, normalise the supplied relativities by exposure
  within each split model level.

## Value

Object of class `rating_refinement`.

## Details

`model_variable` is the variable already used in the GLM.
`split_variable` is the more detailed variable in the portfolio data
that will be used to split one or more levels of `model_variable`. The
`relativities` argument should be a named list describing those splits,
usually built with
[`relativities_list()`](https://mharinga.github.io/insurancerating/reference/relativities_list.md)
and
[`split_level()`](https://mharinga.github.io/insurancerating/reference/split_level.md).

The step is stored on the `rating_refinement` object and is applied when
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called. When `normalize = TRUE`, the supplied relativities are
normalised using exposure so that the refined split keeps the original
level effect on average. This helps prevent an expert split from
unintentionally changing the total premium level for the original model
group.

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

relativities <- relativities_list(
  split_level(
    "residential",
    new_levels = c("flat", "house"),
    relativities = c(0.95, 1.05)
  )
)

refined <- prepare_refinement(model, data = portfolio) |>
  add_relativities(
    model_variable = "construction",
    split_variable = "construction_detail",
    relativities = relativities,
    exposure = "exposure"
  )
```
