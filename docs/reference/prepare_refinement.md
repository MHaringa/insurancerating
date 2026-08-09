# Prepare a model refinement workflow

Create an editable refinement specification from a fitted pricing GLM.
Smoothing, coefficient restrictions, shrinkage, rebasing and sublevel
relativities can then be added in a defined order. These steps do not
alter the fitted GLM until
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

## Usage

``` r
prepare_refinement(model, data = NULL)
```

## Arguments

- model:

  Object of class `glm`.

- data:

  Optional data.frame containing exactly the observations retained in
  the fitted GLM and all required model variables. If model fitting
  omitted rows because of missing values, supply the retained model data
  rather than the original unfiltered data. If `NULL`, the data are
  retrieved from the model object.

## Value

A `rating_refinement` object containing the original GLM, retained model
data and ordered refinement specification. No GLM is fitted again until
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

## Details

`prepare_refinement()` creates a persistent refinement specification.
This object contains the original GLM, the corresponding model data and
the ordered smoothing, restriction, shrinkage, rebasing and relativity
steps. Retain this object during actuarial review so that assumptions
can be inspected, revised and applied again in the same order.

### Actuarial interpretation

Preparing a refinement does not change coefficients, fitted values or
the tariff structure. It separates the original statistical model from
subsequent actuarial adjustments. Each adjustment remains an explicit
step rather than being embedded directly in transformed data or
overwritten model coefficients. This supports comparison between the
unrestricted model and alternative refinement specifications.

[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
applies the stored specification and returns a fitted GLM for model
diagnostics, prediction and tariff reporting. The returned GLM is a
result, not an editable refinement specification. Functions such as
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md),
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
[`add_shrinkage()`](https://mharinga.github.io/insurancerating/reference/add_shrinkage.md),
[`add_rebasing()`](https://mharinga.github.io/insurancerating/reference/add_rebasing.md)
and
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
therefore accept a `rating_refinement` object and do not accept an
ordinary or refitted GLM directly.

A practical iterative workflow therefore keeps both objects:


    refinement <- prepare_refinement(model) |>
      add_smoothing(...)

    fitted_model <- refit(refinement)

    refinement <- refinement |>
      edit_smoothing(...)

    fitted_model <- refit(refinement)

`prepare_refinement()` is normally required only once for such an
iteration. Calling it on a model returned by
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
deliberately starts a new refinement workflow with the already refined
model as its baseline; it does not recover the earlier smoothing or
restriction steps for further editing.

## See also

[`summary.rating_refinement()`](https://mharinga.github.io/insurancerating/reference/summary.rating_refinement.md),
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md),
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
[`add_shrinkage()`](https://mharinga.github.io/insurancerating/reference/add_shrinkage.md),
[`add_rebasing()`](https://mharinga.github.io/insurancerating/reference/add_rebasing.md),
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md),
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)

## Author

Martin Haringa

## Examples

``` r
portfolio <- data.frame(
  claims = c(1, 2, 1, 3, 2, 4),
  exposure = rep(1, 6),
  risk_class = factor(c("A", "B", "A", "B", "A", "B"))
)

model <- glm(
  claims ~ risk_class + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)

refinement <- prepare_refinement(model, data = portfolio) |>
  add_restriction(data.frame(
    risk_class = "B",
    risk_class_restricted = 1.15
  ))

summary(refinement)
#> Refinement specification
#> 
#> Package: insurancerating 0.8.1.9000
#> Created: 2026-08-09 14:24:08 CEST
#> Observations: 6
#> Family: poisson (log link)
#> Base formula:
#>   claims ~ risk_class + offset(log(exposure))
#> Offset: log(exposure)
#> 
#> Refinement steps: 1
#>   1. Restriction: risk_class -> risk_class_restricted (2 levels)
#>      A = 1.00; B = 1.15

fitted_model <- refit(refinement)

# Retain and revise the specification rather than editing fitted_model.
refinement <- refinement |>
  add_restriction(data.frame(
    risk_class = "B",
    risk_class_restricted = 1.10
  ))
#> Updated existing restriction for `risk_class = "B"`: 1.15 -> 1.1

updated_model <- refit(refinement)
```
