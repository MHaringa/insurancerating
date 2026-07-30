# Prepare a model refinement workflow

Start a refinement workflow for a fitted GLM. Refinement steps such as
smoothing, restrictions and expert-based relativities can be added
sequentially and are only applied once
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

Object of class `rating_refinement`. Retain this object when refinement
steps may need to be reviewed or edited after fitting.

## Details

`prepare_refinement()` creates a persistent refinement specification.
This object contains the original GLM, the corresponding model data and
the ordered smoothing, restriction and relativity steps. It is the
object that should be retained and edited during actuarial review.

[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
applies the stored specification and returns a fitted GLM for model
diagnostics, prediction and tariff reporting. The returned GLM is a
result, not an editable refinement specification. Functions such as
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md),
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
and
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
therefore accept a `rating_refinement` object and do not accept an
ordinary or refitted GLM directly.

A practical iterative workflow keeps both objects:


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

[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md),
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md),
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
