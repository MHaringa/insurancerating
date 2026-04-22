# Include reference group in regression output

**\[deprecated\]**

Legacy interface. Prefer
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
for fitted models in the new workflow, but this function remains
available.

## Usage

``` r
rating_factors2(
  model,
  model_data = NULL,
  exposure = TRUE,
  exposure_name = NULL,
  colname = "estimate",
  exponentiate = TRUE,
  round_exposure = 0
)
```

## Arguments

- model:

  glm object produced by [`glm()`](https://rdrr.io/r/stats/glm.html)

- model_data:

  Optional data.frame used to create glm object. If `NULL`, the function
  tries to use `model$data`.

- exposure:

  Logical or character. If `TRUE` (default), exposure is added if it can
  be inferred from the model. If `FALSE`, no exposure is added. If a
  character string is supplied, it is interpreted as the exposure column
  name.

- exposure_name:

  Optional name for the exposure column in the output.

- colname:

  name of coefficient column

- exponentiate:

  logical indicating whether or not to exponentiate the coefficient
  estimates. Defaults to TRUE.

- round_exposure:

  number of digits for exposure (defaults to 0)
