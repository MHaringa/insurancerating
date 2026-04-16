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
  exposure = NULL,
  colname = "estimate",
  exponentiate = TRUE,
  round_exposure = 0
)
```

## Arguments

- model:

  glm object produced by [`glm()`](https://rdrr.io/r/stats/glm.html)

- model_data:

  data.frame used to create glm object

- exposure:

  column in `model_data` with exposure

- colname:

  name of coefficient column

- exponentiate:

  logical indicating whether or not to exponentiate the coefficient
  estimates. Defaults to TRUE.

- round_exposure:

  number of digits for exposure (defaults to 0)
