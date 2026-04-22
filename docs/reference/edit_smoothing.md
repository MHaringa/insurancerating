# Edit an existing smoothing step in a refinement workflow

`edit_smoothing()` modifies a previously added smoothing step in a
`rating_refinement` object. The actual smoothing is only re-applied when
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
is called.

## Usage

``` r
edit_smoothing(
  model,
  variable = NULL,
  step = NULL,
  x1,
  x2,
  overwrite_y1 = NULL,
  overwrite_y2 = NULL,
  knots_x = NULL,
  knots_y = NULL,
  allow_extrapolation = FALSE,
  extrapolation_break_size = NULL
)
```

## Arguments

- model:

  Object of class `rating_refinement`, `smooth` or `restricted`.

- variable:

  Character. The `x_cut` variable of the smoothing step to edit.

- step:

  Optional numeric index of the step to edit.

- x1, x2:

  Numeric. Start and end of the interval over which the smoothing should
  be modified.

- overwrite_y1, overwrite_y2:

  Optional numeric. Overrides for the smoothed values at `x1` and `x2`.

- knots_x, knots_y:

  Optional numeric vectors of equal length.

- allow_extrapolation:

  Logical.

- extrapolation_break_size:

  Numeric scalar (\> 0) or `NULL`.

## Value

Object of class `rating_refinement`.
