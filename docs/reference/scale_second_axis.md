# Scale secondary axis for background plotting

Internal helper to rescale a secondary variable (`s_axis`) so it aligns
with the scale of a first variable (`f_axis`). Adds two new columns to
the data frame: `s_axis_scale` and `s_axis_print`.

## Usage

``` r
scale_second_axis(background, df, dfby, f_axis, s_axis, by)
```

## Value

The input data frame with two additional columns:

- `s_axis_scale` – scaled version of `s_axis`

- `s_axis_print` – rounded version of `s_axis`
