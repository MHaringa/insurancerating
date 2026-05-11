# Automatically create a ggplot for objects obtained from factor analysis

Takes an object produced by
[`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
or
[`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md)
(deprecated NSE interface) and plots the available statistics.

## Usage

``` r
# S3 method for class 'factor_analysis'
autoplot(
  object,
  metrics = NULL,
  ncol = 1,
  show_exposure = TRUE,
  show_exposure_labels = TRUE,
  sort_by_exposure = FALSE,
  level_order = NULL,
  decimal_mark = ",",
  line_color = NULL,
  bar_fill = NULL,
  label_width = 50,
  flip_bars = FALSE,
  show_total = FALSE,
  total_color = NULL,
  total_name = NULL,
  rotate_angle = NULL,
  custom_theme = NULL,
  remove_underscores = FALSE,
  compact_x_axis = TRUE,
  show_plots = NULL,
  background = NULL,
  labels = NULL,
  sort = NULL,
  sort_manual = NULL,
  dec.mark = NULL,
  color = NULL,
  color_bg = NULL,
  coord_flip = NULL,
  remove_x_elements = NULL,
  ...
)
```

## Arguments

- object:

  A `factor_analysis` or `univariate` object produced by
  [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  or
  [`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md).

- metrics:

  Numeric or character vector specifying which metrics to plot (default
  is all available metrics). The numeric positions are:

  - 1\. Frequency (`nclaims / exposure`)

  - 2\. Average severity (`severity / nclaims`)

  - 3\. Risk premium (`severity / exposure`)

  - 4\. Loss ratio (`severity / premium`)

  - 5\. Average premium (`premium / exposure`)

  - 6\. Exposure

  - 7\. Severity

  - 8\. Number of claims

  - 9\. Premium

  Character values can be `"frequency"`, `"average_severity"`,
  `"risk_premium"`, `"loss_ratio"`, `"average_premium"`, `"exposure"`,
  `"claim_amount"`, `"claim_count"`, and `"premium"`.

- ncol:

  Number of columns in output (default = 1).

- show_exposure:

  Show exposure as background bars behind line plots (default = TRUE).

- show_exposure_labels:

  Show labels with the exposure bars (default = TRUE).

- sort_by_exposure:

  Sort risk factor levels into descending order by exposure (default =
  FALSE).

- level_order:

  Custom order for risk factor levels; character vector (default =
  NULL).

- decimal_mark:

  Decimal mark; defaults to `","`.

- line_color:

  Optional override for line/point color. If NULL (default), colors are
  taken from the internal palette. If specified, the chosen color is
  applied to all line-based plots.

- bar_fill:

  Optional override for background bar color. If NULL (default), the
  background color is taken from the internal palette. If specified, the
  chosen color is applied to all background bars.

- label_width:

  Width of labels on the x-axis (default = 10).

- flip_bars:

  Logical. If `TRUE`, flip cartesian coordinates for bar plots (metrics
  6 to 9). This option does not affect the line-based plots for metrics
  1 to 5.

- show_total:

  Show line for total if `by` is used (default = FALSE).

- total_color:

  Color for total line (default = `"black"`).

- total_name:

  Legend name for total line (default = NULL).

- rotate_angle:

  Numeric value for angle of labels on the x-axis (degrees).

- custom_theme:

  List with customized theme options.

- remove_underscores:

  Logical; remove underscores from labels (default = FALSE).

- compact_x_axis:

  Logical. When `TRUE` and `ncol == 1`, x-axis components are removed
  from all plots except the last one. The following elements are
  suppressed:

  - `axis.title.x`

  - `axis.text.x`

  - `axis.ticks.x`

  This prevents duplicated x-axes in vertically stacked patchwork plots.
  Defaults to `TRUE`.

- show_plots:

  Deprecated. Use `metrics` instead.

- background:

  Deprecated alias for `show_exposure`.

- labels:

  Deprecated alias for `show_exposure_labels`.

- sort:

  Deprecated alias for `sort_by_exposure`.

- sort_manual:

  Deprecated alias for `level_order`.

- dec.mark:

  Deprecated alias for `decimal_mark`.

- color:

  Deprecated alias for `line_color`.

- color_bg:

  Deprecated alias for `bar_fill`.

- coord_flip:

  Deprecated alias for `flip_bars`.

- remove_x_elements:

  Deprecated alias for `compact_x_axis`.

- ...:

  Other plotting parameters.

## Value

A `ggplot2` object.

## Author

Marc Haine, Martin Haringa

## Examples

``` r
## --- New usage (SE, recommended) ---
x <- factor_analysis(MTPL2,
                     x = "area",
                     severity = "amount",
                     nclaims = "nclaims",
                     exposure = "exposure")
#> Warning: The `x` argument of `factor_analysis()` is deprecated as of insurancerating
#> 0.9.0.
#> ℹ Please use the `risk_factors` argument instead.
#> ℹ The deprecated feature was likely used in the insurancerating package.
#>   Please report the issue at
#>   <https://github.com/MHaringa/insurancerating/issues>.
#> Warning: The `severity` argument of `factor_analysis()` is deprecated as of
#> insurancerating 0.9.0.
#> ℹ Please use the `claim_amount` argument instead.
#> ℹ The deprecated feature was likely used in the insurancerating package.
#>   Please report the issue at
#>   <https://github.com/MHaringa/insurancerating/issues>.
#> Warning: The `nclaims` argument of `factor_analysis()` is deprecated as of
#> insurancerating 0.9.0.
#> ℹ Please use the `claim_count` argument instead.
#> ℹ The deprecated feature was likely used in the insurancerating package.
#>   Please report the issue at
#>   <https://github.com/MHaringa/insurancerating/issues>.
autoplot(x)
#> Ignoring plots 4, 5, 9: required column(s) not available in object


## --- Deprecated usage (NSE) ---
x_old <- univariate(MTPL2, x = area, severity = amount,
                    nclaims = nclaims, exposure = exposure)
#> Warning: `univariate()` was deprecated in insurancerating 0.8.0.
#> ℹ Please use `factor_analysis()` instead.
#> Warning: The `df` argument of `factor_analysis()` is deprecated as of insurancerating
#> 0.9.0.
#> ℹ Please use the `data` argument instead.
#> ℹ The deprecated feature was likely used in the insurancerating package.
#>   Please report the issue at
#>   <https://github.com/MHaringa/insurancerating/issues>.
autoplot(x_old)
#> Ignoring plots 4, 5, 9: required column(s) not available in object

```
