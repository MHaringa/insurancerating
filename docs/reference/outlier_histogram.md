# Portfolio histogram with tail bins

Visualize the distribution of a numeric portfolio variable while keeping
extreme tails readable.

Insurance portfolios often contain skewed variables such as claim
amounts, premium, exposure, insured sums, deductibles, or fitted
premiums. A few very large policies or claim events can stretch a
regular histogram so much that the body of the portfolio becomes hard to
inspect. `outlier_histogram()` keeps the main range visible and groups
values below `left` or above `right` into dedicated tail bins.

The plot is useful for actuarial portfolio checks, data quality review,
and model preparation: it helps show where most risks are concentrated
while still making the presence of extreme observations explicit.

## Usage

``` r
outlier_histogram(
  data,
  x,
  left = NULL,
  right = NULL,
  line = FALSE,
  bins = 30,
  fill = "steelblue",
  color = "white",
  fill_outliers = "#a7d1a7"
)
```

## Arguments

- data:

  A data.frame containing the portfolio variable to inspect.

- x:

  Character; numeric column in `data` to plot.

- left:

  Optional numeric lower threshold. Values below this threshold are
  grouped into one left-tail bin.

- right:

  Optional numeric upper threshold. Values above this threshold are
  grouped into one right-tail bin.

- line:

  Logical. If `TRUE`, add a density line. Default = `FALSE`.

- bins:

  Integer. Number of bins used for the displayed range. Default = 30.

- fill:

  Fill color for regular histogram bars.

- color:

  Border color for histogram bars.

- fill_outliers:

  Fill color for tail bins. Default = `"#a7d1a7"`.

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Details

This function is intended as an exploratory portfolio diagnostic. It
does not remove or winsorize observations in `data`; it only groups tail
values in the visual display. The labels on the tail bins show the
original range captured by each tail bin.

The method for handling outlier bins is based on
<https://edwinth.github.io/blog/outlier-bin/>.

## Author

Martin Haringa

## Examples

``` r
# Inspect the full premium distribution
outlier_histogram(MTPL2, "premium")


# Keep the portfolio body readable while showing both tails
outlier_histogram(MTPL2, "premium", left = 30, right = 120, bins = 30)

```
