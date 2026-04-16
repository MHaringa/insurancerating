# Root Mean Squared Error (RMSE)

Computes the root mean squared error (RMSE) for a fitted model, defined
as the square root of the mean of squared differences between
predictions and observed values.

## Usage

``` r
rmse(object, data = NULL)
```

## Arguments

- object:

  A fitted model object (e.g. of class `"glm"`).

- data:

  A data frame containing the variables used in the model. Required if
  not already stored in `object`.

## Value

A numeric value: the root mean squared error.

## Details

The RMSE indicates the absolute fit of the model to the data. It can be
interpreted as the standard deviation of the unexplained variance, and
is expressed in the same units as the response variable. Lower values
indicate better model fit.

## Author

Martin Haringa

## Examples

``` r
x <- glm(nclaims ~ area, offset = log(exposure),
         family = poisson(), data = MTPL2)
rmse(x, MTPL2)
#> [1] 0.3564342
```
