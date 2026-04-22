# Fisher's natural breaks classification

Classifies a continuous numeric vector into intervals using Fisher-Jenks
natural breaks. Useful for choropleth mapping or other applications
where grouped ranges are required.

`fisher()` is deprecated as of version 0.8.0. Please use
`fisher_classify()` instead.

## Usage

``` r
fisher_classify(x, n = 7, dig.lab = NULL, diglab = NULL)

fisher(x, n = 7, diglab = 2)
```

## Arguments

- x:

  A numeric vector to be classified.

- n:

  Integer. Number of classes to generate (default = 7).

- dig.lab:

  Integer. Number of significant digits to use for interval labels
  (default = 2).

- diglab:

  Deprecated. Use `dig.lab` instead.

## Value

A factor indicating the interval to which each element of `x` belongs.

## Details

The `"fisher"` style uses the algorithm proposed by Fisher (1958),
commonly referred to as the Fisher-Jenks algorithm. This function is a
thin wrapper around
[`classInt::classIntervals()`](https://r-spatial.github.io/classInt/reference/classIntervals.html).

The argument `diglab` is deprecated and will be removed in a future
version.

## References

Bivand, R. (2018). *classInt: Choose Univariate Class Intervals*. R
package version 0.2-3. <https://CRAN.R-project.org/package=classInt>

Fisher, W. D. (1958). *On grouping for maximum homogeneity*. Journal of
the American Statistical Association, 53, pp. 789–798.
doi:10.1080/01621459.1958.10501479

## Author

Martin Haringa

## Examples

``` r
set.seed(1)
x <- rnorm(100)
fisher_classify(x, n = 5)
#>   [1] (-1.1,-0.34] (-0.34,0.23] (-1.1,-0.34] (1,2.4]      (0.23,1]    
#>   [6] (-1.1,-0.34] (0.23,1]     (0.23,1]     (0.23,1]     (-0.34,0.23]
#>  [11] (1,2.4]      (0.23,1]     (-1.1,-0.34] [-2.2,-1.1]  (1,2.4]     
#>  [16] (-0.34,0.23] (-0.34,0.23] (0.23,1]     (0.23,1]     (0.23,1]    
#>  [21] (0.23,1]     (0.23,1]     (-0.34,0.23] [-2.2,-1.1]  (0.23,1]    
#>  [26] (-0.34,0.23] (-0.34,0.23] [-2.2,-1.1]  (-1.1,-0.34] (0.23,1]    
#>  [31] (1,2.4]      (-0.34,0.23] (0.23,1]     (-0.34,0.23] [-2.2,-1.1] 
#>  [36] (-1.1,-0.34] (-1.1,-0.34] (-0.34,0.23] (1,2.4]      (0.23,1]    
#>  [41] (-0.34,0.23] (-0.34,0.23] (0.23,1]     (0.23,1]     (-1.1,-0.34]
#>  [46] (-1.1,-0.34] (0.23,1]     (0.23,1]     (-0.34,0.23] (0.23,1]    
#>  [51] (0.23,1]     (-1.1,-0.34] (0.23,1]     [-2.2,-1.1]  (1,2.4]     
#>  [56] (1,2.4]      (-1.1,-0.34] (-1.1,-0.34] (0.23,1]     (-0.34,0.23]
#>  [61] (1,2.4]      (-0.34,0.23] (0.23,1]     (-0.34,0.23] (-1.1,-0.34]
#>  [66] (-0.34,0.23] [-2.2,-1.1]  (1,2.4]      (-0.34,0.23] (1,2.4]     
#>  [71] (0.23,1]     (-1.1,-0.34] (0.23,1]     (-1.1,-0.34] [-2.2,-1.1] 
#>  [76] (0.23,1]     (-1.1,-0.34] (-0.34,0.23] (-0.34,0.23] (-1.1,-0.34]
#>  [81] (-1.1,-0.34] (-0.34,0.23] (1,2.4]      [-2.2,-1.1]  (0.23,1]    
#>  [86] (0.23,1]     (1,2.4]      (-0.34,0.23] (0.23,1]     (0.23,1]    
#>  [91] (-1.1,-0.34] (1,2.4]      (1,2.4]      (0.23,1]     (1,2.4]     
#>  [96] (0.23,1]     [-2.2,-1.1]  (-1.1,-0.34] [-2.2,-1.1]  (-1.1,-0.34]
#> Levels: [-2.2,-1.1] (-1.1,-0.34] (-0.34,0.23] (0.23,1] (1,2.4]
```
