# Factor analysis for discrete risk factors

Performs a factor analysis for discrete risk factors in an insurance
portfolio. The following summary statistics are calculated:

- frequency = number of claims / exposure

- average severity = severity / number of claims

- risk premium = severity / exposure

- loss ratio = severity / premium

- average premium = premium / exposure

`univariate_summary()` is kept as a compatibility alias for
`factor_analysis()`. It uses the same standard-evaluation interface and
is **not** deprecated.

`univariate()` is deprecated as of version 0.8.0. Please use
`factor_analysis()` instead with **standard evaluation** (SE), i.e.
column names as character strings.

## Usage

``` r
factor_analysis(
  df,
  x,
  severity = NULL,
  nclaims = NULL,
  exposure = NULL,
  premium = NULL,
  by = NULL
)

univariate_summary(
  df,
  x,
  severity = NULL,
  nclaims = NULL,
  exposure = NULL,
  premium = NULL,
  by = NULL
)

univariate(
  df,
  x,
  severity = NULL,
  nclaims = NULL,
  exposure = NULL,
  premium = NULL,
  by = NULL
)
```

## Arguments

- df:

  A `data.frame` with the insurance portfolio.

- x:

  Character vector: column(s) in `df` with the risk factor(s).

- severity:

  Character, column in `df` with claim amounts (default = NULL).

- nclaims:

  Character, column in `df` with number of claims (default = NULL).

- exposure:

  Character, column in `df` with exposures (default = NULL).

- premium:

  Character, column in `df` with premiums (default = NULL).

- by:

  Character vector of column(s) in `df` to group by in addition to `x`.

## Value

A `data.table` of class `"univariate"` with summary statistics.

## Details

The function computes summary statistics for discrete risk factors.

- **Frequency**: number of claims / exposure

- **Average severity**: severity / number of claims

- **Risk premium**: severity / exposure

- **Loss ratio**: severity / premium

- **Average premium**: premium / exposure

If one or more input arguments are not specified, the related statistics
are omitted from the results.

### Migration from `univariate()`

The function `univariate()` is deprecated as of version 0.8.0 and
replaced by `factor_analysis()`. In addition to the name change, the
interface has also changed:

- `univariate()` used **non-standard evaluation (NSE)**, so column names
  could be passed unquoted (e.g. `x = area`).

- `factor_analysis()` uses **standard evaluation (SE)**, so column names
  must be passed as character strings (e.g. `x = "area"`).

This makes the function easier to use in programmatic workflows.

`univariate()` is still available for backward compatibility but will
emit a deprecation warning and will be removed in a future release.

## Author

Martin Haringa

## Examples

``` r
## --- New usage (SE) ---
factor_analysis(MTPL2,
                x = "area",
                severity = "amount",
                nclaims = "nclaims",
                exposure = "exposure",
                premium = "premium")
#> # A tibble: 4 × 10
#>    area  amount nclaims exposure premium frequency average_severity risk_premium
#>   <int>   <int>   <int>    <dbl>   <int>     <dbl>            <dbl>        <dbl>
#> 1     2 4063270      98    819.    51896    0.120            41462.        4964.
#> 2     3 7945311     113    765.    49337    0.148            70312.       10386.
#> 3     1 6896187     146   1066.    65753    0.137            47234.        6471.
#> 4     0    6922       1     13.3     902    0.0751            6922          520.
#> # ℹ 2 more variables: loss_ratio <dbl>, average_premium <dbl>

## --- Deprecated usage (NSE) ---
univariate(MTPL2,
           x = area,
           severity = amount,
           nclaims = nclaims,
           exposure = exposure,
           premium = premium)
#> # A tibble: 4 × 10
#>    area  amount nclaims exposure premium frequency average_severity risk_premium
#>   <int>   <int>   <int>    <dbl>   <int>     <dbl>            <dbl>        <dbl>
#> 1     2 4063270      98    819.    51896    0.120            41462.        4964.
#> 2     3 7945311     113    765.    49337    0.148            70312.       10386.
#> 3     1 6896187     146   1066.    65753    0.137            47234.        6471.
#> 4     0    6922       1     13.3     902    0.0751            6922          520.
#> # ℹ 2 more variables: loss_ratio <dbl>, average_premium <dbl>
```
