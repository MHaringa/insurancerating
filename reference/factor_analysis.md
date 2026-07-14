# Factor analysis for discrete risk factors

Performs a factor analysis for discrete risk factors in an insurance
portfolio. The following summary statistics are calculated:

- frequency = number of claims / exposure

- average severity = severity / number of claims

- risk premium = severity / exposure

- loss ratio = severity / premium

- average premium = premium / exposure

## Usage

``` r
factor_analysis(
  data = NULL,
  risk_factors = NULL,
  claim_amount = NULL,
  claim_count = NULL,
  exposure = NULL,
  premium = NULL,
  group_by = NULL,
  df = NULL,
  x = NULL,
  severity = NULL,
  nclaims = NULL,
  by = NULL
)
```

## Arguments

- data:

  A `data.frame` with the insurance portfolio.

- risk_factors:

  Character vector: column(s) in `data` with the risk factor(s).

- claim_amount:

  Character, column in `data` with claim amounts (default = NULL).

- claim_count:

  Character, column in `data` with number of claims (default = NULL).

- exposure:

  Character, column in `data` with exposures (default = NULL).

- premium:

  Character, column in `data` with premiums (default = NULL).

- group_by:

  Character vector of column(s) in `data` to group by in addition to
  `risk_factors`.

- df, x, severity, nclaims, by:

  Deprecated argument names. Use `data`, `risk_factors`, `claim_amount`,
  `claim_count`, and `group_by` instead.

## Value

An object of class `"factor_analysis"` and `"univariate"` with summary
statistics.

## Details

The function computes summary statistics for discrete risk factors.

- **Frequency**: number of claims / exposure

- **Average severity**: severity / number of claims

- **Risk premium**: severity / exposure

- **Loss ratio**: severity / premium

- **Average premium**: premium / exposure

If one or more input arguments are not specified, the related statistics
are omitted from the results.

### Migration from [`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md)

The function
[`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md)
is deprecated as of version 0.8.0 and replaced by `factor_analysis()`.
In addition to the name change, the interface has also changed:

- [`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md)
  used **non-standard evaluation (NSE)**, so column names could be
  passed unquoted (e.g. `x = area`).

- `factor_analysis()` uses **standard evaluation (SE)**, so column names
  must be passed as character strings (e.g. `x = "area"`).

This makes the function easier to use in programmatic workflows.

[`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md)
is still available for backward compatibility but will emit a
deprecation warning and will be removed in a future release.

## Author

Martin Haringa

## Examples

``` r
## --- New usage (SE) ---
factor_analysis(MTPL2,
                risk_factors = "area",
                claim_amount = "amount",
                claim_count = "nclaims",
                exposure = "exposure",
                premium = "premium")
#>   area  amount nclaims   exposure premium  frequency average_severity
#> 1    2 4063270      98  818.53973   51896 0.11972540         41461.94
#> 2    3 7945311     113  764.99178   49337 0.14771401         70312.49
#> 3    1 6896187     146 1065.74795   65753 0.13699299         47234.16
#> 4    0    6922       1   13.30685     902 0.07514927          6922.00
#>   risk_premium loss_ratio average_premium
#> 1    4964.0474  78.296400        63.40071
#> 2   10386.1390 161.041632        64.49350
#> 3    6470.7486 104.880188        61.69658
#> 4     520.1832   7.674058        67.78464

## --- Deprecated usage (NSE) ---
univariate(MTPL2,
           x = area,
           severity = amount,
           nclaims = nclaims,
           exposure = exposure,
           premium = premium)
#>   area  amount nclaims   exposure premium  frequency average_severity
#> 1    2 4063270      98  818.53973   51896 0.11972540         41461.94
#> 2    3 7945311     113  764.99178   49337 0.14771401         70312.49
#> 3    1 6896187     146 1065.74795   65753 0.13699299         47234.16
#> 4    0    6922       1   13.30685     902 0.07514927          6922.00
#>   risk_premium loss_ratio average_premium
#> 1    4964.0474  78.296400        63.40071
#> 2   10386.1390 161.041632        64.49350
#> 3    6470.7486 104.880188        61.69658
#> 4     520.1832   7.674058        67.78464
```
