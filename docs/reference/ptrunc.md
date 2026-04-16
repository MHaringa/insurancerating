# Truncated cumulative distribution function

Computes the cumulative distribution function (CDF) of a specified
distribution, truncated to the interval \\(a, b)\\.

The function takes a base distribution (e.g. `"norm"`, `"gamma"`) and
returns the corresponding truncated CDF evaluated at the values in `q`.
Values outside the truncation interval are mapped to the interval
boundaries.

## Usage

``` r
ptrunc(q, spec, a = -Inf, b = Inf, ...)
```

## Arguments

- q:

  A numeric vector of quantiles at which the truncated CDF is evaluated.

- spec:

  A character string specifying the distribution name (e.g. `"norm"`,
  `"gamma"`, `"lnorm"`). The corresponding CDF function `p<spec>` must
  exist.

- a:

  Numeric. Lower truncation bound. Defaults to `-Inf`.

- b:

  Numeric. Upper truncation bound. Defaults to `Inf`.

- ...:

  Additional arguments passed to the underlying distribution function
  (e.g. `mean`, `sd`, `shape`, `scale`).

## Value

A numeric vector of the same length as `q`, containing the truncated CDF
values.

## Details

The truncated CDF is defined as: \$\$ F\_{trunc}(q) =
\frac{F(\min(\max(q, a), b)) - F(a)}{F(b) - F(a)} \$\$

where \\F\\ is the CDF of the original distribution.

The function ensures numerical stability by checking that the truncation
interval lies within the support of the distribution.

## See also

[`dtrunc`](https://mharinga.github.io/insurancerating/reference/dtrunc.md)
for the truncated density function.
