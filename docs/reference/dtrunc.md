# Truncated probability density function

Computes the probability density function (PDF) of a specified
distribution, truncated to the interval \\(a, b)\\.

The function takes a base distribution (e.g. `"norm"`, `"gamma"`) and
returns the corresponding truncated density evaluated at the values in
`x`. Values outside the truncation interval have density zero.

## Usage

``` r
dtrunc(x, spec, a = -Inf, b = Inf, ...)
```

## Arguments

- x:

  A numeric vector of quantiles at which the truncated density is
  evaluated.

- spec:

  A character string specifying the distribution name (e.g. `"norm"`,
  `"gamma"`, `"lnorm"`). The corresponding density function `d<spec>`
  and distribution function `p<spec>` must exist.

- a:

  Numeric. Lower truncation bound. Defaults to `-Inf`.

- b:

  Numeric. Upper truncation bound. Defaults to `Inf`.

- ...:

  Additional arguments passed to the underlying distribution functions
  (e.g. `mean`, `sd`, `shape`, `scale`).

## Value

A numeric vector of the same length as `x`, containing the truncated
density values.

## Details

The truncated density is defined as: \$\$ f\_{trunc}(x) =
\frac{f(x)}{F(b) - F(a)}, \quad \text{for } x \in (a, b) \$\$

and zero outside the truncation interval, where \\f\\ and \\F\\ are the
density and cumulative distribution function of the original
distribution.

The function checks that the truncation interval lies within the support
of the distribution to avoid numerical issues.

## See also

[`ptrunc`](https://mharinga.github.io/insurancerating/reference/ptrunc.md)
for the truncated cumulative distribution function.
