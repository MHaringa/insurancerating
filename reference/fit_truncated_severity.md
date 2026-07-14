# Fit severity distributions to truncated claim data

**\[experimental\]** Estimate an underlying claim severity distribution
when the observed claims are truncated.

## Usage

``` r
fit_truncated_severity(
  losses = NULL,
  distribution = c("gamma", "lognormal"),
  lower_truncation = NULL,
  upper_truncation = NULL,
  start_values = NULL,
  print_initial = TRUE,
  n_variants = 1,
  n_shape_grid = 8,
  n_scale_grid = 8,
  show_progress = FALSE,
  show_summary = TRUE,
  y = NULL,
  dist = NULL,
  left = NULL,
  right = NULL,
  start = NULL,
  trace = NULL,
  report = NULL
)
```

## Arguments

- losses:

  Numeric vector with observed claim severities.

- distribution:

  Severity distribution to fit: `"gamma"` or `"lognormal"`.

- lower_truncation:

  Numeric lower truncation point. Claims at or below this value are
  assumed not to be present in `losses`. Defaults to `0`.

- upper_truncation:

  Numeric upper truncation point. Claims at or above this value are
  assumed not to be present in `losses`. Defaults to `Inf`.

- start_values:

  Optional named list of starting values. If `NULL`, a multi-start
  strategy is used. For a gamma distribution use `shape` and `scale`;
  for a lognormal distribution use `meanlog` and `sdlog`.

- print_initial:

  Deprecated logical retained for backward compatibility.

- n_variants:

  Controls how many local variations around base starts are used.

- n_shape_grid:

  Number of grid points for gamma shape.

- n_scale_grid:

  Number of grid points for gamma scale.

- show_progress:

  Logical. If `TRUE`, prints periodic progress during the fitting loop.

- show_summary:

  Logical. If `TRUE`, prints a short summary at the end.

- y, dist, left, right, start, trace, report:

  Deprecated argument names kept for backward compatibility.

## Value

An object of class
`c("truncated_severity", "truncated_dist", "fitdist")`. The object
contains the fitted distribution parameters from
[`fitdistrplus::fitdist()`](https://lbbe-software.github.io/fitdistrplus/reference/fitdist.html)
and additional attributes:

- truncated_vec:

  The observed losses used for fitting.

- lower_truncation, upper_truncation:

  The truncation bounds.

- fit_attempts:

  Metadata for each attempted start combination.

- n_attempts, n_success, n_failed:

  Fit attempt counts.

- best_attempt_index:

  Index of the selected start combination.

## Details

In insurance pricing, severity models are often fitted on claim amounts
that are not observed over the full range of possible losses. Small
claims may be absent because of a deductible, reporting threshold, or
data extraction rule. Very large claims may be capped, excluded, or
modelled separately as large losses. A standard gamma or lognormal fit
on the remaining observed claims treats that truncated sample as if it
were complete, which can bias the estimated severity distribution.

`fit_truncated_severity()` fits the distribution conditional on the
claim being observed within the truncation interval. This means the
fitted likelihood uses the density divided by the probability mass
between `lower_truncation` and `upper_truncation`. The function is
intended for truncation, where claims outside the interval are absent
from the data. This differs from censoring, where claims outside a limit
are still observed but their exact amount is not known.

Observed losses must lie strictly inside the truncation interval. Values
outside the interval indicate that the bounds do not describe the data
and therefore produce an error.

## Examples

``` r
if (FALSE) { # \dontrun{
observed <- MTPL2$amount[MTPL2$amount > 500 & MTPL2$amount < 10000]
fit <- fit_truncated_severity(
  losses = observed,
  distribution = "gamma",
  lower_truncation = 500,
  upper_truncation = 10000
)
autoplot(fit)
} # }
```
