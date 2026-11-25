# Refinement building blocks

## Introduction

Refinement turns fitted effects into explicit tariff proposals when an
adjustment is useful. You can review the proposal before refitting and
keep a record of what changed. Start with the age-band example below, or
skip to the [advanced options](#advanced-refinement-options) for a
specific technique. See [Getting
Started](https://mharinga.github.io/insurancerating/articles/getting-started.md)
for model construction and the [package
map](https://mharinga.github.io/insurancerating/articles/pricing-workflow-building-blocks.md)
for how refinement fits alongside other pricing tasks.

## Quick start

Suppose the age-band relativities of a frequency model are uneven. Here
we compare them with a smoother age effect. The example keeps the same
age bands so that only their relativities change; this is a candidate
tariff, not an assumption about how age must affect every portfolio.

The [homepage
example](https://mharinga.github.io/insurancerating/#from-model-to-refined-tariff)
starts with the same age bands, including one band for ages 80 and
above. It fits an age-only model and uses new bands for the tariff,
whereas here we keep the original bands and adjust for ZIP and
bonus-malus.

### Fit the starting model

The response is claim count and `log(exposure)` is the offset. ZIP and
bonus-malus remain separate model terms and are used in later examples.

``` r

library(insurancerating)

portfolio <- as.data.frame(MTPL)

age_breaks <- c(18, 29, 39, 49, 59, 69, 79, 95)
portfolio$age_band <- cut(
  portfolio$age_policyholder,
  breaks = age_breaks,
  include.lowest = TRUE
)
portfolio$bm_group <- cut(
  portfolio$bm,
  breaks = c(0, 4, 8, Inf),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)
portfolio$zip <- factor(portfolio$zip)

unrestricted <- glm(
  nclaims ~ age_band + zip + bm_group + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)
```

### Smoothing an ordered effect

Fit the curve to the estimated age-band effects using the underlying
numeric age. The breaks define the output bands, and exposure gives more
weight to levels with more portfolio support.

``` r

refinement <- prepare_refinement(unrestricted, data = portfolio) |>
  add_smoothing(
    model_variable = "age_band",
    source_variable = "age_policyholder",
    breaks = age_breaks,
    smoothing = "spline",
    k = 6,
    weights = "exposure"
  )
initial_age_refinement <- refinement
```

### Compare the proposal and refit

``` r

autoplot(refinement, variable = "age_band")
```

![](refinement-workflow_files/figure-html/unnamed-chunk-4-1.png)

``` r


age_tariff <- refit(refinement, intercept_only = TRUE)
age_effects <- as.data.frame(rating_table(age_tariff, exposure = FALSE))
subset(age_effects, risk_factor == "age_policyholder_smooth")
#>               risk_factor   level est_age_tariff
#> 2 age_policyholder_smooth [18,29]      0.9704716
#> 3 age_policyholder_smooth (29,39]      0.6981457
#> 4 age_policyholder_smooth (39,49]      0.5927132
#> 5 age_policyholder_smooth (49,59]      0.5218063
#> 6 age_policyholder_smooth (59,69]      0.4441382
#> 7 age_policyholder_smooth (69,79]      0.4250005
#> 8 age_policyholder_smooth (79,95]      0.4734956
```

The dashed line is the proposed smooth curve. The resulting age-band
relativities are fixed during the intercept-only refit, which estimates
a new base rate. Keep the `refinement` object to make another proposal;
`age_tariff` is the fitted result. Use
[`audit_refinement()`](#auditing-the-portfolio-effect) to see how
changes in relativities and base rate combine on the portfolio.

## Advanced refinement options

The following examples build on the saved proposal. They are
alternatives or additions, not a required sequence. See
[`?add_smoothing`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
for the full list of curve shapes, weights and spline settings.

### Refining a selected part of the smoothing

[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
defines the initial curve and its structural assumptions.
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md)
can set explicit target values, adjust the relative level, or scale the
change in the curve after an anchor. These are tariff assumptions
layered on the initial smoothing, not additional statistical
observations.

Each call to
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md)
is stored as a separate refinement step. If the same smoothing is edited
more than once, the edits are applied cumulatively in their recorded
order. The `step` argument in
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
can therefore be used to inspect the curve after a particular edit
without introducing a separate revision argument.

#### Explicitly redirecting the curve

Use explicit values when the intended targets are known:

``` r

explicit_age_refinement <- refinement |>
  edit_smoothing(
    model_variable = "age_band",
    from = 32,
    to = 65,
    from_value = 0.95,
    to_value = 1.10,
    control_positions = 51,
    control_values = 1.02
  )

autoplot(explicit_age_refinement, variable = "age_band", x_max = 90)
```

![](refinement-workflow_files/figure-html/unnamed-chunk-5-1.png)

The stored edit redirects the selected interval through the supplied
target values. This is appropriate when those values have a documented
actuarial or implementation basis.

#### Applying a relative local adjustment

Use `adjustment` when the current shape is broadly appropriate but one
region should be somewhat higher or lower:

``` r

relative_age_refinement <- refinement |>
  edit_smoothing(
    model_variable = "age_band",
    from = 32,
    to = 65,
    adjustment = 1.05
  )

autoplot(
  relative_age_refinement,
  variable = "age_band",
  x_max = 90,
  show_initial_smoothing = TRUE
)
```

![](refinement-workflow_files/figure-html/unnamed-chunk-6-1.png)

Here, `adjustment = 1.05` raises the middle of the selected region by up
to 5% relative to the existing smoothing. The multiplier starts at 1 at
`from`, moves towards 1.05 over the available smoothing points, and
returns to 1 at `to`. The unchanged curve is therefore retained outside
the interval and no jump is introduced at either boundary.

With `show_initial_smoothing = TRUE`, the plot includes both the curve
directly after
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
and the cumulative curve at the selected step. This comparison changes
only the plot; it does not alter the stored refinement or the subsequent
refit.

For example, two local adjustments create two successive edit steps:

``` r

cumulative_age_refinement <- relative_age_refinement |>
  edit_smoothing(
    model_variable = "age_band",
    from = 50,
    adjustment = 1.02,
    transition = "linear"
  )

# Step 3 is the cumulative result of the initial smoothing and both edits.
autoplot(
  cumulative_age_refinement,
  step = 3,
  x_max = 90,
  show_initial_smoothing = TRUE
)
```

![](refinement-workflow_files/figure-html/unnamed-chunk-7-1.png)

Selecting `step = 2` would show the curve after the first edit only. The
initial comparison line remains the result of the original
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
step in both plots.

For a tail adjustment, only one boundary is needed. With only `from`,
the adjustment runs from that value to the end of the available
smoothing range. With only `to`, it runs from the beginning of the range
to that value. The transition remains attached to the original curve at
the supplied boundary:

``` r

# Refine the upper tail
edit_smoothing(
  refinement,
  model_variable = "age_band",
  from = 50,
  adjustment = 1.05
)

# Refine the lower tail
edit_smoothing(
  refinement,
  model_variable = "age_band",
  to = 35,
  adjustment = 0.95
)
```

With `transition = NULL`, the default, the transition inherits the
smoothing specification of the step being edited. For example, an
original `smoothing = "increasing_concave"` uses the same structural
shape logic while adapting the entry and exit to their opposite
directions. The resulting curve is checked against the inherited
monotonicity and curvature where applicable.

Two explicit alternatives are available:

``` r

# Continuous straight transitions
edit_smoothing(
  refinement,
  model_variable = "age_band",
  from = 32,
  to = 65,
  adjustment = 1.05,
  transition = "linear"
)

# Immediate changes at both boundaries
edit_smoothing(
  refinement,
  model_variable = "age_band",
  from = 32,
  to = 65,
  adjustment = 1.05,
  transition = "step"
)
```

`"linear"` remains continuous. `"step"` deliberately permits
discontinuities and should therefore be selected only when an immediate
tariff change is intended. Relative adjustments and explicit target
values cannot be combined in one
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md)
call because they express different instructions. They can be recorded
in separate consecutive edit steps when that sequence has a clear
actuarial interpretation.

### Adjusting the slope after an anchor

Sometimes the level of the smoothing is acceptable, while the remaining
change above a selected value is to be made stronger or weaker. In that
case, `slope_adjustment` scales the change relative to the smoothing
value at `from`:

``` r

slope_refinement <- refinement |>
  edit_smoothing(
    model_variable = "age_band",
    from = 50,
    slope_adjustment = 1.10
  )

autoplot(
  slope_refinement,
  variable = "age_band",
  show_initial_smoothing = TRUE
)
```

![](refinement-workflow_files/figure-html/unnamed-chunk-10-1.png)

The curve through age 50 is unchanged. Above age 50, each difference
from the relativity at age 50 is multiplied by 1.10. The curve therefore
remains continuous at the anchor. Where the curve falls below its value
at age 50, the decline relative to that anchor is 10% larger. A value
between 0 and 1 flattens the remaining effect. This is different from
`adjustment`, which changes the relative level over a selected interval.

Each
[`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md)
call records one type of intervention: a relative level adjustment, a
slope adjustment, or explicit target/control-point values. When both
level and slope require adjustment, use two consecutive calls:

``` r

refinement |>
  edit_smoothing(
    model_variable = "age_band",
    from = 30,
    to = 50,
    adjustment = 1.05
  ) |>
  edit_smoothing(
    model_variable = "age_band",
    from = 50,
    slope_adjustment = 1.10
  )
```

The refinement history then retains both actuarial choices as separate
steps.

The transformation preserves the direction of an increasing or
decreasing effect. For a shape-constrained curve, a value above 1 may
nevertheless create a visible change in slope at the anchor and need not
preserve global concavity or convexity across that exact point. This
should therefore be treated as an explicit actuarial intervention and
inspected before refitting.

The remainder of this vignette uses `relative_age_refinement` as the
current proposal:

``` r

refinement <- relative_age_refinement
```

The dashed curve shows the spline after the local adjustment, including
the transition into and out of the edited range. Each orange segment
uses the adjusted curve’s value at its midpoint:

``` r

autoplot(refinement, variable = "age_band")
```

![](refinement-workflow_files/figure-html/unnamed-chunk-13-1.png)

### Interpreting the premium effect

A relativity curve shows the shape of a tariff effect. To read its
magnitude, use
[`premium_change()`](https://mharinga.github.io/insurancerating/reference/premium_change.md)
with a meaningful increment, for example five years of age:

``` r

premium_change(
  refinement,
  variable = "age_band",
  at = seq(20, 60, by = 5),
  increment = 5
)
#> Premium change for age_policyholder
#> 
#> Increment: 5
#> Basis: Effective smoothing curve
#> 
#>  From To Premium change
#>    20 25         -13.4%
#>    25 30         -14.2%
#>    30 35         -10.5%
#>    35 40         -10.2%
#>    40 45          -4.6%
#>    45 50          -5.6%
#>    50 55          -9.2%
#>    55 60          -9.1%
#>    60 65          -5.8%
```

The first row compares the modelled tariff at ages 20 and 25, holding
other factors fixed. It is not a prediction of the change at an
individual policy renewal. The calculation uses the current edited curve
without extrapolating. Omit `increment` to compare a value with twice
that value instead.

For an insured-amount smoothing, `increment = 100000` would ask how much
modelled premium changes for another 100,000 from each displayed
starting value. It might, for example, compare 100,000 with 200,000,
then 200,000 with 300,000. Successive percentage changes may decline,
remain stable or increase depending on the fitted relationship. The
increment is a finite practical comparison, not a derivative or slope,
and no particular pattern is imposed by the package.

By default, `basis = "curve"` evaluates the continuous effective
smoothing at the exact starting and comparison values. This is the
appropriate basis when the question concerns the shape or steepness of
the smoothing itself. To review the premium effect of the implementable
tariff classes instead, use:

``` r

premium_change(
  refinement,
  variable = "age_band",
  at = c(20, 25, 30, 35),
  increment = 5,
  basis = "segments"
)
#> Premium change for age_policyholder
#> 
#> Increment: 5
#> Basis: Tariff segments
#> 
#>  From To Premium change
#>    20 25           0.0%
#>    25 30         -27.8%
#>    30 35           0.0%
#>    35 40         -11.2%
```

With `basis = "segments"`, the helper determines which effective tariff
interval contains each value and compares the corresponding current
segment relativities. The result can therefore be 0% when both values
are in the same segment, even when the underlying curve changes within
that range. A change can also occur discretely when a comparison crosses
a segment boundary. The two bases answer different questions: the curve
describes the underlying smooth relationship, while the segments
describe the tariff that would be applied.

The initial and edited relationships can be compared directly:

``` r

premium_change(
  relative_age_refinement,
  variable = "age_band",
  at = c(20, 25, 30),
  increment = 5,
  steps = c(1, 2)
) |>
  as_gt()
```

| Premium change for +5 age_policyholder |  |  |  |  |
|----|----|----|----|----|
| Based on the effective continuous smoothing curve |  |  |  |  |
| From | To | Step 1 | Step 2 | Difference |
| 20 | 25 | −13.4% | −13.4% | 0.0 pp |
| 25 | 30 | −14.4% | −14.2% | 0.2 pp |
| 30 | 35 | −11.8% | −10.5% | 1.3 pp |

With two selected states, the table shows both premium changes and their
difference in percentage points. This comparison is often easier to
interpret than small visual differences between two relativity curves.
It remains an interpretation of the proposed smoothing and does not
alter the refinement.

### Restricting selected levels

[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
can fix tariff levels at supplied relativities or apply a multiplier to
their current relativities. A restriction may represent an
implementation rule, a supported external assumption or a deliberate
response to an unstable local estimate. It differs from smoothing:
smoothing estimates a structured pattern, whereas a restriction records
an explicit level decision.

``` r

refinement <- refinement |>
  add_restriction(
    restrictions = c("0" = 0.95, "3" = 1.10),
    model_variable = "zip"
  )
```

Only ZIP levels 0 and 3 are supplied here. The other observed ZIP levels
are fixed at their current effective relativities. Consequently, a
partial restriction changes the selected values but still produces a
complete fixed structure for that risk factor. The named-vector form is
preferred because each level remains directly beside its relativity. A
two-column data frame remains a fully supported alternative; its column
names identify the source factor and generated tariff column.

In that data frame, the level column must be character or factor and the
value column must be numeric. Numeric category codes must be converted
explicitly with
[`as.character()`](https://rdrr.io/r/base/character.html) or
[`factor()`](https://rdrr.io/r/base/factor.html). Named-vector input
such as `c("0" = 1.00, "3" = 1.10)` already uses character labels, even
though the category names look numeric.

Alternatively, `restriction_type = "multiplier"` retains an adjustment
relative to the effect that exists at that point in the refinement. Here
the current ZIP 3 relativity is increased by 10%; it is not stored as a
prematurely calculated fixed value.

``` r

prepare_refinement(unrestricted, data = portfolio) |>
  add_restriction(
    restrictions = c("3" = 1.10),
    model_variable = "zip",
    restriction_type = "multiplier"
  )
#> <rating_refinement>
#> Base model: Poisson GLM (log link)
#> Steps: 1
#>   1. Multiplier restriction: zip -> zip_multiplier (4 levels)
```

Multipliers require an existing level and cannot introduce a new factor
or level. Step order is material: a multiplier after a fixed restriction
scales that fixed value, while a later fixed restriction supersedes
earlier multipliers for the same factor. Both choices remain explicit in
the refinement summary and audit trail.

Repeated calls for the same restricted variable update matching levels
and retain earlier restrictions for levels not supplied again. New
levels may be added when they represent explicit tariff assumptions; new
risk factors require the corresponding portfolio column and an explicit
opt-in. These behaviours are documented in the
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
reference.

`allow_new_risk_factors = TRUE` is permission to introduce a new fixed
factor, not a requirement that the factor be new. For a factor already
in the GLM or created by an earlier refinement step, it has no
additional effect and does not itself produce a warning or message. This
also holds for multipliers, which still require an existing relativity
and cannot introduce a new factor. The flag does not request replacement
of another model variable.

A new fixed tariff factor can also replace an existing standalone model
term. For example, `add_restriction(..., replaces = "postal_area")`
records that the new classification substitutes for `postal_area` during
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
rather than adding a second multiplicative effect. The replacement is
shown in the refinement summary and audit. Interactions and transformed
terms must be revised explicitly in the model specification because
removing them is not an unambiguous level restriction.

``` r

autoplot(refinement, variable = "zip")
```

![](refinement-workflow_files/figure-html/unnamed-chunk-19-1.png)

Again, the plot shows the proposed restriction before
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).

### Enforcing an order without smoothing

For grouped continuous variables such as insured amount or
reconstruction value, a tariff may require increasing relativities
without a smooth curve.
[`add_monotonicity()`](https://mharinga.github.io/insurancerating/reference/add_monotonicity.md)
accepts the existing grouped model factor, including ordinary factors
from [`cut()`](https://rdrr.io/r/base/cut.html). Numeric labels such as
`"100"`, `"200"`, `"1000"` and non-overlapping numeric intervals are put
in numeric order. There is no need to convert those factors to
[`ordered()`](https://rdrr.io/r/base/factor.html). Numeric order also
takes precedence for ordered factors: a conflicting declared order
produces a warning, and the projection uses ascending numeric order.
Interval overlap is rejected for both ordinary and ordered factors.

The following alternative uses the existing age bands, whose estimated
effects mostly decrease, to demonstrate the decreasing variant. Choosing
a direction remains an explicit actuarial assumption, not a universal
rule for age. This starts from the unrestricted model and does not
change the refinement below.

``` r

ordinal <- prepare_refinement(unrestricted) |>
  add_monotonicity("age_band", direction = "decreasing", weights = "exposure")
autoplot(ordinal, variable = "age_band")
```

![](refinement-workflow_files/figure-html/unnamed-chunk-20-1.png)

Unlike
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
this keeps the existing categories and only adjusts their relativities;
there is no `source_variable`, new `breaks` or fitted curve. For
non-numeric categories such as `"Low"`, `"Medium"`, `"High"`, declare an
ordered factor with explicit levels before fitting the GLM. That
declared order is retained; alphabetical order is never assumed to be a
tariff order. The original reference level does not have to come first
in the monotonic sequence.

The default isotonic method pools adjacent violations on the
log-relativity scale; equal fitted levels are allowed. It does not fit a
smooth curve. Use `method = "increase_only"` if no level may fall, or
`"decrease_only"` if no level may rise. These restrictions apply to the
factor effect, not the final premium: `refit(ordinal)` fixes the
projected ratios but re-estimates the intercept and remaining free
effects. This differs from estimating a GLM with order constraints in
its likelihood.

After another refinement,
[`add_monotonicity()`](https://mharinga.github.io/insurancerating/reference/add_monotonicity.md)
can be called again for the same model variable. It projects the current
effects and appends a new step; it does not overwrite the earlier
decision. Default output columns receive unique names such as
`age_band_monotone` and `age_band_monotone_1`. At refit, only the latest
effective relativity is applied, without double counting. An explicit
`output_variable` must still name a new column. Reapplying the same
direction to an already monotone effect leaves its values unchanged.

### Shrinking a categorical effect

Suppose you want to keep the order of the fitted bonus-malus effects but
use smaller differences in the tariff, for example because the
differences are not stable across years.
[`add_shrinkage()`](https://mharinga.github.io/insurancerating/reference/add_shrinkage.md)
brings all the relativities closer together without setting a separate
target for each group.

``` r

refinement <- refinement |>
  add_shrinkage(
    model_variable = "bm_group",
    credibility = 0.9,
    weights = "exposure"
  )
```

In this example, holding age and ZIP fixed, the fitted claim frequency
for `Medium` is about 5.0% higher than for `Low` before shrinkage and
4.4% higher afterwards. For `High`, the difference moves from 4.3% lower
to 3.9% lower. Both groups move closer to `Low`; their order does not
change.

`credibility = 0.9` keeps 90% of each difference **on the log scale**.
It is not a 10% reduction in everyone’s premium. A value of 1 leaves the
relativities unchanged, and 0 makes them equal. The value is an
actuarial choice; the function does not estimate credibility from the
claims.

With `weights = "exposure"`, groups with more policy-years carry more
weight in the common centre. The function then rescales the relativities
so their exposure-weighted average is unchanged. That preserves the
average of this factor, not necessarily total expected claims across the
whole portfolio. Claim count can be a weighting basis for severity,
while `weights = "equal"` gives each level the same weight.

``` r

autoplot(refinement, variable = "bm_group")
```

![](refinement-workflow_files/figure-html/unnamed-chunk-22-1.png)

This remains a pre-refit comparison: it shows the current estimated
effect and the proposed shrunken structure stored in the refinement
specification.

### Differentiating between car series within a make

Suppose a motor own-damage portfolio has enough claims to estimate a
make effect, but too few for some car series to estimate their effects
reliably. You could fit a severity GLM by make and use
[`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
to introduce differences between series when other evidence supports
them.

This separate example uses fictitious data. The response is average
repair cost per claim, weighted by the number of claims.

``` r

repairs <- data.frame(
  make = c("BMW", "BMW", "Volkswagen", "Volkswagen"),
  series = c("BMW 3 Series", "BMW X3", "Volkswagen Golf", "Volkswagen Tiguan"),
  exposure = c(240, 60, 350, 100),
  claims = c(24, 6, 28, 7),
  repair_cost = c(72000, 24000, 70000, 21000)
)
repairs$severity <- repairs$repair_cost / repairs$claims

make_model <- glm(
  severity ~ make, family = Gamma(link = "log"),
  weights = claims, data = repairs
)
```

For illustration, suppose repair specialists propose an X3 effect 10%
above the 3 Series and a Tiguan effect 8% above the Golf, for example
based on parts costs and repair times not captured by the GLM. These are
illustrative judgements, not estimates for the actual vehicles.

``` r

series_relativities <- relativities(
  split_level(
    "BMW",
    new_levels = c("BMW 3 Series" = 1.00, "BMW X3" = 1.10)
  ),
  split_level(
    "Volkswagen",
    new_levels = c("Volkswagen Golf" = 1.00, "Volkswagen Tiguan" = 1.08)
  )
)

make_refinement <- prepare_refinement(make_model, data = repairs) |>
  add_relativities(
    model_variable = "make",
    split_variable = "series",
    relativities = series_relativities,
    exposure = "exposure",
    normalize = TRUE,
    output_variable = "vehicle_tariff_segment"
  )
```

With `normalize = TRUE`, the series multipliers are rescaled to average
one within each make, weighted here by policy-years. The X3 effect is
still 10% above the 3 Series effect, not 10% above the BMW average. The
split preserves the exposure-weighted make relativity, but does not fix
the intercept or guarantee the same total expected loss.

``` r

make_tariff <- refit(make_refinement, intercept_only = TRUE)
rating_table(make_tariff, exposure = FALSE)
#>              risk_factor             level est_make_tariff
#> 1            (Intercept)       (Intercept)    3195.9552394
#> 2 vehicle_tariff_segment            BMW X3       1.0784314
#> 3 vehicle_tariff_segment      BMW 3 Series       0.9803922
#> 4 vehicle_tariff_segment Volkswagen Tiguan       0.8621725
#> 5 vehicle_tariff_segment   Volkswagen Golf       0.7983079
```

The table shows the four resulting tariff segments. The make effects
come from the GLM; the differences within a make come from the supplied
multipliers. An unsplit make would keep its existing relativity. A
restriction or shrinkage applied before the split would change the make
effect used as its starting point, not add a second make effect during
refitting.

## Combining and reviewing refinements

Returning to the `MTPL` example, its specification contains the age
smoothing and its edit, the ZIP restriction and the bonus-malus
shrinkage:

``` r

summary(refinement)
#> Refinement specification
#> 
#> Package: insurancerating 0.8.2.9000
#> Created: 2026-11-25 16:34:51 UTC
#> Observations: 30,000
#> Family: poisson (log link)
#> Base formula:
#>   nclaims ~ age_band + zip + bm_group + offset(log(exposure))
#> Offset: log(exposure)
#> 
#> Refinement steps: 4
#>   1. Smoothing: age_band from age_policyholder (method: spline, k: 6, scale: relativity)
#>      shape = spline; 7 intervals over 18 to 95
#>   2. Smoothing edit: age_band (relative adjustment: 1.05 from 32 to 65, transition: inherited)
#>      relative adjustment = 1.05; transition = inherited; cumulative from smoothing step 1
#>   3. Restriction: zip -> zip_restricted (4 levels)
#>      type = fixed; 0 = 0.9500000; 1 = 0.9985459; 2 = 0.9003205; 3 = 1.1000000
#>   4. Shrinkage: bm_group (credibility: 0.9, weights: exposure, weighted mean preserved)
#>      credibility = 0.9; weights = exposure; weighted mean preserved
```

The summary records the base formula, package version, data size and
each refinement in evaluation order. It describes what will be applied;
it does not report a fitted refined model because
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
has not yet been called.

The variable-specific
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
calls above evaluate the stored steps in their recorded order. They
therefore inspect the intended tariff after any preceding adjustments
without silently changing the GLM. The `step` argument can be used when
an earlier stage of a longer specification needs to be reviewed
separately.

[`add_rebasing()`](https://mharinga.github.io/insurancerating/reference/add_rebasing.md)
can be inserted when required to change which resulting level is
displayed as one without changing ratios between levels. Rebasing
changes the representation of an effect, whereas shrinkage changes its
degree of differentiation.

## Refitting the model

``` r

refined_model <- refit(
  refinement,
  intercept_only = TRUE
)
```

[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
applies the stored steps in order, constructs the required tariff
variables and offsets, updates the formula and calls
[`glm()`](https://rdrr.io/r/stats/glm.html) with the original model
family. It does more than copy proposed relativities into an existing
coefficient vector.

The treatment of the remaining model effects depends on
`intercept_only`:

- with `intercept_only = TRUE`, unaffected existing effects are fixed as
  offsets and only the intercept is estimated. Their relative
  differences are preserved while the overall level is recalibrated;
- with `intercept_only = FALSE`, remaining free terms are estimated
  again. Their coefficients may change as the GLM finds a new joint
  optimum conditional on the fixed refinement steps.

Choose between these modes based on which existing relationships are to
remain fixed and which effects can be re-estimated.

Refitting does not turn the proposed tariff changes into unrestricted
statistical estimates.

## Inspecting the fitted result

After
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md),
the result is a fitted GLM and can be reviewed with the normal
interpretation tools:

``` r

head(rating_table(refined_model, exposure = FALSE))
#>      risk_factor       level est_refined_model
#> 1    (Intercept) (Intercept)         0.2208613
#> 2 zip_restricted           0         0.9500000
#> 3 zip_restricted           1         0.9985459
#> 4 zip_restricted           2         0.9003205
#> 5 zip_restricted           3         1.1000000
#> 6       bm_group      Medium         1.0446434
```

This is the **post-refit** result. It is distinct from the preview shown
by `autoplot(refinement)`:

- before
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md):
  proposed tariff adjustments;
- after
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md):
  fitted model under those adjustments.

Printing `refined_model` also reports the original and refitted
formulas, the model family, refit mode and stored refinement steps
before showing the regular GLM output.

## Calibrating the final level

After the tariff structure has been refined and fitted, an externally
selected overall calibration factor can be applied to a log-link model:

``` r

calibrated_model <- adjust_intercept(
  refined_model,
  factor = 1.05
)

evaluation <- extract_model_data(refined_model)
before <- predict(refined_model, newdata = evaluation, type = "response")
after <- predict(calibrated_model, newdata = evaluation, type = "response")
sum(after) / sum(before)
#> [1] 1.05
```

[`adjust_intercept()`](https://mharinga.github.io/insurancerating/reference/adjust_intercept.md)
adds `log(1.05)` to the intercept. Response-scale predictions therefore
increase by exactly 5%, while all non-intercept coefficients and the
ratios between tariff levels remain unchanged. The original
`refined_model` is not modified.

This differs from `refit(intercept_only = TRUE)`. An intercept-only
refit estimates the overall level from the model data conditional on the
stored refinements. Calibration instead applies one explicit total
factor after that fit. It is consequently a final model-level operation:
further refinement and repeated calibration are rejected. If the tariff
structure changes, return to the retained `refinement` specification,
refit it and calibrate the new result.

## Auditing the portfolio effect

Individual coefficient changes are difficult to interpret independently
of the intercept, other model terms and portfolio mix.
[`audit_refinement()`](https://mharinga.github.io/insurancerating/reference/audit_refinement.md)
therefore compares predictions from the unrestricted and refined models
on the same observed portfolio combinations.

``` r

refinement_audit <- audit_refinement(
  refined_model,
  exposure = "exposure",
  metric = "frequency"
)

summary(refinement_audit)
#> Refinement audit
#> 
#> Package: insurancerating 0.8.2.9000
#> Prepared: 2026-11-25 16:34:51 UTC
#> Refitted: 2026-11-25 16:34:57 UTC
#> Audited: 2026-11-25 16:34:57 UTC
#> Measure: frequency (per_exposure)
#> Exposure: exposure
#> 
#> Original formula:
#>   nclaims ~ age_band + zip + bm_group + offset(log(exposure))
#> Refitted formula:
#>   nclaims ~ offset(log(bm_group_shrunk) + log(zip_restricted) + 
#>       log(age_band_smooth) + log(exposure))
#> 
#> Refinement steps: 4
#>   1. Smoothing: age_band from age_policyholder (method: spline, k: 6, scale: relativity)
#>      shape = spline; 7 intervals over 18 to 95
#>   2. Smoothing edit: age_band (relative adjustment: 1.05 from 32 to 65, transition: inherited)
#>      relative adjustment = 1.05; transition = inherited; cumulative from smoothing step 1
#>   3. Restriction: zip -> zip_restricted (4 levels)
#>      type = fixed; 0 = 0.9500000; 1 = 0.9985459; 2 = 0.9003205; 3 = 1.1000000
#>   4. Shrinkage: bm_group (credibility: 0.9, weights: exposure, weighted mean preserved)
#>      credibility = 0.9; weights = exposure; weighted mean preserved
#> 
#> Portfolio effect
#>   Before: 0.137596
#>   After:  0.137596
#>   Change: -6.10623e-16 (-4.438e-13%)
#> 
#> Largest level changes (10 of 14)
#>              risk_factor   level     before      after       change
#>  age_policyholder_smooth (79,95] 0.12287953 0.10554083 -0.017338706
#>           zip_restricted       3 0.13680279 0.15122925  0.014426461
#>           zip_restricted       0 0.14020239 0.12768422 -0.012518172
#>  age_policyholder_smooth (29,39] 0.14773069 0.15443992  0.006709228
#>  age_policyholder_smooth [18,29] 0.22425297 0.21416658 -0.010086392
#>           zip_restricted       2 0.12951920 0.12419468 -0.005324521
#>           zip_restricted       1 0.14376442 0.13785920 -0.005905227
#>  age_policyholder_smooth (49,59] 0.11399379 0.11820568  0.004211889
#>  age_policyholder_smooth (59,69] 0.10160625 0.09812401 -0.003482235
#>  age_policyholder_smooth (69,79] 0.09086268 0.09384845  0.002985770
#>  change_ratio
#>   -0.14110328
#>    0.10545444
#>   -0.08928644
#>    0.04541526
#>   -0.04497774
#>   -0.04110990
#>   -0.04107572
#>    0.03694841
#>   -0.03427186
#>    0.03286025
```

This audit uses `refined_model`, before the additional 5% intercept
adjustment. It reports the portfolio-level frequency before and after
refinement and the corresponding changes by final risk-factor level.
These are predictions from the complete model for the observed portfolio
mix, not isolated coefficient differences.

The refinement and audit objects record package version, timestamps,
formulas and ordered steps. This supports reproducibility, peer review
and documentation of actuarial judgement. Organisational approval,
source-data versioning and formal governance remain outside the package
and should be handled by the user’s normal processes.

## When to revisit the model

Refinement is not a substitute for correcting a poor model. If an
implausible effect is caused by incorrect data, exposure errors, missing
interactions, an inappropriate response definition or poor factor
construction, the model or data preparation should be revisited first.
Refinement is most defensible when the statistical model captures the
main risk structure and the remaining adjustment has a clear tariff
rationale.

## Iterating without losing the specification

Retain both the editable specification and the fitted result:

``` r

refined_model <- refit(refinement)

refinement <- refinement |>
  edit_smoothing(
    model_variable = "age_band",
    from = 32,
    to = 65,
    adjustment = 1.03,
    transition = "linear"
  )

updated_model <- refit(refinement)
```

This appends an edit to the current smoothing. Earlier edits to that
curve remain in effect. To compare an alternative instead of building on
the previous edit, start a separate branch from the saved
`initial_age_refinement`.

Calling
[`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md),
[`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
or another refinement function directly on `refined_model` is
deliberately not supported. Further adjustments belong on the retained
`rating_refinement` object so their ordering and origin remain visible.

## Summary

[`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md)
sets up a specification for proposed adjustments to a fitted model. Use
the techniques that address the tariff question: a single restriction
may be enough, and smoothing or shrinkage need not be part of the same
proposal. [`summary()`](https://rdrr.io/r/base/summary.html) and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
help inspect the proposal as it develops.
[`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
fits the model under that specification;
[`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
and
[`audit_refinement()`](https://mharinga.github.io/insurancerating/reference/audit_refinement.md)
help review the fitted tariff and its portfolio effect. This keeps
proposed changes separate from fitted results and from any decision to
adopt the tariff.

## Where to go next

- [Getting
  Started](https://mharinga.github.io/insurancerating/articles/getting-started.md)
  constructs the initial pricing model used as the starting point for
  refinement.
- [Pricing workflow and package building
  blocks](https://mharinga.github.io/insurancerating/articles/pricing-workflow-building-blocks.md)
  places refinement within the wider package architecture.
- [Model
  validation](https://mharinga.github.io/insurancerating/articles/model-validation.md)
  develops the diagnostics used to assess unrestricted and refined
  models.
