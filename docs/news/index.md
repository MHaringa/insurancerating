# Changelog

## insurancerating 0.8.2

### Changes since 0.8.1

- [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  gains `estimate_name` for assigning exact names to fitted coefficient
  or relativity columns. It accepts one name for one model, a vector in
  model order, or a named vector for multi-model comparisons;
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  and
  [`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)
  retain the custom columns.
- [`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md)
  gains `slope_adjustment`, using `from` as its anchor, for changing the
  remaining development of an effective smoothing after a selected
  anchor. Values before the anchor remain unchanged and the edited curve
  is continuous at the anchor. The default `slope_adjustment = 1` leaves
  the smoothing unchanged.
- [`calibrate_model()`](https://mharinga.github.io/insurancerating/reference/calibrate_model.md)
  adds a final multiplicative calibration step for refined log-link
  GLMs. It changes only the overall fitted level, records the applied
  factor and intercept shift, and remains compatible with
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  and
  [`add_prediction()`](https://mharinga.github.io/insurancerating/reference/add_prediction.md).
- [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  now warns when a newly added numeric interval overlaps the existing
  interval classification. The message identifies the conflicting levels
  and explains how a separate replacement variable can be supplied with
  `replaces` when the intention is to replace the original
  classification.
- [`autoplot.rating_refinement()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_refinement.md)
  gains `show_segments = FALSE` for inspecting a continuous smoothing
  curve without displaying the proposed tariff segments.
- [`premium_change()`](https://mharinga.github.io/insurancerating/reference/premium_change.md)
  interprets the effective smoothing in a refinement using either the
  existing doubling comparison or a fixed increment supplied with
  `increment`. Current and historical refinement states can be selected
  with `steps` and compared with print and
  [`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)
  methods. The default `basis = "curve"` evaluates the continuous
  effective smoothing; `basis = "segments"` compares the implemented
  tariff-interval relativities.
- [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  gains `replaces`, which allows a new fixed tariff factor to explicitly
  replace an existing standalone model term during
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).
  The replacement is retained in refinement summaries and audits. The
  default `replaces = NULL` preserves the existing additive behaviour
  for new factors.
- [`rating_grid_db()`](https://mharinga.github.io/insurancerating/reference/rating_grid_db.md)
  and
  [`merge_date_ranges_db()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges_db.md)
  add lazy database variants for portfolios that should be reduced
  before they are imported into R.
  [`rating_grid_db()`](https://mharinga.github.io/insurancerating/reference/rating_grid_db.md)
  translates grouped counts and sums through `dbplyr`, while
  [`merge_date_ranges_db()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges_db.md)
  implements temporal gaps-and-islands reduction in DuckDB. A new
  large-portfolio vignette compares local and database workflows and
  illustrates lazy reduction of portfolios with 10 and 50 million
  records.
- [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
  and
  [`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
  are now documented together as portfolio-reduction functions. Both use
  grouped `data.table` operations internally and return ordinary
  `data.frame` objects for downstream use.
- [`split_periods_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md)
  and
  [`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)
  are now documented as portfolio time operations. Their calculations
  use local `data.table` objects internally, while their public output
  is always a regular `data.frame`.
  [`split_periods_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md)
  now uses `data` as its first argument (`df` remains available as a
  deprecated named argument) and expands periods without a row-wise R
  loop.
  [`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)
  replaces the technical `nomatch` and `mult` arguments with `unmatched`
  and `multiple_matches`; the old names remain available with
  deprecation warnings.
- [`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
  now uses `merge_gap_days = 1` by default, so only overlapping and
  directly adjacent coverage periods are merged unless a wider
  administrative gap is selected explicitly. The interval reduction is
  now performed as a grouped `data.table` calculation, avoiding row-wise
  R loops for large portfolios. Empty inputs, nested intervals and
  aggregation functions without a `na.rm` argument are handled
  explicitly. The deprecated
  [`reduce()`](https://mharinga.github.io/insurancerating/reference/reduce.md)
  wrapper retains its historical default for compatibility. The first
  argument is now named `data` (`df` remains available as a deprecated
  named argument), and the result is a regular `data.frame` subclass
  while `data.table` remains an internal implementation detail. Together
  with
  [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md),
  the function is documented as part of the portfolio reduction
  workflow.
- [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  now distinguishes structural and comparative level order. Pure numeric
  levels and numeric intervals are ordered from low to high, explicitly
  ordered factors retain their declared level sequence, and nominal
  factors are ordered from the highest to the lowest fitted effect by
  default. Numeric and ordinal order are not displaced by the fitted
  reference level. The new `level_order_by_risk_factor` argument can
  preserve model order or select alphabetical or estimate-based ordering
  for individual factors. Risk factors can retain formula order or be
  sorted alphabetically, and comparisons with several models can select
  the model that determines the estimate ordering. The resulting order
  is retained by
  [`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)
  and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
- [`add_rebasing()`](https://mharinga.github.io/insurancerating/reference/add_rebasing.md)
  can rescale the current relativities of a categorical tariff factor so
  that an explicit reference level equals 1. When no level is supplied,
  the level with the largest portfolio weight is selected. Rebasing
  preserves every ratio between factor levels and can be applied after
  restrictions, shrinkage or sublevel relativities.
- [`add_shrinkage()`](https://mharinga.github.io/insurancerating/reference/add_shrinkage.md)
  can reduce differences between categorical tariff relativities before
  refitting. Relativities are blended with a weighted centre on the log
  scale and rescaled to preserve their weighted arithmetic mean.
  Explicit GLM weights or a simple exposure offset are detected when
  possible; a column name or `weights = "equal"` can be supplied
  directly. The selected credibility, weighting basis and before/after
  relativities are retained in the ordered refinement specification.
- [`audit_refinement()`](https://mharinga.github.io/insurancerating/reference/audit_refinement.md)
  now provides a reproducible comparison between the unrestricted and
  refined GLM on common observed model points. It records the package
  version, formulas and ordered refinement steps, and reports the
  portfolio and exposure-weighted level effects before and after
  refinement. [`summary()`](https://rdrr.io/r/base/summary.html) on a
  pre-refit `rating_refinement` object now gives a structured review of
  the proposed specification, while
  [`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)
  formats the fitted audit for reporting.
- [`bootstrap_coefficients()`](https://mharinga.github.io/insurancerating/reference/bootstrap_coefficients.md)
  now assesses GLM coefficient stability by resampling the estimation
  portfolio rows and refitting the model. Individual failed fits are
  retained as missing replicates instead of stopping the full run.
  [`summary()`](https://rdrr.io/r/base/summary.html) reports link-scale
  or exponentiated results, with `"relativity"` available as an alias,
  and
  [`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)
  provides a formatted coefficient table.
- The function documentation, README and vignettes now use a consistent,
  practical actuarial style. The documentation distinguishes observed
  portfolio experience, fitted model effects, explicit tariff
  assumptions and model diagnostics, and states relevant interpretation
  limits more clearly. Experimental documentation badges have been
  removed, and the low-level
  [`rmse()`](https://mharinga.github.io/insurancerating/reference/rmse.md)
  helper is listed with the package utilities in the reference index.
- [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  keeps `"spline"` as its general-purpose default and fits it as an
  unconstrained penalized cubic regression spline. Polynomial smoothing
  is selected explicitly with `"poly"`, while `"gam"` provides a
  thin-plate GAM comparison. `degree` is limited to polynomial fits, `k`
  is limited to spline methods, and an omitted `k` is adapted to the
  available grouped model points. Its arguments now follow the modelling
  sequence of variables, required `breaks`, smoothing method, complexity
  and weights. Shape-constrained methods now use readable values such as
  `"increasing"`, `"decreasing"`, `"increasing_convex"` and
  `"increasing_concave"`; the former short SCOP codes remain accepted as
  compatibility aliases.
  [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  now focuses only on the initial smoothing and its structural shape.
  The former global strength adjustment has been removed.
  [`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md)
  now supports a multiplicative local `adjustment`, such as `1.05` for
  an increase of up to 5 percent, together with continuous inherited or
  linear transitions and an explicit discontinuous `"step"` transition.
  Relative adjustments are anchored to the unchanged smoothing at both
  interval boundaries and remain separate from explicit target-value
  edits. Each
  [`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md)
  call is stored as a separate cumulative refinement step. In addition,
  [`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md)
  now supports `slope_adjustment`, with `from` as its anchor. This
  leaves the curve unchanged through the anchor and scales only its
  remaining change, allowing a continuous effect to become steeper or
  flatter above a selected source-variable value. In
  [`autoplot.rating_refinement()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_refinement.md),
  `step` selects the cumulative smoothing state and
  `show_initial_smoothing = TRUE` overlays the original
  [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  curve for comparison.
- [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  recognises a split variable created by an earlier
  [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
  step without requiring `allow_new_risk_factors = TRUE`. A partial
  restriction changes the supplied levels and fixes all remaining levels
  at their current derived relativities.
- [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
  now validates sublevels against the observed `split_variable` and
  parent categories against `model_variable` before storing a refinement
  step. Closely matching observed values are suggested for likely
  spelling or spacing errors. The resulting hybrid tariff factor is now
  named with `output_variable`; its default is the model variable
  followed by `_refined`, while application-specific names such as
  `sbi_tariff_segment` can be supplied explicitly.
- [`split_level()`](https://mharinga.github.io/insurancerating/reference/relativity_specification.md)
  and
  [`relativities()`](https://mharinga.github.io/insurancerating/reference/relativity_specification.md)
  are now the primary helpers for defining named sublevel splits and
  share one reference page describing their combined use. The low-level
  [`split_relativities()`](https://mharinga.github.io/insurancerating/reference/split_relativities.md)
  constructor is deprecated because it does not record the parent model
  level required by
  [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md).
- Spline, GAM and shape-constrained smoothing validate the requested
  basis dimension against the number of unique grouped covariate values
  before model fitting.
- [`autoplot.rating_table()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_table.md)
  and
  [`autoplot.factor_analysis()`](https://mharinga.github.io/insurancerating/reference/autoplot.factor_analysis.md)
  abbreviate long risk-factor level labels by default. Shortened labels
  use one terminal period, can be disabled, and can be replaced by
  explicitly supplied display labels. Both methods now also provide
  `legend_position` for consistent legend placement.
- [`autoplot.riskfactor_gam()`](https://mharinga.github.io/insurancerating/reference/autoplot.tariff_effect.md)
  and
  [`autoplot.tariff_segments()`](https://mharinga.github.io/insurancerating/reference/autoplot.tariff_effect.md)
  now use one shared plotting implementation and are documented on one
  reference page. Segment plots retain their boundaries by default and
  can show the underlying GAM curve alone with `show_segments = FALSE`.
- [`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md)
  now uses `segmentation_penalty` as its primary split-control argument;
  `complexity` remains available as a deprecated alias. The method
  divides the fitted GAM curve without applying a second exposure or
  claim-volume weight; these quantities have already informed the GAM
  estimate and remain available through
  [`summary()`](https://rdrr.io/r/base/summary.html) as diagnostics.
  Validation now covers malformed GAM objects, non-finite values and
  whole-number search controls. Tree splits are read from the fitted
  tree structure rather than parsed from printed rules.
  [`add_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/add_tariff_segments.md)
  reapplies stored boundaries to the continuous risk factor, so
  filtering or reordering rows no longer misaligns assignments and
  unsupported values outside the fitted range fail explicitly.

## insurancerating 0.8.1

CRAN release: 2026-07-30

### Overview of changes since 0.8.0

- The excess-loss workflow has been made more portfolio-oriented and
  easier to audit. Threshold assessments now preserve input column
  names, support portfolio-level claim counts and can be presented with
  [`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md).
  [`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md)
  now combines capping and redistribution in one step and returns
  adjusted claim amounts for severity modelling.
- Large-loss cost is redistributed over claim-bearing rows using
  portfolio, risk-factor or partial credibility redistribution. The
  total observed claim cost is preserved. Users can include the
  allocation in one severity response or retain it as a separate
  excess-loading component.
- [`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md)
  is now the primary helper for attaching observed portfolio experience
  to
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  objects. The former
  [`add_observed_experience()`](https://mharinga.github.io/insurancerating/reference/add_observed_experience.md)
  interface remains available as a deprecated compatibility wrapper.
- The documentation for
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md),
  [`add_prediction()`](https://mharinga.github.io/insurancerating/reference/add_prediction.md)
  and the excess-loss workflow has been expanded with more applied
  portfolio examples. The pkgdown configuration and website build
  workflow have also been updated.

### Main API updates

- [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  now stops early with the affected column and missing-value count when
  `model_variable` contains `NA`.
  [`add_prediction()`](https://mharinga.github.io/insurancerating/reference/add_prediction.md)
  preserves `NA` predictions and reports their number in a warning.
- [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  and deprecated
  [`restrict_coef()`](https://mharinga.github.io/insurancerating/reference/restrict_coef.md)
  can include tariff levels that were not observed when the GLM was
  fitted. Their supplied relativities are treated as explicit tariff
  assumptions. Set `allow_new_levels = FALSE` for strict matching
  against observed levels.
- [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  can add a fixed, expert-specified tariff factor that is present in the
  refinement data but absent from the fitted GLM. This requires the
  explicit opt-in `allow_new_risk_factors = TRUE`; the default remains
  `FALSE`. Deprecated
  [`restrict_coef()`](https://mharinga.github.io/insurancerating/reference/restrict_coef.md)
  keeps its historical permissive behaviour and therefore uses `TRUE` by
  default.
- Repeated
  [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  calls for the same risk factor now update the existing restriction in
  place. Newly supplied relativities replace earlier values for those
  levels, while restrictions for other levels and the refinement-step
  order are retained.
- Models returned by
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
  now print a concise refinement summary followed by the regular GLM
  output. The stored call is shown as a readable `glm(...)` call instead
  of the internal function definition.
- [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
  now uses coefficients from an earlier
  [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  step automatically. The requested model variable and the effective
  restricted variable remain separately recorded, restrictions are not
  applied twice, and refinement-step ordering remains explicit. The
  final
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  reports only the derived split variable rather than also showing the
  intermediate restricted variable.
- Refinement documentation now distinguishes the editable
  `rating_refinement` specification from the fitted GLM returned by
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).
  Functions that add or edit refinement steps reject ordinary and
  refitted GLMs with an actionable error. Iterative smoothing therefore
  retains the refinement specification and calls
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md)
  after each adjustment.
- The
  [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
  example now demonstrates that several original model levels can be
  refined in one
  [`relativities()`](https://mharinga.github.io/insurancerating/reference/relativity_specification.md)
  specification by using multiple
  [`split_level()`](https://mharinga.github.io/insurancerating/reference/relativity_specification.md)
  calls.
- Deprecated
  [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)
  again preserves the dynamic estimated-column name produced by
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md),
  and deprecated
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/fit_gam.md)
  correctly forwards a user-supplied `amount` column.

#### Rating tables

- [`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md)
  is now the primary API for enriching a
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  object with observed portfolio experience.
- [`add_observed_experience()`](https://mharinga.github.io/insurancerating/reference/add_observed_experience.md)
  is deprecated and remains available as a compatibility wrapper.
- [`add_portfolio_experience()`](https://mharinga.github.io/insurancerating/reference/add_portfolio_experience.md)
  can calculate portfolio experience automatically from portfolio data
  for all risk factors in a
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md),
  and accepts multiple existing
  [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  objects.
- [`autoplot.rating_table()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_table.md)
  accepts `metric` to choose the attached portfolio experience metric at
  plot time.

#### Excess-loss workflow

- [`assess_excess_threshold()`](https://mharinga.github.io/insurancerating/reference/assess_excess_threshold.md)
  now returns class `"threshold_assessment"`, preserves the original
  group and exposure column names, supports an optional claim-count
  column and uses
  [`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)
  for report-ready threshold comparisons.
- [`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md)
  replaces the former multi-step decomposition, allocation and loading
  workflow. It returns capped, excess, redistributed, adjusted and
  adjusted-average claim amounts using names derived from the supplied
  claim-amount column.
- Redistribution is claim-count weighted. Rows without claims remain
  zero, and the adjusted loss reconciles to the original observed loss.
- [`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md)
  now accepts `redistribution_weight` for risk-sensitive shares and
  `receives_redistribution` for selecting which claim-bearing rows
  receive redistributed excess loss.
- [`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md)
  can now identify large losses that should remain unadjusted and should
  not contribute their excess to the redistribution pool.
- The redistribution choice is now expressed through
  `redistribution_method = "portfolio"`, `"risk_factor"` or `"partial"`.
- Results now inherit from `"excess_redistribution"`. Their
  [`summary()`](https://rdrr.io/r/base/summary.html) method audits
  contributed and received excess loss, net loss shifts and changes in
  average claim amount by risk factor or another selected portfolio
  column.
- [`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md)
  now exposes the risk-factor loading, credibility, portfolio loading,
  blended loading, preservation factor and final redistribution loading.
  Set `calculation_details = FALSE` to omit these row-level audit
  columns from modelling data;
  [`summary()`](https://rdrr.io/r/base/summary.html) retains the
  complete calculation audit.
- [`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md)
  now supports `output = "excess_loading"` in addition to the
  backwards-compatible `"redistributed_claim"` default. The new mode
  keeps capped severity separate and returns allocated excess loss plus
  an excess loading per unit of `redistribution_weight`.

## insurancerating 0.8.0

CRAN release: 2026-06-02

### Main API updates

#### Portfolio analysis

- [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  is now the primary function for univariate/factor-level portfolio
  analysis. It returns objects with primary class `"factor_analysis"`
  while retaining `"univariate"` for compatibility.
- [`plot_severity_distribution()`](https://mharinga.github.io/insurancerating/reference/plot_severity_distribution.md)
  was added for exploratory severity diagnostics by category. It shows
  individual claim observations with mean and median markers, optional
  direct labels, and optional firebrick highlighting for claims above a
  user-supplied threshold.
- [`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md)
  is deprecated and remains available as a compatibility wrapper. The
  old NSE interface is still supported through the deprecated wrapper.
- [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  now validates metric columns and grouping variables early, with
  clearer error messages for missing columns.
- Metrics with a zero or missing denominator now return `NA_real_`
  instead of `Inf` or `NaN`.
- [`autoplot.factor_analysis()`](https://mharinga.github.io/insurancerating/reference/autoplot.factor_analysis.md)
  is the primary plot method. The deprecated `show_plots` argument has
  been replaced by `metrics`.
- The factor-analysis plot keeps the established package styling and now
  uses a consistent grid, axis-line, tick and secondary-axis style.

#### Outlier histograms

- [`outlier_histogram()`](https://mharinga.github.io/insurancerating/reference/outlier_histogram.md)
  has clearer argument names: `lower`, `upper`, `density`, `bar_fill`,
  `bar_color`, `tail_fill`, `tail_color`, and `density_color`.
- Deprecated argument names remain supported: `left`, `right`, `line`,
  `fill`, `color`, and `fill_outliers`.
- The default colours now align with the package palette: light grey
  bars, orange tail bins, and blue density curve.
- Input validation has been expanded for missing columns, non-numeric
  variables, invalid cutoffs, invalid colours, invalid bin counts,
  constant variables and all-missing variables.
- [`histbin()`](https://mharinga.github.io/insurancerating/reference/histbin.md)
  is deprecated and remains available as a compatibility wrapper.

#### Risk factor GAMs and tariff segmentation

- [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
  is the primary spelling for fitting GAMs to continuous risk factors.
  [`riskfactor_gam()`](https://mharinga.github.io/insurancerating/reference/riskfactor_gam.md)
  and
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/fit_gam.md)
  remain available for compatibility, with
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/fit_gam.md)
  deprecated.
- [`risk_factor_gam()`](https://mharinga.github.io/insurancerating/reference/risk_factor_gam.md)
  returns objects with primary class `"risk_factor_gam"`; compatibility
  classes are retained for older code.
- `model = "pure_premium"` replaces the older `model = "burning"`
  wording. The old value remains supported with a lifecycle warning.
- Input validation and documentation for frequency, severity and
  pure-premium GAMs have been improved.
- [`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md)
  replaces
  [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md)
  as the primary API for deriving tariff segments from a fitted
  risk-factor GAM.
- [`derive_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/derive_tariff_segments.md)
  returns objects with primary class `"tariff_segments"`.
- [`add_tariff_segments()`](https://mharinga.github.io/insurancerating/reference/add_tariff_segments.md)
  can add derived tariff segments back to a portfolio.
- [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md)
  remains available as a deprecated compatibility wrapper.
- Split extraction now handles decimal split points correctly.
- Tree-fitting and split-extraction failures now fail clearly instead of
  silently returning one broad tariff interval.
- The tariff-segment plot now recognises confidence interval columns
  produced by the package’s own GAM output.

#### Rating tables

- [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  is the primary API for interpreting fitted GLM coefficients in
  tariff-table form.
  [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)
  and
  [`rating_factors2()`](https://mharinga.github.io/insurancerating/reference/rating_factors2.md)
  are deprecated wrappers.
- [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  now returns objects with primary class `"rating_table"` while
  retaining the legacy `"riskfactor"` class for compatibility.
- `exposure_output` replaces the older `exposure_name` argument.
- `significance` replaces the older `signif_stars` argument.
- Deprecated rating-table wrappers and plotting code have been separated
  more clearly in the source structure.
- [`add_observed_experience()`](https://mharinga.github.io/insurancerating/reference/add_observed_experience.md)
  was added to attach
  [`factor_analysis()`](https://mharinga.github.io/insurancerating/reference/factor_analysis.md)
  output to a
  [`rating_table()`](https://mharinga.github.io/insurancerating/reference/rating_table.md)
  object before plotting. This replaces the earlier direct
  `univariate_*` arguments in
  [`autoplot.rating_table()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_table.md).
- [`autoplot.rating_table()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_table.md)
  now plots attached observed experience from
  [`add_observed_experience()`](https://mharinga.github.io/insurancerating/reference/add_observed_experience.md)
  and uses cleaner, package-consistent plot styling, including a subtle
  secondary exposure axis.

#### Prediction helpers

- [`add_prediction()`](https://mharinga.github.io/insurancerating/reference/add_prediction.md)
  now has clearer naming arguments: `predictions`, `prefix`,
  `confidence`, and `interval_names`.
- `var` and `conf_int` are deprecated in favour of `predictions` and
  `confidence`.
- Confidence interval columns now use `_lower` and `_upper` suffixes by
  default.
- The function now validates `alpha`, confidence settings, duplicate
  output names, name collisions with existing columns, missing models
  and non-GLM inputs.

#### Model data and rating grids

- [`extract_model_data()`](https://mharinga.github.io/insurancerating/reference/extract_model_data.md)
  replaces
  [`model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md)
  as the primary API for extracting model data from fitted models.
- [`model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md)
  is deprecated and remains available as a wrapper.
- [`rating_grid()`](https://mharinga.github.io/insurancerating/reference/rating_grid.md)
  now uses base R internally and returns a regular `data.frame`.
- Plain GLM metadata extraction has been improved so `rating_grid(glm)`
  groups by model terms as expected.
- [`as_gt()`](https://mharinga.github.io/insurancerating/reference/as_gt.md)
  now formats `rating_table` objects as grouped tariff tables, with
  optional significance stars and locale-aware formatting for fitted
  effects and exposure.
- Refinement metadata is now joined by the related original/new factor
  columns instead of being cross-joined onto every rating-grid row.

#### Model refinement

- The refinement API has been clarified around
  `prepare_refinement() |> add_*() |> refit()`.
- [`prepare_refinement()`](https://mharinga.github.io/insurancerating/reference/prepare_refinement.md)
  now explains row-count differences caused by omitted model
  observations. Its error identifies missing or non-finite model inputs,
  including the source variables used inside transformed model terms.
- A large-loss workflow was added for adjusted severity modelling:
  [`assess_excess_threshold()`](https://mharinga.github.io/insurancerating/reference/assess_excess_threshold.md)
  and
  [`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md).
- [`assess_excess_threshold()`](https://mharinga.github.io/insurancerating/reference/assess_excess_threshold.md)
  compares candidate large-loss thresholds and shows the impact on
  excess loss, capped loss and pure premium.
- [`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md)
  caps claim amounts and redistributes the observed excess burden across
  claims before a severity GLM is fitted. It supports portfolio,
  risk-factor and partial redistribution.
- Automatic credibility in
  [`redistribute_excess_loss()`](https://mharinga.github.io/insurancerating/reference/redistribute_excess_loss.md)
  uses the transparent formula `Z = n / (n + credibility_threshold)`
  with `credibility_basis = "claims"` or `"excess_records"`.
- Redistribution always preserves the total observed claim cost and
  returns an adjusted average claim amount suitable for a
  claim-count-weighted severity GLM.
- [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  now uses `model_variable` and `source_variable` as the primary
  argument names.
- GAM and shape-constrained smoothing now validate the requested basis
  dimension against the number of unique grouped covariate values before
  model fitting. Polynomial smoothing similarly validates `degree`, with
  actionable errors when the requested curve is too complex for the
  available model levels.
- [`autoplot.rating_refinement()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_refinement.md)
  accepts `x_max` and `y_max` for smoothing steps, allowing users to
  limit the visible axis ranges without changing the smoothing fit or
  the underlying refinement data.
- [`edit_smoothing()`](https://mharinga.github.io/insurancerating/reference/edit_smoothing.md)
  now uses clearer in-object editing arguments for adjusting smoothing
  settings without supplying an external data frame.
- [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md)
  can now accept a partial restriction data frame. Missing levels are
  automatically filled with the already fitted GLM relativities, so
  users can adjust only selected levels.
- [`add_relativities()`](https://mharinga.github.io/insurancerating/reference/add_relativities.md)
  now uses `model_variable` and `split_variable`.
- [`relativities()`](https://mharinga.github.io/insurancerating/reference/relativity_specification.md)
  replaces `relativities_list()` as the helper for building relativity
  specifications.
- [`restrict_coef()`](https://mharinga.github.io/insurancerating/reference/restrict_coef.md),
  [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)
  and
  [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)
  remain deprecated compatibility wrappers and now link clearly to
  [`add_restriction()`](https://mharinga.github.io/insurancerating/reference/add_restriction.md),
  [`add_smoothing()`](https://mharinga.github.io/insurancerating/reference/add_smoothing.md)
  and
  [`refit()`](https://mharinga.github.io/insurancerating/reference/refit.md).
- [`autoplot.rating_refinement()`](https://mharinga.github.io/insurancerating/reference/autoplot.rating_refinement.md)
  no longer carries an experimental badge and uses the package plot
  theme.
- The refinement documentation has been expanded with applied examples
  and a clearer explanation of smoothing, restrictions, relativities and
  refitting.

#### Reference levels

- [`set_reference_level()`](https://mharinga.github.io/insurancerating/reference/set_reference_level.md)
  replaces
  [`biggest_reference()`](https://mharinga.github.io/insurancerating/reference/biggest_reference.md)
  as the primary helper for choosing factor reference levels.
- The default method is `method = "largest_weight"`.
- A manual `level` argument was added so a specific reference level can
  be selected explicitly.
- [`biggest_reference()`](https://mharinga.github.io/insurancerating/reference/biggest_reference.md)
  remains available as a deprecated compatibility wrapper.

#### Time utilities

- [`split_periods_to_months()`](https://mharinga.github.io/insurancerating/reference/split_periods_to_months.md),
  [`merge_date_ranges()`](https://mharinga.github.io/insurancerating/reference/merge_date_ranges.md)
  and
  [`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)
  now avoid mutating caller-visible input data.
- [`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)
  replaces
  [`rows_per_date()`](https://mharinga.github.io/insurancerating/reference/rows_per_date.md)
  as the primary API for matching event dates, such as claim dates, to
  active portfolio rows.
- [`period_to_months()`](https://mharinga.github.io/insurancerating/reference/period_to_months.md),
  [`rows_per_date()`](https://mharinga.github.io/insurancerating/reference/rows_per_date.md)
  and
  [`reduce()`](https://mharinga.github.io/insurancerating/reference/reduce.md)
  remain available as deprecated compatibility wrappers.
- Date interval validation, column validation, aggregation validation,
  `nomatch` validation and `mult` validation have been improved.
- R CMD check notes from data.table helper columns in
  [`active_rows_by_date()`](https://mharinga.github.io/insurancerating/reference/active_rows_by_date.md)
  have been resolved.

#### Model validation and performance

- [`bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/bootstrap_performance.md)
  now has an explicit `metric = "rmse"` argument.
- `sampling = c("bootstrap", "split")` was added to distinguish
  bootstrap out-of-bag evaluation from split validation.
- Deprecated arguments `n` and `frac` remain supported as aliases for
  `n_resamples` and `sample_fraction`.
- Character and factor rating variables are handled more robustly across
  resamples so prediction does not fail when a level is absent from a
  training sample.
- [`autoplot.bootstrap_performance()`](https://mharinga.github.io/insurancerating/reference/autoplot.bootstrap_performance.md)
  now uses a package-consistent visual style: subtle grey histogram,
  transparent blue density, orange original-model reference line, subtle
  confidence interval lines and no gap between the bars and x-axis.
- [`bootstrap_rmse()`](https://mharinga.github.io/insurancerating/reference/bootstrap_rmse.md)
  remains available as a deprecated compatibility wrapper and returned
  objects retain class `"bootstrap_rmse"` for older code.
- [`check_overdispersion()`](https://mharinga.github.io/insurancerating/reference/check_overdispersion.md)
  now validates non-GLM input, checks for Poisson models and fails
  clearly when residual degrees of freedom are not positive.
- `print.overdispersion()` now bases its conclusion on the original
  p-value rather than a rounded display value.
- [`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md)
  now validates inputs, uses all scaled residuals for the KS test,
  handles empty residual vectors clearly and documents the DHARMa-based
  residual workflow for actuarial users.
- [`autoplot.check_residuals()`](https://mharinga.github.io/insurancerating/reference/autoplot.check_residuals.md)
  now has a controllable `max_points` argument and uses ASCII messages
  and the package plot theme.

#### Truncated severity distributions

- [`fit_truncated_severity()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_severity.md)
  replaces
  [`fit_truncated_dist()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_dist.md)
  as the primary API for fitting distributions to truncated claim
  severities.
- Returned objects use primary class `"truncated_severity"` while
  compatibility with `"truncated_dist"` is retained.
- [`fit_truncated_dist()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_dist.md)
  remains available as a deprecated compatibility wrapper.
- Observations outside the truncation interval now fail clearly instead
  of only warning and continuing.
- Validation has been expanded for truncation bounds, optimisation
  starts, grid sizes, reporting options and random generator arguments.
- Public random generators
  [`rlnormt()`](https://mharinga.github.io/insurancerating/reference/rlnormt.md)
  and
  [`rgammat()`](https://mharinga.github.io/insurancerating/reference/rgammat.md)
  now validate sample size, distribution parameters, finite intervals
  and positive truncation mass.
- Plot argument names were modernised to `ecdf_geom`, `x_label`,
  `y_label`, `show_title`, `digits` and `truncation_digits`, with old
  names supported for compatibility.

#### Fisher-Jenks classification

- [`fisher_classify()`](https://mharinga.github.io/insurancerating/reference/fisher_classify.md)
  and
  [`fisher()`](https://mharinga.github.io/insurancerating/reference/fisher.md)
  are deprecated because Fisher-Jenks classification is a
  general-purpose grouping method and is not directly tied to the
  insurance-rating workflow.
- `classInt` moved from `Imports` to `Suggests`.

### Documentation, website and tests

- The README and vignettes have been revised to present the package as a
  set of actuarial pricing building blocks rather than a prescribed
  pricing method.
- The former “Pricing principles” vignette was replaced by “Pricing
  workflow building blocks”.
- The refinement vignette was rewritten with a more practical tone and
  current API examples.
- pkgdown reference sections were reorganised; deprecated functions are
  grouped under “Deprecated” and internal S3 methods are no longer
  listed as primary reference topics.
- New and expanded tests cover tariff segmentation, rating tables,
  observed experience plotting, refinement workflows, model-data
  extraction, model performance, overdispersion, residual checks,
  outlier histograms, truncated distributions, time utilities and factor
  analysis.

## insurancerating 0.7.5

CRAN release: 2024-10-09

- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)
  now always returns correct output when column with exposure in data is
  not named `exposure`
- `intercept_only` in
  [`update_glm()`](https://mharinga.github.io/insurancerating/reference/update_glm.md)
  is added to apply the manual changes and refit the intercept, ensuring
  that the changes have no impact on the other variables.
- `smoothing` in
  [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)
  is added to choose smoothing specification
- The README has been revised

## insurancerating 0.7.4

CRAN release: 2024-05-20

- [`bootstrap_rmse()`](https://mharinga.github.io/insurancerating/reference/bootstrap_rmse.md)
  now uses `after_stat(density)` instead of the deprecated dot-dot
  notation
- `custom_theme` in `autoplot.univariate()` is added to customize the
  theme

## insurancerating 0.7.3

CRAN release: 2024-05-09

- `autoplot.univariate()` now generates a plot even when there are
  missing values in the rows
- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)
  now always returns the correct coefficients when used on a
  ‘refitsmooth’ or ‘refitrestricted’ class of GLM.

## insurancerating 0.7.2

CRAN release: 2022-12-20

- [`update_glm()`](https://mharinga.github.io/insurancerating/reference/update_glm.md)
  now always returns the correct interval in case the function is used
  in combination with
  [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)

## insurancerating 0.7.1

CRAN release: 2022-09-06

- `rotate_angle` in `autoplot.univariate()` is added to rotate x-labels
- [`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md)
  now accepts external vectors for `x`; `vec_ext()` must be used

## insurancerating 0.7.0

CRAN release: 2022-07-08

- [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)
  now gives correct results for intervals with scientific notation
- [`reduce()`](https://mharinga.github.io/insurancerating/reference/reduce.md)
  now returns no errors anymore for columns with dates in POSIXt format

## insurancerating 0.6.9

CRAN release: 2021-12-11

- [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)
  is renamed to
  [`update_glm()`](https://mharinga.github.io/insurancerating/reference/update_glm.md)
- [`construct_model_points()`](https://mharinga.github.io/insurancerating/reference/construct_model_points.md)
  and
  [`model_data()`](https://mharinga.github.io/insurancerating/reference/model_data.md)
  are added to create model points

## insurancerating 0.6.8

CRAN release: 2021-11-10

- `show_total` in `autoplot.univariate()` is added to add line for total
  of groups in case `by` is used in
  [`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md);
  `total_color` can be used to change the color of the line, and
  `total_name` is added to change the name of the legend for the line
- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)
  now accepts GLMs with an intercept only
- [`fit_truncated_dist()`](https://mharinga.github.io/insurancerating/reference/fit_truncated_dist.md)
  is added to fit the original distribution (gamma, lognormal) from
  truncated severity data
- `join_to_nearest()` now returns NA in case NA is used as input

## insurancerating 0.6.7

CRAN release: 2021-07-28

- [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)
  now returns an error message when intervals are not obtained by cut()
- `get_data()` is added to return the data used in
  [`refit_glm()`](https://mharinga.github.io/insurancerating/reference/refit_glm.md)

## insurancerating 0.6.6

CRAN release: 2021-05-19

- `summary.reduce()` now gives correct aggregation for periods “months”
  and “quarters”
- [`rows_per_date()`](https://mharinga.github.io/insurancerating/reference/rows_per_date.md)
  is added to determine active portfolio for a certain date

## insurancerating 0.6.5

CRAN release: 2021-03-22

- [`smooth_coef()`](https://mharinga.github.io/insurancerating/reference/smooth_coef.md)
  and
  [`restrict_coef()`](https://mharinga.github.io/insurancerating/reference/restrict_coef.md)
  are added for model refinement
- [`histbin()`](https://mharinga.github.io/insurancerating/reference/histbin.md)
  now uses darkblue as default fill color

## insurancerating 0.6.4

CRAN release: 2021-01-12

- In `summary.reduce()`, `name` can be used to change the name of the
  new column in the output.
- Dataset `MTPL` now contains extra columns for `power`, `bm`, and
  `zip`.
- Some functions in `insight` are renamed, therefore
  `insight::format_table()` is replaced with `insight::export_table()`.

## insurancerating 0.6.3

CRAN release: 2020-10-28

- [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/fit_gam.md)
  for pure premium is now using average premium for each x calculated as
  sum(pure_premium \* exposure) / sum(exposure) instead of
  sum(pure_premium) / sum(exposure)
  ([\#2](https://github.com/MHaringa/insurancerating/issues/2)).
- [`histbin()`](https://mharinga.github.io/insurancerating/reference/histbin.md)
  is added to create histograms with outliers
- `reduce` now returns a data.frame as output

## insurancerating 0.6.2

CRAN release: 2020-06-08

- `check_normality()` is now depreciated; use
  [`check_residuals()`](https://mharinga.github.io/insurancerating/reference/check_residuals.md)
  instead to detect overall deviations from the expected distribution
- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)
  now shows significance stars for p-values
- [`period_to_months()`](https://mharinga.github.io/insurancerating/reference/period_to_months.md)
  arithmetic operations with dates are rewritten; much faster
- [`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md)
  now has argument `by` to determine summary statistics for different
  subgroups

## insurancerating 0.6.1

CRAN release: 2020-04-29

- `univariate_all()` and `autoplot.univ_all()` are now depreciated; use
  [`univariate()`](https://mharinga.github.io/insurancerating/reference/univariate.md)
  and `autoplot.univariate()` instead
- [`check_overdispersion()`](https://mharinga.github.io/insurancerating/reference/check_overdispersion.md),
  `check_normality()`,
  [`model_performance()`](https://mharinga.github.io/insurancerating/reference/model_performance.md),
  [`bootstrap_rmse()`](https://mharinga.github.io/insurancerating/reference/bootstrap_rmse.md),
  and
  [`add_prediction()`](https://mharinga.github.io/insurancerating/reference/add_prediction.md)
  are added to test model quality and return performance metrics
- [`reduce()`](https://mharinga.github.io/insurancerating/reference/reduce.md)
  is added to reduce an insurance portfolio by merging redundant date
  ranges

## insurancerating 0.6.0

CRAN release: 2020-04-10

- `label_width` in
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  is added to wrap long labels in multiple lines
- `sort_manual` in
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  is added to sort risk factors into an own ordering
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  now works without manually loading package `ggplot2` and `patchwork`
  first
- [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)
  now returns an object of class `riskfactor`
- `autoplot.riskfactor()` is added to create the corresponding plots to
  the output given by
  [`rating_factors()`](https://mharinga.github.io/insurancerating/reference/rating_factors.md)

## insurancerating 0.5.2

CRAN release: 2020-03-30

- `autoplot.univ_all()` now gives correct labels on the x-axis when
  `ncol` \> 1.

## insurancerating 0.5.1

CRAN release: 2020-03-29

- A package website is added using pkgdown.
- [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md)
  and
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/fit_gam.md)
  now only returns tariff classes and fitted gam respectively; other
  items are stored as attributes.
- `univariate_frequency()`, `univariate_average_severity()`,
  `univariate_risk_premium()`, `univariate_loss_ratio()`,
  `univariate_average_premium()`, `univariate_exposure()`, and
  `univariate_all()` are added to perform an univariate analysis on an
  insurance portfolio.
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  creates the corresponding plots to the summary statistics calculated
  by `univariate_*`.

## insurancerating 0.5.0

CRAN release: 2020-03-12

- [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md)
  is now split in
  [`fit_gam()`](https://mharinga.github.io/insurancerating/reference/fit_gam.md)
  and
  [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md).
- A vignette is added on how to use the package.

## insurancerating 0.4.3

CRAN release: 2019-11-01

- [`period_to_months()`](https://mharinga.github.io/insurancerating/reference/period_to_months.md)
  is added to split rows with a time period longer than one month to
  multiple rows with a time period of exactly one month each.

## insurancerating 0.4.2

CRAN release: 2019-05-31

- In
  [`construct_tariff_classes()`](https://mharinga.github.io/insurancerating/reference/construct_tariff_classes.md),
  `model` now also accepts ‘severity’ as specification.
