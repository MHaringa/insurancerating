pkgname <- "insurancerating"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('insurancerating')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("active_rows_by_date")
### * active_rows_by_date

flush(stderr()); flush(stdout())

### Name: active_rows_by_date
### Title: Find active portfolio rows for event dates
### Aliases: active_rows_by_date

### ** Examples

library(lubridate)
portfolio <- data.frame(
begin1 = ymd(c("2014-01-01", "2014-01-01")),
end = ymd(c("2014-03-14", "2014-05-10")),
termination = ymd(c("2014-03-14", "2014-05-10")),
exposure = c(0.2025, 0.3583),
premium =  c(125, 150),
car_type = c("BMW", "TESLA"))

## Find active rows on different dates
dates0 <- data.frame(active_date = seq(ymd("2014-01-01"), ymd("2014-05-01"),
by = "months"))
active_rows_by_date(
  portfolio,
  dates0,
  period_start = "begin1",
  period_end = "end",
  date = "active_date"
)

## With extra identifiers (merge claim date with time interval in portfolio)
claim_dates <- data.frame(claim_date = ymd("2014-01-01"),
car_type = c("BMW", "VOLVO"))

### Only rows are returned that can be matched
active_rows_by_date(
  portfolio,
  claim_dates,
  period_start = "begin1",
  period_end = "end",
  date = "claim_date",
  by = "car_type"
)

### When row cannot be matched, NA is returned for that row
active_rows_by_date(
  portfolio,
  claim_dates,
  period_start = "begin1",
  period_end = "end",
  date = "claim_date",
  by = "car_type",
  nomatch = NA
)




cleanEx()
nameEx("add_observed_experience")
### * add_observed_experience

flush(stderr()); flush(stdout())

### Name: add_observed_experience
### Title: Add observed portfolio experience to a rating table
### Aliases: add_observed_experience

### ** Examples

df <- MTPL2
df$area <- as.factor(df$area)

model <- glm(
  nclaims ~ area + offset(log(exposure)),
  family = poisson(),
  data = df
)

observed <- factor_analysis(
  df,
  risk_factors = "area",
  claim_count = "nclaims",
  exposure = "exposure"
)

rating_table(model, model_data = df, exposure = "exposure") |>
  add_observed_experience(observed, metric = "frequency") |>
  autoplot(risk_factors = "area")




cleanEx()
nameEx("add_prediction")
### * add_prediction

flush(stderr()); flush(stdout())

### Name: add_prediction
### Title: Add Model Predictions to a Data Frame
### Aliases: add_prediction

### ** Examples

mod1 <- glm(nclaims ~ age_policyholder,
            data = MTPL,
            offset = log(exposure),
            family = poisson())

# Add predicted values
mtpl_pred <- add_prediction(MTPL, mod1)

# Add predicted values with confidence bounds
mtpl_pred_ci <- add_prediction(MTPL, mod1, confidence = TRUE)




cleanEx()
nameEx("add_relativities")
### * add_relativities

flush(stderr()); flush(stdout())

### Name: add_relativities
### Title: Add expert-based relativities to a refinement workflow
### Aliases: add_relativities

### ** Examples

portfolio <- data.frame(
  claims = c(1, 2, 1, 3, 2, 4),
  exposure = rep(1, 6),
  construction = factor(c("residential", "commercial", "residential",
                          "commercial", "residential", "commercial")),
  construction_detail = factor(c("flat", "shop", "house",
                                 "office", "flat", "shop"))
)

model <- glm(
  claims ~ construction + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)

relativities <- relativities(
  split_level(
    "residential",
    new_levels = c("flat", "house"),
    relativities = c(0.95, 1.05)
  )
)

refined <- prepare_refinement(model, data = portfolio) |>
  add_relativities(
    model_variable = "construction",
    split_variable = "construction_detail",
    relativities = relativities,
    exposure = "exposure"
  )




cleanEx()
nameEx("add_restriction")
### * add_restriction

flush(stderr()); flush(stdout())

### Name: add_restriction
### Title: Add coefficient restrictions to a refinement workflow
### Aliases: add_restriction

### ** Examples

portfolio <- data.frame(
  claims = c(1, 2, 1, 3, 2, 4),
  exposure = rep(1, 6),
  postal_area = factor(c("A", "B", "C", "A", "B", "C"))
)

model <- glm(
  claims ~ postal_area + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)

restrictions <- data.frame(
  postal_area = "C",
  relativity = 1.10
)

refined <- prepare_refinement(model, data = portfolio) |>
  add_restriction(restrictions)




cleanEx()
nameEx("add_smoothing")
### * add_smoothing

flush(stderr()); flush(stdout())

### Name: add_smoothing
### Title: Add smoothing to a refinement workflow
### Aliases: add_smoothing

### ** Examples

## Not run: 
##D library(dplyr)
##D 
##D age_policyholder_frequency <- risk_factor_gam(
##D   data = MTPL,
##D   claim_count = "nclaims",
##D   risk_factor = "age_policyholder",
##D   exposure = "exposure"
##D )
##D 
##D age_segments_freq <- derive_tariff_segments(age_policyholder_frequency)
##D 
##D dat <- MTPL |>
##D   add_tariff_segments(age_segments_freq, name = "age_policyholder_freq_cat") |>
##D   mutate(across(where(is.character), as.factor)) |>
##D   mutate(across(where(is.factor), ~ set_reference_level(., exposure)))
##D 
##D freq <- glm(
##D   nclaims ~ bm + age_policyholder_freq_cat,
##D   offset = log(exposure),
##D   family = poisson(),
##D   data = dat
##D )
##D 
##D sev <- glm(
##D   amount ~ zip,
##D   weights = nclaims,
##D   family = Gamma(link = "log"),
##D   data = dat |> filter(amount > 0)
##D )
##D 
##D premium_df <- dat |>
##D   add_prediction(freq, sev) |>
##D   mutate(premium = pred_nclaims_freq * pred_amount_sev)
##D 
##D burn_unrestricted <- glm(
##D   premium ~ zip + bm + age_policyholder_freq_cat,
##D   weights = exposure,
##D   family = Gamma(link = "log"),
##D   data = premium_df
##D )
##D 
##D ref <- prepare_refinement(burn_unrestricted) |>
##D   add_smoothing(
##D     model_variable = "age_policyholder_freq_cat",
##D     source_variable = "age_policyholder",
##D     breaks = seq(18, 95, 5),
##D     weights = "exposure"
##D   )
## End(Not run)




cleanEx()
nameEx("add_tariff_segments")
### * add_tariff_segments

flush(stderr()); flush(stdout())

### Name: add_tariff_segments
### Title: Add derived tariff segments to portfolio data
### Aliases: add_tariff_segments

### ** Examples

## Not run: 
##D age_segments <- risk_factor_gam(
##D   MTPL,
##D   risk_factor = "age_policyholder",
##D   claim_count = "nclaims",
##D   exposure = "exposure"
##D ) |>
##D   derive_tariff_segments()
##D 
##D MTPL |>
##D   add_tariff_segments(age_segments, name = "age_policyholder_segment")
## End(Not run)




cleanEx()
nameEx("autoplot.bootstrap_performance")
### * autoplot.bootstrap_performance

flush(stderr()); flush(stdout())

### Name: autoplot.bootstrap_performance
### Title: Autoplot for bootstrap_performance objects
### Aliases: autoplot.bootstrap_performance

### ** Examples

## Not run: 
##D mod1 <- glm(nclaims ~ age_policyholder, data = MTPL,
##D             offset = log(exposure), family = poisson())
##D x <- bootstrap_performance(mod1, MTPL, n_resamples = 100,
##D                            show_progress = FALSE)
##D autoplot(x)
## End(Not run)




cleanEx()
nameEx("autoplot.factor_analysis")
### * autoplot.factor_analysis

flush(stderr()); flush(stdout())

### Name: autoplot.factor_analysis
### Title: Automatically create a ggplot for objects obtained from factor
###   analysis
### Aliases: autoplot.factor_analysis

### ** Examples

## --- New usage (SE, recommended) ---
x <- factor_analysis(MTPL2,
                     x = "area",
                     severity = "amount",
                     nclaims = "nclaims",
                     exposure = "exposure")
autoplot(x)

## --- Deprecated usage (NSE) ---
x_old <- univariate(MTPL2, x = area, severity = amount,
                    nclaims = nclaims, exposure = exposure)
autoplot(x_old)




cleanEx()
nameEx("autoplot.riskfactor_gam")
### * autoplot.riskfactor_gam

flush(stderr()); flush(stdout())

### Name: autoplot.riskfactor_gam
### Title: Autoplot for GAM objects from 'risk_factor_gam()'
### Aliases: autoplot.riskfactor_gam

### ** Examples

## Not run: 
##D library(ggplot2)
##D fit <- risk_factor_gam(MTPL,
##D                        risk_factor = "age_policyholder",
##D                        claim_count = "nclaims",
##D                        exposure = "exposure")
##D 
##D autoplot(fit, show_observations = TRUE)
## End(Not run)




cleanEx()
nameEx("bootstrap_performance")
### * bootstrap_performance

flush(stderr()); flush(stdout())

### Name: bootstrap_performance
### Title: Bootstrapped model performance
### Aliases: bootstrap_performance

### ** Examples

## Not run: 
##D mod1 <- glm(nclaims ~ age_policyholder, data = MTPL,
##D             offset = log(exposure), family = poisson())
##D 
##D # Use all records
##D x <- bootstrap_performance(mod1, MTPL, n_resamples = 80,
##D                            show_progress = FALSE)
##D print(x)
##D autoplot(x)
##D 
##D # Use 80% of records and evaluate on the remaining records
##D x_frac <- bootstrap_performance(mod1, MTPL, n_resamples = 50,
##D                                 sample_fraction = .8, sampling = "split",
##D                                 show_progress = FALSE)
##D autoplot(x_frac)
## End(Not run)




cleanEx()
nameEx("check_overdispersion")
### * check_overdispersion

flush(stderr()); flush(stdout())

### Name: check_overdispersion
### Title: Check overdispersion of a Poisson claim frequency model
### Aliases: check_overdispersion

### ** Examples

x <- glm(nclaims ~ area, offset = log(exposure),
         family = poisson(), data = MTPL2)
check_overdispersion(x)




cleanEx()
nameEx("check_residuals")
### * check_residuals

flush(stderr()); flush(stdout())

### Name: check_residuals
### Title: Check simulation-based model residuals
### Aliases: check_residuals

### ** Examples

## Not run: 
##D m1 <- glm(nclaims ~ area, offset = log(exposure),
##D           family = poisson(), data = MTPL2)
##D cr <- check_residuals(m1, n_simulations = 50)
##D autoplot(cr)
## End(Not run)




cleanEx()
nameEx("derive_tariff_segments")
### * derive_tariff_segments

flush(stderr()); flush(stdout())

### Name: derive_tariff_segments
### Title: Derive insurance tariff segments
### Aliases: derive_tariff_segments

### ** Examples

## Not run: 
##D library(dplyr)
##D 
##D # Recommended new usage (SE)
##D age_segments <- risk_factor_gam(MTPL,
##D                                 risk_factor = "age_policyholder",
##D                                 claim_count = "nclaims",
##D                                 exposure = "exposure") |>
##D   derive_tariff_segments()
##D 
##D MTPL |>
##D   add_tariff_segments(age_segments, name = "age_policyholder_segment")
## End(Not run)




cleanEx()
nameEx("edit_smoothing")
### * edit_smoothing

flush(stderr()); flush(stdout())

### Name: edit_smoothing
### Title: Edit an existing smoothing step in a refinement workflow
### Aliases: edit_smoothing

### ** Examples

set.seed(42)
driver_age <- rep(seq(20, 59), each = 4)
exposure <- rep(1, length(driver_age))
age_band <- cut(
  driver_age,
  breaks = c(18, 30, 40, 50, 60),
  include.lowest = TRUE
)
expected_claims <- exp(
  -1.7 + 0.018 * (driver_age - 20) + 0.0006 * (driver_age - 40)^2
)
portfolio <- data.frame(
  claims = rpois(length(driver_age), exposure * expected_claims),
  exposure = exposure,
  driver_age = driver_age,
  age_band = age_band
)

model <- glm(
  claims ~ age_band + offset(log(exposure)),
  family = poisson(),
  data = portfolio
)

refined <- prepare_refinement(model, data = portfolio) |>
  add_smoothing(
    model_variable = "age_band",
    source_variable = "driver_age",
    breaks = c(18, 30, 40, 50, 60),
    degree = 2,
    weights = "exposure"
  ) |>
  edit_smoothing(
    model_variable = "age_band",
    from = 30,
    to = 50,
    from_value = 1.00,
    to_value = 1.10,
    control_positions = c(40),
    control_values = c(1.05)
  )

refined_model <- refit(refined)




cleanEx()
nameEx("extract_model_data")
### * extract_model_data

flush(stderr()); flush(stdout())

### Name: extract_model_data
### Title: Extract model data
### Aliases: extract_model_data

### ** Examples

## Not run: 
##D library(insurancerating)
##D 
##D pmodel <- glm(
##D   breaks ~ wool + tension,
##D   data = warpbreaks,
##D   family = poisson(link = "log")
##D )
##D 
##D extract_model_data(pmodel)
## End(Not run)




cleanEx()
nameEx("factor_analysis")
### * factor_analysis

flush(stderr()); flush(stdout())

### Name: factor_analysis
### Title: Factor analysis for discrete risk factors
### Aliases: factor_analysis

### ** Examples

## --- New usage (SE) ---
factor_analysis(MTPL2,
                risk_factors = "area",
                claim_amount = "amount",
                claim_count = "nclaims",
                exposure = "exposure",
                premium = "premium")

## --- Deprecated usage (NSE) ---
univariate(MTPL2,
           x = area,
           severity = amount,
           nclaims = nclaims,
           exposure = exposure,
           premium = premium)




cleanEx()
nameEx("fisher_classify")
### * fisher_classify

flush(stderr()); flush(stdout())

### Name: fisher_classify
### Title: Fisher's natural breaks classification
### Aliases: fisher_classify

### ** Examples

set.seed(1)
x <- rnorm(100)
fisher_classify(x, n = 5)




cleanEx()
nameEx("fit_truncated_severity")
### * fit_truncated_severity

flush(stderr()); flush(stdout())

### Name: fit_truncated_severity
### Title: Fit severity distributions to truncated claim data
### Aliases: fit_truncated_severity

### ** Examples

## Not run: 
##D observed <- MTPL2$amount[MTPL2$amount > 500 & MTPL2$amount < 10000]
##D fit <- fit_truncated_severity(
##D   losses = observed,
##D   distribution = "gamma",
##D   lower_truncation = 500,
##D   upper_truncation = 10000
##D )
##D autoplot(fit)
## End(Not run)




cleanEx()
nameEx("merge_date_ranges")
### * merge_date_ranges

flush(stderr()); flush(stdout())

### Name: merge_date_ranges
### Title: Reduce portfolio periods by merging adjacent date ranges
### Aliases: merge_date_ranges

### ** Examples

portfolio <- data.frame(
  policy_nr   = rep("12345", 11),
  productgroup= rep("fire", 11),
  product     = rep("contents", 11),
  begin_dat   = as.Date(c(16709,16740,16801,17410,17440,17805,17897,
                          17956,17987,18017,18262), origin="1970-01-01"),
  end_dat     = as.Date(c(16739,16800,16831,17439,17531,17896,17955,
                          17986,18016,18261,18292), origin="1970-01-01"),
  premium     = c(89,58,83,73,69,94,91,97,57,65,55)
)

# Merge periods
pt1 <- merge_date_ranges(
  portfolio,
  period_start = "begin_dat",
  period_end = "end_dat",
  group_by = c("policy_nr", "productgroup", "product"),
  merge_gap_days = 5
)

# Aggregate per period
summary(pt1, period = "days", policy_nr, productgroup, product)

# Merge periods and sum premium per period
pt2 <- merge_date_ranges(
  portfolio,
  period_start = "begin_dat",
  period_end = "end_dat",
  group_by = c("policy_nr", "productgroup", "product"),
  aggregate_cols = "premium",
  merge_gap_days = 5
)

# Create summary with aggregation per week
summary(pt2, period = "weeks", policy_nr, productgroup, product)




cleanEx()
nameEx("model_performance")
### * model_performance

flush(stderr()); flush(stdout())

### Name: model_performance
### Title: Performance of fitted GLMs
### Aliases: model_performance

### ** Examples

m1 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
          data = MTPL2)
m2 <- glm(nclaims ~ area + premium, offset = log(exposure), family = poisson(),
          data = MTPL2)
model_performance(m1, m2)




cleanEx()
nameEx("outlier_histogram")
### * outlier_histogram

flush(stderr()); flush(stdout())

### Name: outlier_histogram
### Title: Portfolio histogram with tail bins
### Aliases: outlier_histogram

### ** Examples

# Inspect the full premium distribution
outlier_histogram(MTPL2, "premium")

# Keep the portfolio body readable while showing both tails
outlier_histogram(MTPL2, "premium", lower = 30, upper = 120, bins = 30)




cleanEx()
nameEx("rating_grid")
### * rating_grid

flush(stderr()); flush(stdout())

### Name: rating_grid
### Title: Construct observed rating-grid points from model data or a data
###   frame
### Aliases: rating_grid

### ** Examples

## Not run: 
##D rating_grid(mtcars, group_by = c("cyl", "vs"))
##D 
##D rating_grid(
##D   mtcars,
##D   group_by = c("cyl", "vs"),
##D   exposure = "disp",
##D   exposure_by = "gear",
##D   aggregate_cols = "mpg"
##D )
##D 
##D pmodel <- glm(
##D   breaks ~ wool + tension,
##D   data = warpbreaks,
##D   family = poisson(link = "log")
##D )
##D 
##D pmodel |>
##D   extract_model_data() |>
##D   rating_grid()
## End(Not run)




cleanEx()
nameEx("refit")
### * refit

flush(stderr()); flush(stdout())

### Name: refit
### Title: Refit a prepared refinement workflow
### Aliases: refit

### ** Examples

zip_df <- data.frame(
  zip = c(0, 1, 2, 3),
  zip_adj = c(0.8, 0.9, 1.0, 1.2)
)

model <- glm(
  nclaims ~ zip + offset(log(exposure)),
  family = poisson(),
  data = MTPL
)

refined_model <- prepare_refinement(model) |>
  add_restriction(zip_df) |>
  refit(intercept_only = TRUE)




cleanEx()
nameEx("relativities")
### * relativities

flush(stderr()); flush(stdout())

### Name: relativities
### Title: Combine multiple level splits into relativities
### Aliases: relativities

### ** Examples

relativities(
  split_level("construction",
              c("residential", "commercial", "civil"),
              c(1.00, 1.10, 1.25))
)




cleanEx()
nameEx("risk_factor_gam")
### * risk_factor_gam

flush(stderr()); flush(stdout())

### Name: risk_factor_gam
### Title: Fit a GAM for a continuous risk factor
### Aliases: risk_factor_gam

### ** Examples

## --- Recommended new usage (SE) ---
# Column names must be passed as strings
risk_factor_gam(MTPL,
                risk_factor = "age_policyholder",
                claim_count = "nclaims",
                exposure = "exposure")

## --- Deprecated usage (NSE) ---
# This still works but will show a warning
fit_gam(MTPL,
        nclaims = nclaims,
        x = age_policyholder,
        exposure = exposure)




cleanEx()
nameEx("rmse")
### * rmse

flush(stderr()); flush(stdout())

### Name: rmse
### Title: Root Mean Squared Error (RMSE)
### Aliases: rmse

### ** Examples

x <- glm(nclaims ~ area, offset = log(exposure),
         family = poisson(), data = MTPL2)
rmse(x, MTPL2)




cleanEx()
nameEx("set_reference_level")
### * set_reference_level

flush(stderr()); flush(stdout())

### Name: set_reference_level
### Title: Set the reference level of a factor
### Aliases: set_reference_level

### ** Examples

## Not run: 
##D library(dplyr)
##D df <- chickwts |>
##D mutate(across(where(is.character), as.factor)) |>
##D mutate(across(where(is.factor), ~set_reference_level(., weight)))
##D 
##D set_reference_level(df$feed, method = "manual", reference_level = "casein")
## End(Not run)




cleanEx()
nameEx("split_level")
### * split_level

flush(stderr()); flush(stdout())

### Name: split_level
### Title: Define a level split with relativities
### Aliases: split_level

### ** Examples

split_level(
  level = "construction",
  new_levels = c("residential", "commercial", "civil"),
  relativities = c(1.00, 1.10, 1.25)
)




cleanEx()
nameEx("split_periods_to_months")
### * split_periods_to_months

flush(stderr()); flush(stdout())

### Name: split_periods_to_months
### Title: Split policy periods into monthly rows
### Aliases: split_periods_to_months

### ** Examples

library(lubridate)
portfolio <- data.frame(
  begin_date = ymd(c("2014-01-01", "2014-01-01")),
  end_date   = ymd(c("2014-03-14", "2014-05-10")),
  exposure   = c(0.2025, 0.3583),
  premium    = c(125, 150)
)

# New SE interface
split_periods_to_months(portfolio,
  period_start = "begin_date",
  period_end = "end_date",
  prorate_cols = c("premium", "exposure")
)

# Old NSE interface (deprecated)
## Not run: 
##D period_to_months(portfolio, begin_date, end_date, premium, exposure)
## End(Not run)




cleanEx()
nameEx("split_relativities")
### * split_relativities

flush(stderr()); flush(stdout())

### Name: split_relativities
### Title: Construct a relativities mapping for level splitting
### Aliases: split_relativities

### ** Examples

split_relativities(
  new_levels = c("residential", "commercial", "civil"),
  relativities = c(1.00, 1.10, 1.25)
)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
