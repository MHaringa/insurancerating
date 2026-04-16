# Extract model data

**\[experimental\]**

`model_data()` retrieves underlying data from fitted models. It works
for objects of class `"glm"`, as well as objects produced by refitting
procedures (`"refitsmooth"` or `"refitrestricted"`).

The compatibility wrapper `extract_model_data()` remains available and
is **not** deprecated.

`extract_model_data()` is kept as a compatibility alias for
`model_data()`. It is **not** deprecated.

## Usage

``` r
model_data(x)

extract_model_data(x)
```

## Arguments

- x:

  An object of class `"glm"`, `"refitsmooth"`, or `"refitrestricted"`.

## Value

A `data.frame` of class `"model_data"`, containing the cleaned model
data with additional attributes:

- `rf` — names of risk factors in the model

- `offweights` — weights or offsets if present

- `mgd_rst`, `mgd_smt` — merged restrictions/smooths (refit objects
  only)

- `new_nm`, `old_nm` — new and old column names (refit objects only)

## Details

For GLM objects, the function:

- returns the original data used in the model,

- attaches attributes with the relevant rating factors and any
  weights/offsets.

For refit objects, the function:

- strips out auxiliary columns used for smoothing/restrictions,

- attaches attributes with information about rating factors, merged
  smooths, restrictions, and offsets.

## Author

Martin Haringa

## Examples

``` r
if (FALSE) { # \dontrun{
library(insurancerating)
library(dplyr)

# Fit GAM for claim frequency
age_policyholder_frequency <- riskfactor_gam(data = MTPL,
                                             nclaims = "nclaims",
                                             x = "age_policyholder",
                                             exposure = "exposure")

# Determine clusters
clusters_freq <- construct_tariff_classes(age_policyholder_frequency)

# Add clusters to MTPL portfolio
dat <- MTPL |>
mutate(age_policyholder_freq_cat = clusters_freq$tariff_classes) |>
mutate(across(where(is.character), as.factor)) |>
mutate(across(where(is.factor), ~biggest_reference(., exposure)))

# Fit frequency and severity model
freq <- glm(nclaims ~ bm + age_policyholder_freq_cat, offset = log(exposure),
            family = poisson(), data = dat)
sev <- glm(amount ~ bm + zip, weights = nclaims,
           family = Gamma(link = "log"), data = dat |> filter(amount > 0))

# Add predictions for freq and sev to data, and calculate premium
premium_df <- dat |>
add_prediction(freq, sev) |>
mutate(premium = pred_nclaims_freq * pred_amount_sev)

# Fit unrestricted model
burn_unrestricted <- glm(premium ~ zip + bm + age_policyholder_freq_cat,
weights = exposure, family = Gamma(link = "log"), data = premium_df)

# Impose smoothing and refit model
burn_restricted <- burn_unrestricted |>
add_smoothing(x_cut = "age_policyholder_freq_cat",
x_org = "age_policyholder",
breaks = seq(18, 95, 5)) |>
refit_glm()

# Extract model data
model_data(burn_restricted)
} # }
```
