library(insurancerating)
library(dplyr)

context("rating_factors")



# Restricted glm ----------------------------------------------------------

# Fit frequency and severity model
freq <- glm(nclaims ~ bm + zip, offset = log(exposure), family = poisson(),
            data = MTPL)
sev <- glm(amount ~ bm + zip, weights = nclaims,
           family = Gamma(link = "log"),
           data = MTPL %>% filter(amount > 0))

# Add predictions for freq and sev to data, and calculate premium
premium_df <- MTPL %>%
  add_prediction(freq, sev) %>%
  mutate(premium = pred_nclaims_freq * pred_amount_sev)

# Restrictions on risk factors for region (zip)
zip_df <- data.frame(zip = c(0,1,2,3), zip_rst = c(0.8, 0.9, 1, 1.2))

# Fit unrestricted model
burn <- glm(premium ~ bm + zip, weights = exposure,
            family = Gamma(link = "log"), data = premium_df)

# Fit restricted model
burn_rst <- burn %>%
  restrict_coef(., zip_df) %>%
  update_glm()




# Smoothed glm ------------------------------------------------------------

# Fit GAM for claim frequency
age_policyholder_frequency <- fit_gam(data = MTPL,
                                      nclaims = nclaims,
                                      x = age_policyholder,
                                      exposure = exposure)

# Determine clusters
clusters_freq <- construct_tariff_classes(age_policyholder_frequency)

# Add clusters to MTPL portfolio
dat <- MTPL %>%
  mutate(age_policyholder_freq_cat = clusters_freq$tariff_classes) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), ~biggest_reference(., exposure)))

# Fit frequency and severity model
freq <- glm(nclaims ~ bm + age_policyholder_freq_cat, offset = log(exposure),
            family = poisson(), data = dat)
sev <- glm(amount ~ bm + zip, weights = nclaims,
           family = Gamma(link = "log"), data = dat %>% filter(amount > 0))

# Add predictions for freq and sev to data, and calculate premium
premium_df <- dat %>%
  add_prediction(freq, sev) %>%
  mutate(premium = pred_nclaims_freq * pred_amount_sev)

# Fit unrestricted model
burn_unrestricted <- glm(premium ~ zip + bm + age_policyholder_freq_cat,
                         weights = exposure,
                         family = Gamma(link = "log"),
                         data = premium_df)

# Impose smoothing and create figure
burn_unrestricted %>%
  smooth_coef(x_cut = "age_policyholder_freq_cat",
              x_org = "age_policyholder",
              breaks = seq(18, 95, 5)) %>%
  autoplot()

# Impose smoothing and refit model
burn_smooth <- burn_unrestricted %>%
  smooth_coef(x_cut = "age_policyholder_freq_cat",
              x_org = "age_policyholder",
              breaks = seq(18, 95, 5)) %>%
  update_glm()





# Glm ---------------------------------------------------------------------

MTPL2a <- MTPL2
MTPL2a$area <- as.factor(MTPL2a$area)
x <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
         data = MTPL2a)

df <- MTPL2 %>%
  mutate(across(c(area), as.factor)) %>%
  mutate(across(c(area), ~biggest_reference(., exposure)))

mod1 <- glm(nclaims ~ area + premium, offset = log(exposure),
            family = poisson(), data = df)
mod2 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
            data = df)




# Tests -------------------------------------------------------------------

testthat::test_that(
  "No errors are returned for smoothed glm objects", {
    testthat::expect_error(rating_factors(burn_smooth), NA)
  }
)

testthat::test_that(
  "No errors are returned for restricted glm objects", {
    testthat::expect_error(rating_factors(burn_rst), NA)
  }
)

testthat::test_that(
  "No errors are returned for glm objects", {
    testthat::expect_error(rating_factors(x), NA)
  }
)

testthat::test_that(
  "No errors are returned for multiple glm objects with model_data", {
    testthat::expect_error(rating_factors(mod1, mod2, model_data = df,
                                          exposure = exposure), NA)
  }
)
