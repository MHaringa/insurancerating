library(insurancerating)
library(dplyr)

testthat::context("rating_factors")



# Restricted glm ----------------------------------------------------------

# Fit frequency and severity model
freq <- glm(nclaims ~ bm + zip, offset = log(exposure), family = poisson(),
            data = MTPL)
sev <- glm(amount ~ bm + zip, weights = nclaims,
           family = Gamma(link = "log"),
           data = MTPL %>% filter(amount > 0))

# Add predictions for freq and sev to data, and calculate premium
premium_df <- MTPL |>
  add_prediction(freq, sev) |>
  mutate(premium = pred_nclaims_freq * pred_amount_sev)

# Restrictions on risk factors for region (zip)
zip_df <- data.frame(zip = c(0, 1, 2, 3),
                     zip_rst = c(0.8, 0.9, 1, 1.2))

# Fit unrestricted model
burn <- glm(premium ~ bm + zip, weights = exposure,
            family = Gamma(link = "log"), data = premium_df)

# Fit restricted model
burn_rst <- restrict_coef(burn, zip_df) |>
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
                         weights = exposure,
                         family = Gamma(link = "log"),
                         data = premium_df)

# Impose smoothing and refit model
suppressWarnings({
  burn_smooth <- burn_unrestricted |>
    smooth_coef(x_cut = "age_policyholder_freq_cat",
                x_org = "age_policyholder",
                breaks = seq(18, 95, 5)) |>
    update_glm()
})





# Glm ---------------------------------------------------------------------

mtpl2a <- MTPL2
mtpl2a$area <- as.factor(mtpl2a$area)
x <- glm(nclaims ~ area, offset = log(exposure), family = poisson(),
         data = mtpl2a)

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
    testthat::expect_error(rating_factors(burn_smooth, signif_stars = FALSE), NA)
  }
)

testthat::test_that(
  "No errors are returned for restricted glm objects", {
    testthat::expect_error(rating_factors(burn_rst, signif_stars = FALSE), NA)
  }
)

testthat::test_that(
  "No errors are returned for glm objects", {
    testthat::expect_error(rating_factors(x, signif_stars = FALSE), NA)
  }
)

testthat::test_that(
  "No errors are returned for multiple glm objects with model_data", {
    testthat::expect_error(rating_factors(mod1, mod2, model_data = df,
                                          exposure = exposure,
                                          signif_stars = FALSE), NA)
  }
)


filter_zip <- rating_factors(burn_smooth)$df |>
  dplyr::filter(risk_factor == "zip")

testthat::test_that(
  "NA values are set to 1 for smoothed glm", {
    testthat::expect_gte(sum(filter_zip$est_burn_smooth), 0)
  }
)

filter_area <- rating_factors(mod1)$df |>
  dplyr::filter(risk_factor == "area")

testthat::test_that(
  "NA values are set to 1 for non smoothed or non restricted glm", {
    testthat::expect_gte(sum(filter_area$est_burn_smooth), 0)
  }
)


zip_df <- data.frame(zip = c(0,1,2,3),
                     zip_restricted = c(0.8, 0.9, 1, 1.2))

burn_restricted2 <- restrict_coef(burn_unrestricted, zip_df) |>
  update_glm()

filter_zip_rst <- rating_factors(burn_restricted2)$df |>
  dplyr::filter(risk_factor == "zip_restricted")

testthat::test_that(
  "Check if all values are not equal to one", {
    testthat::expect_false(var(filter_zip_rst$est_burn_restricted2) == 0)
  }
)


