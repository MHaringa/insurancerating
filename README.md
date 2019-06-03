
<!-- README.md is generated from README.Rmd. Please edit that file -->

# insurancerating

The goal of insurancerating is to give analytic techniques that can be
used in insurance rating. It provides a data driven strategy for the
construction of tariff classes in P\&C insurance. The goal is to bin the
continuous factors such that categorical risk factors result which
capture the effect of the covariate on the response in an accurate way,
while being easy to use in a generalized linear model (GLM).

It also adds functionality showing additional lines for the reference
categories in the levels of the coefficients in the output of a
generalized linear regression analysis. In addition it implements a
procedure determining the level of a factor with the largest exposure,
and thereafter changing the base level of the factor to this level.

## Installation

You can install insurancerating from github with:

``` r
# install.packages("devtools")
devtools::install_github("MHaringa/insurancerating")
```

## Example

This is a basic example which shows the techniques provided in
insurancerating.

The first part shows how to construct tariff classes for the variable
*age\_policyholder* in the MTPL dataset:

``` r
library(insurancerating)
library(ggplot2)
library(dplyr)

# Claim frequency classes
age_policyholder_frequency <- construct_tariff_classes(data = MTPL, 
                                                       nclaims = nclaims, 
                                                       x = age_policyholder, 
                                                       exposure = exposure)

# Claim severity classes
age_policyholder_severity <- construct_tariff_classes(data = MTPL, 
                                                      nclaims = nclaims, 
                                                      x = age_policyholder, 
                                                      exposure = exposure, 
                                                      amount = amount, 
                                                      model = "severity")
```

Show classes for the claim frequency (the points show the ratio between
the observed number of claims and exposure for each age):

``` r
autoplot(age_policyholder_frequency, add_points = TRUE)
```

![](README-figfreq-1.png)<!-- -->

The figure shows that younger policyholders have a higer risk profile.
The fitted GAM is lower than might be expected from the observed claim
frequency for policyholders of age 19. This is because there are very
few young policyholders of age 19 present in the portfolio.

Show classes for the claim severity:

``` r
autoplot(age_policyholder_severity, add_points = TRUE)
```

![](README-figsev-1.png)<!-- -->

The second part adds the constructed tariff classes for the variable
*age\_policyholder* to the dataset, and sets the base level of the
factor *age\_policyholder* to the level with the largest exposure. In
this example the class for ages (39,50\], which contains the largest
exposure.

``` r

dat <- MTPL %>%
  mutate(age_policyholder_freq_cat = age_policyholder_frequency$tariff_classes) %>%
  mutate(age_policyholder_sev_cat = age_policyholder_severity$tariff_classes) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, list(~biggest_reference(., exposure)))

str(dat)
#> 'data.frame':    32731 obs. of  6 variables:
#>  $ age_policyholder         : int  43 21 54 44 20 38 68 45 76 30 ...
#>  $ nclaims                  : int  0 0 0 1 1 0 0 1 0 0 ...
#>  $ exposure                 : num  1 1 1 1 0.852 ...
#>  $ amount                   : num  0 0 0 57540 2057 ...
#>  $ age_policyholder_freq_cat: Factor w/ 9 levels "(39,50]","[18,25]",..: 1 2 5 1 2 4 7 1 8 3 ...
#>  $ age_policyholder_sev_cat : Factor w/ 6 levels "(46,81]","[18,25]",..: 5 2 1 5 2 4 1 5 1 3 ...
```

The last part is to fit a *generalized linear model*. The function
rating\_factors prints the output including the reference
group.

``` r
model <- glm(nclaims ~ age_policyholder_freq_cat, offset = log(exposure), family = "poisson", data = dat)
rating_factors(model)
#>                         term     cluster  estimate
#> 1                  Intercept (Intercept) 0.1368181
#> 2  age_policyholder_freq_cat     (39,50] 1.0000000
#> 3  age_policyholder_freq_cat     [18,25] 1.9438228
#> 4  age_policyholder_freq_cat     (25,32] 1.3234995
#> 5  age_policyholder_freq_cat     (32,39] 1.0568538
#> 6  age_policyholder_freq_cat     (50,57] 0.8919696
#> 7  age_policyholder_freq_cat     (57,64] 0.7423998
#> 8  age_policyholder_freq_cat     (64,71] 0.7379362
#> 9  age_policyholder_freq_cat     (71,83] 0.7021348
#> 10 age_policyholder_freq_cat     (83,95] 0.6933378
```
