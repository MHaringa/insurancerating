
<!-- README.md is generated from README.Rmd. Please edit that file -->

# insurancerating

The goal of insurancerating is to give analytic techniques that can be
used in insurance rating. In particular, the methods can be used to
determine class intervals for continuous numerical variables in P\&C
insurance.

## Installation

You can install insurancerating from github with:

``` r
# install.packages("devtools")
devtools::install_github("MHaringa/insurancerating")
```

## Example

This is a basic example which shows you how to find class intervals for
the continuous variable *age\_policyholder*. This first block does this
for the claim frequency and the second block for the claim severity.

### Claim frequency

Categories added to the predicted number of claims for the continuous
variable *age\_policyholder*.

``` r
library(insurancerating)
gam_x <- gam_frequency(MTPL, nclaims, age_policyholder, exposure)

# Show plot with predicted number of claims from the frequency model.
gam_frequency_plot(gam_x)
```

![](man/figures/example-1.png)<!-- -->

``` r

# Determine splits
clusters <- gam_frequency_clusters(gam_x)

# Show corresponding splits for each element in age_policyholder
gam_frequency_clusters_plot(gam_x, clusters)
```

![](man/figures/example-2.png)<!-- -->
