
<!-- README.md is generated from README.Rmd. Please edit that file -->

# insurancerating

The goal of insurancerating is to give analytic techniques that can be
used in insurance rating. In particular, the methods can be used to
construct tariff classes in P\&C insurance.

## Installation

You can install insurancerating from github with:

``` r
# install.packages("devtools")
devtools::install_github("MHaringa/insurancerating")
```

## Example

This is a basic example which shows you how to construct tariff classes
for the variable *age\_policyholder* in the MTPL dataseet.

``` r
library(insurancerating)
library(ggplot2)

x <- construct_tariff_classes(data = MTPL, nclaims = nclaims, x = age_policyholder, exposure = exposure)

# Show tariff classes
autoplot(x)
```

![](man/figures/example-1.png)<!-- -->
