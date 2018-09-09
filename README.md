
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
clusters <- clustering_frequency(MTPL, nclaims, age_policyholder, exposure)

# Show plot with predicted number of claims from the frequency model, binned by decision trees.
clusters[[2]]
```

![](man/figures/example-1.png)<!-- -->

``` r

# Show splits
clusters[[3]]
#> [1] 18 31 38 53 60 67 80 95

# Show corresponding splits for each element in age_policyholder
head(clusters[[1]])
#> [1] (38,53] [18,31] (53,60] (38,53] [18,31] (31,38]
#> Levels: [18,31] (31,38] (38,53] (53,60] (60,67] (67,80] (80,95]
```

### Claim severity

Categories added to the predicted average cost of a claim for the
continuous variable *age\_policyholder*. The claim severity is the
amount of loss associated with an average insurance claim.

``` r
library(insurancerating)
clusters <- clustering_severity(MTPL, amount, age_policyholder, nclaims, color_splits = "deepskyblue")

# Show plot with predicted average cost of a claim from the severity model, binned by decision trees.
clusters[[2]]
```

![](man/figures/example2-1.png)<!-- -->

``` r

# Show splits
clusters[[3]]
#> [1] 18 24 38 52 61 68 88

# Show corresponding splits for each element in age_policyholder
head(clusters[[1]])
#> [1] (38,52] [18,24] (38,52] (24,38] (38,52] (52,61]
#> Levels: [18,24] (24,38] (38,52] (52,61] (61,68] (68,88]
```
