# Motor Third Party Liability (MTPL) portfolio

A dataset containing the characteristics of 30,000 policyholders in a
Dutch Motor Third Party Liability (MTPL) insurance portfolio. Includes
information on policyholder characteristics, vehicle attributes, and
claims.

## Usage

``` r
MTPL
```

## Format

A data frame containing 30,000 rows and 7 variables:

- age_policyholder:

  Age of the policyholder (in years).

- nclaims:

  Number of claims.

- exposure:

  Exposure, expressed in years. For example, if a vehicle is insured
  from July 1, the exposure equals 0.5 for that year.

- amount:

  Claim severity (in euros).

- power:

  Engine power of the vehicle (in kilowatts).

- bm:

  Bonus-malus level (0–22). Higher levels indicate worse claim history.

- zip:

  Region indicator (0–3).

## Author

Martin Haringa
