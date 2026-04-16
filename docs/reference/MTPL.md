# Motor Third Party Liability (MTPL) portfolio

A dataset containing the characteristics of 30,000 policyholders in a
Dutch Motor Third Party Liability (MTPL) insurance portfolio. Includes
information on demographics, vehicle, and claims.

## Usage

``` r
MTPL
```

## Format

A data frame with 30,000 rows and 7 variables:

- age_policyholder:

  Age of the policyholder (in years).

- nclaims:

  Number of claims.

- exposure:

  Exposure. For example, if a vehicle is insured as of July 1 for a
  given year, the exposure equals 0.5 for that year.

- amount:

  Claim amount in euros.

- power:

  Engine power of the vehicle (in kilowatts).

- bm:

  Level on the 23-level (0–22) bonus-malus scale (the higher the level,
  the worse the claim history).

- zip:

  Region indicator (0–3).

## Author

Martin Haringa
