#' Characteristics of 30,000 policyholders in a Motor Third Party Liability (MTPL) portfolio.
#'
#' A dataset containing the age, number of claims, exposure, claim amount, power, bm, and region of 30,000 policyholders.
#'
#' @usage MTPL
#'
#' @format A data frame with 30,000 rows and 7 variables:
#' \describe{
#'   \item{age_policyholder}{age of policyholder, in years.}
#'   \item{nclaims}{number of claims.}
#'   \item{exposure}{exposure, for example, if a vehicle is insured as of July 1 for a certain year, then during that year, this would represent an exposure of 0.5 to the insurance company.}
#'   \item{amount}{claim amount in Euros.}
#'   \item{power}{engine power of vehicle (in kilowatts).}
#'   \item{bm}{level occupied in the 23-level bonus-malus scale (the higher the level occupied, the worse the claim history).}
#'   \item{zip}{region indicator.}
#' }
#'
#' @author Martin Haringa
#'
#' @source The data is derived from the portfolio of a large Dutch motor insurance company.
"MTPL"


