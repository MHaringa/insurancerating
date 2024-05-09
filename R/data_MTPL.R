#' Characteristics of 30,000 policyholders in a Motor Third Party Liability
#' (MTPL) portfolio.
#'
#' @description A dataset containing the age, number of claims, exposure,
#' claim amount, power, bm, and region of 30,000 policyholders.
#'
#' @usage MTPL
#'
#' @format A data frame with 30,000 rows and 7 variables:
#' \describe{
#'   \item{age_policyholder}{age of policyholder, in years.}
#'   \item{nclaims}{number of claims.}
#'   \item{exposure}{exposure, for example, if a vehicle is insured as of
#'   July 1 for a certain year, then during that year, this would represent an
#'   exposure of 0.5 to the insurance company.}
#'   \item{amount}{claim amount in Euros.}
#'   \item{power}{engine power of vehicle (in kilowatts).}
#'   \item{bm}{level occupied in the 23-level (0-22) bonus-malus scale (the
#'   higher the level occupied, the worse the claim history).}
#'   \item{zip}{region indicator (0-3).}
#' }
#'
#' @author Martin Haringa
#'
#' @source The data is derived from the portfolio of a large Dutch motor
#' insurance company.
"MTPL"


#' Characteristics of 3,000 policyholders in a Motor Third Party Liability
#' (MTPL) portfolio.
#'
#' @description A dataset containing the area, number of claims, exposure,
#' claim amount, exposure, and premium of 3,000 policyholders
#'
#' @usage MTPL2
#'
#' @format A data frame with 3,000 rows and 6 variables:
#' \describe{
#'   \item{customer_id}{customer id}
#'   \item{area}{region where customer lives (0-3)}
#'   \item{nclaims}{number of claims}
#'   \item{amount}{claim amount (severity)}
#'   \item{exposure}{exposure}
#'   \item{premium}{earned premium}
#' }
#'
#' @author Martin Haringa
#'
#' @source The data is derived from the portfolio of a large Dutch motor
#' insurance company.
"MTPL2"
