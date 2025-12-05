#' Motor Third Party Liability (MTPL) portfolio
#'
#' @description
#' A dataset containing the characteristics of 30,000 policyholders in a Dutch
#' Motor Third Party Liability (MTPL) insurance portfolio. Includes information
#' on demographics, vehicle, and claims.
#'
#' @format A data frame with 30,000 rows and 7 variables:
#' \describe{
#'   \item{age_policyholder}{Age of the policyholder (in years).}
#'   \item{nclaims}{Number of claims.}
#'   \item{exposure}{Exposure. For example, if a vehicle is insured as of
#'     July 1 for a given year, the exposure equals 0.5 for that year.}
#'   \item{amount}{Claim amount in euros.}
#'   \item{power}{Engine power of the vehicle (in kilowatts).}
#'   \item{bm}{Level on the 23-level (0–22) bonus-malus scale (the higher the
#'     level, the worse the claim history).}
#'   \item{zip}{Region indicator (0–3).}
#' }
#'
#' @author Martin Haringa
"MTPL"


#' Motor Third Party Liability (MTPL) portfolio (3,000 policyholders)
#'
#' @description
#' A dataset containing characteristics of 3,000 policyholders in a Dutch Motor
#' Third Party Liability (MTPL) insurance portfolio. Includes information on
#' region, claims, exposure, and premium.
#'
#' @format A data frame with 3,000 rows and 6 variables:
#' \describe{
#'   \item{customer_id}{Unique customer identifier.}
#'   \item{area}{Region where the customer lives (0–3).}
#'   \item{nclaims}{Number of claims.}
#'   \item{amount}{Claim amount (severity).}
#'   \item{exposure}{Exposure, expressed in years.}
#'   \item{premium}{Earned premium.}
#' }
#'
#' @author Martin Haringa
"MTPL2"
