#' Motor Third Party Liability (MTPL) portfolio
#'
#' @description
#' A dataset containing the characteristics of 30,000 policyholders in a Dutch
#' Motor Third Party Liability (MTPL) insurance portfolio. Includes information
#' on policyholder characteristics, vehicle attributes, and claims.
#'
#' @format A data frame containing 30,000 rows and 7 variables:
#' \describe{
#'   \item{age_policyholder}{Age of the policyholder (in years).}
#'   \item{nclaims}{Number of claims.}
#'   \item{exposure}{Exposure, expressed in years. For example, if a vehicle is
#'     insured from July 1, the exposure equals 0.5 for that year.}
#'   \item{amount}{Claim severity (in euros).}
#'   \item{power}{Engine power of the vehicle (in kilowatts).}
#'   \item{bm}{Bonus-malus level (0–22). Higher levels indicate worse claim history.}
#'   \item{zip}{Region indicator (0–3).}
#' }
#'
#' @author Martin Haringa
"MTPL"


#' Motor Third Party Liability (MTPL) portfolio (3,000 policyholders)
#'
#' @description
#' A dataset containing the characteristics of 3,000 policyholders in a Dutch
#' Motor Third Party Liability (MTPL) insurance portfolio. Includes information
#' on region, claims, exposure, and premium.
#'
#' @format A data frame containing 3,000 rows and 6 variables:
#' \describe{
#'   \item{customer_id}{Unique customer identifier.}
#'   \item{area}{Region where the customer lives (0–3).}
#'   \item{nclaims}{Number of claims.}
#'   \item{amount}{Claim severity (in euros).}
#'   \item{exposure}{Exposure, expressed in years.}
#'   \item{premium}{Earned premium.}
#' }
#'
#' @author Martin Haringa
"MTPL2"
