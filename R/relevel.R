#' Set reference group to the group with largest exposure
#'
#' @description This function specifies the first level of a factor to the level with the largest exposure. Levels of factors are sorted
#' using an alphabetic ordering. If the factor is used in a regression context, then the first level will be the reference. For insurance
#' applications it is common to specify the reference level to the level with the largest exposure.
#'
#' @param x an unordered factor
#' @param weight a vector containing weights (e.g. exposure). Should be numeric.
#'
#' @author Martin Haringa
#'
#' @references Kaas, Rob & Goovaerts, Marc & Dhaene, Jan & Denuit, Michel. (2008). Modern Actuarial Risk Theory: Using R.
#' doi:10.1007/978-3-540-70998-5.
#'
#' @importFrom stats relevel
#'
#' @return a factor of the same length as x
#' @export biggest_reference
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' df <- chickwts %>%
#' mutate_if(is.character, as.factor) %>%
#' mutate_if(is.factor, funs(biggest_reference(., weight)))
#' }
biggest_reference <- function(x, weight) {
  if(!is.numeric(weight)) weight <- is.numeric(weight)
  counts <- sort(tapply(weight, x, FUN = sum), decreasing = TRUE)
  relevel(x, ref = names(counts)[1])
}


