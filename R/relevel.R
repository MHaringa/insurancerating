#' Relevel factor to biggest group
#'
#' @description Argument for setting the reference group to the biggest group in linear regression.
#'
#' @param x A vector
#' @param exposure A vector with exposures
#'
#' @return A factor with releveled levels
#' @export big.ref
#'
#' @examples mtcars2 <- mtcars1 %>%
#' mutate_if(is.factor, funs(big.ref(., qsec)))
big.ref <- function(x, exposure) {
  counts <- sort(tapply(exposure, x, FUN = sum), decreasing = TRUE)
  relevel(x, ref = names(counts)[1])
}

