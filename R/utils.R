#' @keywords internal
make_stars <- function(pval) {
  if ( is.na(pval) ) {pval <- is.numeric(pval)}
  if(pval <= 0.001)
    stars = "***"
  else if(pval > 0.001 & pval <= 0.01)
    stars = "**"
  else if(pval > 0.01 & pval <= 0.05)
    stars = "*"
  else if(pval > 0.05 & pval <= 0.1)
    stars = "."
  else {stars = ""}
  stars
}


#' @keywords internal
elapsed_days <- function(end_date){
  as.POSIXlt(end_date)$mday - 1
}



#' Get splits from partykit object
#' @noRd
#'
#' @param x A party object.
#'
get_splits <- function(x) {

  lrp <- utils::getFromNamespace(".list.rules.party", "partykit")
  splits_list <- lrp(x)
  last_line <- unname(splits_list[length(splits_list)])

  # Remove punctuation marks
  splits_vector <- regmatches(last_line, gregexpr("[[:digit:]]+", last_line))

  splits <- as.numeric(unlist(splits_vector))
  return(splits)
}


#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot










