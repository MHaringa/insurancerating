#' Breaks for continuous variables in insurance rating (severity)
#'
#' @description The function provides an interface to finding class intervals for continuous numerical variables, for example to bin the continuous factors
#' such that categorical risk factors result which capture the effect of the covariate on the response in an accurate way,
#' while being easy to use in a generalized linear model (GLM).
#'
#' @param data dfd
#' @param nclaims ada
#' @param x ad
#' @param exposure adf
#' @param cp ddf
#' @param color_splits df
#' @param color_gam dfd
#' @param show_splits dfdf
#'
#' @return A list with components:
#' @return \item{x_clusters}{df}
#' @return \item{gam_plot}{}
#' @return \item{clusters}{Numerical vector with splits.}
#'
#' @export gam_cut
#'
#' @author Martin Haringa
#'
#' @import mgcv
#' @import rpart
#' @import ggplot2
#'
#' @examples clustering_severity(MTPL, amount, age_policyholder)
clustering_severity <- function(data, amount, x, nclaims, cp = 0, color_splits = "red", color_gam = "steelblue", show_splits = TRUE){

  # Turn into character vector
  amount <- deparse(substitute(amount))
  x <- deparse(substitute(x))
  nclaims <- deparse(substitute(nclaims))


  df0 <- data.frame("amount" = data[[amount]], "x" = data[[x]], "nclaims" = data[[nclaims]])

  df <- df0[df0$amount > 0, ]

  # Fit GAM
  gam_x <- mgcv::gam(amount ~ s(x), data = df, family = Gamma("log"), weights = nclaims)

  # Invisible plot; interest in return value from plot function
  png("temp.xyz")
  gam0 <- plot(gam_x, se = FALSE, rug = FALSE, shift = mean(predict(gam_x)), trans = exp)
  dev.off()
  file.remove("temp.xyz")
  invisible(gam0)

  df1 <- data.frame(x = gam0[[1]]$x, y = exp(gam0[[1]]$fit + mean(predict(gam_x))))

  name <- gam0[[1]]$xlab
  elements <- gam0[[1]]$raw
  counting <- rle(sort(elements))
  counting2 <- setNames(data.frame(counting[[2]]), name)
  pred <- as.numeric(mgcv::predict.gam(gam_x, counting2, type = 'response'))
  new <- data.frame(counting2, pred, n = counting[[1]])

  tree.ageph <- rpart::rpart(pred ~ x, data = new, weights = n, control = rpart::rpart.control(cp = cp))
  split.ageph <- sort(as.numeric(tree.ageph$splits[,4]))
  gam_plot <- ggplot(data = df1, aes(x = x, y = y)) +
    geom_line(color = color_gam) +
    theme_bw(base_size = 12) +
    {if(show_splits) geom_vline(xintercept = split.ageph, color = color_splits, linetype = 2)} +
    labs(y = "Average cost of a claim", x = x)

  clusters <- c(min(counting[[2]]), floor(split.ageph), max(counting[[2]]))
  x_clusters <- cut(df$x, breaks = clusters, include.lowest = TRUE)

  return(list(x_clusters,
              gam_plot,
              clusters))
}
