#' Breaks for continuous variables in insurance rating
#'
#' @param data
#' @param nclaims
#' @param x
#' @param exposure
#'
#' @return
#' @export gam_cut
#'
#' @author Martin Haringa
#'
#' @import mgcv
#' @import rpart
#' @import ggplot2
#'
#' @examples
gam_cut <- function(data, nclaims, x, exposure, cp = 0, color_splits = "red", color_gam = "steelblue", show_splits = TRUE){

  # Turn into character vector
  nclaims <- deparse(substitute(nclaims))
  x <- deparse(substitute(x))
  exposure <- deparse(substitute(exposure))

  df <- data.frame("nclaims" = data[[nclaims]], "x" = data[[x]], "exposure" = data[[exposure]])

  # Fit GAM
  gam_x <- mgcv::gam(nclaims ~ s(x), data = df, family = poisson(), offset = log(exposure))

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

  pred_plot <- ggplot(data = df1, aes(x = x, y = y)) +
    geom_line(color = color_gam) +
    theme_bw(base_size = 12) +
    {if(show_splits) geom_vline(xintercept = split.ageph, color = color_splits, linetype = 2)} +
    labs(y = "Predicted # of claims", x = x)

  clusters <- c(min(counting[[2]]), floor(xf), max(counting[[2]]))
  x_clusters <- cut(df$x, breaks = clusters, include.lowest = TRUE)

  return(list(x_clusters,
              pred_plot,
              split.ageph))
}
