#' Automatically create a ggplot for objects obtained from construct_tariff_classes()
#'
#' @description Takes a fitted gam object produced by \code{construct_tariff_classes()} and plots the component smooth functions that make it up,
#' on the scale of the predicted number of claims.
#'
#' @param gam_x A fitted gam object as produced by \code{construct_tariff_classes()}.
#' @param conf.int Determines whether 95\% confidence intervals will be plotted. The default is \code{conf.int = FALSE}.
#' @param clusters Numerical vector with splits as produced by \code{construct_tariff_classes()}.
#' @param color_gam A color can be specified either by name (e.g.: "red") or by hexadecimal code (e.g. : "#FF1234") (default is "steelblue").
#' @param color_splits change the color of the splits in the graph ("grey50" is default).
#' @param xstep Set step size for horizontal axis (default is \code{10}).
#'
#' @return Produces plot showing the smooth components of a fitted GAM.
#'
#' @import ggplot2
#'
#' @examples
#' library(ggplot2)
#' x <- construct_tariff_classes(MTPL, nclaims, age_policyholder, exposure)
#' autoplot(x)
#'
#' @author Martin Haringa
#'
#' @export
autoplot.insurancerating <- function(gam_x, conf.int = FALSE, clusters = TRUE, color_gam = "steelblue", color_splits = "grey50", xstep = 10){
  gamcluster <- gam_x[[1]]
  df <- gam_x[[2]]
  xlab <- gam_x[[3]]

  gam_plot <- ggplot(data = df, aes(x = x, y = predicted)) +
    geom_line(color = color_gam) +
    theme_bw(base_size = 12) +
    {if(isTRUE(clusters)) geom_vline(xintercept = gamcluster, color = color_splits, linetype = 2)} +
    {if(isTRUE(conf.int)) geom_ribbon(aes(ymin = lwr_95, ymax = upr_95), alpha = 0.12)} +
    {if(!isTRUE(clusters)) scale_x_continuous(breaks = seq(floor(min(df$x)), ceiling(max(df$x)), by = xstep))} +
    {if(isTRUE(clusters)) scale_x_continuous(breaks = gamcluster)} +
    labs(y = "Predicted claim frequency", x = xlab)
  return(gam_plot)
}
