#' Automatically create a ggplot for objects obtained from construct_tariff_classes()
#'
#' @description Takes an object produced by \code{construct_tariff_classes()}, and plots the predicted claim frequency.
#' In addition the constructed tariff classes are shown.
#'
#' @param x an object as produced by \code{construct_tariff_classes()}
#' @param conf_int determines whether 95\% confidence intervals will be plotted. The default is \code{conf_int = FALSE}
#' @param clusters numerical vector with splits as produced by \code{construct_tariff_classes()}
#' @param color_gam a color can be specified either by name (e.g.: "red") or by hexadecimal code (e.g. : "#FF1234") (default is "steelblue")
#' @param color_splits change the color of the splits in the graph ("grey50" is default)
#' @param xstep set step size for labels horizontal axis
#' @param add_points add observed frequency/severity points for each level of the variable for which tariff classes are constructed
#' @param size_points size for points (1 is default)
#' @param color_points change the color of the points in the graph ("black" is default)
#' @param rotate_labels rotate x-labels 45 degrees (this might be helpful for overlapping x-labels)
#' @param remove_outliers do not show observations above this number in the plot. This might be helpful for outliers.
#'
#' @return a ggplot object
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
autoplot.insurancerating <- function(x, conf_int = FALSE, clusters = TRUE, color_gam = "steelblue", color_splits = "grey50",
                                     xstep = NULL, add_points = FALSE, size_points = 1, color_points = "black", rotate_labels = FALSE,
                                     remove_outliers = NULL){
  gamcluster <- x[[1]]
  df <- x[[2]]
  xlab <- x[[3]]
  ylab <- x[[5]]
  points <- x[[6]]

  if(isTRUE(conf_int) & sum(df$upr_95 > 1e9) > 0){
    message("The confidence bounds are too large to show.")
  }

  if(is.numeric(remove_outliers) & isTRUE(add_points)) {
    if (ylab == "frequency") points <- points[points$frequency < remove_outliers, ]
    if (ylab == "severity") points <- points[points$avg_claimsize < remove_outliers, ]
    if (ylab == "burning") points <- points[points$avg_premium < remove_outliers, ]
  }

  gam_plot <- ggplot(data = df, aes(x = x, y = predicted)) +
    geom_line(color = color_gam) +
    theme_bw(base_size = 12) +
    {if(isTRUE(clusters)) geom_vline(xintercept = gamcluster, color = color_splits, linetype = 2)} +
    {if(isTRUE(conf_int) & sum(df$upr_95 > 1e9) == 0) geom_ribbon(aes(ymin = lwr_95, ymax = upr_95), alpha = 0.12)} +
    {if(!isTRUE(clusters) & is.numeric(xstep)) scale_x_continuous(breaks = seq(floor(min(df$x)), ceiling(max(df$x)), by = xstep))} +
    {if(isTRUE(clusters)) scale_x_continuous(breaks = gamcluster)} +
    {if(isTRUE(add_points) & ylab == "frequency") geom_point(data = points, aes(x = x, y = frequency), size = size_points, color = color_points)} +
    {if(isTRUE(add_points) & ylab == "severity") geom_point(data = points, aes(x = x, y = avg_claimsize), size = size_points, color = color_points)} +
    {if(isTRUE(add_points) & ylab == "burning") geom_point(data = points, aes(x = x, y = avg_premium), size = size_points, color = color_points)} +
    {if(ylab == "severity") scale_y_continuous(labels = scales::comma)} +
    {if(isTRUE(rotate_labels)) theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) } +
    labs(y = paste0("Predicted ", ylab), x = xlab)

  return(gam_plot)
}
