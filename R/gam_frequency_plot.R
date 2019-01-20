#' GAM frequency plot
#'
#' @description Takes a fitted gam object produced by \code{gam_frequency()} and plots the component smooth functions that make it up, on the scale of the predicted number of claims.
#'
#' @param gam_x A fitted gam object as produced by \code{gam_frequency()}.
#' @param xlab If supplied then this will be used as the x label.
#' @param xstep Set step size for horizontal axis (default is \code{10}).
#' @param color_gam A color can be specified either by name (e.g.: "red") or by hexadecimal code (e.g. : "#FF1234") (default is "steelblue").
#'
#' @return Produces plot showing the smooth components of a fitted GAM.
#'
#' @export gam_frequency_plot
#'
#' @author Martin Haringa
#'
#' @import mgcv
#' @import ggplot2
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom graphics plot
#' @importFrom stats poisson
#' @importFrom stats predict
#'
#' @examples gam_fit <- gam_frequency(MTPL, nclaims, age_policyholder, exposure)
#' @examples gam_frequency_plot(gam_fit)
gam_frequency_plot <- function(gam_x, xlab = "x", xstep = 10, color_gam = "steelblue"){

  tryCatch(predict(gam_x), error = function(e) {
    message('First argument should be output obtained from "gam_frequency" function')
  })

  # Invisible plot; interest in return value from plot function
  png("temp.xyz")
  gam0 <- plot(gam_x, se = FALSE, rug = FALSE, shift = mean(predict(gam_x)), trans = exp)
  dev.off()
  file.remove("temp.xyz")
  invisible(gam0)

  df <- data.frame(x = gam0[[1]]$x, y = exp(gam0[[1]]$fit + mean(predict(gam_x))))

  gam_plot <- ggplot(data = df, aes(x = x, y = y)) +
    geom_line(color = color_gam) +
    theme_bw(base_size = 12) +
    scale_x_continuous(breaks = seq(floor(min(df$x)), ceiling(max(df$x)), by = xstep)) +
    labs(y = "Predicted # of claims", x = xlab)

  return(gam_plot)
}
