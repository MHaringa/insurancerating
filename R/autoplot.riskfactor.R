#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' Automatically create a ggplot for objects obtained from rating_factors()
#'
#' @description Takes an object produced by \code{univariate()}, and plots the available input.
#'
#' @param object riskfactor object produced by \code{rating_factors()}
#' @param risk_factors character vector to define which factors are included. Defaults to all risk factors.
#' @param ncol number of columns in output (default is 1)
#' @param labels show labels with the exposure (default is TRUE)
#' @param dec.mark control the format of the decimal point, as well as the mark between intervals before the decimal point, choose either "," (default) or "."
#' @param ylab modify label for the y-axis
#' @param color change the color of the points and line ("dodgerblue" is default)
#' @param color_bg change the color of the histogram ("#E7B800" is default)
#' @param linetype use different linetypes (default is FALSE)
#' @param ... other plotting parameters to affect the plot
#'
#' @return a ggplot2 object
#' @export
#'
#' @import ggplot2
#' @importFrom patchwork wrap_plots
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' library(dplyr)
#' df <- MTPL2 %>%
#'     mutate_at(vars(area), as.factor) %>%
#'     mutate_at(vars(area), ~biggest_reference(., exposure))
#'
#' mod1 <- glm(nclaims ~ area + premium, offset = log(exposure), family = poisson(), data = df)
#' mod2 <- glm(nclaims ~ area, offset = log(exposure), family = poisson(), data = df)
#'
#' x <- rating_factors(mod1, mod2, model_data = df, exposure = exposure)
#' autoplot(x)
#'
autoplot.riskfactor <- function(object, risk_factors = NULL, ncol = 1, labels = TRUE, dec.mark = ",",
                                ylab = "rate", color = "dodgerblue", color_bg = "#E7B800", linetype = FALSE, ...){

  if ( !inherits(object, "riskfactor")){
    stop("autoplot.riskfactor requires a riskfactor object, use object = object")
  }

  df <- object$df
  models <- object$models
  models_nm <- paste0("est_", models)
  exposure_nm <- object$exposure
  expon <- object$expon

  df <- df[df$risk_factor != df$level, ]

  df_long <- tidyr::pivot_longer(df, cols = models_nm, names_to = "model", values_to = "est")
  df_long$model <- gsub("^est_", "", df_long$model)

  if ( !isTRUE(expon) ){
    df_long$est = exp(df_long$est)
  }


  if ( dec.mark == "," ){
    sep_fn <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  } else{
    sep_fn <- function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)
  }

  if ( is.null(risk_factors) ){
    rf_names <- unique(df$risk_factor)
  } else if ( all(risk_factors %in% df$risk_factor)) {
    rf_names <- risk_factors
  } else{
    rf_diff <- setdiff(risk_factors, unique(df$risk_factor))
    stop(paste(rf_diff, collapse = ", "), " unknown risk_factor(s)")
  }


  fig_list <- list()
  for ( i in 1:length(rf_names)){

    df1 <- df_long[df_long$risk_factor == rf_names[i],]
    df1[["level"]] <- factor(df1[["level"]], levels = unique(df1[["level"]]))

    if (exposure_nm != "NULL") {
      df1$s_axis_scale <- df1[[exposure_nm]] / max(df1[[exposure_nm]], na.rm = TRUE) * max(df1[["est"]], na.rm = TRUE)
      df1$y_print <- round(df1[[exposure_nm]], 0)
      df1 <- df1[!is.na(df1$est),]
      df1_bar <- unique(df1[, c("risk_factor", "level", exposure_nm, "s_axis_scale", "y_print")])
    }

    fig_list[[paste0("p", i)]] <- ggplot2::ggplot(data = df1) +
      ggplot2::theme_minimal() +
      { if (exposure_nm == "NULL")  ggplot2::scale_y_continuous(labels = sep_fn, limits = c(0, NA), expand = expansion(mult = c(0, 0.02))) } +
      { if (exposure_nm != "NULL") ggplot2::geom_bar(data = df1_bar, aes(x = .data[["level"]], y = .data[["s_axis_scale"]]),
                                                      stat = "identity", color = color_bg, fill = color_bg, alpha = 0.4) } +
      { if (exposure_nm != "NULL") ggplot2::scale_y_continuous(labels = sep_fn, limits = c(0, NA), expand = expansion(mult = c(0, 0.02)),
                                                                sec.axis = sec_axis(~ . * max(df1_bar[[exposure_nm]]) / max(df1[["est"]]),
                                                                                    name = exposure_nm, labels = sep_fn)) } +
      ggplot2::geom_point(aes(x = .data[["level"]], y = .data[["est"]], group = .data[["model"]], color = .data[["model"]])) +
      { if (isTRUE(linetype)) ggplot2::geom_line(aes(x = .data[["level"]], y = .data[["est"]], group = .data[["model"]],
                                                     color = .data[["model"]], linetype = .data[["model"]])) } +
      { if (!isTRUE(linetype)) ggplot2::geom_line(aes(x = .data[["level"]], y = .data[["est"]], group = .data[["model"]],
                                                      color = .data[["model"]])) } +
      { if ( isTRUE(labels) & exposure_nm != "NULL") ggplot2::geom_text(aes(x = .data[["level"]],
                                                                            y = .data[["s_axis_scale"]], label = sep_fn(.data[["y_print"]])),
                                                vjust = "inward", size = 3) } +
      ggplot2::labs(x = rf_names[i], y = ylab) +
      ggplot2::theme(legend.title = element_blank())
  }

  pl <- patchwork::wrap_plots(fig_list, ncol = ncol)
  return(pl)
}
