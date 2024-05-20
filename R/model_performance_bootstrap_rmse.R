#' Bootstrapped RMSE
#'
#' @description Generate `n` bootstrap replicates to compute `n` root mean
#' squared errors.
#'
#' @param model a model object
#' @param data data used to fit model object
#' @param n number of bootstrap replicates (defaults to 50)
#' @param frac fraction used in training set if cross-validation is applied
#' (defaults to 1)
#' @param show_progress show progress bar (defaults to TRUE)
#' @param rmse_model numeric RMSE to show as vertical dashed line in autoplot()
#' (defaults to NULL)
#'
#' @return A list with components
#' \item{rmse_bs}{numerical vector with `n` root mean squared errors}
#' \item{rmse_mod}{root mean squared error for fitted (i.e. original) model}
#'
#' @author Martin Haringa
#'
#' @details To test the predictive ability of the fitted model it might be
#' helpful to determine the variation in the computed RMSE. The variation is
#' calculated by computing the root mean squared errors from `n` generated
#' bootstrap replicates. More precisely, for each iteration a sample with
#' replacement is taken from the data set and the model is refitted using
#' this sample. Then, the root mean squared error is calculated.
#'
#' @importFrom stats formula
#' @importFrom stats predict
#' @importFrom stats update
#'
#' @examples
#' \dontrun{
#' mod1 <- glm(nclaims ~ age_policyholder, data = MTPL,
#'     offset = log(exposure), family = poisson())
#'
#' # Use all records in MTPL
#' x <- bootstrap_rmse(mod1, MTPL, n = 80, show_progress = FALSE)
#' print(x)
#' autoplot(x)
#'
#' # Use 80% of records to test whether predictive ability depends on which 80%
#' # is used. This might for example be useful in case portfolio contains large
#' # claim sizes
#' x_frac <- bootstrap_rmse(mod1, MTPL, n = 50, frac = .8,
#'  show_progress = FALSE)
#' autoplot(x_frac) # Variation is quite small for Poisson GLM
#' }
#'
#' @export
bootstrap_rmse <- function(model, data, n = 50, frac = 1, show_progress = TRUE,
                           rmse_model = NULL) {

  if (frac > 1 || frac <= 0) {
    stop("frac should be in interval: (0,1]", call. = FALSE)
  }

  data <- as.data.frame(data)
  n_sample <- floor(frac * nrow(data))

  if (is.null(rmse_model)) {
    rmse_model <- rmse(model, data)
  }

  if (is.numeric(rmse_model)) {
    rmse_model <- rmse_model
  }

  rmse_vec <- vector(mode = "numeric", length = n)

  # Set progress bar
  if (isTRUE(show_progress)) {
    pb <- utils::txtProgressBar(max = n, style = 3)
  }

  for (i in 1:n){

    if (isTRUE(show_progress)) {
      utils::setTxtProgressBar(pb, i)
    }

    train_rows <- sample(c(TRUE, FALSE), n_sample, replace = TRUE)
    train <- data[train_rows, ]
    test <- data[!train_rows, ]
    model_train <- stats::update(model, . ~ ., data = train)
    model_fm <- stats::formula(model_train)[[2L]]

    if (frac < 1) {
      resp <- eval(model_fm, as.data.frame(test))
      x <- resp - stats::predict(model_train, test, type = "response")
      rmse_vec[i] <- sqrt(mean(x ^ 2, na.rm = TRUE))
    }

    if (frac == 1) {
      resp <- eval(model_fm, as.data.frame(train))
      x <- resp - stats::predict(model_train, train, type = "response")
      rmse_vec[i] <- sqrt(mean(x^2, na.rm = TRUE))
    }
  }

  return(structure(list(rmse_bs = rmse_vec,
                        rmse_mod = rmse_model),
                   class = "bootstrap_rmse"))
}

#' @export
print.bootstrap_rmse <- function(x, ...) {
  print(x$rmse_bs)
}

#' @export
as.vector.bootstrap_rmse <- function(x, ...) {
  return(as.vector(x$rmse_bs))
}


#' Automatically create a ggplot for objects obtained from bootstrap_rmse()
#'
#' @description Takes an object produced by `bootstrap_rmse()`, and plots the
#' simulated RMSE
#'
#' @param object bootstrap_rmse object produced by `bootstrap_rmse()`
#' @param fill color to fill histogram (default is "steelblue")
#' @param color color to plot line colors of histogram
#' @param ... other plotting parameters to affect the plot
#'
#' @author Martin Haringa
#'
#' @importFrom stats quantile
#' @importFrom ggplot2 after_stat
#'
#' @return a ggplot object
#'
#' @export
autoplot.bootstrap_rmse <- function(object, fill = NULL, color = NULL, ...) {

  rmse_bs <- object$rmse_bs
  rmse_mod <- object$rmse_mod
  dat <- data.frame(x = rmse_bs)

  if (is.null(fill) && is.null(color)) {
    fill <- "steelblue"
    color <- darken_color(fill)[2]
  }

  if (is.null(fill) && !is.null(color)) {
    color <- color
    fill <- lighten_color(color)[2]
  }

  if (!is.null(fill) && is.null(color)) {
    fill <- fill
    color <- darken_color(fill)[2]
  }

  conf_bounds <- tryCatch(
    {
      as.vector(stats::quantile(rmse_bs, c(0.025, 0.975)))
    },
    error = function(e) {
      NULL
    }
  )

  ggplot2::ggplot(dat, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            fill = fill,
                            alpha = 1,
                            bins = 30,
                            color = color) +
    ggplot2::geom_density(alpha = .4,
                          fill = "antiquewhite3",
                          color = "grey") +
    ggplot2::geom_vline(xintercept = rmse_mod, linetype = 2) + {
      if (!is.null(conf_bounds)) {
        ggplot2::geom_vline(xintercept = conf_bounds, linetype = 3)
      }
    } +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Bootstrapped RMSE", x = "(Simulated) RMSE",
                  y = "Density")
}
