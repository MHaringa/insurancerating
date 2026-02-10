#' Extract model data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' `extract_model_data()` retrieves underlying data from fitted models. It works
#' for objects of class `"glm"`, as well as objects produced by refitting
#' procedures (`"refitsmooth"` or `"refitrestricted"`).
#'
#' The wrapper [model_data()] is deprecated as of version 0.8.0; please use
#' [extract_model_data()] instead.
#'
#' @param x An object of class `"glm"`, `"refitsmooth"`, or `"refitrestricted"`.
#'
#' @details
#' For GLM objects, the function:
#' - returns the original data used in the model,
#' - attaches attributes with the relevant rating factors and any weights/offsets.
#'
#' For refit objects, the function:
#' - strips out auxiliary columns used for smoothing/restrictions,
#' - attaches attributes with information about rating factors, merged smooths,
#'   restrictions, and offsets.
#'
#' @return A `data.frame` of class `"model_data"`, containing the cleaned model
#' data with additional attributes:
#' \itemize{
#'   \item `rf` — names of risk factors in the model
#'   \item `offweights` — weights or offsets if present
#'   \item `mgd_rst`, `mgd_smt` — merged restrictions/smooths (refit objects only)
#'   \item `new_nm`, `old_nm` — new and old column names (refit objects only)
#' }
#'
#' @author Martin Haringa
#'
#' @examples
#' \dontrun{
#' library(insurancerating)
#' library(dplyr)
#'
#' # Fit GAM for claim frequency
#' age_policyholder_frequency <- riskfactor_gam(data = MTPL,
#'                                              nclaims = "nclaims",
#'                                              x = "age_policyholder",
#'                                              exposure = "exposure")
#'
#' # Determine clusters
#' clusters_freq <- construct_tariff_classes(age_policyholder_frequency)
#'
#' # Add clusters to MTPL portfolio
#' dat <- MTPL |>
#' mutate(age_policyholder_freq_cat = clusters_freq$tariff_classes) |>
#' mutate(across(where(is.character), as.factor)) |>
#' mutate(across(where(is.factor), ~biggest_reference(., exposure)))
#'
#' # Fit frequency and severity model
#' freq <- glm(nclaims ~ bm + age_policyholder_freq_cat, offset = log(exposure),
#'             family = poisson(), data = dat)
#' sev <- glm(amount ~ bm + zip, weights = nclaims,
#'            family = Gamma(link = "log"), data = dat |> filter(amount > 0))
#'
#' # Add predictions for freq and sev to data, and calculate premium
#' premium_df <- dat |>
#' add_prediction(freq, sev) |>
#' mutate(premium = pred_nclaims_freq * pred_amount_sev)
#'
#' # Fit unrestricted model
#' burn_unrestricted <- glm(premium ~ zip + bm + age_policyholder_freq_cat,
#' weights = exposure, family = Gamma(link = "log"), data = premium_df)
#'
#' # Impose smoothing and refit model
#' burn_restricted <- burn_unrestricted |>
#' add_smoothing(x_cut = "age_policyholder_freq_cat",
#' x_org = "age_policyholder",
#' breaks = seq(18, 95, 5)) |>
#' refit_glm()
#'
#' # Extract model data
#' extract_model_data(burn_restricted)
#' }
#'
#' @export
extract_model_data <- function(x) {

  if (!inherits(x, c("refitsmooth", "refitrestricted", "glm"))) {
    stop(
      "Input must be of class refitsmooth, glm or of class refitrestricted",
      call. = FALSE
    )
  }

  as_df <- function(z) {
    if (inherits(z, "data.table")) z <- as.data.frame(z)
    as.data.frame(z, stringsAsFactors = FALSE)
  }

  cls <- class(x)

  if (cls[length(cls)] == "glm") {
    out <- as_df(x$data)

    rf <- rating_table(x, signif_stars = FALSE)$df
    rf <- as_df(rf)
    colnames(rf)[3] <- "estimate"

    rf2_nm <- unique(rf$risk_factor[rf$risk_factor != "(Intercept)"])

    lst_call <- as.list(x$call)
    offweights <- NULL

    if (!is.null(lst_call$weights)) {
      offweights <- append(offweights, as.character(lst_call$weights))
    }
    if (!is.null(lst_call$offset)) {
      offweights <- append(offweights, as.character(lst_call$offset)[2])
    }

    attr(out, "offweights") <- offweights
    attr(out, "rf") <- rf2_nm
  } else {

    xdf <- as_df(x$data)
    xdf_nm <- names(xdf)

    rem_nm <- c(
      "breaks_min", "breaks_max", "start_oc", "end_oc",
      "start_", "end_", "avg_", "risk_factor"
    )

    keep_nm <- xdf_nm[!xdf_nm %in% rem_nm]
    out <- xdf[, keep_nm, drop = FALSE]
    out <- as_df(out)

    attr(out, "new_nm") <- attr(x, "new_col_nm")
    attr(out, "old_nm") <- attr(x, "old_col_nm")

    rf <- attr(x, "rf")
    mgd_smt <- attr(x, "mgd_smt")

    for (i in seq_along(mgd_smt)) {
      zsm <- gsub("_smooth$", "", mgd_smt[[i]][2])
      rf[rf == zsm] <- mgd_smt[[i]][1]
    }

    attr(out, "rf") <- rf
    attr(out, "mgd_rst") <- attr(x, "mgd_rst")
    attr(out, "mgd_smt") <- mgd_smt
    attr(out, "offweights") <- attr(x, "offweights")
  }

  out <- as_df(out)
  class(out) <- c("model_data", class(out))
  out
}



#' @rdname extract_model_data
#' @export
model_data <- function(x) {
  lifecycle::deprecate_warn("0.7.5.9000", "model_data()",
                            "extract_model_data()")
  extract_model_data(x)
}


#' Construct model points from Generalized Linear Model
#'
#' @description `r lifecycle::badge('experimental')` `construct_model_points()`
#' is used to construct model points from generalized linear models, and must
#' be preceded by `model_data()`. `construct_model_points()` can also be used
#' in combination with a data.frame.
#'
#' @param x Object of class model_data or of class data.frame
#' @param exposure column with exposure
#' @param exposure_by split column exposure by (e.g. year)
#' @param agg_cols list of columns to aggregate (sum) by, e.g. number of claims
#' @param drop_na drop na values (default to FALSE)
#'
#' @author Martin Haringa
#'
#' @importFrom stats na.omit
#' @importFrom stats as.formula
#' @import data.table
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' # With data.frame
#' library(dplyr)
#' mtcars |>
#'  select(cyl, vs) |>
#'  construct_model_points()
#'
#' mtcars |>
#'   select(cyl, vs, disp) |>
#'   construct_model_points(exposure = disp)
#'
#' mtcars |>
#'  select(cyl, vs, disp, gear) |>
#'  construct_model_points(exposure = disp, exposure_by = gear)
#'
#' mtcars |>
#'  select(cyl, vs, disp, gear, mpg) |>
#'  construct_model_points(exposure = disp, exposure_by = gear,
#'    agg_cols = list(mpg))
#'
#' # With glm
#' library(datasets)
#' data1 <- warpbreaks |>
#'  mutate(jaar = c(rep(2000, 10), rep(2010, 44))) |>
#'  mutate(exposure = 1) |>
#'  mutate(nclaims = 2)
#'
#' pmodel <- glm(breaks ~ wool + tension, data1, offset = log(exposure),
#'  family = poisson(link = "log"))
#'
#' model_data(pmodel) |>
#'  construct_model_points()
#'
#' model_data(pmodel) |>
#'  construct_model_points(agg_cols = list(nclaims))
#'
#' model_data(pmodel) |>
#'  construct_model_points(exposure = exposure, exposure_by = jaar) |>
#'  add_prediction(pmodel)
#'  }
#'
#' @export
construct_model_points <- function(x, exposure = NULL, exposure_by = NULL,
                                   agg_cols = NULL, drop_na = FALSE) {

  aggcols0 <- tryCatch(
    vapply(substitute(agg_cols)[-1], deparse, FUN.VALUE = character(1)),
    error = function(e) {
      stop("agg_cols must be a list, use agg_cols = list(var1, var2, var3)",
           call. = FALSE)
    })

  exposure_nm <- deparse(substitute(exposure))
  exposure_by_nm <- deparse(substitute(exposure_by))

  xdf <- x

  if (!inherits(x, c("model_data"))) {
    if (!inherits(x, "data.frame")) {
      stop("Input must be of class model_data, use model_data() to create data",
           call. = FALSE)
    }
    offweights <- NULL
    xdf <- data.frame(xdf)
    premium_nm <- setdiff(names(xdf), c(aggcols0, exposure_nm, exposure_by_nm))
    premium_df <- xdf[, premium_nm, drop = FALSE]
    premium_df <- unique(premium_df)
  }

  if (inherits(x, c("model_data"))) {
    premium_nm <- attr(x, "rf")
    if (isTRUE(exposure_nm %in% premium_nm)) {
      stop("Column exposure is already used as covariate in model.",
           call. = FALSE)
    }

    offweights <- unique(attr(x, "offweights"))

    if (isTRUE(exposure_nm %in% c(premium_nm, offweights)) &&
        exposure_by_nm == "NULL") {
      warning("Column exposure is already used in model.",
              call. = FALSE)
    }

    if (isTRUE(any(aggcols0 %in% offweights))) {
      stop("Column in list agg_cols is already used in model.",
           call. = FALSE)
    }

    if (isTRUE(exposure_by_nm %in% premium_nm)) {
      stop("Column exposure_by is already used as covariate in model.",
           call. = FALSE)
    }

    if (identical(offweights, exposure_nm)) {
      offweights <- paste0(offweights, "_99")
      xdf[[offweights]] <- xdf[[exposure_nm]]
    }
    if (isTRUE(offweights %in% aggcols0)) {
      offweights <- NULL
    }
    aggcols0 <- append(aggcols0, offweights)
    premium_df <- xdf[, premium_nm, drop = FALSE]
    premium_df <- unique(premium_df)
  }

  if (exposure_nm == "NULL") {

    xdf <- data.table::data.table(xdf)

    if (exposure_by_nm == "NULL") {

      if (length(aggcols0) == 0) {
        xdt_agg <- xdf[, .(count = .N), by = premium_nm]
      }

      if (length(aggcols0) > 0) {
        xdt_agg <- xdf[, c(.N, lapply(.SD, sum, na.rm = TRUE)),
                       by = premium_nm, .SDcols = aggcols0]
        data.table::setnames(xdt_agg, old = "N", new = "count")
      }
    }

    if (exposure_by_nm != "NULL") {

      if (length(aggcols0) == 0) {

        xdt_agg0 <- xdf[
          , .(count = .N), by = c(premium_nm, exposure_by_nm)][
            , (exposure_by_nm) := paste0("count_", get(exposure_by_nm))]

        f <- construct_fm(premium_nm, exposure_by_nm)
        xdt_agg <- data.table::dcast(xdt_agg0, f, value.var = "count")
      }

      if (length(aggcols0) > 0) {
        xdt_agg0 <- xdf[
          , c(.N, lapply(.SD, sum, na.rm = TRUE)), by =
            c(premium_nm, exposure_by_nm), .SDcols = aggcols0][
              , (exposure_by_nm) := paste0("count_", get(exposure_by_nm))]

        f <- construct_fm(c(premium_nm, aggcols0), exposure_by_nm)
        xdt_agg <- data.table::dcast(xdt_agg0, f, value.var = "N")
      }
    }

    xdt_agg <- data.frame(xdt_agg)
  }

  if (exposure_nm != "NULL") {
    xdf <- data.table::data.table(xdf)

    if (exposure_by_nm == "NULL") {

      if (length(aggcols0) == 0) {
        xdt_agg <- xdf[, lapply(.SD, sum, na.rm = TRUE), by = premium_nm,
                       .SDcols = exposure_nm]
      }

      if (length(aggcols0) > 0) {
        xdt_agg <- xdf[, lapply(.SD, sum, na.rm = TRUE), by = premium_nm,
                       .SDcols = c(aggcols0, exposure_nm)]
      }
    }

    if (exposure_by_nm != "NULL") {

      if (length(aggcols0) == 0) {

        xdt_agg0 <- xdf[
          , lapply(.SD, sum, na.rm = TRUE), by =
            c(premium_nm, exposure_by_nm), .SDcols = exposure_nm][
              , (exposure_by_nm) := paste0(exposure_nm, "_",
                                           get(exposure_by_nm))]

        f <- construct_fm(premium_nm, exposure_by_nm)
        xdt_agg <- data.table::dcast(xdt_agg0, f, value.var = exposure_nm)
      }

      if (length(aggcols0) > 0) {

        xdt_agg0 <- xdf[
          , lapply(.SD, sum, na.rm = TRUE), by = c(premium_nm, exposure_by_nm),
          .SDcols = c(aggcols0, exposure_nm)][
            , (exposure_by_nm) := paste0(exposure_nm, "_", get(exposure_by_nm))]

        f <- construct_fm(premium_nm, exposure_by_nm)
        xdt_agg <- data.table::dcast(xdt_agg0, f, value.var = exposure_nm)

        if (isTRUE(offweights %in% aggcols0)) {
          xdt_ext <- xdt_agg0[, lapply(.SD, sum, na.rm = TRUE), by = premium_nm,
                              .SDcols = aggcols0]
          xdt_agg <- merge(xdt_agg, xdt_ext, all.x = TRUE)
        }
      }
    }

    if (inherits(x, c("model_data"))) {
      data.table::setnames(xdt_agg, old = offweights,
                           new = gsub("_99$", "", offweights))
    }
    xdt_agg <- data.frame(xdt_agg)
  }

  if (isTRUE(drop_na)) {
    premium_vec <- sapply(premium_df, function(x) unique(na.omit(x)))
  }

  if (!isTRUE(drop_na)) {
    premium_vec <- sapply(premium_df, unique)
  }

  premium_complete <- Reduce(function(...) merge(..., by = NULL), premium_vec)
  names(premium_complete) <- names(premium_df)

  refinement_df <- NULL
  if (inherits(x, c("model_data"))) {
    mgd_rst <- attr(x, "mgd_rst")
    mgd_smt <- attr(x, "mgd_smt")
    refinement_nm <- append(mgd_rst, mgd_smt)
    refinement_df <- lapply(refinement_nm, function(y) x[, y, drop = FALSE])
    refinement_df <- lapply(refinement_df, unique)
  }

  premium_refinement_lst <- c(list(premium_complete), refinement_df)
  premium_join <- Reduce(function(...) merge(..., all.x = TRUE),
                         premium_refinement_lst)
  merge(premium_join, xdt_agg, all.x = TRUE)
}
