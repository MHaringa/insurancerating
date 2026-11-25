.audit_individual_impact <- function(before, after, weights, weighting,
                                     breaks, thresholds, tolerance, keep) {
  if (!is.numeric(breaks) || length(breaks) < 2L || anyNA(breaks) ||
      is.unsorted(breaks, strictly = TRUE) || breaks[1L] != -Inf ||
      utils::tail(breaks, 1L) != Inf) {
    stop("`impact_breaks` must increase strictly from -Inf to Inf.", call. = FALSE)
  }
  if (!is.numeric(thresholds) || any(!is.finite(thresholds) | thresholds < 0)) {
    stop("`impact_thresholds` must contain finite non-negative values.", call. = FALSE)
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      !is.finite(tolerance) || tolerance < 0) {
    stop("`impact_tolerance` must be a finite non-negative scalar.", call. = FALSE)
  }
  valid <- is.finite(before) & before > 0 & is.finite(after) & after >= 0
  change <- rep(NA_real_, length(before))
  change[valid] <- after[valid] / before[valid] - 1
  valid <- valid & is.finite(change)
  change[!valid] <- NA_real_
  use <- valid & weights > 0
  x <- change[use]
  w <- weights[use]
  total <- sum(w)
  if (!is.finite(total)) stop("Total impact weight must be finite.", call. = FALSE)
  share <- function(select) if (total > 0) sum(w[select]) / total else NA_real_
  probabilities <- c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)
  quantiles <- rep(NA_real_, length(probabilities))
  if (total > 0) {
    order <- order(x)
    cum <- cumsum(w[order]) / total
    cum[length(cum)] <- 1
    quantiles <- x[order][findInterval(probabilities, cum, left.open = TRUE) + 1L]
  }
  band <- cut(x, breaks, include.lowest = TRUE, dig.lab = 10)
  band_count <- tabulate(as.integer(band), nlevels(band))
  band_weight <- numeric(length(band_count))
  if (length(w)) {
    sums <- rowsum(w, as.integer(band))
    band_weight[as.integer(rownames(sums))] <- sums[, 1L]
  }
  out <- list(
    summary = data.frame(
      weighting = weighting, records = length(change), invalid_records = sum(!valid),
      contributing_records = sum(use), total_weight = total, min = quantiles[1L],
      weighted_mean = if (total > 0) sum(x * (w / total)) else NA_real_,
      median = quantiles[4L], max = quantiles[7L],
      increase = share(x > tolerance), decrease = share(x < -tolerance),
      unchanged = share(abs(x) <= tolerance)
    ),
    quantiles = data.frame(probability = probabilities, change = quantiles),
    bands = data.frame(band = factor(levels(band), levels(band), ordered = TRUE),
                       records = band_count, weight = band_weight,
                       share = if (total > 0) band_weight / total else NA_real_),
    thresholds = data.frame(
      threshold = thresholds,
      increase = vapply(thresholds, function(t) share(x > t), numeric(1)),
      decrease = vapply(thresholds, function(t) share(x < -t), numeric(1)),
      either = vapply(thresholds, function(t) share(abs(x) > t), numeric(1))
    )
  )
  if (keep) out$records <- data.frame(before = unname(before), after = unname(after),
                                      change = change, weight = weights)
  out
}

#' Plot individual tariff impact bands
#' @param object An [audit_refinement()] result with `individual = TRUE`.
#' @param ... Not used.
#' @return A ggplot object.
#' @export
autoplot.refinement_audit <- function(object, ...) {
  .check_dots_empty(...)
  impact <- object$individual_impact
  if (is.null(impact)) {
    stop("Run audit_refinement(..., individual = TRUE) before plotting.", call. = FALSE)
  }
  weighting <- if (impact$summary$weighting == "exposure") "Exposure" else "Records"
  ggplot2::ggplot(impact$bands, ggplot2::aes(.data$band, .data$share)) +
    ggplot2::geom_col(fill = .plot_palette_ir()$frequency, na.rm = TRUE) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::labs(x = "Relative change (decimal)", y = paste("Share of", tolower(weighting)),
                  title = paste("Individual impact:", object$metric)) +
    ggplot2::theme_minimal() + .plot_grid_theme_ir()
}
