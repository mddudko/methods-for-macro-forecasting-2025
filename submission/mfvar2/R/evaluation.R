# ==============================================================================
# evaluation.R - Forecast accuracy evaluation
# ==============================================================================
# Implements evaluation metrics as specified in instruction chunk 6 (section 11)
# ==============================================================================
# Plain-language summary for non-specialists:
# - Once we have forecasts, this file scores how well they did compared with a simpler AR(2) rule of
#   thumb.
# - It reports easy-to-read error statistics (like RMSE and MAE) so we know whether the fancy model
#   was worth the effort.

#' Evaluate forecast accuracy
#'
#' Computes RMSE, MAE, and optionally Diebold-Mariano tests comparing
#' MF-BVAR to AR(2) benchmarks.
#'
#' @param mfvar_forecasts Forecasts from MF-BVAR
#' @param ar2_forecasts Forecasts from AR(2) models
#' @param actuals Actual realized values
#' @param test_dm Logical: run Diebold-Mariano test? (default: FALSE)
#'
#' @return Data frame with evaluation metrics
#'
#' @export
evaluate_forecast_accuracy <- function(mfvar_forecasts,
                                       ar2_forecasts,
                                       actuals,
                                       test_dm = FALSE) {
  # Compute forecast errors
  errors_mfvar <- mfvar_forecasts - actuals
  errors_ar2 <- ar2_forecasts - actuals

  # Metrics
  results <- data.frame(
    model = c("MFVAR", "AR2"),
    RMSE = c(
      sqrt(mean(errors_mfvar^2, na.rm = TRUE)),
      sqrt(mean(errors_ar2^2, na.rm = TRUE))
    ),
    MAE = c(
      mean(abs(errors_mfvar), na.rm = TRUE),
      mean(abs(errors_ar2), na.rm = TRUE)
    ),
    Bias = c(
      mean(errors_mfvar, na.rm = TRUE),
      mean(errors_ar2, na.rm = TRUE)
    )
  )

  # Relative performance
  results$RMSE_ratio <- results$RMSE[1] / results$RMSE[2]

  return(results)
}

# ============================================================================
# Rolling evaluation summaries
# ============================================================================

#' Summarize accuracy from rolling forecast results
#'
#' Consumes the output of [rolling_forecasts_mf_bvar()] and produces
#' publication-ready accuracy tables (RMSE, MAE, Bias), optional
#' Diebold-Mariano statistics versus AR(2) benchmarks, and fan chart files.
#'
#' @param rolling_results Object returned by [rolling_forecasts_mf_bvar()].
#' @param metrics Character vector of metrics to report (subset of
#'   `c("rmse", "mae", "bias")`).
#' @param include_dm Logical flag toggling Diebold-Mariano tests.
#' @param dm_loss Loss function for DM tests ("rmse" = squared errors,
#'   "mae" = absolute errors).
#' @param export_path Optional file path (CSV) to save the accuracy table.
#' @param include_plots Logical: generate fan chart plots?
#' @param plot_dir Directory where fan charts will be saved (created if
#'   needed when `include_plots = TRUE`).
#' @param fan_chart_vars Optional subset of variables for plots. Defaults to
#'   all variables.
#' @param fan_chart_origins Optional subset of origins (yearmon or index).
#'   Defaults to the last origin.
#' @param plot_format File extension for saved plots ("pdf" or "png").
#'
#' @return List with `accuracy_table`, `dm_table`, and `plot_files`.
#'
#' @export
summarize_rolling_accuracy <- function(rolling_results,
                                       metrics = c("rmse", "mae", "bias"),
                                       include_dm = TRUE,
                                       dm_loss = c("rmse", "mae"),
                                       export_path = NULL,
                                       include_plots = FALSE,
                                       plot_dir = NULL,
                                       fan_chart_vars = NULL,
                                       fan_chart_origins = NULL,
                                       plot_format = c("pdf", "png")) {
  if (!inherits(rolling_results, "rolling_results")) {
    stop("rolling_results must be output from rolling_forecasts_mf_bvar()")
  }

  dm_loss <- match.arg(dm_loss)
  plot_format <- match.arg(plot_format)
  metrics <- unique(tolower(metrics))

  mf_forecasts <- rolling_results$mfvar_forecasts_median
  ar2_forecasts <- rolling_results$ar2_forecasts
  actuals <- rolling_results$actuals

  if (is.null(mf_forecasts) || is.null(actuals)) {
    stop("Rolling results do not contain median forecasts or actuals")
  }

  errors_mf <- mf_forecasts - actuals
  errors_ar2 <- if (!is.null(ar2_forecasts)) ar2_forecasts - actuals else NULL

  vars <- dimnames(mf_forecasts)[[2]]
  horizons <- rolling_results$horizons

  metric_values <- lapply(metrics, function(metric) {
    switch(metric,
      rmse = .calc_metric(errors_mf, errors_ar2, horizons, fun = function(x) sqrt(mean(x^2, na.rm = TRUE))),
      mae = .calc_metric(errors_mf, errors_ar2, horizons, fun = function(x) mean(abs(x), na.rm = TRUE)),
      bias = .calc_metric(errors_mf, errors_ar2, horizons, fun = function(x) mean(x, na.rm = TRUE)),
      stop(sprintf("Unknown metric '%s'", metric))
    )
  })
  names(metric_values) <- metrics

  # Assemble tidy table ----------------------------------------------------
  accuracy_rows <- list()
  for (metric in metrics) {
    mat <- metric_values[[metric]]
    df <- data.frame(
      variable = rep(vars, each = length(horizons)),
      horizon = rep(horizons, times = length(vars)),
      metric = metric,
      mfvar = as.vector(mat$mfvar),
      ar2 = as.vector(mat$ar2),
      ratio = as.vector(mat$ratio),
      stringsAsFactors = FALSE
    )
    accuracy_rows[[length(accuracy_rows) + 1]] <- df
  }
  accuracy_table <- do.call(rbind, accuracy_rows)

  if (!is.null(export_path)) {
    utils::write.csv(accuracy_table, export_path, row.names = FALSE)
  }

  # Diebold-Mariano tests --------------------------------------------------
  dm_table <- NULL
  if (include_dm && !is.null(errors_ar2)) {
    dm_results <- list()
    for (v_idx in seq_along(vars)) {
      for (h_idx in seq_along(horizons)) {
        e1 <- errors_mf[, v_idx, h_idx]
        e2 <- errors_ar2[, v_idx, h_idx]
        dm <- .dm_test(e1, e2, horizon = horizons[h_idx], loss = dm_loss)
        dm_results[[length(dm_results) + 1]] <- data.frame(
          variable = vars[v_idx],
          horizon = horizons[h_idx],
          statistic = dm$statistic,
          p_value = dm$p_value,
          stringsAsFactors = FALSE
        )
      }
    }
    dm_table <- do.call(rbind, dm_results)
  }

  # Fan chart plots --------------------------------------------------------
  plot_files <- NULL
  if (include_plots) {
    if (is.null(plot_dir)) {
      stop("plot_dir must be supplied when include_plots = TRUE")
    }
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

    if (is.null(fan_chart_vars)) {
      fan_chart_vars <- vars
    }

    origins <- rolling_results$origins
    if (is.null(fan_chart_origins)) {
      fan_chart_origins <- tail(origins, 1)
    }

    fan_chart_indices <- match(as.character(fan_chart_origins), as.character(origins))
    fan_chart_indices <- fan_chart_indices[!is.na(fan_chart_indices)]

    if (length(fan_chart_indices) == 0) {
      warning("No valid origins found for fan chart export")
    } else {
      plot_files <- c()
      for (idx in fan_chart_indices) {
        forecast_obj <- rolling_results$mfvar_forecast_objects[[idx]]
        if (is.null(forecast_obj)) next

        for (var in fan_chart_vars) {
          p <- tryCatch(
            plot_fan_chart(forecast_obj, variable = var),
            error = function(e) NULL
          )
          if (is.null(p)) next

          file_name <- sprintf(
            "fan_chart_%s_%s.%s",
            gsub("[[:^alnum:]]+", "_", var),
            gsub("[[:^alnum:]]+", "_", origins[idx]),
            plot_format
          )
          file_path <- file.path(plot_dir, file_name)
          ggplot2::ggsave(filename = file_path, plot = p, device = plot_format, width = 7, height = 4)
          plot_files <- c(plot_files, file_path)
        }
      }
    }
  }

  list(
    accuracy_table = accuracy_table,
    dm_table = dm_table,
    plot_files = plot_files
  )
}

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

.calc_metric <- function(errors_mf, errors_ar2, horizons, fun) {
  mf <- apply(errors_mf, c(2, 3), fun)
  if (is.null(errors_ar2)) {
    ar2 <- matrix(NA_real_, nrow = nrow(mf), ncol = ncol(mf))
    ratio <- matrix(NA_real_, nrow = nrow(mf), ncol = ncol(mf))
  } else {
    ar2 <- apply(errors_ar2, c(2, 3), fun)
    ratio <- mf / ar2
  }
  dimnames(mf) <- dimnames(errors_mf)[-1]
  list(mfvar = mf, ar2 = ar2, ratio = ratio)
}

.dm_test <- function(errors_1, errors_2, horizon, loss = c("rmse", "mae")) {
  loss <- match.arg(loss)
  ok <- stats::complete.cases(errors_1, errors_2)
  e1 <- errors_1[ok]
  e2 <- errors_2[ok]

  n <- length(e1)
  if (n < 5) {
    return(list(statistic = NA_real_, p_value = NA_real_))
  }

  d_raw <- if (loss == "rmse") {
    e1^2 - e2^2
  } else {
    abs(e1) - abs(e2)
  }

  dbar <- mean(d_raw, na.rm = TRUE)
  d <- d_raw - dbar
  lag <- max(1, min(horizon - 1, n - 1))

  # HAC variance (Newey-West with Bartlett kernel)
  gamma0 <- mean(d^2, na.rm = TRUE)
  hac <- gamma0
  if (lag >= 1) {
    for (ell in 1:lag) {
      weight <- 1 - ell / (lag + 1)
      cov_term <- mean(d[(ell + 1):n] * d[1:(n - ell)], na.rm = TRUE)
      hac <- hac + 2 * weight * cov_term
    }
  }

  if (!is.finite(hac) || hac <= 0) {
    return(list(statistic = NA_real_, p_value = NA_real_))
  }

  statistic <- sqrt(n) * dbar / sqrt(hac)
  p_value <- 2 * (1 - stats::pnorm(abs(statistic)))

  list(statistic = statistic, p_value = p_value)
}
