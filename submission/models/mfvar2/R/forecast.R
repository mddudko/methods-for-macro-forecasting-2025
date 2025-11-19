# ==============================================================================
# forecast.R - Forecasting with mixed frequency BVAR
# ==============================================================================
# Implements forecasting as specified in instruction chunk 6 (section 8):
# - Simulation-based predictive distributions
# - Handle monthly and quarterly outputs (flow aggregation)
# - Compute empirical quantiles
# ==============================================================================
# Plain-language summary for non-specialists:
# - After the model is estimated, this file rolls the simulations forward to see where each variable
#   might land over the next few months.
# - It produces fan charts and percentile bands, so you can say things like “there’s a 75% chance GDP
#   stays between these two values six months from now.”

#' Generate forecasts from MF-BVAR posterior
#'
#' Simulates forecast paths forward from posterior draws, handling mixed
#' frequency outputs. For flow quarterly variables, aggregates monthly
#' forecasts to quarterly.
#'
#' @param posterior mfvar_posterior object from estimate_mf_bvar()
#' @param horizon_months Integer forecast horizon in months
#' @param n_sim Integer number of forecast simulations per posterior draw
#' @param quantiles Numeric vector of quantiles to compute (default: c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
#' @param seed Random seed for reproducibility
#'
#' @return List (mfvar_forecast object) with:
#'   - forecasts_monthly: array (n_vars x horizon_months x n_quantiles)
#'   - forecasts_quarterly: list with quarterly aggregated forecasts
#'   - sim_paths: sample of simulation paths for visualization
#'   - metadata: metadata from posterior
#'   - horizon_months: forecast horizon
#'
#' @export
#' @importFrom stats quantile rnorm
forecast_mf_bvar <- function(posterior,
                             horizon_months = 12,
                             n_sim = 1000,
                             quantiles = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
                             seed = NULL) {
  if (!is.null(seed)) {
    old_seed <- .Random.seed
    on.exit(
      {
        .Random.seed <<- old_seed
      },
      add = TRUE
    )
    set.seed(seed)
  }

  n <- length(posterior$metadata$vars)
  p <- posterior$metadata$p
  n_draws <- dim(posterior$A_draws)[3]
  include_intercept <- .posterior_has_intercept(posterior, n, p)

  # Sample posterior draws to use
  draw_indices <- sample(1:n_draws, min(n_sim, n_draws), replace = (n_sim > n_draws))

  # Storage for simulations
  all_sims <- array(NA, dim = c(n, horizon_months, length(draw_indices)))

  # Get state dimensions
  # Handle both 'states' and 'states_draws' field names for backwards compatibility
  states_array <- if (!is.null(posterior$states)) posterior$states else posterior$states_draws

  state_dim <- dim(states_array)[1] # Should be n*p
  T_len <- dim(states_array)[2]
  n_state_draws <- dim(states_array)[3]

  # For each posterior draw, simulate forward
  for (sim_idx in 1:length(draw_indices)) {
    draw_idx <- draw_indices[sim_idx]

    # Extract parameters for this draw
    A_matrix <- posterior$A_draws[, , draw_idx]
    Sigma <- posterior$Sigma_draws[, , draw_idx]
    A_list <- .unpack_matrix_to_A_list(A_matrix, n, p)
    intercept_vec <- .extract_intercept_from_matrix(A_matrix, include_intercept, n, p)

    # Initialize forecast from last state
    # States are stacked: [y_t, y_{t-1}, ..., y_{t-p+1}]
    # Sample a random state draw
    state_draw_idx <- sample(1:n_state_draws, 1)
    last_state <- states_array[, T_len, state_draw_idx]

    # Extract last p observations from state
    # State structure: [y_t vars 1:n, y_{t-1} vars 1:n, ...]
    y_hist <- matrix(NA, p, n)
    for (lag in 1:p) {
      row_start <- (lag - 1) * n + 1
      row_end <- lag * n
      y_hist[lag, ] <- last_state[row_start:row_end]
    }
    # y_hist[1,] is most recent, y_hist[p,] is oldest

    # Simulate forward
    for (h in 1:horizon_months) {
      # Compute conditional mean
      y_mean <- intercept_vec
      for (lag in 1:p) {
        if (h > lag) {
          # Use simulated value
          y_mean <- y_mean + A_list[[lag]] %*% all_sims[, h - lag, sim_idx]
        } else {
          # Use historical value from state
          # y_hist[1,] is t, y_hist[2,] is t-1, etc.
          # For h=1, lag=1: use y_hist[1,] (most recent)
          # For h=1, lag=2: use y_hist[2,] (one period back)
          y_mean <- y_mean + A_list[[lag]] %*% y_hist[lag, ]
        }
      }

      # Draw innovation
      epsilon <- rnorm(n) * sqrt(diag(Sigma))

      # Forecast
      all_sims[, h, sim_idx] <- y_mean + epsilon
    }
  }

  # Compute quantiles
  forecasts_monthly <- array(NA, dim = c(n, horizon_months, length(quantiles)))
  dimnames(forecasts_monthly) <- list(
    posterior$metadata$vars,
    paste0("h", 1:horizon_months),
    paste0("q", quantiles)
  )

  for (i in 1:n) {
    for (h in 1:horizon_months) {
      forecasts_monthly[i, h, ] <- quantile(all_sims[i, h, ], probs = quantiles, na.rm = TRUE)
    }
  }

  # Aggregate to quarterly for quarterly variables
  forecasts_quarterly <- .aggregate_to_quarterly(
    all_sims,
    posterior$metadata,
    quantiles
  )

  return(structure(
    list(
      forecasts_monthly = forecasts_monthly,
      forecasts_quarterly = forecasts_quarterly,
      sim_paths = all_sims[, , 1:min(100, length(draw_indices))], # Store subset
      metadata = posterior$metadata,
      horizon_months = horizon_months,
      quantiles = quantiles
    ),
    class = "mfvar_forecast"
  ))
}

# ==============================================================================
# Helper functions (kept in this file so sourcing works without extra dependencies)
# ==============================================================================

#' Determine whether posterior draws include an intercept column
#' @keywords internal
.posterior_has_intercept <- function(posterior, n, p) {
  lag_cols <- n * p

  prior_flag <- NULL
  if (!is.null(posterior$prior) && !is.null(posterior$prior$structure)) {
    prior_flag <- posterior$prior$structure$include_intercept
  }
  if (!is.null(prior_flag)) {
    return(isTRUE(prior_flag))
  }

  metadata_flag <- NULL
  if (!is.null(posterior$metadata) && !is.null(posterior$metadata$include_intercept)) {
    metadata_flag <- posterior$metadata$include_intercept
  }
  if (!is.null(metadata_flag)) {
    return(isTRUE(metadata_flag))
  }

  n_coef_cols <- dim(posterior$A_draws)[2]
  return(n_coef_cols > lag_cols)
}

#' Extract intercept vector from coefficient matrix if present
#' @keywords internal
.extract_intercept_from_matrix <- function(A_matrix, include_intercept, n, p) {
  lag_cols <- n * p
  if (include_intercept && ncol(A_matrix) >= lag_cols + 1) {
    return(A_matrix[, lag_cols + 1])
  }
  return(rep(0, n))
}

#' Aggregate monthly simulations to quarterly frequency
#' @keywords internal
.aggregate_to_quarterly <- function(sims, metadata, quantiles) {
  n <- dim(sims)[1]
  horizon_months <- dim(sims)[2]
  n_sims <- dim(sims)[3]

  n_quarters <- ceiling(horizon_months / 3)

  quarterly_forecasts <- list()

  for (i in 1:n) {
    var_name <- metadata$vars[i]

    if (metadata$freq[var_name] == "quarterly") {
      qtr_sims <- matrix(NA, n_quarters, n_sims)

      for (q in 1:n_quarters) {
        months_in_q <- ((q - 1) * 3 + 1):min(q * 3, horizon_months)

        if (metadata$type[var_name] == "flow") {
          subset_3d <- sims[i, months_in_q, , drop = FALSE]
          qtr_sims[q, ] <- apply(subset_3d, 3, function(x) mean(x, na.rm = TRUE))
        } else {
          qtr_sims[q, ] <- sims[i, max(months_in_q), ]
        }
      }

      qtr_quantiles <- matrix(NA, n_quarters, length(quantiles))
      for (q in 1:n_quarters) {
        qtr_quantiles[q, ] <- stats::quantile(qtr_sims[q, ], probs = quantiles, na.rm = TRUE)
      }

      quarterly_forecasts[[var_name]] <- qtr_quantiles
    }
  }

  return(quarterly_forecasts)
}

#' Plot fan chart for a variable
#'
#' @param forecast mfvar_forecast object
#' @param variable Character variable name to plot
#' @param historical Optional historical data to include
#' @param intervals Probability intervals to plot (default: c(0.5, 0.7, 0.9))
#'
#' @return ggplot object
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line labs theme_minimal
plot_fan_chart <- function(forecast,
                           variable,
                           historical = NULL,
                           intervals = c(0.5, 0.7, 0.9)) {
  if (!variable %in% forecast$metadata$vars) {
    stop(paste("Variable", variable, "not found in forecast"))
  }

  var_idx <- which(forecast$metadata$vars == variable)

  # Extract forecasts
  fcast <- forecast$forecasts_monthly[var_idx, , ]
  horizon <- 1:ncol(fcast)

  # Build data frame for plotting
  median_vals <- fcast[, "q0.5"]

  plot_data <- data.frame(
    horizon = horizon,
    median = median_vals
  )

  # Add intervals
  for (int in intervals) {
    lower_q <- (1 - int) / 2
    upper_q <- 1 - lower_q
    plot_data[[paste0("lower_", int)]] <- fcast[, paste0("q", lower_q)]
    plot_data[[paste0("upper_", int)]] <- fcast[, paste0("q", upper_q)]
  }

  # Base plot
  p <- ggplot(plot_data, aes(x = horizon))

  # Add ribbons (largest to smallest)
  alphas <- seq(0.2, 0.6, length.out = length(intervals))
  for (i in length(intervals):1) {
    int <- intervals[i]
    p <- p + geom_ribbon(
      aes(ymin = .data[[paste0("lower_", int)]], ymax = .data[[paste0("upper_", int)]]),
      fill = "steelblue",
      alpha = alphas[i]
    )
  }

  # Add median line
  p <- p + geom_line(aes(y = median), color = "darkblue", size = 1.2)

  # Labels
  p <- p + labs(
    title = paste("Fan Chart:", variable),
    x = "Horizon (months)",
    y = "Forecast"
  ) + theme_minimal()

  return(p)
}

#' Print method for mfvar_forecast
#' @export
print.mfvar_forecast <- function(x, ...) {
  cat("MF-BVAR Forecast\n")
  cat("================\n")
  cat(sprintf("Variables: %d\n", length(x$metadata$vars)))
  cat(sprintf("Horizon: %d months\n", x$horizon_months))
  cat(sprintf("Quantiles: %s\n", paste(x$quantiles, collapse = ", ")))
  cat(sprintf(
    "\nMonthly forecasts: %d x %d x %d array\n",
    dim(x$forecasts_monthly)[1],
    dim(x$forecasts_monthly)[2],
    dim(x$forecasts_monthly)[3]
  ))
  cat(sprintf("Quarterly forecasts: %d variables\n", length(x$forecasts_quarterly)))
  invisible(x)
}
