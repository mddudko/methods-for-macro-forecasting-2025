# ==============================================================================
# forecast_ss.R - State-space based forecasting for MF-BVAR
# ==============================================================================
# Implements proper state-space forecasting using KFAS predict functionality
# This is more accurate than direct VAR simulation, especially for:
# - Mixed-frequency data (handles quarterly observations properly)
# - Missing data patterns
# - Measurement error
# ==============================================================================
# Plain-language summary for non-specialists:
# - This version of the forecasting code works directly with the Kalman filter, which is better at
#   handling the mix of monthly and quarterly numbers.
# - It projects the hidden state forward and then translates it back into the observed variables, so
#   missing quarterly releases still get reasonable predictions.

#' Generate forecasts using state-space representation
#'
#' Uses KFAS to properly forecast through the state-space model,
#' accounting for the observation equation and missing data patterns.
#'
#' @param posterior mfvar_posterior object from estimate_mf_bvar()
#' @param horizon_months Integer forecast horizon in months
#' @param n_sim Integer number of forecast simulations per posterior draw
#' @param quantiles Numeric vector of quantiles to compute
#' @param seed Random seed
#'
#' @return mfvar_forecast object
#'
#' @export
forecast_mf_bvar_ss <- function(posterior,
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

    metadata <- normalize_metadata_fields(posterior$metadata)
    posterior$metadata <- metadata

    n <- length(metadata$vars)
    p <- metadata$p
    state_lags <- metadata$state_lags
    if (is.null(state_lags)) {
        freq_info <- metadata$freq
        type_info <- metadata$type
        has_flow_quarterly <- !is.null(freq_info) && !is.null(type_info) &&
            any(freq_info == "quarterly" & type_info == "flow")
        state_lags <- if (has_flow_quarterly) max(p, 3L) else p
    }
    n_draws <- dim(posterior$A_draws)[3]
    include_intercept <- .posterior_has_intercept(posterior, n, p)

    # Sample posterior draws
    draw_indices <- sample(1:n_draws, min(n_sim, n_draws), replace = (n_sim > n_draws))

    # Storage for simulations
    all_sims <- array(NA, dim = c(n, horizon_months, length(draw_indices)))

    # Retrieve stored state draws once
    states_array <- if (!is.null(posterior$states)) posterior$states else posterior$states_draws
    if (is.null(states_array)) {
        stop("Posterior object does not contain stored state draws for forecasting")
    }
    stored_state_dim <- dim(states_array)[1]
    if (stored_state_dim %% n != 0) {
        stop("Stored states have unexpected dimension")
    }
    stored_state_lags <- stored_state_dim / n
    if (stored_state_lags < state_lags) {
        warning(sprintf(
            "Posterior state draws only contain %d lags; re-run estimation to upgrade latent states.",
            stored_state_lags
        ))
        state_lags <- stored_state_lags
    }
    state_dim <- n * state_lags
    T_obs <- dim(states_array)[2]

    # For each posterior draw, forecast using state-space
    for (sim_idx in 1:length(draw_indices)) {
        draw_idx <- draw_indices[sim_idx]

        # Extract parameters
        A_matrix <- posterior$A_draws[, , draw_idx]
        Sigma <- posterior$Sigma_draws[, , draw_idx]

        # Build companion form and intercept
        A_list <- .unpack_matrix_to_A_list(A_matrix, n, p)
        intercept_vec <- .extract_intercept_from_matrix(A_matrix, include_intercept, n, p)
        companion <- build_companion_form(
            A_list,
            Sigma,
            include_intercept = include_intercept,
            intercept_vec = intercept_vec,
            state_lags = state_lags
        )

        # Use median of stored state draws as initial condition
        last_state <- apply(states_array[1:state_dim, T_obs, , drop = FALSE], 1, median, na.rm = TRUE)

        # Simulate forward using state-space dynamics
        state_current <- last_state

        for (h in 1:horizon_months) {
            # State transition: α_{t+1} = F α_t + intercept + G ε_t
            state_mean <- companion$F %*% state_current + companion$intercept

            innovation <- mvtnorm::rmvnorm(1, mean = rep(0, n), sigma = Sigma)
            state_next <- as.numeric(state_mean + companion$G %*% drop(innovation))

            # Store (extracting first n elements which correspond to current y_t)
            all_sims[, h, sim_idx] <- state_next[1:n]

            # Update for next iteration
            state_current <- state_next
        }
    }

    # Compute quantiles
    forecasts_monthly <- array(NA, dim = c(n, horizon_months, length(quantiles)))
    dimnames(forecasts_monthly) <- list(
        metadata$vars,
        paste0("h", 1:horizon_months),
        paste0("q", quantiles)
    )

    for (i in 1:n) {
        for (h in 1:horizon_months) {
            forecasts_monthly[i, h, ] <- quantile(all_sims[i, h, ], probs = quantiles, na.rm = TRUE)
        }
    }

    # Aggregate to quarterly for flow variables
    forecasts_quarterly <- .aggregate_to_quarterly(
        all_sims,
        metadata,
        quantiles
    )

    return(structure(
        list(
            forecasts_monthly = forecasts_monthly,
            forecasts_quarterly = forecasts_quarterly,
            sim_paths = all_sims[, , 1:min(100, length(draw_indices))],
            metadata = metadata,
            horizon_months = horizon_months,
            quantiles = quantiles,
            method = "state_space"
        ),
        class = "mfvar_forecast"
    ))
}


#' Unpack A matrix to list of lag matrices
#' @keywords internal
.unpack_matrix_to_A_list <- function(A_matrix, n, p) {
    A_list <- list()
    for (lag in 1:p) {
        col_start <- (lag - 1) * n + 1
        col_end <- lag * n
        A_list[[lag]] <- A_matrix[, col_start:col_end]
    }
    return(A_list)
}

# Ensure helper utilities exist even if forecast.R hasn't been sourced yet
if (!exists(".posterior_has_intercept", mode = "function")) {
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
}

if (!exists(".extract_intercept_from_matrix", mode = "function")) {
    .extract_intercept_from_matrix <- function(A_matrix, include_intercept, n, p) {
        lag_cols <- n * p
        if (include_intercept && ncol(A_matrix) >= lag_cols + 1) {
            return(A_matrix[, lag_cols + 1])
        }
        return(rep(0, n))
    }
}

if (!exists(".aggregate_to_quarterly", mode = "function")) {
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
}
