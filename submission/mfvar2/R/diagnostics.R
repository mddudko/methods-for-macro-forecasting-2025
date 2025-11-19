# ==============================================================================
# diagnostics.R - Forecast diagnostics (PIT, coverage, comovement)
# ==============================================================================
# Provides helper functions to compute probability integral transforms (PITs),
# central interval coverage rates, and optional comovement probabilities from
# rolling MF-BVAR forecast objects.
# ==============================================================================

#' Compute PIT and coverage diagnostics for rolling forecasts
#'
#' Given the output of [rolling_forecasts_mf_bvar()], compute probability
#' integral transforms (PITs) using the stored simulation paths as well as
#' empirical coverage rates for user-supplied central intervals. Optionally
#' evaluate joint (co-movement) probabilities based on user-defined rules.
#'
#' @param rolling_results Object returned by [rolling_forecasts_mf_bvar()].
#' @param intervals Numeric vector with central interval probabilities (e.g.,
#'   c(0.5, 0.7, 0.9)).
#' @param comovement_sets Optional list describing joint-event rules. Each
#'   element must be a list with fields `name`, `variables`, `directions`
#'   ("<" or ">"), `thresholds` (numeric vector), and optional `horizons`
#'   restricting the rule to a subset of horizons.
#'
#' @return List with components:
#'   - `pit`: array [origin x variable x horizon] of PIT values
#'   - `coverage`: data frame with coverage rates by variable/horizon/interval
#'   - `comovement`: array [origin x horizon x rule] (if rules supplied)
#'
#' @export
compute_pit_diagnostics <- function(rolling_results,
                                    intervals = c(0.5, 0.7, 0.9),
                                    comovement_sets = NULL) {
    if (!inherits(rolling_results, "rolling_results")) {
        stop("rolling_results must be output from rolling_forecasts_mf_bvar()")
    }

    origins <- rolling_results$origins
    horizons <- rolling_results$horizons
    metadata <- rolling_results$metadata
    n_origins <- length(origins)
    n_h <- length(horizons)
    vars <- metadata$vars
    n_vars <- length(vars)

    pit_array <- array(NA_real_,
        dim = c(n_origins, n_vars, n_h),
        dimnames = list(
            origin = as.character(origins),
            variable = vars,
            horizon = paste0("h", horizons)
        )
    )

    actuals <- rolling_results$actuals
    quantiles_array <- rolling_results$mfvar_forecasts_quantiles
    q_vals <- rolling_results$settings$forecast_quantiles

    if (is.null(actuals) || is.null(quantiles_array) || is.null(q_vals)) {
        warning("Rolling results do not contain the information needed for diagnostics.")
        return(list(pit = pit_array, coverage = NULL, comovement = NULL))
    }

    # PIT computation using stored simulation paths
    for (i in seq_len(n_origins)) {
        sim_obj <- rolling_results$mfvar_forecast_objects[[i]]
        if (is.null(sim_obj) || is.null(sim_obj$sim_paths)) next

        sim_paths <- sim_obj$sim_paths # dimensions: n_vars x horizon x n_paths
        if (length(dim(sim_paths)) != 3) next

        n_paths <- dim(sim_paths)[3]
        if (n_paths < 1) next

        for (v_idx in seq_len(n_vars)) {
            for (h_idx in seq_len(n_h)) {
                h <- horizons[h_idx]
                if (h > dim(sim_paths)[2]) next

                actual_val <- actuals[i, v_idx, h_idx]
                if (is.na(actual_val)) next

                draws <- sim_paths[v_idx, h, ]
                pit_array[i, v_idx, h_idx] <- mean(draws <= actual_val, na.rm = TRUE)
            }
        }
    }

    # Coverage diagnostics ----------------------------------------------------
    coverage_records <- list()
    if (!is.null(intervals) && length(intervals) > 0) {
        for (interval in intervals) {
            lower_prob <- (1 - interval) / 2
            upper_prob <- 1 - lower_prob

            lower_idx <- which.min(abs(q_vals - lower_prob))
            upper_idx <- which.min(abs(q_vals - upper_prob))

            for (v_idx in seq_len(n_vars)) {
                for (h_idx in seq_len(n_h)) {
                    lower_band <- quantiles_array[, v_idx, h_idx, lower_idx]
                    upper_band <- quantiles_array[, v_idx, h_idx, upper_idx]
                    actual_vec <- actuals[, v_idx, h_idx]

                    in_band <- actual_vec >= lower_band & actual_vec <= upper_band
                    coverage_rate <- mean(in_band, na.rm = TRUE)

                    coverage_records[[length(coverage_records) + 1]] <- data.frame(
                        variable = vars[v_idx],
                        horizon = horizons[h_idx],
                        interval = interval,
                        coverage = coverage_rate,
                        stringsAsFactors = FALSE
                    )
                }
            }
        }
    }
    coverage_df <- if (length(coverage_records) > 0) {
        do.call(rbind, coverage_records)
    } else {
        NULL
    }

    # Comovement probabilities -----------------------------------------------
    comovement_array <- NULL
    if (!is.null(comovement_sets) && length(comovement_sets) > 0) {
        n_rules <- length(comovement_sets)
        comovement_array <- array(NA_real_,
            dim = c(n_origins, n_h, n_rules),
            dimnames = list(
                origin = as.character(origins),
                horizon = paste0("h", horizons),
                rule = vapply(comovement_sets, `[[`, character(1), "name")
            )
        )

        for (i in seq_len(n_origins)) {
            sim_obj <- rolling_results$mfvar_forecast_objects[[i]]
            if (is.null(sim_obj) || is.null(sim_obj$sim_paths)) next
            sim_paths <- sim_obj$sim_paths
            if (length(dim(sim_paths)) != 3) next
            n_paths <- dim(sim_paths)[3]
            if (n_paths < 1) next

            for (rule_idx in seq_len(n_rules)) {
                rule <- comovement_sets[[rule_idx]]
                vars_rule <- rule$variables
                dirs_rule <- rule$directions
                thresh_rule <- rule$thresholds
                horiz_subset <- if (!is.null(rule$horizons)) rule$horizons else horizons

                if (length(vars_rule) != length(dirs_rule) ||
                    length(vars_rule) != length(thresh_rule)) {
                    stop(sprintf("Rule '%s' has inconsistent variable/direction/threshold lengths.", rule$name))
                }

                var_indices <- match(vars_rule, vars)
                if (any(is.na(var_indices))) {
                    stop(sprintf("Rule '%s' references unknown variables.", rule$name))
                }

                horizon_idx <- match(horiz_subset, horizons)
                horizon_idx <- horizon_idx[!is.na(horizon_idx)]
                if (length(horizon_idx) == 0) next

                for (h_idx in horizon_idx) {
                    if (horizons[h_idx] > dim(sim_paths)[2]) next

                    mask <- rep(TRUE, n_paths)
                    for (k in seq_along(var_indices)) {
                        draws <- sim_paths[var_indices[k], horizons[h_idx], ]
                        if (dirs_rule[k] == "<") {
                            mask <- mask & (draws < thresh_rule[k])
                        } else if (dirs_rule[k] == ">") {
                            mask <- mask & (draws > thresh_rule[k])
                        } else {
                            stop("directions must be '<' or '>'")
                        }
                    }
                    comovement_array[i, h_idx, rule_idx] <- mean(mask, na.rm = TRUE)
                }
            }
        }
    }

    list(
        pit = pit_array,
        coverage = coverage_df,
        comovement = comovement_array
    )
}
