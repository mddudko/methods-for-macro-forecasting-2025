# ============================================================================
# forecast_utils.R - Shared helpers for MF-BVAR forecasting
# ============================================================================
# Plain-language summary for non-specialists:
# - These are small helper tools the forecasting code relies on. They check whether the model learned
#   an intercept (baseline level), pull that information out, and convert monthly simulations into
#   quarterly totals when needed.
# - Think of them as the glue that keeps the forecasting math tidy and consistent.

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
    if (!is.null(posterior$metadata)) {
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
