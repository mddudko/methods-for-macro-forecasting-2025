# ==============================================================================
# benchmark.R - AR(2) benchmark models
# ==============================================================================
# Implements AR(2) benchmarks as specified in instruction chunk 6 (section 10)
# ==============================================================================
# Plain-language summary for non-specialists:
# - These functions build the simple “baseline” forecasts that only look at each series’s last two
#   values. They act like a control group so we can check whether the full MF-BVAR is delivering extra
#   value.

#' Fit AR(2) model for a single series
#'
#' @param series Numeric vector or zoo object (time series)
#' @param include_intercept Logical (default: TRUE)
#'
#' @return List with coefficients and residual variance
#'
#' @export
fit_ar2 <- function(series, include_intercept = TRUE) {
  # Convert zoo to numeric vector if needed
  if (inherits(series, "zoo")) {
    series <- as.numeric(series)
  }

  series_complete <- series[complete.cases(series)]
  n <- length(series_complete)

  if (n < 10) {
    stop("Insufficient data for AR(2)")
  }

  # Build design matrix
  y <- series_complete[3:n]
  X <- cbind(series_complete[2:(n - 1)], series_complete[1:(n - 2)])

  if (include_intercept) {
    X <- cbind(1, X)
  }

  # OLS
  beta <- solve(crossprod(X), crossprod(X, y))
  resid <- y - X %*% beta
  sigma2 <- sum(resid^2) / (length(y) - ncol(X))

  return(list(
    coefficients = beta,
    sigma2 = sigma2,
    last_values = series_complete[(n - 1):n]
  ))
}

#' Generate AR(2) forecasts
#'
#' @param fit AR(2) fit from fit_ar2()
#' @param horizon Integer forecast horizon
#'
#' @return Vector of forecasts
#'
#' @export
forecast_ar2 <- function(fit, horizon) {
  forecasts <- numeric(horizon)

  # Extract coefficients (accounting for intercept position)
  if (length(fit$coefficients) == 3) {
    # With intercept: [intercept, phi1, phi2]
    intercept <- fit$coefficients[1]
    phi1 <- fit$coefficients[2]
    phi2 <- fit$coefficients[3]
  } else {
    # Without intercept: [phi1, phi2]
    intercept <- 0
    phi1 <- fit$coefficients[1]
    phi2 <- fit$coefficients[2]
  }

  # Initialize
  y_lag1 <- fit$last_values[2]
  y_lag2 <- fit$last_values[1]

  for (h in 1:horizon) {
    forecasts[h] <- intercept + phi1 * y_lag1 + phi2 * y_lag2

    # Update lags
    y_lag2 <- y_lag1
    y_lag1 <- forecasts[h]
  }

  return(forecasts)
}

#' Fit AR(2) for all variables in dataset
#'
#' @param data_prepared mfvar_data object
#' @param origin_index Integer index of last training observation
#'
#' @return List of AR(2) fits
#'
#' @export
fit_ar2_all_variables <- function(data_prepared, origin_index) {
  fits <- list()

  for (v in data_prepared$metadata$vars) {
    series <- data_prepared$data[1:origin_index, v]

    fits[[v]] <- tryCatch(
      fit_ar2(series),
      error = function(e) NULL
    )
  }

  return(fits)
}
