# ==============================================================================
# prior.R - Minnesota prior construction
# ==============================================================================
# Implements Minnesota prior as specified in Schorfheide & Song (2013):
# - Five types of dummy observations (λ₁, λ₂, λ₃, λ₄, λ₅)
# - Dummy 1: Own-lag random walk prior (λ₁)
# - Dummy 2: Lag decay prior (λ₂)
# - Dummy 3: Covariance prior for Σ (λ₃)
# - Dummy 4: Sum-of-coefficients prior (λ₄) - NEW
# - Dummy 5: Co-persistence prior (λ₅) - NEW
# - Preliminary AR fits to estimate σ_i scales
# ==============================================================================
# Plain-language summary for non-specialists:
# - This file encodes sensible “default behaviors” for each economic series so the model doesn’t chase
#   every wiggle in the data. Imagine giving the model gentle guidelines like “inflation usually keeps
#   moving like last month unless there’s strong evidence otherwise.”
# - Each λ parameter controls how tight those guidelines are. The code turns those guidelines into
#   numbers the estimation routine can digest.

#' Build Minnesota prior for VAR coefficients (Schorfheide & Song 2013 version)
#'
#' Constructs prior mean and covariance for VAR coefficients following
#' Schorfheide & Song (2013) with all five dummy observation types:
#' - Dummy 1: Random walk prior (own first lag coefficient = 1)
#' - Dummy 2: Lag decay (higher lags shrunk more)
#' - Dummy 3: Prior on Σ (residual covariance)
#' - Dummy 4: Sum-of-coefficients (long-run behavior)
#' - Dummy 5: Co-persistence (joint stationarity)
#'
#' @param data zoo object with transformed variables
#' @param p Integer lag order for VAR
#' @param lambda1 Overall tightness parameter (λ₁, smaller = stronger shrinkage)
#' @param lambda2 Lag decay exponent (λ₂, default: 1)
#' @param lambda3 Prior weight on Σ (λ₃, default: 1)
#' @param lambda4 Sum-of-coefficients weight (λ₄, default: 1)
#' @param lambda5 Co-persistence weight (λ₅, default: 1)
#' @param kappa_cross Cross-equation shrinkage factor (default: 0.5)
#' @param kappa_own Own-lag shrinkage factor (default: 1)
#' @param sigma_scales Optional vector of scale factors (if NULL, estimated from AR)
#' @param include_intercept Logical: include intercept term? (default: TRUE)
#' @param verbose Logical: print progress? (default: FALSE)
#'
#' @return List with:
#'   - prior_mean: vector of prior means for all coefficients
#'   - prior_variance: vector of prior variances for all coefficients
#'   - sigma_scales: scale factors used
#'   - hyperparameters: list of λ₁, λ₂, λ₃, λ₄, λ₅ values
#'   - structure: information about coefficient organization
#'   - y_mean: sample means for sum-of-coefficients dummy
#'
#' @export
build_minnesota_prior <- function(data,
                                  p,
                                  lambda1,
                                  lambda2 = 1,
                                  lambda3 = 1,
                                  lambda4 = 1,
                                  lambda5 = 1,
                                  kappa_cross = 0.5,
                                  kappa_own = 1,
                                  sigma_scales = NULL,
                                  include_intercept = TRUE,
                                  verbose = FALSE) {
  if (verbose) cat("Building Minnesota prior (Schorfheide & Song 2013)...\n")

  # Get dimensions
  n <- ncol(data)
  var_names <- colnames(data)

  # Estimate scale factors if not provided
  if (is.null(sigma_scales)) {
    if (verbose) cat("  Estimating scale factors from univariate AR models...\n")
    sigma_scales <- .estimate_sigma_scales(data, p_ar = min(p, 4), verbose)
  }

  # Compute sample means for sum-of-coefficients dummy
  y_mean <- colMeans(data, na.rm = TRUE)

  # Number of coefficients per equation
  k_per_eq <- n * p + (if (include_intercept) 1 else 0)

  # Initialize prior mean and variance
  prior_mean <- matrix(0, nrow = n, ncol = k_per_eq)
  prior_variance <- matrix(0, nrow = n, ncol = k_per_eq)

  # Fill in prior specifications equation by equation
  for (i in 1:n) {
    # Scale factor for equation i
    sigma_i <- sigma_scales[i]

    # Loop through lags
    for (lag in 1:p) {
      # Indices for this lag's coefficients
      lag_start <- (lag - 1) * n + 1
      lag_end <- lag * n

      for (j in 1:n) {
        coef_idx <- lag_start + (j - 1)

        # Prior mean: 1 for own first lag, 0 otherwise
        if (i == j && lag == 1) {
          prior_mean[i, coef_idx] <- 1
        } else {
          prior_mean[i, coef_idx] <- 0
        }

        # Prior variance using Minnesota formula with λ₂ (lag decay)
        sigma_j <- sigma_scales[j]

        # Cross-equation factor
        kappa_ij <- if (i == j) kappa_own else kappa_cross

        # Variance formula: [λ₁² · κ_ij² / l^λ₂] · (σ_i² / σ_j²)
        prior_variance[i, coef_idx] <- (lambda1^2 * kappa_ij^2 / lag^lambda2) * (sigma_i^2 / sigma_j^2)
      }
    }

    # Intercept prior (if included)
    if (include_intercept) {
      prior_mean[i, k_per_eq] <- 0
      # Large variance for intercept (weak prior)
      prior_variance[i, k_per_eq] <- 10 * sigma_i^2
    }
  }

  if (verbose) {
    cat(sprintf("  Variables: %d\n", n))
    cat(sprintf("  Lags: %d\n", p))
    cat(sprintf("  Coefficients per equation: %d\n", k_per_eq))
    cat(sprintf(
      "  Hyperparameters: λ₁=%.3f, λ₂=%.2f, λ₃=%.2f, λ₄=%.2f, λ₅=%.2f\n",
      lambda1, lambda2, lambda3, lambda4, lambda5
    ))
  }

  return(structure(
    list(
      prior_mean = prior_mean,
      prior_variance = prior_variance,
      sigma_scales = sigma_scales,
      y_mean = y_mean,
      hyperparameters = list(
        lambda1 = lambda1,
        lambda2 = lambda2,
        lambda3 = lambda3,
        lambda4 = lambda4,
        lambda5 = lambda5,
        kappa_cross = kappa_cross,
        kappa_own = kappa_own
      ),
      structure = list(n = n, p = p, k_per_eq = k_per_eq, var_names = var_names, include_intercept = include_intercept)
    ),
    class = "minnesota_prior"
  ))
}

#' Convert Minnesota prior to dummy observations (Schorfheide & Song 2013)
#'
#' Encodes the Minnesota prior as fictitious observations following
#' Schorfheide & Song (2013) with all five dummy observation types:
#' - Dummy 1: Own-lag (random walk) controlled by λ₁
#' - Dummy 2: Lag decay controlled by λ₂
#' - Dummy 3: Prior on Σ controlled by λ₃
#' - Dummy 4: Sum-of-coefficients controlled by λ₄
#' - Dummy 5: Co-persistence controlled by λ₅
#'
#' @param prior minnesota_prior object from build_minnesota_prior()
#'
#' @return List with:
#'   - Y_dummy: matrix of dummy dependent variables
#'   - X_dummy: matrix of dummy regressors
#'   - n_dummy: number of dummy observations added
#'
#' @export
build_dummy_observations <- function(prior) {
  n <- prior$structure$n
  p <- prior$structure$p
  lambda1 <- prior$hyperparameters$lambda1
  lambda2 <- prior$hyperparameters$lambda2
  lambda3 <- prior$hyperparameters$lambda3
  lambda4 <- prior$hyperparameters$lambda4
  lambda5 <- prior$hyperparameters$lambda5
  kappa_cross <- prior$hyperparameters$kappa_cross
  sigma <- prior$sigma_scales
  y_mean <- prior$y_mean
  include_intercept <- prior$structure$include_intercept
  k <- n * p + (if (include_intercept) 1 else 0)

  # Dummy 1: Own-lag random walk prior (controlled by λ₁)
  # Y_d1 = diag(σ/λ₁), X_d1 = [diag(σ/λ₁), 0, ..., 0]
  Y_d1 <- diag(sigma / lambda1, n)
  X_d1 <- matrix(0, n, k)
  X_d1[, 1:n] <- diag(sigma / lambda1, n)

  # Dummy 2: Lag decay (controlled by λ₂)
  # For each lag l, weight by l^(-λ₂)
  Y_d2_list <- list()
  X_d2_list <- list()

  for (lag in 1:p) {
    # Weight for this lag: (λ₁ · κ) / l^λ₂
    weight <- (lambda1 * kappa_cross) / (lag^lambda2)

    Y_d2_lag <- matrix(0, n, n)
    X_d2_lag <- matrix(0, n, k)

    # Fill diagonal block for this lag
    lag_start <- (lag - 1) * n + 1
    lag_end <- lag * n
    X_d2_lag[, lag_start:lag_end] <- diag(sigma * weight, n)

    Y_d2_list[[lag]] <- Y_d2_lag
    X_d2_list[[lag]] <- X_d2_lag
  }

  Y_d2 <- do.call(rbind, Y_d2_list)
  X_d2 <- do.call(rbind, X_d2_list)

  # Dummy 3: Prior on Σ (controlled by λ₃)
  # Adds λ₃ observations: Y_d3 = diag(σ), X_d3 = 0
  # This gives prior information about residual variances
  Y_d3 <- diag(sigma, n) / lambda3
  X_d3 <- matrix(0, n, k)

  # Dummy 4: Sum-of-coefficients prior (controlled by λ₄) - NEW
  # Imposes that Σ_{l=1}^p φ_{i,i,l} ≈ 1 (long-run unit root behavior)
  # Y_d4 = ȳ / λ₄, X_d4 = [ȳ/λ₄, ȳ/λ₄, ..., ȳ/λ₄, 0]  (p repeats of ȳ/λ₄)
  Y_d4 <- matrix(y_mean / lambda4, nrow = 1)
  X_d4 <- matrix(0, 1, k)
  for (lag in 1:p) {
    lag_start <- (lag - 1) * n + 1
    lag_end <- lag * n
    X_d4[1, lag_start:lag_end] <- y_mean / lambda4
  }

  # Dummy 5: Co-persistence prior (controlled by λ₅) - NEW
  # Imposes joint stationarity: if all variables at steady state, they remain there
  # Y_d5 = ȳ / λ₅, X_d5 = [ȳ/λ₅, ȳ/λ₅, ..., ȳ/λ₅, 1/λ₅]  (p repeats + intercept)
  Y_d5 <- matrix(y_mean / lambda5, nrow = 1)
  X_d5 <- matrix(0, 1, k)
  for (lag in 1:p) {
    lag_start <- (lag - 1) * n + 1
    lag_end <- lag * n
    X_d5[1, lag_start:lag_end] <- y_mean / lambda5
  }
  if (include_intercept) {
    X_d5[1, k] <- 1 / lambda5
  }

  # Combine all dummies
  Y_dummy <- rbind(Y_d1, Y_d2, Y_d3, Y_d4, Y_d5)
  X_dummy <- rbind(X_d1, X_d2, X_d3, X_d4, X_d5)

  return(list(
    Y_dummy = Y_dummy,
    X_dummy = X_dummy,
    n_dummy = nrow(Y_dummy)
  ))
}

#' Wrapper for Minnesota dummy observation construction
#'
#' Provides a backwards-compatible alias for older documentation naming the
#' dummy-observation helper `minnesota_to_dummies()`.
#'
#' @inheritParams build_dummy_observations
#' @return Same list as `build_dummy_observations()`
#' @export
minnesota_to_dummies <- function(prior) {
  build_dummy_observations(prior)
}

#' Build inverse-gamma prior for residual variances (DEPRECATED - use build_mniw_prior)
#'
#' Constructs weakly informative inverse-gamma priors for the diagonal
#' elements of the residual covariance matrix.
#'
#' @param n Integer number of variables
#' @param sigma_scales Vector of scale estimates
#' @param nu0 Prior degrees of freedom (default: 3)
#' @param scale_factor Factor to multiply scale estimate (default: 1)
#'
#' @return List with:
#'   - shape: vector of shape parameters (ν₀/2)
#'   - scale: vector of scale parameters (ν₀·σ²/2)
#'
#' @export
build_sigma_prior <- function(n, sigma_scales, nu0 = 3, scale_factor = 1) {
  # Inverse-Gamma(α, β) with α = ν₀/2, β = ν₀·σ²/2
  shape <- rep(nu0 / 2, n)
  scale <- (nu0 * (sigma_scales * scale_factor)^2) / 2

  return(list(shape = shape, scale = scale, nu0 = nu0))
}

#' Build full Normal-Inverted Wishart (MNIW) prior
#'
#' Constructs complete MNIW prior for Bayesian VAR as used in
#' Schorfheide & Song (2015). This is the conjugate prior that allows
#' joint sampling of (A, Σ) in the Gibbs sampler.
#'
#' Prior specification:
#'   vec(A) | Σ ~ N(m₀, Σ ⊗ Ω₀)
#'   Σ ~ IW(Ψ₀, ν₀)
#'
#' where IW = Inverse-Wishart distribution
#'
#' @param minnesota_prior minnesota_prior object from build_minnesota_prior()
#' @param nu0 Prior degrees of freedom for Inverse-Wishart (default: n + 2)
#' @param Psi0_scale Scale factor for Ψ₀ (default: 1)
#'
#' @return List (mniw_prior object) with:
#'   - m0: vec(A₀) prior mean (k*n vector where k = n*p + 1)
#'   - Omega0: Prior precision for coefficients (k x k)
#'   - Psi0: Prior scale matrix for IW (n x n)
#'   - nu0: Prior degrees of freedom for IW (scalar)
#'   - n: Number of variables
#'   - p: Number of lags
#'   - k: Number of regressors per equation
#'   - A_mean: Prior mean matrix (n x k) used in conjugate updates
#'   - Omega: Alias for Omega0 for readability in likelihood helpers
#'   - include_intercept: Logical flag copied from Minnesota prior
#'   - minnesota: Original minnesota_prior object
#'
#' @export
build_mniw_prior <- function(minnesota_prior, nu0 = NULL, Psi0_scale = 1) {
  n <- minnesota_prior$structure$n
  p <- minnesota_prior$structure$p
  k <- minnesota_prior$structure$k_per_eq # n*p + 1 (includes intercept)

  # Default degrees of freedom: just identified prior
  if (is.null(nu0)) {
    nu0 <- n + 2
  }

  # Prior mean: Stack by equations (each equation has same structure)
  # Minnesota gives n x k matrix, vec by row gives n*k vector
  m0 <- as.vector(t(minnesota_prior$prior_mean)) # Stack by row: [eq1, eq2, ..., eqn]

  # Prior precision Ω₀: In MNIW with Kronecker structure, Ω₀ is k x k
  # It's the SAME for all equations (homoscedastic prior on coefficients)
  # Take first equation's prior variance diagonal
  var_eq1 <- minnesota_prior$prior_variance[1, ] # k-vector
  Omega0 <- diag(1 / var_eq1) # k x k

  # Prior scale matrix Ψ₀ for Inverse-Wishart
  # Set Ψ₀ = diag(σ₁², ..., σₙ²) scaled appropriately
  sigma_scales <- minnesota_prior$sigma_scales
  Psi0 <- diag((nu0 - n - 1) * (sigma_scales * Psi0_scale)^2)

  # Ensure Psi0 is positive definite
  if (any(diag(Psi0) <= 0)) {
    warning("Some Psi0 diagonal elements non-positive, setting to 1")
    diag(Psi0)[diag(Psi0) <= 0] <- 1
  }

  return(structure(
    list(
      m0 = m0, # n*k vector
      Omega0 = Omega0, # k x k matrix
      Psi0 = Psi0, # n x n matrix
      nu0 = nu0, # scalar
      n = n,
      p = p,
      k = k,
      A_mean = minnesota_prior$prior_mean,
      Omega = Omega0,
      include_intercept = minnesota_prior$structure$include_intercept,
      minnesota = minnesota_prior
    ),
    class = "mniw_prior"
  ))
}

#' Print method for mniw_prior
#' @export
print.mniw_prior <- function(x, ...) {
  cat("Normal-Inverted Wishart Prior for VAR\n")
  cat("======================================\n")
  cat(sprintf("Variables (n): %d\n", x$n))
  cat(sprintf("Lags (p): %d\n", x$p))
  cat(sprintf("Coefficients per equation (k): %d\n", x$k))
  cat(sprintf("Total parameters: %d\n", length(x$m0)))
  cat("\nInverse-Wishart parameters:\n")
  cat(sprintf("  Degrees of freedom (ν₀): %d\n", x$nu0))
  cat(sprintf("  Scale matrix Ψ₀: %d x %d\n", nrow(x$Psi0), ncol(x$Psi0)))
  cat(sprintf(
    "  Diagonal elements: %s\n",
    paste(sprintf("%.3f", diag(x$Psi0)[1:min(5, x$n)]), collapse = ", ")
  ))
  if (x$n > 5) cat("  ...\n")
  cat("\nNormal parameters:\n")
  cat(sprintf("  Prior mean m₀: length %d\n", length(x$m0)))
  cat(sprintf("  Prior precision Ω₀: %d x %d (diagonal)\n", nrow(x$Omega0), ncol(x$Omega0)))
  invisible(x)
}

# ==============================================================================
# Internal helper functions
# ==============================================================================

#' Estimate scale factors from univariate AR models
#' @keywords internal
.estimate_sigma_scales <- function(data, p_ar = 4, verbose = FALSE) {
  n <- ncol(data)
  sigma_vec <- numeric(n)

  for (i in 1:n) {
    series <- data[, i]
    series_complete <- series[complete.cases(series)]

    if (length(series_complete) < p_ar + 10) {
      # Not enough data, use sample sd
      sigma_vec[i] <- sd(series_complete, na.rm = TRUE)
      if (is.na(sigma_vec[i]) || sigma_vec[i] == 0) sigma_vec[i] <- 1
      next
    }

    # Fit AR(p_ar) model
    ar_fit <- tryCatch(
      ar(series_complete, order.max = p_ar, aic = FALSE, method = "ols"),
      error = function(e) NULL
    )

    if (!is.null(ar_fit) && !is.null(ar_fit$var.pred)) {
      sigma_vec[i] <- sqrt(ar_fit$var.pred)
    } else {
      # Fallback to sample sd
      sigma_vec[i] <- sd(series_complete, na.rm = TRUE)
    }

    # Ensure positive
    if (is.na(sigma_vec[i]) || sigma_vec[i] <= 0) sigma_vec[i] <- 1
  }

  if (verbose) {
    cat("    Estimated scales: ", paste(sprintf("%.3f", sigma_vec), collapse = ", "), "\n")
  }

  return(sigma_vec)
}

#' Extract prior for a single equation
#'
#' @param prior minnesota_prior object
#' @param equation_index Integer index of equation (1 to n)
#'
#' @return List with mean vector and variance vector for that equation
#' @export
extract_equation_prior <- function(prior, equation_index) {
  if (equation_index < 1 || equation_index > prior$structure$n) {
    stop("Invalid equation index")
  }

  return(list(
    mean = prior$prior_mean[equation_index, ],
    variance = prior$prior_variance[equation_index, ],
    sigma_scale = prior$sigma_scales[equation_index]
  ))
}

#' Print method for minnesota_prior
#' @export
print.minnesota_prior <- function(x, ...) {
  cat("Minnesota Prior for VAR\n")
  cat("======================\n")
  cat(sprintf("Variables: %d\n", x$structure$n))
  cat(sprintf("Lags: %d\n", x$structure$p))
  cat(sprintf("Coefficients per equation: %d\n", x$structure$k_per_eq))
  cat("\nHyperparameters:\n")
  cat(sprintf("  Overall tightness (λ): %.4f\n", x$hyperparameters$lambda))
  cat(sprintf("  Lag decay (θ): %.2f\n", x$hyperparameters$theta))
  cat(sprintf("  Cross-equation shrinkage (κ): %.2f\n", x$hyperparameters$kappa_cross))
  cat("\nScale factors (σ):\n")
  cat("  ", paste(sprintf("%.3f", x$sigma_scales), collapse = ", "), "\n")
  invisible(x)
}
