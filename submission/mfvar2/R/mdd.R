# ==============================================================================
# mdd.R - Marginal data density and hyperparameter tuning
# ==============================================================================
# Implements empirical Bayes hyperparameter selection following
# Schorfheide & Song (2013):
# - Marginal data density p(data | λ₁,...,λ₅) using Geweke (1999) harmonic mean
# - 5D grid search over (λ₁, λ₂, λ₃, λ₄, λ₅)
# - λ₃ typically fixed at 1
# - Returns optimal hyperparameters and diagnostics
# ==============================================================================
# Plain-language summary for non-specialists:
# - This file tries different “prior strength” settings and keeps the ones that make the overall model
#   most plausible given the data. It’s like tuning the firmness of those rubber bands mentioned in the
#   prior file.
# - The method, called marginal data density, is a statistical score that rewards settings that explain
#   the observed history without overfitting.

#' Tune Minnesota hyperparameters via marginal data density (Schorfheide & Song 2013)
#'
#' Selects all five Minnesota hyperparameters by maximizing the marginal
#' data density using Geweke's (1999) harmonic mean estimator.
#'
#' Default grids follow Schorfheide & Song (2013):
#' - λ₁: [0.05, 0.1, 0.15, 0.2, 0.3, 0.5] (overall tightness)
#' - λ₂: [1, 2, 3, 4, 5] (lag decay)
#' - λ₃: 1 (fixed, covariance prior)
#' - λ₄: [1, 2, 3, 4, 5] (sum-of-coefficients)
#' - λ₅: [1, 2, 3, 4, 5] (co-persistence)
#' Grid size: 6 × 5 × 5 × 5 = 750 combinations
#'
#' @param data_prepared Output from prepare_data_snb() or mfvar_data object
#' @param p Integer VAR lag order
#' @param lambda1_grid Vector of λ₁ values (default: c(0.05, 0.1, 0.15, 0.2, 0.3, 0.5))
#' @param lambda2_grid Vector of λ₂ values (default: c(1, 2, 3, 4, 5))
#' @param lambda3 Fixed λ₃ value (default: 1)
#' @param lambda4_grid Vector of λ₄ values (default: c(1, 2, 3, 4, 5))
#' @param lambda5_grid Vector of λ₅ values (default: c(1, 2, 3, 4, 5))
#' @param n_gibbs_mdd Number of Gibbs draws for MDD computation (default: 2000)
#' @param burnin_mdd Burn-in for MDD Gibbs sampler (default: 1000)
#' @param verbose Logical: print progress? (default: TRUE)
#' @param seed Random seed for reproducibility
#'
#' @return List (mfvar_hyperparams object) with:
#'   - lambda1_optimal: selected λ₁
#'   - lambda2_optimal: selected λ₂
#'   - lambda3_optimal: selected λ₃ (fixed)
#'   - lambda4_optimal: selected λ₄
#'   - lambda5_optimal: selected λ₅
#'   - log_mdd: log marginal data density at optimum
#'   - grid_results: data frame with all grid evaluations
#'   - method: "grid_5d"
#'
#' @export
tune_minnesota_hyper <- function(data_prepared,
                                 p,
                                 lambda1_grid = c(0.05, 0.1, 0.15, 0.2, 0.3, 0.5),
                                 lambda2_grid = c(1, 2, 3, 4, 5),
                                 lambda3 = 1,
                                 lambda4_grid = c(1, 2, 3, 4, 5),
                                 lambda5_grid = c(1, 2, 3, 4, 5),
                                 n_gibbs_mdd = 2000,
                                 burnin_mdd = 1000,
                                 verbose = TRUE,
                                 seed = NULL) {
  if (verbose) cat("=== Hyperparameter Tuning (Schorfheide & Song 2013) ===\n\n")

  # Extract data
  if (inherits(data_prepared, "mfvar_data")) {
    y_data <- data_prepared$data
    metadata <- data_prepared$metadata
  } else {
    stop("data_prepared must be output from prepare_data_snb()")
  }

  # Expand 5D grid (λ₃ is fixed)
  grid <- expand.grid(
    lambda1 = lambda1_grid,
    lambda2 = lambda2_grid,
    lambda3 = lambda3,
    lambda4 = lambda4_grid,
    lambda5 = lambda5_grid,
    stringsAsFactors = FALSE
  )

  if (verbose) {
    cat(sprintf("5D Grid search over %d combinations:\n", nrow(grid)))
    cat(sprintf("  λ₁ (tightness): %s\n", paste(lambda1_grid, collapse = ", ")))
    cat(sprintf("  λ₂ (lag decay): %s\n", paste(lambda2_grid, collapse = ", ")))
    cat(sprintf("  λ₃ (Σ prior): %.2f (fixed)\n", lambda3))
    cat(sprintf("  λ₄ (sum-of-coef): %s\n", paste(lambda4_grid, collapse = ", ")))
    cat(sprintf("  λ₅ (co-persist): %s\n", paste(lambda5_grid, collapse = ", ")))
    cat(sprintf(
      "\nThis will take significant time (~%.0f min if each MDD takes 10 sec)\n\n",
      nrow(grid) * 10 / 60
    ))
  }

  # Compute marginal data density for each grid point
  grid$log_mdd <- NA_real_

  for (i in 1:nrow(grid)) {
    if (verbose && (i %% max(1, nrow(grid) %/% 20) == 0 || i <= 5)) {
      cat(sprintf(
        "  [%d/%d] (%.1f%%) λ₁=%.3f, λ₂=%.1f, λ₄=%.1f, λ₅=%.1f\n",
        i, nrow(grid), 100 * i / nrow(grid),
        grid$lambda1[i], grid$lambda2[i], grid$lambda4[i], grid$lambda5[i]
      ))
    }

    grid$log_mdd[i] <- tryCatch(
      .compute_marginal_data_density_geweke(
        y_data = y_data,
        metadata = metadata,
        p = p,
        lambda1 = grid$lambda1[i],
        lambda2 = grid$lambda2[i],
        lambda3 = grid$lambda3[i],
        lambda4 = grid$lambda4[i],
        lambda5 = grid$lambda5[i],
        n_gibbs = n_gibbs_mdd,
        burnin = burnin_mdd,
        seed = seed
      ),
      error = function(e) {
        if (verbose) cat(sprintf("    Error: %s\n", e$message))
        return(-Inf)
      }
    )
  }

  # Find best grid point
  best_idx <- which.max(grid$log_mdd)
  lambda1_best <- grid$lambda1[best_idx]
  lambda2_best <- grid$lambda2[best_idx]
  lambda3_best <- grid$lambda3[best_idx]
  lambda4_best <- grid$lambda4[best_idx]
  lambda5_best <- grid$lambda5[best_idx]
  log_mdd_best <- grid$log_mdd[best_idx]

  if (verbose) {
    cat("\n=== Grid Search Results ===\n")
    cat(sprintf("  Best λ₁ (tightness): %.4f\n", lambda1_best))
    cat(sprintf("  Best λ₂ (lag decay): %.2f\n", lambda2_best))
    cat(sprintf("  Best λ₃ (Σ prior): %.2f (fixed)\n", lambda3_best))
    cat(sprintf("  Best λ₄ (sum-of-coef): %.2f\n", lambda4_best))
    cat(sprintf("  Best λ₅ (co-persist): %.2f\n", lambda5_best))
    cat(sprintf("  Log MDD: %.2f\n", log_mdd_best))
  }

  return(structure(
    list(
      lambda1_optimal = lambda1_best,
      lambda2_optimal = lambda2_best,
      lambda3_optimal = lambda3_best,
      lambda4_optimal = lambda4_best,
      lambda5_optimal = lambda5_best,
      log_mdd = log_mdd_best,
      grid_results = grid,
      method = "grid_5d"
    ),
    class = "mfvar_hyperparams"
  ))
}

#' Plot marginal data density grid results
#'
#' Creates a simple visualization of the log marginal data density evaluated
#' over the hyperparameter grid used by [tune_minnesota_hyper()]. If an
#' `mfvar_hyperparams` object is provided, the embedded `grid_results` data
#' frame is used automatically.
#'
#' @param results Either the object returned by [tune_minnesota_hyper()] or a
#'   data frame with at least the columns `lambda1`, `lambda2`, `lambda4`,
#'   `lambda5`, and `log_mdd`.
#' @return A `ggplot` object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point facet_grid labs theme_minimal label_both
plot_mdd_grid <- function(results) {
  grid_df <- if (inherits(results, "mfvar_hyperparams")) {
    results$grid_results
  } else {
    results
  }

  required_cols <- c("lambda1", "lambda2", "lambda4", "lambda5", "log_mdd")
  missing_cols <- setdiff(required_cols, names(grid_df))
  if (length(missing_cols) > 0) {
    stop("Grid results are missing columns: ", paste(missing_cols, collapse = ", "))
  }

  grid_df$lambda4 <- factor(grid_df$lambda4, levels = sort(unique(grid_df$lambda4)))
  grid_df$lambda5 <- factor(grid_df$lambda5, levels = sort(unique(grid_df$lambda5)))

  ggplot(grid_df, aes(x = lambda1, y = lambda2, color = log_mdd)) +
    geom_point(size = 3) +
    facet_grid(lambda4 ~ lambda5, labeller = label_both) +
    labs(
      x = expression(lambda[1] * " (tightness)"),
      y = expression(lambda[2] * " (lag decay)"),
      color = "log MDD",
      title = "Marginal data density over hyperparameter grid"
    ) +
    theme_minimal()
}

# ==============================================================================
# Internal MDD computation functions
# ==============================================================================

#' Compute marginal data density using Geweke's harmonic mean estimator
#'
#' Implements Geweke (1999) harmonic mean estimator for marginal likelihood
#' as used in Schorfheide & Song (2013).
#'
#' Formula: p(Y|λ) ≈ [1/N Σ f(W^(i)) / p(Y,W^(i)|λ)]^(-1)
#'
#' where W = latent unobserved quarterly data
#'       f(W) = truncated multivariate normal importance function
#'
#' @param y_data zoo object with data
#' @param metadata Metadata list
#' @param p Integer lag order
#' @param lambda1 λ₁ hyperparameter
#' @param lambda2 λ₂ hyperparameter
#' @param lambda3 λ₃ hyperparameter
#' @param lambda4 λ₄ hyperparameter
#' @param lambda5 λ₅ hyperparameter
#' @param n_gibbs Number of Gibbs draws (default: 2000)
#' @param burnin Burn-in period (default: 1000)
#' @param seed Random seed
#'
#' @return Scalar log marginal data density
#'
#' @keywords internal
.compute_marginal_data_density_geweke <- function(y_data, metadata, p,
                                                  lambda1 = NULL, lambda2 = NULL, lambda3 = NULL,
                                                  lambda4 = NULL, lambda5 = NULL,
                                                  lambda = NULL, theta = NULL, kappa_cross = NULL,
                                                  n_gibbs = 2000, burnin = 1000,
                                                  seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  metadata <- normalize_metadata_fields(metadata)

  # Backward compatibility with 3-parameter interface
  if (is.null(lambda1) && !is.null(lambda)) lambda1 <- lambda
  if (is.null(lambda2) && !is.null(theta)) lambda2 <- theta
  if (is.null(lambda5) && !is.null(kappa_cross)) lambda5 <- kappa_cross

  if (is.null(lambda1)) lambda1 <- 0.2
  if (is.null(lambda2)) lambda2 <- 1
  if (is.null(lambda3)) lambda3 <- 1
  if (is.null(lambda4)) lambda4 <- 1
  if (is.null(lambda5)) lambda5 <- 1

  n <- length(metadata$vars)
  T_len <- nrow(y_data)

  # Check if we have enough data
  Y_mat <- as.matrix(y_data)
  complete_rows <- complete.cases(Y_mat)
  if (sum(complete_rows) < p + 20) {
    return(-Inf)
  }

  # Build Minnesota prior with all 5 hyperparameters
  minnesota_prior <- build_minnesota_prior(
    data = y_data,
    p = p,
    lambda1 = lambda1,
    lambda2 = lambda2,
    lambda3 = lambda3,
    lambda4 = lambda4,
    lambda5 = lambda5,
    include_intercept = TRUE,
    verbose = FALSE
  )

  # Build MNIW prior
  mniw_prior <- build_mniw_prior(minnesota_prior, nu0 = n + 2)

  # Initialize Gibbs sampler
  init <- .initialize_gibbs_mdd(y_data, metadata, p, minnesota_prior)
  A_current <- init$A_list
  Sigma_current <- init$Sigma
  intercept_current <- init$intercept

  # Storage for draws
  total_iter <- burnin + n_gibbs
  W_draws <- vector("list", n_gibbs)
  A_draws <- vector("list", n_gibbs)
  Sigma_draws <- vector("list", n_gibbs)

  draw_idx <- 0
  for (iter in 1:total_iter) {
    # Step 1: Draw latent states
    states_draw <- .gibbs_draw_states_mdd(
      y_data = y_data,
      metadata = metadata,
      A_list = A_current,
      Sigma = Sigma_current,
      p = p,
      intercept = intercept_current
    )

    y_latent <- extract_yt_from_states(states_draw, n)

    # Step 2 & 3: Draw (A, Σ) from MNIW
    mniw_draw <- .gibbs_draw_mniw(y_latent, mniw_prior, p, seed = NULL)
    A_current <- mniw_draw$A_list
    Sigma_current <- mniw_draw$Sigma
    intercept_current <- mniw_draw$intercept

    # Store draws after burnin
    if (iter > burnin) {
      draw_idx <- draw_idx + 1
      W_draws[[draw_idx]] <- .extract_latent_W(y_latent, y_data, metadata)
      A_draws[[draw_idx]] <- list(A_list = A_current, intercept = intercept_current)
      Sigma_draws[[draw_idx]] <- Sigma_current
    }
  }

  # Step 2: Construct importance function f(W)
  W_matrix <- do.call(rbind, lapply(W_draws, function(w) {
    if (length(w) > 0) as.vector(w) else numeric(0)
  }))

  if (nrow(W_matrix) < 10 || ncol(W_matrix) == 0) {
    # No latent variables - use analytical MDD
    return(.compute_mdd_analytical(y_data, metadata, p, mniw_prior))
  }

  W_mean <- colMeans(W_matrix, na.rm = TRUE)
  W_cov <- cov(W_matrix, use = "pairwise.complete.obs")
  W_cov <- W_cov + diag(1e-6, ncol(W_cov)) # Regularization

  # Truncation parameter (Geweke 1999)
  d_W <- length(W_mean)
  tau <- qchisq(0.9, df = d_W)

  # Step 3: Compute harmonic mean
  log_ratios <- numeric(n_gibbs)

  for (i in 1:n_gibbs) {
    W_i <- W_draws[[i]]

    if (length(W_i) == 0) {
      log_ratios[i] <- 0
      next
    }

    # log f(W_i): truncated normal density
    log_f_W <- .log_truncated_mvnorm(as.vector(W_i), W_mean, W_cov, tau)

    # log p(Y, W_i | λ): joint density
    log_joint <- .compute_log_joint_mniw(
      W_i, y_data, metadata, A_draws[[i]], Sigma_draws[[i]],
      mniw_prior, p
    )

    log_ratios[i] <- log_f_W - log_joint
  }

  # Harmonic mean with log-sum-exp trick
  valid_ratios <- log_ratios[is.finite(log_ratios)]

  if (length(valid_ratios) < n_gibbs / 2) {
    return(-1e10) # Too many invalid ratios
  }

  max_log_ratio <- max(valid_ratios)
  log_mean_exp <- max_log_ratio + log(mean(exp(valid_ratios - max_log_ratio)))
  log_mdd <- -log_mean_exp

  return(log_mdd)
}

# Backwards-compatible alias used by unit tests and older scripts
.compute_marginal_data_density <- .compute_marginal_data_density_geweke

#' @keywords internal
.initialize_gibbs_mdd <- function(y_data, metadata, p, prior) {
  if (exists(".initialize_gibbs", mode = "function")) {
    return(.initialize_gibbs(y_data, metadata, p, prior, init_method = "ols"))
  }

  # Fallback: simple zero initialization if sampler helper is unavailable
  n <- ncol(y_data)
  A_matrix <- prior$prior_mean[, seq_len(n * p), drop = FALSE]
  A_list <- vector("list", p)
  for (lag in seq_len(p)) {
    col_start <- (lag - 1) * n + 1
    col_end <- lag * n
    A_list[[lag]] <- A_matrix[, col_start:col_end, drop = FALSE]
  }
  Sigma <- diag(prior$sigma_scales^2)
  lag_cols <- n * p
  intercept <- if (isTRUE(prior$structure$include_intercept) && ncol(prior$prior_mean) >= lag_cols + 1) {
    prior$prior_mean[, lag_cols + 1]
  } else {
    rep(0, n)
  }
  list(A_list = A_list, Sigma = Sigma, intercept = intercept)
}

#' Extract latent unobserved W values
#'
#' Extracts the quarterly values that were unobserved (missing) in the data.
#' For monthly-quarterly mixed frequency, these are the within-quarter monthly values.
#'
#' @keywords internal
.extract_latent_W <- function(y_latent, y_data, metadata) {
  metadata <- normalize_metadata_fields(metadata)
  # Find which variables are quarterly (aggregated from monthly)
  freq_code <- metadata$frequency_code
  quarterly_vars <- which(freq_code == 2)

  if (length(quarterly_vars) == 0) {
    return(numeric(0))
  }

  T_len <- ncol(y_latent)
  W_vals <- c()

  # For quarterly variables, extract the 2 missing monthly values per quarter
  for (var_idx in quarterly_vars) {
    for (t in seq(3, T_len, by = 3)) { # Every third month (end of quarter)
      # Missing values are at t-2 and t-1 (first two months of quarter)
      W_vals <- c(W_vals, y_latent[var_idx, t - 2], y_latent[var_idx, t - 1])
    }
  }

  return(W_vals)
}

#' Compute log density of truncated multivariate normal
#'
#' Implements Geweke (1999) truncated MVN importance function:
#'   f(W) = N(W | μ, Σ) / P(χ² ≤ τ)
#'   where truncation requires (W - μ)' Σ^{-1} (W - μ) ≤ τ
#'
#' @keywords internal
.log_truncated_mvnorm <- function(W, mu, Sigma, tau) {
  d <- length(W)

  # Compute Mahalanobis distance
  W_centered <- W - mu
  Sigma_inv <- solve(Sigma)
  mahal_dist <- as.numeric(crossprod(W_centered, Sigma_inv %*% W_centered))

  # Check truncation
  if (mahal_dist > tau) {
    return(-Inf) # Outside truncation region
  }

  # Log of untruncated MVN density
  log_det_Sigma <- determinant(Sigma, logarithm = TRUE)$modulus[1]
  log_mvn <- -0.5 * d * log(2 * pi) - 0.5 * log_det_Sigma - 0.5 * mahal_dist

  # Truncation constant: P(χ²_d ≤ τ)
  log_trunc_const <- pchisq(tau, df = d, log.p = TRUE)

  return(log_mvn - log_trunc_const)
}

#' Compute log joint density p(Y, W | λ)
#'
#' Uses MNIW conjugacy: p(Y, W | λ) = p(Y | W, A, Σ) × p(A, Σ | λ)
#' Integrated over (A, Σ) using closed form from MNIW.
#'
#' @keywords internal
.compute_log_joint_mniw <- function(W, y_data, metadata, coeffs, Sigma, mniw_prior, p) {
  metadata <- normalize_metadata_fields(metadata)
  # Normalize coefficient representation
  if (is.list(coeffs) && !is.null(coeffs$A_list)) {
    A_list <- coeffs$A_list
    intercept_vec <- coeffs$intercept
  } else {
    A_list <- coeffs
    intercept_vec <- NULL
  }

  n <- nrow(A_list[[1]])
  if (is.null(intercept_vec)) intercept_vec <- rep(0, n)

  # Reconstruct full y_latent with W values filled in
  y_latent <- t(as.matrix(y_data)) # Variables x time

  # Fill in latent W values for quarterly variables
  freq_code <- metadata$frequency_code
  quarterly_vars <- which(freq_code == 2)

  if (length(quarterly_vars) > 0 && length(W) > 0) {
    T_len <- ncol(y_latent)
    W_idx <- 1

    for (var_idx in quarterly_vars) {
      for (t in seq(3, T_len, by = 3)) {
        if (W_idx <= length(W)) {
          y_latent[var_idx, t - 2] <- W[W_idx]
          W_idx <- W_idx + 1
        }
        if (W_idx <= length(W)) {
          y_latent[var_idx, t - 1] <- W[W_idx]
          W_idx <- W_idx + 1
        }
      }
    }
  }

  # Compute log likelihood p(Y | W, A, Σ)
  log_lik <- .compute_log_likelihood_mniw(y_latent, A_list, Sigma, p, intercept_vec)

  # Compute log prior p(A, Σ | λ)
  # For MNIW: log p(A, Σ) = log NIW(Σ) + log N(vec(A) | vec(A₀), Σ ⊗ Ω₀)
  n <- nrow(y_latent)
  k <- ncol(mniw_prior$A_mean)

  Sigma_inv <- solve(Sigma)
  log_det_Sigma <- determinant(Sigma, logarithm = TRUE)$modulus[1]

  # NIW part for Σ
  nu_0 <- mniw_prior$nu0
  S_0 <- mniw_prior$Psi0
  log_det_S0 <- determinant(S_0, logarithm = TRUE)$modulus[1]

  log_prior_Sigma <- 0.5 * nu_0 * log_det_S0 -
    0.5 * (nu_0 + n + 1) * log_det_Sigma -
    0.5 * sum(diag(Sigma_inv %*% S_0))

  # Matrix normal part for A
  A_mat <- do.call(cbind, A_list)
  lag_cols <- n * p
  if (isTRUE(mniw_prior$include_intercept)) {
    A_mat <- cbind(A_mat[, seq_len(lag_cols), drop = FALSE], intercept_vec)
  }
  if (ncol(A_mat) < k) {
    A_mat <- cbind(A_mat, matrix(0, nrow(A_mat), k - ncol(A_mat)))
  }
  A_0 <- mniw_prior$A_mean
  Omega_0 <- mniw_prior$Omega
  Omega_0_inv <- solve(Omega_0)
  log_det_Omega0 <- determinant(Omega_0, logarithm = TRUE)$modulus[1]

  A_diff <- A_mat - A_0
  quad_term <- A_diff %*% Omega_0_inv %*% t(A_diff)
  log_prior_A <- -0.5 * k * log_det_Sigma -
    0.5 * n * log_det_Omega0 -
    0.5 * sum(diag(Sigma_inv %*% quad_term))

  return(log_lik + log_prior_Sigma + log_prior_A)
}

#' Compute VAR log likelihood p(Y | W, A, Σ)
#'
#' @keywords internal
.compute_log_likelihood_mniw <- function(y_latent, A_list, Sigma, p, intercept = NULL) {
  n <- nrow(y_latent)
  T_len <- ncol(y_latent)
  T_eff <- T_len - p
  intercept_vec <- if (is.null(intercept)) rep(0, n) else intercept

  if (T_eff <= 0) {
    return(-Inf)
  }

  # Compute residuals
  residuals <- matrix(0, n, T_eff)

  for (t in (p + 1):T_len) {
    y_t <- y_latent[, t, drop = FALSE]
    y_pred <- matrix(intercept_vec, nrow = n, ncol = 1)

    for (lag in 1:length(A_list)) {
      y_lag <- y_latent[, t - lag, drop = FALSE]
      y_pred <- y_pred + A_list[[lag]] %*% y_lag
    }

    residuals[, t - p] <- y_t - y_pred
  }

  # Log likelihood: sum of multivariate normal densities
  Sigma_inv <- solve(Sigma)
  log_det_Sigma <- determinant(Sigma, logarithm = TRUE)$modulus[1]

  log_lik <- 0
  for (t in 1:T_eff) {
    u_t <- residuals[, t, drop = FALSE]
    log_lik <- log_lik - 0.5 * n * log(2 * pi) - 0.5 * log_det_Sigma -
      0.5 * as.numeric(crossprod(u_t, Sigma_inv %*% u_t))
  }

  return(log_lik)
}

#' Compute analytical MDD when no latent variables
#'
#' For fully observed data, MDD can be computed in closed form using MNIW conjugacy.
#'
#' @keywords internal
.compute_mdd_analytical <- function(y_data, metadata, p, mniw_prior) {
  # When all data is observed, use closed-form MNIW marginal likelihood
  # log p(Y | λ) = log |Ω*|^(n/2) / |Ω₀|^(n/2) +
  #                log |S₀|^(ν₀/2) / |S*|^(ν*/2) +
  #                Γ_n(ν*/2) / Γ_n(ν₀/2)

  Y_full <- t(as.matrix(y_data)) # n × T matrix
  n <- nrow(Y_full)
  T_len <- ncol(Y_full)
  T_eff <- T_len - p
  include_intercept <- isTRUE(mniw_prior$include_intercept)
  k <- ncol(mniw_prior$A_mean)

  # Prior parameters
  nu_0 <- mniw_prior$nu0
  S_0 <- mniw_prior$Psi0
  Omega_0 <- mniw_prior$Omega
  A_0 <- mniw_prior$A_mean

  # Construct X and Y matrices
  Y_mat <- t(Y_full[, (p + 1):T_len, drop = FALSE]) # T_eff × n
  X_mat <- matrix(0, T_eff, k)

  for (i in 1:T_eff) {
    for (lag in 1:p) {
      col_range <- ((lag - 1) * n + 1):(lag * n)
      X_mat[i, col_range] <- Y_full[, p + i - lag]
    }
  }
  if (include_intercept) {
    X_mat[, k] <- 1
  }

  # Posterior parameters
  Omega_0_inv <- solve(Omega_0)
  Omega_star_inv <- Omega_0_inv + crossprod(X_mat)
  Omega_star <- solve(Omega_star_inv)

  A_star <- Omega_star %*% (Omega_0_inv %*% t(A_0) + crossprod(X_mat, Y_mat))

  nu_star <- nu_0 + T_eff
  S_star <- S_0 +
    crossprod(Y_mat) +
    A_0 %*% Omega_0_inv %*% t(A_0) -
    t(A_star) %*% Omega_star_inv %*% A_star

  # Log marginal likelihood
  log_det_Omega_star <- determinant(Omega_star, logarithm = TRUE)$modulus[1]
  log_det_Omega_0 <- determinant(Omega_0, logarithm = TRUE)$modulus[1]
  log_det_S_star <- determinant(S_star, logarithm = TRUE)$modulus[1]
  log_det_S_0 <- determinant(S_0, logarithm = TRUE)$modulus[1]

  log_mdd <- 0.5 * n * (log_det_Omega_star - log_det_Omega_0) +
    0.5 * nu_0 * log_det_S_0 - 0.5 * nu_star * log_det_S_star +
    .log_multivariate_gamma(nu_star / 2, n) -
    .log_multivariate_gamma(nu_0 / 2, n) -
    0.5 * T_eff * n * log(pi)

  return(log_mdd)
}

#' Log of multivariate gamma function
#'
#' Γ_p(a) = π^(p(p-1)/4) ∏_{j=1}^p Γ(a - (j-1)/2)
#'
#' @keywords internal
.log_multivariate_gamma <- function(a, p) {
  log_gamma_sum <- sum(lgamma(a - (0:(p - 1)) / 2))
  log_pi_term <- 0.25 * p * (p - 1) * log(pi)
  return(log_pi_term + log_gamma_sum)
}

#' Draw states for MDD computation (simplified version)
#' @keywords internal
.gibbs_draw_states_mdd <- function(y_data, metadata, A_list, Sigma, p, intercept = NULL) {
  n <- ncol(y_data)
  metadata <- normalize_metadata_fields(metadata)
  intercept_vec <- if (is.null(intercept)) rep(0, n) else intercept

  # Use the full state space sampler with Carter-Kohn smoother
  metadata$p <- p

  state_lags <- metadata$state_lags
  if (is.null(state_lags)) {
    freq_info <- metadata$freq
    type_info <- metadata$type
    has_flow_quarterly <- !is.null(freq_info) && !is.null(type_info) &&
      any(freq_info == "quarterly" & type_info == "flow")
    state_lags <- if (has_flow_quarterly) max(p, 3L) else p
    metadata$state_lags <- state_lags
  }

  companion <- build_companion_form(A_list, Sigma, state_lags = state_lags)
  dates <- zoo::index(y_data)
  Z_info <- build_Z_list(metadata, dates, state_lags = state_lags)

  y_centered_mat <- sweep(as.matrix(y_data), 2, intercept_vec, FUN = "-")
  y_centered <- zoo::zoo(y_centered_mat, order.by = dates)
  init_state <- build_initial_state(y_centered, state_lags, method = "diffuse")

  model <- build_kfas_model(
    y_obs = as.matrix(y_centered),
    companion_form = companion,
    Z_list = Z_info$Z_list,
    a1 = init_state$a1,
    P1 = init_state$P1
  )

  states <- carter_kohn_smoother(model, nsim = 1, seed = NULL)[, , 1]

  if (any(intercept_vec != 0)) {
    for (lag in 0:(state_lags - 1)) {
      row_idx <- (lag * n + 1):((lag + 1) * n)
      states[row_idx, ] <- states[row_idx, , drop = FALSE] + intercept_vec
    }
  }

  return(states)
}
