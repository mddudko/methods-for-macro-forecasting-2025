# ==============================================================================
# state_space.R - State-space representation for mixed frequency VAR
# ==============================================================================
# Implements state-space form as specified in instruction chunk 3 (section 4):
# - Companion form for VAR(p): X_t = F·X_{t-1} + G·η_t
# - State vector: X_t = [y_t', y_{t-1}', ..., y_{t-p+1}']'
# - Time-varying observation matrices Z_t for mixed frequency
# - Three observation types: monthly, stock quarterly, flow quarterly
# - Mariano-Murasawa (2003) aggregation for flow variables
# ==============================================================================
# Plain-language summary for non-specialists:
# - These functions convert the VAR into a “state-space” view where the model keeps track of today’s
#   value plus lagged values as one compact state. That format lets the Kalman filter smoothly fill in
#   the missing quarterly pieces using the available monthly clues.
# - You can think of it as the bookkeeping layer that translates between the data we observe and the
#   hidden states the sampler works with.

#' Build companion form matrices for VAR(p)
#'
#' Converts VAR(p) coefficient matrices into companion form for state-space
#' representation. The state vector X_t stacks current and lagged values of y_t.
#'
#' @param A_list List of p coefficient matrices (each n x n)
#' @param Sigma Residual covariance matrix (n x n)
#' @param include_intercept Logical: does model include intercept? (default: FALSE)
#' @param intercept_vec If include_intercept=TRUE, vector of intercepts (length n)
#'
#' @return List with:
#'   - F: Companion transition matrix (np x np)
#'   - G: Selection matrix for shocks (np x n)
#'   - Q: Covariance of state innovations (np x np)
#'   - n: Number of variables
#'   - p: Number of lags
#'   - state_dim: Dimension of state vector (n*p)
#'
#' @export
#' @importFrom Matrix Matrix
build_companion_form <- function(A_list,
                                 Sigma,
                                 include_intercept = FALSE,
                                 intercept_vec = NULL,
                                 state_lags = NULL) {
  p_model <- length(A_list)
  n <- nrow(A_list[[1]])

  if (is.null(state_lags) || state_lags < p_model) {
    state_lags <- p_model
  }

  if (state_lags > p_model) {
    zeros_needed <- state_lags - p_model
    zero_block <- matrix(0, n, n)
    padding <- replicate(zeros_needed, zero_block, simplify = FALSE)
    A_list <- c(A_list, padding)
  }

  p <- state_lags
  state_dim <- n * p

  # Build companion matrix F
  F <- matrix(0, state_dim, state_dim)

  # First n rows: VAR coefficients
  for (lag in 1:p) {
    col_start <- (lag - 1) * n + 1
    col_end <- lag * n
    F[1:n, col_start:col_end] <- A_list[[lag]]
  }

  # Remaining rows: identity blocks for lagged states
  if (p > 1) {
    for (lag in 1:(p - 1)) {
      row_start <- lag * n + 1
      row_end <- (lag + 1) * n
      col_start <- (lag - 1) * n + 1
      col_end <- lag * n
      F[row_start:row_end, col_start:col_end] <- diag(n)
    }
  }

  # Build selection matrix G (shocks only affect current y_t)
  G <- matrix(0, state_dim, n)
  G[1:n, ] <- diag(n)

  # Innovation covariance Q
  # Note: Q should be n x n (covariance of η_t), NOT state_dim x state_dim
  # KFAS will compute R*Q*R' internally for the state equation covariance
  Q <- Sigma

  # Handle intercept if present
  # Note: In state-space with intercept, we augment state or use mean-adjusted form
  # For simplicity, assume data is already demeaned or intercept is absorbed
  intercept_term <- if (include_intercept && !is.null(intercept_vec)) {
    c(intercept_vec, rep(0, state_dim - n))
  } else {
    rep(0, state_dim)
  }

  return(structure(
    list(
      F = F,
      G = G,
      Q = Q,
      n = n,
      p = p,
      state_dim = state_dim,
      intercept = intercept_term
    ),
    class = "companion_form"
  ))
}

#' Build time-varying observation matrix Z_t for mixed frequency
#'
#' Constructs observation matrix that maps state vector to observed data,
#' handling three cases:
#' 1. Monthly variables: direct observation of y_t component
#' 2. Stock quarterly: observe y_t at quarter-end months
#' 3. Flow quarterly: observe sum y_t + y_{t-1} + y_{t-2} at quarter-end
#'
#' @param metadata List with freq (frequency) and type (flow/stock) per variable
#' @param month_index Integer index within sample (1 = first month)
#' @param state_dim Dimension of state vector (n*p)
#' @param n Number of variables
#' @param is_quarter_end Logical: is this month a quarter-end? (Mar/Jun/Sep/Dec)
#'
#' @return Z matrix (n_obs x state_dim) where n_obs <= n depending on availability
#'
#' @export
build_Z_matrix <- function(metadata,
                           month_index,
                           state_dim,
                           n,
                           is_quarter_end,
                           state_lags = NULL) {
  vars <- metadata$vars
  freq <- metadata$freq
  type <- metadata$type

  # Initialize Z as n x state_dim matrix
  Z <- matrix(0, n, state_dim)
  obs_mask <- rep(FALSE, n) # Which variables are observed this month

  if (is.null(state_lags)) {
    state_lags <- max(1L, state_dim %/% n)
  }

  flow_quarterly <- !is.null(freq) && !is.null(type) && any(freq == "quarterly" & type == "flow")
  if (flow_quarterly && state_lags < 3) {
    stop("Flow-type quarterly variables require at least 3 lags in the state vector.")
  }

  for (i in 1:n) {
    var_name <- vars[i]

    if (freq[var_name] == "monthly") {
      # Monthly: always observe y_t[i]
      Z[i, i] <- 1
      obs_mask[i] <- TRUE
    } else if (freq[var_name] == "quarterly") {
      # Quarterly: only observe at quarter-end
      if (is_quarter_end) {
        if (type[var_name] == "stock") {
          # Stock: observe y_t[i]
          Z[i, i] <- 1
          obs_mask[i] <- TRUE
        } else if (type[var_name] == "flow") {
          # Flow: observe 1/3 * (y_t + y_{t-1} + y_{t-2}) per Mariano-Murasawa (2003)
          for (lag in 0:2) {
            col_idx <- lag * n + i
            if (col_idx > state_dim) {
              stop("Insufficient lags in state vector for flow aggregation")
            }
            Z[i, col_idx] <- Z[i, col_idx] + 1 / 3
          }
          obs_mask[i] <- TRUE
        }
      }
      # If not quarter-end, Z[i, :] remains zero and obs_mask[i] = FALSE
    }
  }

  # Return only rows corresponding to observed variables
  # In practice, we pass full Z to Kalman filter and it handles NA observations
  return(list(
    Z = Z,
    obs_mask = obs_mask,
    n_obs = sum(obs_mask)
  ))
}

#' Build observation matrix for all time periods
#'
#' Creates list of Z_t matrices for entire sample, accounting for which
#' variables are observed at each time point.
#'
#' @param metadata Metadata list from prepare_data_snb
#' @param dates Vector of dates (yearmon)
#' @param p Lag order (if NULL, reads from metadata$p)
#'
#' @return List with:
#'   - Z_list: list of Z matrices (one per time period)
#'   - obs_mask_list: list of observation masks
#'   - quarter_end_indices: indices of quarter-end months
#'
#' @export
#' @importFrom stats cycle
build_Z_list <- function(metadata, dates, p = NULL, state_lags = NULL) {
  T_len <- length(dates)
  n <- length(metadata$vars)

  lag_count <- NULL
  if (!is.null(state_lags)) {
    lag_count <- state_lags
  } else if (!is.null(p)) {
    lag_count <- p
  } else if (!is.null(metadata$state_lags)) {
    lag_count <- metadata$state_lags
  } else if (!is.null(metadata$p)) {
    lag_count <- metadata$p
  }

  if (is.null(lag_count)) {
    stop("Lag order must be provided via p, state_lags, or metadata")
  }

  state_dim <- n * lag_count

  # Identify quarter-end months (cycle = 3, 6, 9, 12 for Mar, Jun, Sep, Dec)
  date_cycles <- stats::cycle(dates)
  is_qtr_end <- date_cycles %in% c(3, 6, 9, 12)

  Z_list <- vector("list", T_len)
  obs_mask_list <- vector("list", T_len)

  for (t in 1:T_len) {
    Z_info <- build_Z_matrix(
      metadata = metadata,
      month_index = t,
      state_dim = state_dim,
      n = n,
      is_quarter_end = is_qtr_end[t],
      state_lags = lag_count
    )
    Z_list[[t]] <- Z_info$Z
    obs_mask_list[[t]] <- Z_info$obs_mask
  }

  return(list(
    Z_list = Z_list,
    obs_mask_list = obs_mask_list,
    quarter_end_indices = which(is_qtr_end)
  ))
}

#' Construct KFAS state-space model object
#'
#' Builds a KFAS SSModel for the mixed frequency VAR with time-varying
#' observation matrices.
#'
#' @param y_obs Matrix of observed data (T x n) with NA for unobserved
#' @param companion_form Output from build_companion_form()
#' @param Z_list List of observation matrices from build_Z_list()
#' @param a1 Initial state mean (state_dim x 1)
#' @param P1 Initial state covariance (state_dim x state_dim)
#' @param H Observation error covariance (default: matrix(0) for no measurement error)
#'
#' @return KFAS SSModel object ready for filtering/smoothing
#'
#' @export
#' @importFrom KFAS SSModel SSMcustom
build_kfas_model <- function(y_obs, companion_form, Z_list, a1, P1, H = NULL) {
  T_len <- nrow(y_obs)
  n <- ncol(y_obs)
  state_dim <- companion_form$state_dim

  # Default H: no measurement error
  if (is.null(H)) {
    H <- array(0, dim = c(n, n, T_len))
  } else if (length(dim(H)) == 2) {
    # Replicate H across time if constant
    H <- array(H, dim = c(n, n, T_len))
  }

  # Convert Z_list to 3D array for KFAS
  Z_array <- array(0, dim = c(n, state_dim, T_len))
  for (t in 1:T_len) {
    Z_array[, , t] <- Z_list[[t]]
  }

  # Build KFAS model using SSMcustom
  model <- SSModel(
    y_obs ~ -1 + SSMcustom(
      Z = Z_array,
      T = companion_form$F,
      R = as.matrix(companion_form$G),
      Q = as.matrix(companion_form$Q),
      a1 = a1,
      P1 = P1
    ),
    H = H
  )

  return(model)
}

#' Run Kalman filter and smoother
#'
#' @param model KFAS SSModel object
#'
#' @return KFS object with filtered and smoothed estimates
#'
#' @export
#' @importFrom KFAS KFS
run_kalman_filter_smoother <- function(model) {
  kfs <- KFS(model, filtering = c("state", "mean"), smoothing = c("state", "mean"))
  return(kfs)
}

#' Carter-Kohn simulation smoother for state draws
#'
#' Draws from p(X_{1:T} | Y_{1:T}, θ) using the simulation smoother.
#' This is used in the Gibbs sampler to sample latent states.
#'
#' @param model KFAS SSModel object
#' @param nsim Number of simulations (default: 1)
#' @param seed Random seed for reproducibility
#'
#' @return Array of state draws (state_dim x T x nsim)
#'
#' @export
#' @importFrom KFAS simulateSSM
carter_kohn_smoother <- function(model, nsim = 1, seed = NULL) {
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

  # KFAS simulateSSM with type="states" performs simulation smoothing
  sim <- simulateSSM(model, type = "states", nsim = nsim)

  # sim is array (T x state_dim x nsim) - already excludes initial state
  # Transpose to (state_dim x T x nsim)
  states_sim <- aperm(sim, c(2, 1, 3))

  return(states_sim)
}

#' Extract y_t from state vector
#'
#' The state vector X_t = [y_t', y_{t-1}', ..., y_{t-p+1}']' stacks lags.
#' This function extracts the current y_t components.
#'
#' @param states Matrix (state_dim x T) of states
#' @param n Number of variables
#'
#' @return Matrix (n x T) of y_t values
#'
#' @export
extract_yt_from_states <- function(states, n) {
  # First n rows of states are y_t
  return(states[1:n, , drop = FALSE])
}

#' Build initial state distribution
#'
#' Constructs prior for initial state a1 and P1 for Kalman filter.
#'
#' @param data zoo object with data
#' @param p Integer lag order
#' @param method Method for initialization: "unconditional", "ols", or "diffuse"
#'
#' @return List with a1 (mean) and P1 (covariance)
#'
#' @export
build_initial_state <- function(data, p, method = "unconditional") {
  n <- ncol(data)
  state_dim <- n * p

  if (method == "diffuse") {
    # Diffuse prior
    a1 <- rep(0, state_dim)
    P1 <- diag(1e6, state_dim)
  } else if (method == "ols") {
    # Use first p observations
    if (nrow(data) < p + 1) {
      stop("Insufficient data for OLS initialization")
    }

    a1 <- numeric(state_dim)
    for (lag in 1:p) {
      lag_start <- (lag - 1) * n + 1
      lag_end <- lag * n
      a1[lag_start:lag_end] <- as.numeric(data[p + 1 - lag, ])
    }

    # Initial uncertainty
    data_var <- apply(data, 2, var, na.rm = TRUE)
    P1 <- diag(rep(data_var, p))
  } else {
    # Unconditional mean and variance
    a1 <- rep(0, state_dim)
    data_var <- apply(data, 2, var, na.rm = TRUE)
    P1 <- diag(rep(data_var, p))
  }

  return(list(a1 = a1, P1 = P1))
}

#' Print method for companion_form
#' @export
print.companion_form <- function(x, ...) {
  cat("Companion Form for VAR\n")
  cat("=====================\n")
  cat(sprintf("Variables: %d\n", x$n))
  cat(sprintf("Lags: %d\n", x$p))
  cat(sprintf("State dimension: %d\n", x$state_dim))
  cat(sprintf("Transition matrix F: %d x %d\n", nrow(x$F), ncol(x$F)))
  cat(sprintf("Selection matrix G: %d x %d\n", nrow(x$G), ncol(x$G)))
  invisible(x)
}

#' Normalize metadata frequency/type vectors
#' @keywords internal
normalize_metadata_fields <- function(metadata) {
  vars <- metadata$vars
  if (is.null(vars)) {
    return(metadata)
  }

  align_vector <- function(vec) {
    if (is.null(vec)) {
      return(NULL)
    }
    if (length(vec) < length(vars)) {
      vec <- rep_len(vec, length(vars))
    }
    if (is.null(names(vec))) {
      names(vec) <- vars[seq_along(vec)]
    }
    vec
  }

  freq <- metadata$freq
  if (is.null(freq) && !is.null(metadata$frequency_code)) {
    freq <- ifelse(metadata$frequency_code == 1, "monthly", "quarterly")
  }
  metadata$freq <- align_vector(freq)

  type_field <- metadata$type
  if (is.null(type_field) && !is.null(metadata$agg_type)) {
    type_field <- metadata$agg_type
  }
  metadata$type <- align_vector(type_field)

  freq_code <- metadata$frequency_code
  if (is.null(freq_code) && !is.null(metadata$freq)) {
    freq_code <- ifelse(metadata$freq == "quarterly", 2L, 1L)
  }
  metadata$frequency_code <- align_vector(freq_code)

  metadata
}
