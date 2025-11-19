# ==============================================================================
# rolling.R - Rolling and expanding window forecast evaluation
# ==============================================================================
# Implements recursive forecasting as specified in instruction chunk 6 (section 9)
# Now with REAL-TIME vintage support following Schorfheide & Song (2013)
# ==============================================================================
# Plain-language summary for non-specialists:
# - This file reruns the model through time to mimic how a forecaster would have behaved in real time.
# - It lets us test whether the model would have beaten the AR(2) benchmark using only the information
#   that was available back then (including older data vintages).

#' Rolling or expanding window forecast evaluation with real-time vintages
#'
#' Performs pseudo-out-of-sample forecast evaluation using either:
#' - Simple expanding window (uses final revised data)
#' - Real-time expanding window (uses vintages with publication lags)
#'
#' @param data_prepared mfvar_data object OR vintage_database object
#' @param p Integer VAR lag order
#' @param window_type "expanding" or "rolling"
#' @param min_window Minimum training window size (months)
#' @param rolling_width If window_type="rolling", fixed window width (months)
#' @param horizons Vector of forecast horizons to evaluate (months)
#' @param hyperparameters Hyperparameters (if NULL, tune for first window then reuse)
#' @param n_draws MCMC draws per window (default: 1000)
#' @param burnin MCMC burnin per window (default: 500)
#' @param reuse_hyperparams Logical: tune once or per window? (default: TRUE)
#' @param include_ar2 Logical: also compute AR(2) benchmarks? (default: TRUE)
#' @param use_vintages Logical: use real-time vintages? (default: TRUE if vintage_db provided)
#' @param verbose Logical
#' @param seed Random seed
#'
#' @return List with forecasts, actuals, AR(2) benchmarks, and evaluation
#'
#' @export
rolling_forecasts_mf_bvar <- function(data_prepared,
                                      p,
                                      window_type = "expanding",
                                      min_window = 120,
                                      rolling_width = NULL,
                                      horizons = c(1, 3, 6, 12),
                                      hyperparameters = NULL,
                                      n_draws = 1000,
                                      burnin = 500,
                                      reuse_hyperparams = TRUE,
                                      include_ar2 = TRUE,
                                      use_vintages = NULL,
                                      verbose = TRUE,
                                      seed = NULL,
                                      forecast_quantiles = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
                                      n_forecast_sim = 1000,
                                      compute_diagnostics = TRUE,
                                      diagnostic_intervals = c(0.5, 0.7, 0.9),
                                      diagnostic_comovement = NULL) {
  if (!window_type %in% c("expanding", "rolling")) {
    stop("window_type must be either 'expanding' or 'rolling'")
  }

  if (window_type == "rolling" && is.null(rolling_width)) {
    stop("rolling_width must be supplied when window_type = 'rolling'")
  }

  if (length(horizons) == 0) {
    stop("At least one forecast horizon must be supplied")
  }

  max_horizon <- max(horizons)

  # RNG handling ------------------------------------------------------------
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

  if (verbose) cat("=== Rolling Window Forecast Evaluation ===\n\n")

  # Determine if using vintages --------------------------------------------
  is_vintage_db <- inherits(data_prepared, "vintage_database")

  if (is.null(use_vintages)) {
    use_vintages <- is_vintage_db
  }

  if (use_vintages && !is_vintage_db) {
    stop("use_vintages = TRUE requires a vintage_database object")
  }

  if (!use_vintages && !inherits(data_prepared, "mfvar_data")) {
    stop("data_prepared must be an mfvar_data object when use_vintages = FALSE")
  }

  # Extract base data & metadata ------------------------------------------
  if (use_vintages) {
    if (verbose) cat("Using REAL-TIME vintages for forecasting\n\n")
    final_data <- data_prepared$final_data
    metadata <- data_prepared$metadata
    forecast_origins <- data_prepared$origins
    information_sets <- data_prepared$information_sets
  } else {
    if (verbose) cat("Using final revised data (no vintages)\n\n")
    final_data <- data_prepared$data
    metadata <- data_prepared$metadata
    information_sets <- NULL

    time_index <- zoo::index(final_data)
    total_obs <- length(time_index)

    if (total_obs < min_window + max_horizon) {
      stop("Not enough observations for requested window and horizons")
    }

    first_origin_idx <- max(min_window, p + 1)
    if (window_type == "rolling") {
      first_origin_idx <- max(first_origin_idx, rolling_width)
    }

    last_origin_idx <- total_obs - max_horizon
    if (last_origin_idx <= first_origin_idx) {
      stop("Insufficient data to compute any forecast origins given horizons")
    }

    forecast_origins <- .generate_forecast_origins(
      first = time_index[first_origin_idx],
      last = time_index[last_origin_idx],
      frequency = "quarterly"
    )

    # ensure origins align with available sample window
    origin_in_sample <- forecast_origins %in% time_index
    if (!all(origin_in_sample)) {
      forecast_origins <- forecast_origins[origin_in_sample]
    }

    if (length(forecast_origins) == 0) {
      stop("No valid forecast origins found for the supplied data and settings")
    }
  }

  horizons <- sort(unique(horizons))
  n_origins <- length(forecast_origins)
  var_names <- metadata$vars
  n_vars <- length(var_names)
  n_quant <- length(forecast_quantiles)

  if (verbose) {
    cat(sprintf("Forecast origins: %d\n", n_origins))
    cat(sprintf("First origin: %s\n", forecast_origins[1]))
    cat(sprintf("Last origin: %s\n", forecast_origins[n_origins]))
    cat(sprintf("Horizons (months): %s\n", paste(horizons, collapse = ", ")))
    cat(sprintf("MCMC draws per origin: %d (burn-in %d)\n", n_draws, burnin))
    cat("\n")
  }

  # Storage objects --------------------------------------------------------
  forecast_median <- array(NA_real_,
    dim = c(n_origins, n_vars, length(horizons)),
    dimnames = list(
      origin = as.character(forecast_origins),
      variable = var_names,
      horizon = paste0("h", horizons)
    )
  )

  forecast_quant <- array(NA_real_,
    dim = c(n_origins, n_vars, length(horizons), n_quant),
    dimnames = list(
      origin = as.character(forecast_origins),
      variable = var_names,
      horizon = paste0("h", horizons),
      quantile = paste0("q", forecast_quantiles)
    )
  )

  actuals_array <- array(NA_real_,
    dim = c(n_origins, n_vars, length(horizons)),
    dimnames = list(
      origin = as.character(forecast_origins),
      variable = var_names,
      horizon = paste0("h", horizons)
    )
  )

  ar2_array <- if (include_ar2) {
    array(NA_real_,
      dim = c(n_origins, n_vars, length(horizons)),
      dimnames = list(
        origin = as.character(forecast_origins),
        variable = var_names,
        horizon = paste0("h", horizons)
      )
    )
  } else {
    NULL
  }

  posterior_list <- vector("list", n_origins)
  forecast_obj_list <- vector("list", n_origins)
  hyperparams_used <- vector("list", n_origins)
  ar2_fits_list <- if (include_ar2) vector("list", n_origins) else NULL
  status <- rep("success", n_origins)

  final_index <- zoo::index(final_data)

  if (!use_vintages) {
    base_mfvar <- data_prepared
  }

  # Rolling loop -----------------------------------------------------------
  for (i in seq_along(forecast_origins)) {
    origin <- forecast_origins[i]
    origin_chr <- as.character(origin)

    if (verbose && (i == 1 || i %% max(1, n_origins %/% 10) == 0)) {
      cat(sprintf("[%d/%d] Origin %s\n", i, n_origins, origin_chr))
    }

    # Select estimation data ------------------------------------------------
    if (use_vintages) {
      vintage_data <- data_prepared$vintages[[i]]
      vintage_index <- zoo::index(vintage_data)
      available_idx <- which(vintage_index <= origin)

      if (length(available_idx) < min_window) {
        status[i] <- "insufficient_data"
        next
      }

      end_idx <- max(available_idx)
      start_idx <- if (window_type == "expanding") {
        1
      } else {
        max(1, end_idx - rolling_width + 1)
      }

      if ((end_idx - start_idx + 1) < min_window) {
        start_idx <- max(1, end_idx - min_window + 1)
      }

      est_data_zoo <- vintage_data[start_idx:end_idx, , drop = FALSE]

      estimation_data <- structure(
        list(
          data = est_data_zoo,
          metadata = metadata
        ),
        class = "mfvar_data"
      )

      info_set <- if (!is.null(information_sets)) information_sets[i] else NA_character_
    } else {
      time_index <- zoo::index(base_mfvar$data)
      if (!origin %in% time_index) {
        status[i] <- "origin_not_in_sample"
        next
      }

      origin_idx <- which(time_index == origin)

      start_idx <- if (window_type == "expanding") {
        max(1, origin_idx - min_window + 1)
      } else {
        origin_idx - rolling_width + 1
      }

      if (start_idx < 1) {
        status[i] <- "insufficient_data"
        next
      }

      if ((origin_idx - start_idx + 1) < min_window) {
        start_idx <- origin_idx - min_window + 1
      }

      if (start_idx < 1) {
        status[i] <- "insufficient_data"
        next
      }

      est_data_zoo <- base_mfvar$data[start_idx:origin_idx, , drop = FALSE]

      estimation_data <- structure(
        list(
          data = est_data_zoo,
          metadata = metadata,
          transformation_params = base_mfvar$transformation_params,
          unit_root_tests = base_mfvar$unit_root_tests,
          original_data = base_mfvar$original_data
        ),
        class = "mfvar_data"
      )

      info_set <- NA_character_
    }

    # Hyperparameter selection ---------------------------------------------
    hyper_this <- hyperparameters
    if (is.null(hyperparameters)) {
      if (reuse_hyperparams && i > 1 && !is.null(hyperparams_used[[i - 1]])) {
        hyper_this <- hyperparams_used[[i - 1]]
      } else {
        hyper_this <- tryCatch(
          tune_minnesota_hyper(
            data_prepared = estimation_data,
            p = p,
            verbose = FALSE,
            seed = if (!is.null(seed)) seed + i else NULL
          ),
          error = function(e) {
            if (verbose) cat(sprintf("  Warning: hyperparameter tuning failed (%s)\n", e$message))
            NULL
          }
        )
      }
    }

    if (is.null(hyper_this)) {
      status[i] <- "tuning_failed"
      next
    }

    hyperparams_used[[i]] <- hyper_this

    # Estimation ------------------------------------------------------------
    posterior <- tryCatch(
      estimate_mf_bvar(
        data_prepared = estimation_data,
        p = p,
        hyperparameters = hyper_this,
        n_draws = n_draws,
        burnin = burnin,
        verbose = FALSE,
        seed = if (!is.null(seed)) seed + 1000 + i else NULL
      ),
      error = function(e) {
        if (verbose) cat(sprintf("  Warning: estimation failed (%s)\n", e$message))
        NULL
      }
    )

    if (is.null(posterior)) {
      status[i] <- "estimation_failed"
      next
    }

    posterior_list[[i]] <- posterior

    # Forecast --------------------------------------------------------------
    forecast_obj <- tryCatch(
      forecast_mf_bvar(
        posterior = posterior,
        horizon_months = max_horizon,
        n_sim = n_forecast_sim,
        quantiles = forecast_quantiles,
        seed = if (!is.null(seed)) seed + 2000 + i else NULL
      ),
      error = function(e) {
        if (verbose) cat(sprintf("  Warning: forecasting failed (%s)\n", e$message))
        NULL
      }
    )

    if (is.null(forecast_obj)) {
      status[i] <- "forecast_failed"
      next
    }

    forecast_obj_list[[i]] <- forecast_obj

    quant_index <- forecast_obj$quantiles
    median_idx <- which.min(abs(quant_index - 0.5))

    for (h_idx in seq_along(horizons)) {
      h <- horizons[h_idx]

      if (h > forecast_obj$horizon_months) next

      forecast_quant[i, , h_idx, ] <- forecast_obj$forecasts_monthly[, h, , drop = FALSE]
      forecast_median[i, , h_idx] <- forecast_obj$forecasts_monthly[, h, median_idx]

      target_date <- .shift_yearmon(origin, h)
      if (target_date %in% final_index) {
        actuals_array[i, , h_idx] <- as.numeric(final_data[target_date, ])
      }
    }

    # Attach information-set metadata for this origin
    if (!is.null(information_sets)) {
      information_sets[i] <- info_set
    }

    # AR(2) benchmark -------------------------------------------------------
    if (include_ar2) {
      ar2_fits <- tryCatch(
        fit_ar2_all_variables(
          data_prepared = estimation_data,
          origin_index = nrow(est_data_zoo)
        ),
        error = function(e) {
          if (verbose) cat(sprintf("  Warning: AR(2) fitting failed (%s)\n", e$message))
          NULL
        }
      )

      if (!is.null(ar2_fits)) {
        ar2_fore_vals <- matrix(NA_real_,
          nrow = n_vars, ncol = length(horizons),
          dimnames = list(variable = var_names, horizon = paste0("h", horizons))
        )

        for (v in seq_along(var_names)) {
          fit <- ar2_fits[[var_names[v]]]
          if (is.null(fit)) next

          fc <- tryCatch(
            forecast_ar2(fit, horizon = max_horizon),
            error = function(e) {
              if (verbose) cat(sprintf("    Warning: AR(2) forecast failed for %s (%s)\n", var_names[v], e$message))
              rep(NA_real_, max_horizon)
            }
          )

          if (length(fc) >= max_horizon) {
            ar2_fore_vals[v, ] <- fc[horizons]
          }
        }

        ar2_array[i, , ] <- ar2_fore_vals
        ar2_fits_list[[i]] <- ar2_fits
      } else {
        status[i] <- ifelse(status[i] == "success", "ar2_failed", status[i])
      }
    }
  }

  results <- structure(
    list(
      origins = forecast_origins,
      information_sets = information_sets,
      horizons = horizons,
      metadata = metadata,
      mfvar_forecasts_median = forecast_median,
      mfvar_forecasts_quantiles = forecast_quant,
      actuals = actuals_array,
      mfvar_posterior = posterior_list,
      mfvar_forecast_objects = forecast_obj_list,
      hyperparameters = hyperparams_used,
      status = status,
      ar2_forecasts = ar2_array,
      ar2_fits = ar2_fits_list,
      settings = list(
        p = p,
        window_type = window_type,
        min_window = min_window,
        rolling_width = rolling_width,
        n_draws = n_draws,
        burnin = burnin,
        reuse_hyperparams = reuse_hyperparams,
        include_ar2 = include_ar2,
        use_vintages = use_vintages,
        forecast_quantiles = forecast_quantiles,
        n_forecast_sim = n_forecast_sim,
        compute_diagnostics = compute_diagnostics,
        diagnostic_intervals = diagnostic_intervals
      )
    ),
    class = "rolling_results"
  )

  if (compute_diagnostics) {
    results$diagnostics <- compute_pit_diagnostics(
      rolling_results = results,
      intervals = diagnostic_intervals,
      comovement_sets = diagnostic_comovement
    )
  } else {
    results$diagnostics <- NULL
  }

  if (verbose) {
    cat("\nRolling evaluation complete.\n")
    if (any(results$status != "success")) {
      tab <- table(results$status)
      cat("Status summary:\n")
      print(tab)
    }
  }

  results
}

#' Real-time expanding window evaluation with vintage data
#'
#' Implements the full Schorfheide & Song (2013) real-time forecasting exercise.
#'
#' @param vintage_db vintage_database object from create_real_time_vintages()
#' @param p Integer VAR lag order
#' @param horizons Vector of forecast horizons (months)
#' @param hyperparameters Optional hyperparameters (if NULL, tuned on first origin)
#' @param n_draws MCMC draws per origin
#' @param burnin MCMC burnin per origin
#' @param retune_frequency How often to retune hyperparameters (NULL = once, 1 = every origin)
#' @param verbose Logical
#' @param seed Random seed
#'
#' @return real_time_results object with:
#'   - forecasts: list by origin, variable, horizon
#'   - actuals: realized values for evaluation
#'   - information_sets: +0/+1/+2 classification
#'   - origins: forecast origins
#'   - accuracy_by_info_set: RMSE by information set
#'
#' @export
real_time_expanding_window <- function(vintage_db,
                                       p,
                                       horizons = c(1, 3, 6, 12),
                                       hyperparameters = NULL,
                                       n_draws = 2000,
                                       burnin = 1000,
                                       retune_frequency = NULL,
                                       verbose = TRUE,
                                       seed = NULL) {
  if (!inherits(vintage_db, "vintage_database")) {
    stop("vintage_db must be a vintage_database object")
  }

  if (verbose) {
    cat("========================================\n")
    cat("REAL-TIME EXPANDING WINDOW EVALUATION\n")
    cat("========================================\n\n")
  }

  origins <- vintage_db$origins
  n_origins <- length(origins)
  info_sets <- vintage_db$information_sets
  vars <- colnames(vintage_db$final_data)
  n_vars <- length(vars)

  if (verbose) {
    cat(sprintf("Origins: %d\n", n_origins))
    cat(sprintf("Variables: %d\n", n_vars))
    cat(sprintf("Horizons: %s\n", paste(horizons, collapse = ", ")))
    cat("\n")
  }

  # Initialize storage
  forecasts_mfvar <- array(NA, dim = c(n_origins, n_vars, length(horizons)))
  dimnames(forecasts_mfvar) <- list(
    origin = as.character(origins),
    variable = vars,
    horizon = paste0("h", horizons)
  )

  # Track hyperparameters used
  hyperparams_used <- vector("list", n_origins)

  # Loop over forecast origins
  for (i in seq_along(origins)) {
    origin <- origins[i]
    info_set <- info_sets[i]

    if (verbose) {
      cat(sprintf(
        "\n[%d/%d] Origin: %s (Info set: %s)\n",
        i, n_origins, origin, info_set
      ))
    }

    # Get vintage data for this origin
    vintage_data <- get_vintage_at_origin(vintage_db, origin, return_metadata = FALSE)

    # Create temporary mfvar_data object
    data_for_estimation <- structure(
      list(
        data = vintage_data,
        metadata = vintage_db$metadata
      ),
      class = "mfvar_data"
    )

    # Tune or reuse hyperparameters
    if (is.null(hyperparameters) ||
      (!is.null(retune_frequency) && (i - 1) %% retune_frequency == 0)) {
      if (verbose) cat("  Tuning hyperparameters...\n")

      hyper <- tryCatch(
        {
          tune_minnesota_hyper(
            data_prepared = data_for_estimation,
            p = p,
            lambda_grid = c(0.05, 0.1, 0.2, 0.5, 1.0),
            verbose = FALSE,
            seed = seed
          )
        },
        error = function(e) {
          if (verbose) cat(sprintf("  Warning: Tuning failed (%s), using defaults\n", e$message))
          list(lambda_optimal = 0.2, theta_optimal = 1, kappa_cross_optimal = 0.5)
        }
      )

      hyperparams_used[[i]] <- hyper
    } else {
      if (i == 1) {
        hyperparams_used[[i]] <- hyperparameters
      } else {
        hyperparams_used[[i]] <- hyperparams_used[[i - 1]]
      }
    }

    # Estimate model
    if (verbose) cat("  Estimating MF-BVAR...\n")

    model <- tryCatch(
      {
        estimate_mf_bvar(
          data_prepared = data_for_estimation,
          p = p,
          hyperparameters = hyperparams_used[[i]],
          n_draws = n_draws,
          burnin = burnin,
          verbose = FALSE,
          seed = if (!is.null(seed)) seed + i else NULL
        )
      },
      error = function(e) {
        if (verbose) cat(sprintf("  Error in estimation: %s\n", e$message))
        return(NULL)
      }
    )

    if (is.null(model)) {
      if (verbose) cat("  Skipping this origin due to estimation failure\n")
      next
    }

    # Generate forecasts
    if (verbose) cat("  Generating forecasts...\n")

    for (h_idx in seq_along(horizons)) {
      h <- horizons[h_idx]

      fcst <- tryCatch(
        {
          forecast_mf_bvar(
            posterior = model,
            horizon_months = h,
            n_sim = 500,
            seed = if (!is.null(seed)) seed + i + h else NULL
          )
        },
        error = function(e) {
          if (verbose) cat(sprintf("  Warning: Forecast h=%d failed\n", h))
          return(NULL)
        }
      )

      if (!is.null(fcst)) {
        # Store point forecasts (use median = 50th percentile)
        median_idx <- which(fcst$quantiles == 0.5)
        for (v_idx in seq_along(vars)) {
          v <- vars[v_idx]
          # fcst$forecasts_monthly is [variables x horizons x quantiles]
          # We want the h-th horizon (h=1,3,6 means row h in matrix)
          forecasts_mfvar[i, v, h_idx] <- fcst$forecasts_monthly[v, h, median_idx]
        }
      }
    }

    if (verbose && i %% max(1, n_origins %/% 10) == 0) {
      pct <- round(100 * i / n_origins)
      cat(sprintf("\n==> Progress: %d%% complete\n", pct))
    }
  }

  if (verbose) {
    cat("\n========================================\n")
    cat("REAL-TIME EVALUATION COMPLETE\n")
    cat("========================================\n\n")
  }

  # Compute actuals (for forecast evaluation)
  actuals <- .extract_actuals_for_evaluation(
    vintage_db$final_data, origins, horizons
  )

  # Compute accuracy by information set
  accuracy <- .compute_accuracy_by_info_set(
    forecasts_mfvar, actuals, info_sets
  )

  return(structure(
    list(
      forecasts = forecasts_mfvar,
      actuals = actuals,
      origins = origins,
      information_sets = info_sets,
      hyperparameters = hyperparams_used,
      accuracy_by_info_set = accuracy,
      vintage_db = vintage_db
    ),
    class = "real_time_results"
  ))
}

#' Extract actuals for forecast evaluation
#' @keywords internal
.extract_actuals_for_evaluation <- function(final_data, origins, horizons) {
  vars <- colnames(final_data)
  time_index <- zoo::index(final_data)
  n_origins <- length(origins)

  actuals <- array(NA, dim = c(n_origins, length(vars), length(horizons)))
  dimnames(actuals) <- list(
    origin = as.character(origins),
    variable = vars,
    horizon = paste0("h", horizons)
  )

  for (i in seq_along(origins)) {
    for (h_idx in seq_along(horizons)) {
      h <- horizons[h_idx]
      target_date <- .shift_yearmon(origins[i], h)

      # Find corresponding index in final data
      target_idx <- which(time_index == target_date)

      if (length(target_idx) == 1) {
        actuals[i, , h_idx] <- as.numeric(final_data[target_idx, ])
      }
    }
  }

  return(actuals)
}

#' Compute accuracy by information set
#' @keywords internal
.compute_accuracy_by_info_set <- function(forecasts, actuals, info_sets) {
  errors <- forecasts - actuals

  # RMSE by variable, horizon, and information set
  vars <- dimnames(forecasts)[[2]]
  horizons <- dimnames(forecasts)[[3]]
  info_set_types <- unique(info_sets)

  results <- list()

  for (info in info_set_types) {
    info_idx <- which(info_sets == info)

    rmse_matrix <- matrix(NA, nrow = length(vars), ncol = length(horizons))
    rownames(rmse_matrix) <- vars
    colnames(rmse_matrix) <- horizons

    for (v_idx in seq_along(vars)) {
      for (h_idx in seq_along(horizons)) {
        err <- errors[info_idx, v_idx, h_idx]
        rmse_matrix[v_idx, h_idx] <- sqrt(mean(err^2, na.rm = TRUE))
      }
    }

    results[[info]] <- rmse_matrix
  }

  return(results)
}

#' Generate forecast origins (helper function)
#' @keywords internal
.generate_forecast_origins <- function(first, last, frequency = "quarterly") {
  first <- zoo::as.yearmon(first)
  last <- zoo::as.yearmon(last)

  if (frequency == "quarterly") {
    # Quarterly: end of Mar, Jun, Sep, Dec
    all_months <- seq(first, last, by = 1 / 12)
    qtr_end_months <- sapply(all_months, function(m) {
      month_num <- as.integer(format(m, "%m"))
      month_num %in% c(3, 6, 9, 12)
    })
    origins <- all_months[qtr_end_months]
  } else if (frequency == "monthly") {
    origins <- seq(first, last, by = 1 / 12)
  } else {
    stop("frequency must be 'quarterly' or 'monthly'")
  }

  return(origins)
}

#' Shift yearmon by months (helper function)
#' @keywords internal
.shift_yearmon <- function(ym, shift) {
  ym_numeric <- as.numeric(ym)
  ym_shifted <- ym_numeric + shift / 12
  zoo::as.yearmon(ym_shifted)
}
