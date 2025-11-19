# ==============================================================================
# data.R - Data acquisition, alignment, and transformation
# ==============================================================================
# Implements data handling as specified in instruction chunks 1-2:
# - SNB data ingestion from CSV or data frames
# - Date parsing and frequency detection
# - Unified monthly calendar construction
# - Transformations: log, differencing, seasonal adjustment
# - Unit root testing for stationarity decisions
# - Flow vs stock classification
# - Metadata storage for transparency
# ==============================================================================
# Plain-language summary for non-specialists:
# - Think of this file as the “data washer.” It loads raw spreadsheets from the Swiss National Bank,
#   lines up every series on one monthly calendar, and fills in gaps sensibly.
# - It decides which variables move every month versus every quarter and labels whether a number is a
#   running total (flow) or a level (stock).
# - It applies simple rules—like taking logs or removing trends—so the modeling code later on gets
#   nicely standardized inputs along with a recipe (metadata) explaining every transformation.

#' Prepare SNB data for mixed frequency VAR
#'
#' This function implements the full data preparation pipeline:
#' 1. Load data from CSV files or data frames
#' 2. Parse dates and detect frequency (monthly/quarterly)
#' 3. Build unified monthly calendar
#' 4. Apply transformations (log, differencing)
#' 5. Classify variables as flow or stock
#' 6. Handle seasonal adjustment
#' 7. Optional standardization
#'
#' @param data_sources Either:
#'   - Character vector of CSV file paths
#'   - Single data frame with date column and variables
#'   - Named list of data frames
#' @param quarterly_vars Character vector of quarterly variable names (NULL = auto-detect)
#' @param flow_vars Character vector of flow-type quarterly variables (default: NULL means all quarterly are stock)
#' @param start_date Optional start date for sample (character "YYYY-MM-DD" or Date)
#' @param end_date Optional end date for sample (character "YYYY-MM-DD" or Date)
#' @param log_transform Character vector of variables to log-transform (NULL = auto-detect positive vars)
#' @param difference Logical: apply differencing based on unit root tests? (default: TRUE)
#' @param adf_pvalue p-value threshold for ADF test (default: 0.05)
#' @param seasonal_dummies Logical: include seasonal dummies? (default: TRUE)
#' @param standardize Logical: standardize to mean 0, sd 1? (default: FALSE)
#' @param verbose Logical: print progress messages? (default: TRUE)
#'
#' @return List with components:
#'   - data: zoo object with monthly grid, NA for unobserved quarterly months
#'   - metadata: list with frequency, type (flow/stock), transformations per variable
#'   - transformation_params: list with means, sds, log indicators for reversing
#'   - unit_root_tests: data frame with ADF/KPSS test results
#'   - original_data: original data before transformation for reference
#'
#' @export
#' @importFrom zoo as.yearmon zoo
#' @importFrom stats sd complete.cases
prepare_data_snb <- function(data_sources,
                             quarterly_vars = NULL,
                             flow_vars = NULL,
                             start_date = NULL,
                             end_date = NULL,
                             log_transform = NULL,
                             difference = TRUE,
                             adf_pvalue = 0.05,
                             seasonal_dummies = TRUE,
                             standardize = FALSE,
                             verbose = TRUE) {
  if (verbose) cat("=== SNB Data Preparation ===\n\n")

  # Step 1: Load and parse data
  if (verbose) cat("Step 1: Loading data...\n")
  df <- .load_data_sources(data_sources)

  # Step 2: Parse dates and detect frequency
  if (verbose) cat("Step 2: Parsing dates and detecting frequencies...\n")
  df <- .parse_dates(df)
  freq_detected <- .detect_frequency(df, quarterly_vars, verbose)

  # Step 3: Build unified monthly calendar
  if (verbose) cat("Step 3: Building unified monthly calendar...\n")
  monthly_grid <- .build_monthly_calendar(df, freq_detected, start_date, end_date, verbose)

  # Step 4: Determine log transformations
  if (verbose) cat("Step 4: Determining transformations...\n")
  log_vars <- .determine_log_transform(monthly_grid$data, log_transform, freq_detected$vars)

  # Step 5: Run unit root tests
  if (verbose) cat("Step 5: Running unit root tests...\n")
  unit_root_results <- .run_unit_root_tests(monthly_grid$data, freq_detected, adf_pvalue, verbose)

  # Step 6: Apply transformations
  if (verbose) cat("Step 6: Applying transformations...\n")
  transformed <- .apply_transformations(
    monthly_grid$data,
    log_vars,
    unit_root_results,
    difference,
    freq_detected,
    verbose
  )

  # Step 7: Classify flow vs stock
  if (verbose) cat("Step 7: Classifying flow vs stock types...\n")
  var_types <- .classify_flow_stock(freq_detected$freq, flow_vars)

  # Step 8: Seasonal adjustment (add dummies if requested)
  seasonal_info <- list(dummies_included = seasonal_dummies, method = "none")
  if (seasonal_dummies) {
    seasonal_info$method <- "dummy_variables"
    if (verbose) cat("  Seasonal dummies will be added during VAR estimation\n")
  }

  # Step 9: Optional standardization
  if (standardize) {
    if (verbose) cat("Step 8: Standardizing variables...\n")
    transformed <- .standardize_data(transformed, verbose)
  }

  # Compile metadata
  metadata <- list(
    vars = freq_detected$vars,
    freq = freq_detected$freq,
    type = var_types,
    frequency_code = stats::setNames(ifelse(freq_detected$freq == "monthly", 1L, 2L), freq_detected$vars),
    transformation = transformed$applied_transforms,
    seasonal_adjustment = seasonal_info,
    sample_start = zoo::index(transformed$data)[1],
    sample_end = zoo::index(transformed$data)[length(zoo::index(transformed$data))]
  )

  if (verbose) {
    cat("\n=== Preparation Complete ===\n")
    cat(sprintf("  Variables: %d\n", length(metadata$vars)))
    cat(sprintf(
      "  Sample: %s to %s (%d months)\n",
      metadata$sample_start, metadata$sample_end, nrow(transformed$data)
    ))
    cat(sprintf("  Monthly vars: %d\n", sum(metadata$freq == "monthly")))
    cat(sprintf(
      "  Quarterly vars: %d (%d flow, %d stock)\n",
      sum(metadata$freq == "quarterly"),
      sum(metadata$type == "flow"),
      sum(metadata$type == "stock")
    ))
    cat("\n")
  }

  return(structure(
    list(
      data = transformed$data,
      metadata = metadata,
      transformation_params = transformed$params,
      unit_root_tests = unit_root_results,
      original_data = monthly_grid$original
    ),
    class = "mfvar_data"
  ))
}

# ==============================================================================
# Internal helper functions
# ==============================================================================

#' Load data from various sources
#' @keywords internal
.load_data_sources <- function(data_sources) {
  if (is.character(data_sources)) {
    # CSV file paths
    df_list <- lapply(data_sources, function(path) {
      if (!file.exists(path)) stop(paste("File not found:", path))
      read.csv(path, stringsAsFactors = FALSE)
    })
    # Merge all
    df <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), df_list)
  } else if (is.data.frame(data_sources)) {
    df <- data_sources
  } else if (is.list(data_sources)) {
    df <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), data_sources)
  } else {
    stop("data_sources must be character vector (CSV paths), data frame, or list of data frames")
  }

  if (!"date" %in% names(df)) {
    stop("Data must contain a 'date' column")
  }

  return(df)
}

#' Parse dates to yearmon
#' @keywords internal
.parse_dates <- function(df) {
  if (is.character(df$date)) {
    # Try parsing as Date first
    df$date <- tryCatch(
      as.Date(df$date),
      error = function(e) {
        # Try as yearmon
        zoo::as.yearmon(df$date)
      }
    )
  }

  # Convert to yearmon if not already
  if (!inherits(df$date, "yearmon")) {
    df$date <- zoo::as.yearmon(df$date)
  }

  # Sort by date
  df <- df[order(df$date), ]
  return(df)
}

#' Detect frequency of each variable
#' @keywords internal
.detect_frequency <- function(df, quarterly_vars, verbose) {
  vars <- setdiff(names(df), "date")
  freq <- setNames(rep("monthly", length(vars)), vars)

  if (!is.null(quarterly_vars)) {
    # User-specified quarterly variables
    freq[quarterly_vars] <- "quarterly"
  } else {
    # Auto-detect based on missingness pattern
    for (v in vars) {
      non_na <- df[!is.na(df[[v]]), ]
      if (nrow(non_na) > 0) {
        # Check if observations are roughly quarterly spaced
        date_seq <- zoo::as.yearmon(non_na$date)
        if (length(date_seq) > 1) {
          gaps <- diff(as.numeric(date_seq) * 12)
          median_gap <- median(gaps)
          if (median_gap >= 2.5) { # Approximately quarterly
            freq[v] <- "quarterly"
          }
        }
      }
    }
  }

  if (verbose) {
    monthly_vars <- names(freq)[freq == "monthly"]
    quarterly_vars <- names(freq)[freq == "quarterly"]
    cat(sprintf("  Monthly: %s\n", paste(monthly_vars, collapse = ", ")))
    cat(sprintf("  Quarterly: %s\n", paste(quarterly_vars, collapse = ", ")))
  }

  return(list(vars = vars, freq = freq))
}

#' Build unified monthly calendar with NA for unobserved quarters
#' @keywords internal
.build_monthly_calendar <- function(df, freq_detected, start_date, end_date, verbose) {
  # Determine date range
  date_range <- range(df$date, na.rm = TRUE)
  if (!is.null(start_date)) {
    start_ym <- zoo::as.yearmon(as.Date(start_date))
    date_range[1] <- max(date_range[1], start_ym)
  }
  if (!is.null(end_date)) {
    end_ym <- zoo::as.yearmon(as.Date(end_date))
    date_range[2] <- min(date_range[2], end_ym)
  }

  # Create complete monthly sequence
  all_months <- seq(date_range[1], date_range[2], by = 1 / 12)

  # Initialize grid
  grid_list <- list()

  for (v in freq_detected$vars) {
    if (freq_detected$freq[v] == "monthly") {
      # Direct mapping
      vals <- df[[v]][match(all_months, df$date)]
    } else {
      # Quarterly: map to quarter-end months only
      vals <- rep(NA, length(all_months))
      quarter_end_indices <- which(stats::cycle(all_months) %in% c(3, 6, 9, 12))

      for (idx in quarter_end_indices) {
        month_date <- all_months[idx]
        match_idx <- which(df$date == month_date)
        if (length(match_idx) > 0) {
          vals[idx] <- df[[v]][match_idx[1]]
        }
      }
    }
    grid_list[[v]] <- vals
  }

  # Create zoo object
  grid_data <- do.call(cbind, grid_list)
  colnames(grid_data) <- freq_detected$vars
  grid_zoo <- zoo::zoo(grid_data, order.by = all_months)

  if (verbose) {
    cat(sprintf(
      "  Date range: %s to %s (%d months)\n",
      start(grid_zoo), end(grid_zoo), nrow(grid_zoo)
    ))
  }

  return(list(data = grid_zoo, original = grid_zoo))
}

#' Determine which variables to log-transform
#' @keywords internal
.determine_log_transform <- function(data, log_transform, vars) {
  if (!is.null(log_transform)) {
    return(log_transform)
  }

  # Auto-detect: positive variables
  log_vars <- character(0)
  for (v in vars) {
    vals <- data[, v]
    vals_complete <- vals[complete.cases(vals)]
    if (length(vals_complete) > 0 && all(vals_complete > 0)) {
      log_vars <- c(log_vars, v)
    }
  }

  return(log_vars)
}

#' Run unit root tests (ADF and KPSS)
#' @keywords internal
.run_unit_root_tests <- function(data, freq_detected, adf_pvalue, verbose) {
  # Check if urca package is available
  has_urca <- requireNamespace("urca", quietly = TRUE)
  has_tseries <- requireNamespace("tseries", quietly = TRUE)

  if (!has_urca && !has_tseries) {
    if (verbose) cat("  Unit root testing packages not available. Skipping tests.\n")
    return(NULL)
  }

  results <- data.frame(
    variable = character(),
    adf_statistic = numeric(),
    adf_pvalue = numeric(),
    kpss_statistic = numeric(),
    kpss_pvalue = numeric(),
    decision = character(),
    stringsAsFactors = FALSE
  )

  for (v in freq_detected$vars) {
    series <- data[, v]
    series_complete <- series[complete.cases(series)]

    if (length(series_complete) < 20) {
      if (verbose) cat(sprintf("    %s: insufficient data\n", v))
      next
    }

    # ADF test
    adf_result <- if (has_tseries) {
      tryCatch(
        tseries::adf.test(series_complete, k = 1),
        error = function(e) list(statistic = NA, p.value = NA)
      )
    } else {
      list(statistic = NA, p.value = NA)
    }

    # KPSS test
    kpss_result <- if (has_tseries) {
      tryCatch(
        tseries::kpss.test(series_complete, null = "Level"),
        error = function(e) list(statistic = NA, p.value = NA)
      )
    } else {
      list(statistic = NA, p.value = NA)
    }

    # Decision: if ADF p-value > threshold, series has unit root -> difference
    decision <- if (!is.na(adf_result$p.value) && adf_result$p.value > adf_pvalue) {
      "difference"
    } else {
      "level"
    }

    results <- rbind(results, data.frame(
      variable = v,
      adf_statistic = adf_result$statistic,
      adf_pvalue = adf_result$p.value,
      kpss_statistic = kpss_result$statistic,
      kpss_pvalue = kpss_result$p.value,
      decision = decision,
      stringsAsFactors = FALSE
    ))
  }

  if (verbose && nrow(results) > 0) {
    cat("  Unit root test results:\n")
    for (i in seq_len(nrow(results))) {
      cat(sprintf(
        "    %s: ADF p=%.3f -> %s\n",
        results$variable[i],
        results$adf_pvalue[i],
        results$decision[i]
      ))
    }
  }

  return(results)
}

#' Apply transformations (log, difference)
#' @keywords internal
.apply_transformations <- function(data, log_vars, unit_root_results, difference, freq_detected, verbose) {
  transformed_data <- data
  applied_transforms <- setNames(rep("level", length(freq_detected$vars)), freq_detected$vars)
  params <- list(means = list(), sds = list(), log_vars = log_vars)

  for (v in freq_detected$vars) {
    series <- transformed_data[, v]
    transform_chain <- character(0)

    # Apply log if specified
    if (v %in% log_vars) {
      series <- log(series)
      transform_chain <- c(transform_chain, "log")
    }

    # Apply differencing if decided
    if (difference && !is.null(unit_root_results)) {
      var_result <- unit_root_results[unit_root_results$variable == v, ]
      if (nrow(var_result) > 0 && var_result$decision == "difference") {
        series <- diff(series, lag = 1)
        transform_chain <- c(transform_chain, "diff")
      }
    }

    # Store transformation
    if (length(transform_chain) == 0) {
      applied_transforms[v] <- "level"
    } else {
      applied_transforms[v] <- paste(transform_chain, collapse = "_")
    }

    transformed_data[, v] <- series
  }

  if (verbose) {
    cat("  Transformations applied:\n")
    for (v in freq_detected$vars) {
      cat(sprintf("    %s: %s\n", v, applied_transforms[v]))
    }
  }

  return(list(
    data = transformed_data,
    applied_transforms = applied_transforms,
    params = params
  ))
}

#' Classify variables as flow or stock
#' @keywords internal
.classify_flow_stock <- function(freq, flow_vars) {
  types <- setNames(rep("stock", length(freq)), names(freq))

  # Monthly variables are neither flow nor stock in the quarterly sense
  types[freq == "monthly"] <- "monthly"

  # Quarterly variables default to stock unless specified as flow
  if (!is.null(flow_vars)) {
    types[flow_vars] <- "flow"
  }

  return(types)
}

#' Standardize data
#' @keywords internal
.standardize_data <- function(transformed, verbose) {
  data <- transformed$data
  params <- transformed$params

  for (v in colnames(data)) {
    series <- data[, v]
    series_complete <- series[complete.cases(series)]

    if (length(series_complete) > 0) {
      m <- mean(series_complete)
      s <- sd(series_complete)

      if (s > 0) {
        data[, v] <- (series - m) / s
        params$means[[v]] <- m
        params$sds[[v]] <- s
      }
    }
  }

  if (verbose) {
    cat(sprintf("  Standardized %d variables\n", length(params$means)))
  }

  transformed$data <- data
  transformed$params <- params
  return(transformed)
}
