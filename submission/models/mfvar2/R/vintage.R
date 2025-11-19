# ==============================================================================
# vintage.R - Real-time data vintage construction and publication lag modeling
# ==============================================================================
# Implements real-time forecasting workflow from Schorfheide & Song (2013):
# - Data vintage construction: simulate what was available at each forecast origin
# - Publication lag modeling: different variables released at different times
# - Information set classification: +0, +1, +2 months within quarter
# - Real-time data matrix with revision structure
# ==============================================================================
# Plain-language summary for non-specialists:
# - These utilities rebuild the world as analysts actually saw it in real time. Different indicators
#   arrive with different publication delays and sometimes get revised; this file keeps track of that
#   messy reality so the rolling evaluations stay honest.

#' Create real-time data vintages
#'
#' Constructs a real-time database where each vintage represents the information
#' set available at a specific forecast origin. This simulates the actual data
#' availability that a forecaster would face in real time.
#'
#' Key features:
#' - Publication lags: Monthly variables available with k-month lag
#' - Quarterly variables: Released only at quarter-end with additional lag
#' - Revisions: Early releases can be replaced by revised values in later vintages
#' - Financial variables: Available immediately (0 lag)
#'
#' @param final_data zoo object with final revised data (from prepare_data_snb)
#' @param metadata Metadata list with frequency, type information
#' @param publication_lags Named list of publication lags by variable (in months)
#'   Default: monthly = 1 month, quarterly = 2 months, financial = 0 months
#' @param revision_structure Optional list describing revision process
#' @param forecast_origins Vector of dates (yearmon) for forecast origins
#' @param verbose Logical: print progress?
#'
#' @return vintage_database object with:
#'   - vintages: list of zoo objects, one per forecast origin
#'   - origins: vector of forecast origin dates
#'   - information_sets: classification of each origin (+0, +1, +2)
#'   - publication_lags: publication lag by variable
#'   - availability_matrix: logical matrix [origin x variable x time]
#'
#' @export
create_real_time_vintages <- function(final_data,
                                      metadata,
                                      publication_lags = NULL,
                                      revision_structure = NULL,
                                      forecast_origins = NULL,
                                      verbose = TRUE) {
    if (verbose) cat("=== Creating Real-Time Data Vintages ===\n\n")

    # Extract variables and time index
    vars <- colnames(final_data)
    time_index <- zoo::index(final_data)
    n_vars <- length(vars)

    # Set default publication lags if not provided
    if (is.null(publication_lags)) {
        publication_lags <- .default_publication_lags(vars, metadata, verbose)
    }

    # Define forecast origins if not provided
    if (is.null(forecast_origins)) {
        # Default: quarterly origins starting after sufficient history
        min_obs <- 120 # 10 years of history
        first_origin <- time_index[min(min_obs, length(time_index) - 24)]
        last_origin <- time_index[length(time_index) - 12] # Leave 1 year for evaluation

        forecast_origins <- .generate_forecast_origins(
            first_origin, last_origin,
            frequency = "quarterly"
        )
    }

    n_origins <- length(forecast_origins)

    if (verbose) {
        cat(sprintf("  Variables: %d\n", n_vars))
        cat(sprintf("  Time periods: %d months\n", length(time_index)))
        cat(sprintf("  Forecast origins: %d\n", n_origins))
        cat(sprintf("  First origin: %s\n", forecast_origins[1]))
        cat(sprintf("  Last origin: %s\n", forecast_origins[n_origins]))
        cat("\n  Publication lags:\n")
        for (v in vars) {
            cat(sprintf("    %s: %d month(s)\n", v, publication_lags[[v]]))
        }
        cat("\n")
    }

    # Initialize storage
    vintages <- vector("list", n_origins)
    information_sets <- character(n_origins)

    # Build each vintage
    if (verbose) cat("Building vintages...\n")

    for (i in seq_along(forecast_origins)) {
        origin <- forecast_origins[i]

        # Create vintage for this origin
        vintage <- .construct_single_vintage(
            final_data = final_data,
            metadata = metadata,
            origin = origin,
            publication_lags = publication_lags,
            revision_structure = revision_structure
        )

        vintages[[i]] <- vintage$data
        information_sets[i] <- vintage$info_set

        if (verbose && i %% max(1, n_origins %/% 10) == 0) {
            cat(sprintf(
                "  Progress: %d/%d vintages (%.0f%%)\n",
                i, n_origins, 100 * i / n_origins
            ))
        }
    }

    if (verbose) cat("\n=== Vintage Construction Complete ===\n\n")

    return(structure(
        list(
            vintages = vintages,
            origins = forecast_origins,
            information_sets = information_sets,
            publication_lags = publication_lags,
            metadata = metadata,
            final_data = final_data
        ),
        class = "vintage_database"
    ))
}

#' Construct single vintage for a forecast origin
#'
#' @keywords internal
.construct_single_vintage <- function(final_data, metadata, origin,
                                      publication_lags, revision_structure) {
    vars <- colnames(final_data)
    time_index <- zoo::index(final_data)

    # Initialize vintage with NAs
    vintage_data <- final_data
    vintage_data[, ] <- NA

    # Determine which quarter the origin is in
    origin_year <- as.integer(format(origin, "%Y"))
    origin_month <- as.integer(format(origin, "%m"))
    origin_quarter <- ceiling(origin_month / 3)

    # Classify information set (+0, +1, +2)
    months_in_quarter <- origin_month - (origin_quarter - 1) * 3
    if (months_in_quarter == 1) {
        info_set <- "+0" # End of first month: no within-quarter data
    } else if (months_in_quarter == 2) {
        info_set <- "+1" # End of second month: 1 month of data
    } else {
        info_set <- "+2" # End of third month: 2 months of data
    }

    # Fill in available data based on publication lags
    for (v in vars) {
        lag <- publication_lags[[v]]
        freq <- metadata$freq[[v]]

        # Latest period that could be published by origin
        latest_available <- .shift_yearmon(origin, -lag)

        if (freq == "monthly") {
            # Monthly: all observations up to latest_available
            available_idx <- time_index <= latest_available
            vintage_data[available_idx, v] <- final_data[available_idx, v]
        } else if (freq == "quarterly") {
            # Quarterly: only quarter-end observations, subject to publication lag
            for (t_idx in seq_along(time_index)) {
                t <- time_index[t_idx]

                # Is this a quarter-end month?
                t_month <- as.integer(format(t, "%m"))
                is_qtr_end <- (t_month %in% c(3, 6, 9, 12))

                if (is_qtr_end) {
                    # Would this quarter's data be published by origin?
                    publication_date <- .shift_yearmon(t, lag)

                    if (publication_date <= origin) {
                        vintage_data[t_idx, v] <- final_data[t_idx, v]
                    }
                }
            }
        }
    }

    # Apply revision structure if provided
    if (!is.null(revision_structure)) {
        vintage_data <- .apply_revisions(
            vintage_data, final_data, origin, revision_structure
        )
    }

    return(list(
        data = vintage_data,
        info_set = info_set
    ))
}

#' Get data available at specific forecast origin
#'
#' Extract the vintage (information set) that would be available at a
#' specific forecast origin from a vintage database.
#'
#' @param vintage_db vintage_database object from create_real_time_vintages()
#' @param origin Date or yearmon specifying the forecast origin
#' @param return_metadata Logical: also return metadata about information set?
#'
#' @return If return_metadata=FALSE: zoo object with data available at origin.
#'   If return_metadata=TRUE: list with data, information_set, and statistics.
#'
#' @export
get_vintage_at_origin <- function(vintage_db, origin, return_metadata = FALSE) {
    if (!inherits(vintage_db, "vintage_database")) {
        stop("vintage_db must be a vintage_database object")
    }

    origin <- zoo::as.yearmon(origin)

    # Find matching origin
    origin_idx <- which(vintage_db$origins == origin)

    if (length(origin_idx) == 0) {
        stop(sprintf("Origin %s not found in vintage database", origin))
    }

    if (length(origin_idx) > 1) {
        warning(sprintf("Multiple vintages found for origin %s, using first", origin))
        origin_idx <- origin_idx[1]
    }

    vintage_data <- vintage_db$vintages[[origin_idx]]

    if (!return_metadata) {
        return(vintage_data)
    }

    # Calculate statistics about data availability
    n_obs_avail <- colSums(!is.na(vintage_data))
    pct_avail <- 100 * n_obs_avail / nrow(vintage_data)

    return(list(
        data = vintage_data,
        origin = origin,
        information_set = vintage_db$information_sets[origin_idx],
        availability = data.frame(
            variable = names(n_obs_avail),
            n_obs = n_obs_avail,
            pct_available = pct_avail,
            row.names = NULL
        )
    ))
}

#' Set default publication lags based on variable type
#'
#' @keywords internal
.default_publication_lags <- function(vars, metadata, verbose = FALSE) {
    lags <- list()

    for (v in vars) {
        freq <- metadata$freq[[v]]

        # Check if variable is financial (interest rates, stock prices)
        is_financial <- .is_financial_variable(v)

        if (is_financial) {
            # Financial variables: immediate availability
            lags[[v]] <- 0
        } else if (freq == "monthly") {
            # Monthly real/price variables: 1 month publication lag
            lags[[v]] <- 1
        } else if (freq == "quarterly") {
            # Quarterly variables: 2 month lag after quarter end
            # (e.g., Q1 GDP released in early June)
            lags[[v]] <- 2
        } else {
            # Default
            lags[[v]] <- 1
        }
    }

    if (verbose) {
        cat("Default publication lags assigned:\n")
        for (v in vars) {
            cat(sprintf(
                "  %s (%s): %d month(s)\n",
                v, metadata$freq[[v]], lags[[v]]
            ))
        }
    }

    return(lags)
}

#' Check if variable is financial
#'
#' @keywords internal
.is_financial_variable <- function(var_name) {
    financial_keywords <- c(
        "interest", "rate", "bond", "yield", "stock", "equity",
        "sp500", "ftse", "dax", "smi", "exchange", "forex"
    )

    var_lower <- tolower(var_name)
    any(sapply(financial_keywords, function(kw) grepl(kw, var_lower)))
}

#' Generate forecast origins
#'
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

#' Shift yearmon by months
#'
#' @keywords internal
.shift_yearmon <- function(ym, shift) {
    ym_numeric <- as.numeric(ym)
    ym_shifted <- ym_numeric + shift / 12
    zoo::as.yearmon(ym_shifted)
}

#' Apply data revisions
#'
#' Simulate the revision process where early data releases are noisy
#' and later vintages contain revised values.
#'
#' @keywords internal
.apply_revisions <- function(vintage_data, final_data, origin,
                             revision_structure) {
    # Simple revision model: early releases add noise
    # More sophisticated models could use actual revision triangles

    if (is.null(revision_structure$variables)) {
        return(vintage_data)
    }

    for (v in revision_structure$variables) {
        if (v %in% colnames(vintage_data)) {
            # Identify observations that were recently released
            recent_obs <- !is.na(vintage_data[, v])

            if (revision_structure$method == "noise") {
                # Add noise to recent observations
                noise_sd <- revision_structure$noise_sd * sd(final_data[, v], na.rm = TRUE)
                n_recent <- sum(recent_obs)
                if (n_recent > 0) {
                    noise <- rnorm(n_recent, 0, noise_sd)
                    vintage_data[recent_obs, v] <- vintage_data[recent_obs, v] + noise
                }
            }
        }
    }

    return(vintage_data)
}

#' Create publication lag configuration
#'
#' Helper function to specify custom publication lags for variables.
#'
#' @param ... Named arguments: variable_name = lag_in_months
#' @param default_monthly Default lag for monthly variables (default: 1)
#' @param default_quarterly Default lag for quarterly variables (default: 2)
#' @param default_financial Default lag for financial variables (default: 0)
#'
#' @return Named list of publication lags
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lags <- publication_lag_config(
#'     cpi = 1,
#'     gdp = 2,
#'     interest_rate = 0,
#'     default_monthly = 1
#' )
#' }
publication_lag_config <- function(...,
                                   default_monthly = 1,
                                   default_quarterly = 2,
                                   default_financial = 0) {
    custom_lags <- list(...)

    structure(
        list(
            custom = custom_lags,
            defaults = list(
                monthly = default_monthly,
                quarterly = default_quarterly,
                financial = default_financial
            )
        ),
        class = "publication_lag_config"
    )
}

#' Print vintage database summary
#'
#' @param x vintage_database object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.vintage_database <- function(x, ...) {
    cat("Real-Time Vintage Database\n")
    cat("==========================\n\n")

    cat(sprintf("Number of vintages: %d\n", length(x$vintages)))
    cat(sprintf("First origin: %s\n", x$origins[1]))
    cat(sprintf("Last origin: %s\n", x$origins[length(x$origins)]))
    cat(sprintf("Variables: %d\n", ncol(x$vintages[[1]])))

    cat("\nInformation set distribution:\n")
    info_table <- table(x$information_sets)
    for (i in names(info_table)) {
        cat(sprintf("  %s: %d origins\n", i, info_table[i]))
    }

    cat("\nPublication lags:\n")
    for (v in names(x$publication_lags)) {
        cat(sprintf("  %s: %d month(s)\n", v, x$publication_lags[[v]]))
    }

    invisible(x)
}
