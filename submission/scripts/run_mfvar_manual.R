#!/usr/bin/env Rscript

# Mixed-Frequency VAR using custom mfvar2 package
# ---------------------------------------------------------------
# This runner adapts the main-branch data pipeline to the mfvar2
# package (custom Gibbs sampler). It mirrors the behaviour of the
# package-based workflow while keeping all inputs/outputs inside the
# submission repo.
# ---------------------------------------------------------------

source(file.path("R", "setup.R"))
source(file.path("R", "data_processing.R"))
source(file.path("R", "mfvar2_adapter.R"))

activate_project()
load_required_packages(c(required_pkgs, "pkgload"))

pkgload::load_all(file.path("models", "mfvar2"), export_all = FALSE, quiet = TRUE)

cli_args <- commandArgs(trailingOnly = TRUE)

arg_has <- function(flag) any(cli_args == flag)
arg_value <- function(prefix, default = NULL) {
	match <- cli_args[grepl(paste0("^", prefix, "="), cli_args)]
	if (!length(match)) return(default)
	val <- sub(paste0("^", prefix, "="), "", match[1])
	if (!nzchar(val)) return(default)
	val
}

parse_numeric_grid <- function(value, default) {
	if (is.null(value)) return(default)
	nums <- suppressWarnings(as.numeric(strsplit(value, ",")[[1]]))
	nums <- nums[is.finite(nums)]
	if (!length(nums)) {
		return(default)
	}
	unique(nums)
}

fast_mode <- arg_has("--fast")
output_dir <- arg_value("--output-dir", file.path("output", "forecasts", "mfvar2"))
horizon_months <- as.integer(arg_value("--horizon", "12"))
if (is.na(horizon_months) || horizon_months < 1) {
	stop("Invalid --horizon value. Provide a positive integer (months).")
}

micro_grid_mode <- arg_has("--micro-grid")
lambda1_grid_arg <- arg_value("--lambda1-grid")
lambda2_grid_arg <- arg_value("--lambda2-grid")
lambda4_grid_arg <- arg_value("--lambda4-grid")
lambda5_grid_arg <- arg_value("--lambda5-grid")

dir_create <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE)

OUT_DIR <- output_dir
CSV_DIR <- file.path(OUT_DIR, "csv")
MODEL_DIR <- file.path(OUT_DIR, "models")
for (dir in c(OUT_DIR, CSV_DIR, MODEL_DIR)) dir_create(dir)

runner_seed <- as.integer(Sys.getenv("MFVAR2_SEED", "321"))
if (is.na(runner_seed)) runner_seed <- 321L

run_config <- list(
	tune = !fast_mode && !arg_has("--skip-tuning"),
	n_draws = if (fast_mode) 1000L else 4000L,
	burnin = if (fast_mode) 300L else 1000L,
	n_sim = if (fast_mode) 250L else 1000L
)

if (micro_grid_mode) {
	run_config$tune <- TRUE
}

lambda_defaults <- list(
	lambda1 = c(0.04, 0.06, 0.08),
	lambda2 = c(1, 2),
	lambda3 = 1,
	lambda4 = c(1),
	lambda5 = c(1)
)

lambda_grids <- list(
	lambda1 = parse_numeric_grid(lambda1_grid_arg, lambda_defaults$lambda1),
	lambda2 = parse_numeric_grid(lambda2_grid_arg, lambda_defaults$lambda2),
	lambda3 = lambda_defaults$lambda3,
	lambda4 = parse_numeric_grid(lambda4_grid_arg, lambda_defaults$lambda4),
	lambda5 = parse_numeric_grid(lambda5_grid_arg, lambda_defaults$lambda5)
)

run_micro_grid_tuning <- function(data_prepared,
		p,
		lambda_grids,
		n_gibbs_mdd,
		burnin_mdd,
		seed) {
	if (!inherits(data_prepared, "mfvar_data")) {
		stop("micro-grid tuning requires an mfvar_data object")
	}
	y_data <- data_prepared$data
	metadata <- data_prepared$metadata
	grid <- expand.grid(
		lambda1 = lambda_grids$lambda1,
		lambda2 = lambda_grids$lambda2,
		lambda3 = lambda_grids$lambda3,
		lambda4 = lambda_grids$lambda4,
		lambda5 = lambda_grids$lambda5,
		stringsAsFactors = FALSE
	)
	if (!nrow(grid)) {
		stop("micro-grid produced zero combinations; check lambda grids")
	}
	message(sprintf("→ Micro-grid tuning over %d combinations", nrow(grid)))
	start_time <- Sys.time()
	grid$log_mdd <- NA_real_
	for (i in seq_len(nrow(grid))) {
		combo <- grid[i, ]
		iter_start <- Sys.time()
		log_mdd <- tryCatch(
			mfvar2:::`.compute_marginal_data_density_geweke`(
				y_data = y_data,
				metadata = metadata,
				p = p,
				lambda1 = combo$lambda1,
				lambda2 = combo$lambda2,
				lambda3 = combo$lambda3,
				lambda4 = combo$lambda4,
				lambda5 = combo$lambda5,
				n_gibbs = n_gibbs_mdd,
				burnin = burnin_mdd,
				seed = seed
			),
			error = function(e) {
				warning(sprintf("Micro-grid combo failed: %s", e$message))
				return(-Inf)
			}
		)
		grid$log_mdd[i] <- log_mdd
		iter_elapsed <- as.numeric(difftime(Sys.time(), iter_start, units = "secs"))
		total_elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
		avg_time <- total_elapsed / i
		remaining <- (nrow(grid) - i) * avg_time
		message(sprintf(
			"  • Combo %02d/%02d | λ1=%.3f λ2=%.1f λ4=%.1f λ5=%.1f | %.1fs (ETA %.1fm)",
			i,
			nrow(grid),
			combo$lambda1,
			combo$lambda2,
			combo$lambda4,
			combo$lambda5,
			iter_elapsed,
			remaining / 60
		))
	}
	best_idx <- which.max(grid$log_mdd)
	best <- grid[best_idx, ]
	message(sprintf(
		"→ Micro-grid best: λ1=%.3f λ2=%.1f λ3=%.1f λ4=%.1f λ5=%.1f | log MDD %.2f",
		best$lambda1,
		best$lambda2,
		best$lambda3,
		best$lambda4,
		best$lambda5,
		best$log_mdd
	))
	list(
		hyperparameters = list(
			lambda1 = best$lambda1,
			lambda2 = best$lambda2,
			lambda3 = best$lambda3,
			lambda4 = best$lambda4,
			lambda5 = best$lambda5
		),
		grid = grid
	)
}

message(sprintf("→ mfvar2 runner | fast mode: %s | tuning: %s", fast_mode, run_config$tune))

start_time <- Sys.time()

adapter <- prepare_mfvar2_input(verbose = TRUE)
qdat_raw <- read_quarterly_data(file.path(".", "data"))
last_obs_qtr <- tail(qdat_raw$qtr, 1)

hyperparams <- NULL
grid_details <- NULL
if (run_config$tune) {
	if (micro_grid_mode) {
		micro_res <- run_micro_grid_tuning(
			data_prepared = adapter$prepared,
			p = 2,
			lambda_grids = lambda_grids,
			n_gibbs_mdd = if (fast_mode) 750L else 2000L,
			burnin_mdd = if (fast_mode) 300L else 1000L,
			seed = runner_seed
		)
		hyperparams <- micro_res$hyperparameters
		grid_details <- micro_res$grid
	} else {
		message("→ tuning hyperparameters (mfvar2)")
		hyperparams <- mfvar2::tune_minnesota_hyper(
			data_prepared = adapter$prepared,
			p = 2,
			lambda1_grid = c(0.05, 0.1, 0.15, 0.2, 0.3),
			lambda2_grid = c(1, 2, 3, 4, 5),
			lambda3 = 1,
			lambda4_grid = c(1, 2, 3, 4, 5),
			lambda5_grid = c(1, 2, 3, 4, 5),
			n_gibbs_mdd = if (fast_mode) 750L else 2000L,
			burnin_mdd = if (fast_mode) 300L else 1000L,
			verbose = TRUE,
			seed = runner_seed
		)
	}
} else {
	hyperparams <- list(
		lambda1 = 0.2,
		lambda2 = 1.0,
		lambda3 = 1.0,
		lambda4 = 1.0,
		lambda5 = 1.0
	)
}

posterior <- mfvar2::estimate_mf_bvar(
	data_prepared = adapter$prepared,
	p = 2,
	hyperparameters = hyperparams,
	n_draws = run_config$n_draws,
	burnin = run_config$burnin,
	thinning = 1L,
	verbose = TRUE,
	seed = runner_seed
)

forecast_obj <- mfvar2::forecast_mf_bvar(
	posterior = posterior,
	horizon_months = horizon_months,
	n_sim = run_config$n_sim,
	seed = runner_seed
)

results <- list(
	posterior = posterior,
	forecasts = forecast_obj,
	hyperparameters = hyperparams,
	diagnostics = posterior$diagnostics,
	settings = list(
		p = 2,
		n_draws = run_config$n_draws,
		burnin = run_config$burnin,
		n_sim = run_config$n_sim,
		forecast_horizon = horizon_months
	)
)

forecasts <- results$forecasts
quantiles <- forecasts$quantiles
median_idx <- which.min(abs(quantiles - 0.5))

quarterly_full <- lapply(names(forecasts$forecasts_quarterly), function(var) {
	qmat <- forecasts$forecasts_quarterly[[var]]
	if (is.null(qmat)) return(NULL)
	q_df <- tibble::as_tibble(qmat)
	if (ncol(q_df) != length(quantiles)) {
		colnames(q_df) <- paste0("quantile_", seq_len(ncol(q_df)))
	} else {
		colnames(q_df) <- paste0("q", quantiles)
	}
	q_df <- q_df |>
		dplyr::mutate(
			variable = var,
			step_ahead = dplyr::row_number()
		) |>
		tidyr::pivot_longer(
			cols = tidyselect::starts_with("q"),
			names_to = "quantile",
			values_to = "value"
		) |>
		dplyr::mutate(
			quantile = readr::parse_number(.data$quantile)
		)
	q_df$value <- dplyr::if_else(q_df$variable == "exch_rate", exp(q_df$value), q_df$value)
	q_df
}) |> dplyr::bind_rows()

if (!nrow(quarterly_full)) {
	stop("mfvar2 did not produce quarterly forecasts – cannot continue.")
}

quarterly_full <- quarterly_full |>
	dplyr::mutate(
		quarter_end = zoo::as.Date(last_obs_qtr + (.data$step_ahead / 4), frac = 1)
	)

targets <- quarterly_full |>
	dplyr::filter(.data$quantile == quantiles[median_idx]) |>
	dplyr::filter(.data$step_ahead %in% c(1L, 4L)) |>
	dplyr::mutate(
		horizon = dplyr::case_when(
			.data$step_ahead == 1L ~ "1-step ahead",
			.data$step_ahead == 4L ~ "1-year ahead",
			TRUE ~ NA_character_
		)
	) |>
	dplyr::select(variable, horizon, quarter_end, median = value)

readr::write_csv(quarterly_full, file.path(CSV_DIR, "mfvar2_forecasts_quarterly.csv"))
readr::write_csv(targets, file.path(CSV_DIR, "mfvar2_forecasts_targets.csv"))

saveRDS(results, file.path(MODEL_DIR, "mfvar2_results.rds"))

summary_path <- file.path(OUT_DIR, "mfvar2_summary.txt")
sink(summary_path)
cat("mfvar2 manual workflow summary\n")
cat(rep("=", 40), "\n", sep = "")
cat(sprintf("Sample: %s to %s\n", adapter$sample_start, adapter$sample_end))
cat(sprintf("Quarterly vars: %s\n", paste(adapter$quarterly_vars, collapse = ", ")))
cat(sprintf("Monthly vars: %s\n", paste(adapter$monthly_vars, collapse = ", ")))
cat(sprintf("Lag order: %d\n", results$settings$p))
cat(sprintf("Draws: %d (burn-in %d)\n", results$settings$n_draws, results$settings$burnin))
cat(sprintf("Forecast horizon: %d months\n", horizon_months))
cat("Selected hyperparameters (if tuned):\n")
print(results$hyperparameters)
if (!is.null(grid_details)) {
	cat("\nMicro-grid evaluation (top rows):\n")
	print(utils::head(grid_details[order(-grid_details$log_mdd), ], n = min(5, nrow(grid_details))))
}
sink()

elapsed <- difftime(Sys.time(), start_time, units = "mins")
message(sprintf("✓ mfvar2 run complete in %.1f minutes", as.numeric(elapsed)))
message(sprintf("Outputs written to %s", OUT_DIR))
