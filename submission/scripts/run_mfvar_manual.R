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

fast_mode <- arg_has("--fast")
output_dir <- arg_value("--output-dir", file.path("output", "forecasts", "mfvar2"))
horizon_months <- as.integer(arg_value("--horizon", "12"))
if (is.na(horizon_months) || horizon_months < 1) {
	stop("Invalid --horizon value. Provide a positive integer (months).")
}

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

message(sprintf("→ mfvar2 runner | fast mode: %s | tuning: %s", fast_mode, run_config$tune))

start_time <- Sys.time()

adapter <- prepare_mfvar2_input(verbose = TRUE)
qdat_raw <- read_quarterly_data(file.path(".", "data"))
last_obs_qtr <- tail(qdat_raw$qtr, 1)

if (run_config$tune) {
	message("→ tuning hyperparameters (mfvar2)")
	hyperparams <- mfvar2::tune_minnesota_hyper(
		data_prepared = adapter$prepared,
		p = 2,
		lambda1_grid = c(0.05, 0.1, 0.15, 0.2, 0.3),
		lambda2_grid = c(1, 2, 3, 4, 5),
		lambda3_grid = c(1),
		lambda4_grid = c(1, 2, 3, 4, 5),
		lambda5_grid = c(1, 2, 3, 4, 5),
		n_gibbs_mdd = if (fast_mode) 750L else 2000L,
		burnin_mdd = if (fast_mode) 300L else 1000L,
		verbose = TRUE,
		seed = runner_seed
	)
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
sink()

elapsed <- difftime(Sys.time(), start_time, units = "mins")
message(sprintf("✓ mfvar2 run complete in %.1f minutes", as.numeric(elapsed)))
message(sprintf("Outputs written to %s", OUT_DIR))
