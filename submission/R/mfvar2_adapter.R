# Helpers to adapt main-branch data structures to the mfvar2 package
# ------------------------------------------------------------------
# These utilities bridge the existing data pipeline (quarterly targets
# stored under data/processed/) with the custom mfvar2 workflow. The
# goal is to reuse the cleaned/filtered inputs already produced by the
# main branch without copying raw data from other branches.

prepare_mfvar2_input <- function(
  data_dir = file.path(".", "data"),
  monthly_vars = resolve_monthly_indicators(),
  quarterly_vars = target_variables,
  start_date = NULL,
  end_date = NULL,
  difference = FALSE,
  verbose = TRUE
) {
  if (!requireNamespace("mfvar2", quietly = TRUE)) {
    stop("The mfvar2 package must be available (loaded via pkgload::load_all).")
  }

  qdat <- read_quarterly_data(data_dir)
  if (!is.null(start_date)) {
    start_qtr <- zoo::as.yearqtr(start_date)
    qdat <- dplyr::filter(qdat, .data$qtr >= start_qtr)
  }
  if (!is.null(end_date)) {
    end_qtr <- zoo::as.yearqtr(end_date)
    qdat <- dplyr::filter(qdat, .data$qtr <= end_qtr)
  }
  if (!nrow(qdat)) {
    stop("Quarterly sample is empty after applying date filters.")
  }

  monthly <- read_combined_timeseries(data_dir, variables = monthly_vars)
  monthly_df <- monthly$data |> dplyr::arrange(date)

  if (!is.null(start_date)) {
    monthly_df <- dplyr::filter(monthly_df, date >= as.Date(start_date))
  }
  if (!is.null(end_date)) {
    monthly_df <- dplyr::filter(monthly_df, date <= as.Date(end_date))
  }

  if (!nrow(monthly_df)) {
    stop("Monthly combined dataset is empty after filtering – cannot build mfvar2 input.")
  }

  # Apply publication lags to mimic the availability assumptions used elsewhere.
  if (length(monthly_publication_lags)) {
    for (var in names(monthly_publication_lags)) {
      lag_months <- monthly_publication_lags[[var]]
      if (!is.null(monthly_df[[var]]) && lag_months > 0) {
        monthly_df[[var]] <- dplyr::lag(monthly_df[[var]], n = lag_months)
      }
    }
  }

  q_monthly <- qdat |>
    dplyr::mutate(
      date = zoo::as.Date(zoo::as.yearmon(qtr) + 2 / 12)
    ) |>
    dplyr::select(
      date,
      dplyr::all_of(intersect(quarterly_vars, names(qdat)))
    )

  # Align on the overlapping window so mfvar2 sees a single monthly grid.
  overlap_start <- max(min(monthly_df$date, na.rm = TRUE), min(q_monthly$date, na.rm = TRUE))
  overlap_end <- min(max(monthly_df$date, na.rm = TRUE), max(q_monthly$date, na.rm = TRUE))
  combined <- monthly_df |>
    dplyr::filter(date >= overlap_start, date <= overlap_end) |>
    dplyr::left_join(q_monthly, by = "date") |>
    dplyr::arrange(date)

  if (!nrow(combined)) {
    stop("No overlapping period between monthly indicators and quarterly targets for mfvar2.")
  }

  flow_vars <- intersect(c("gdp_growth", "inflation"), quarterly_vars)

  data_prep <- mfvar2::prepare_data_snb(
    data_sources = combined,
    quarterly_vars = quarterly_vars,
    flow_vars = flow_vars,
    start_date = start_date,
    end_date = end_date,
    difference = difference,
    verbose = verbose
  )

  list(
    prepared = data_prep,
    monthly_vars = monthly_vars,
    quarterly_vars = quarterly_vars,
    sample_start = min(combined$date, na.rm = TRUE),
    sample_end = max(combined$date, na.rm = TRUE)
  )
}
