# Latent state extraction utilities for MF-VAR models

extract_latent_states <- function(model, summary = c("mean", "median")) {
  if (missing(model) || is.null(model)) {
    stop("'model' must be a fitted mfbvar object returned by estimate_mfvar_model().")
  }

  summary <- match.arg(summary)
  Z <- model[["Z"]]
  if (is.null(Z)) {
    stop("Supplied model does not contain state draws (slot 'Z').")
  }

  time_names <- dimnames(Z)[[1]]
  var_names <- dimnames(Z)[[2]]
  if (is.null(time_names) || is.null(var_names)) {
    stop("State draws are missing dimension names; cannot construct time series output.")
  }

  collapse_fun <- switch(
    summary,
    mean = function(x) mean(x, na.rm = TRUE),
    median = function(x) stats::median(x, na.rm = TRUE)
  )

  collapsed <- apply(Z, c(1, 2), collapse_fun)
  collapsed <- as.matrix(collapsed)

  states_df <- tibble::tibble(date = as.Date(time_names))
  for (var in var_names) {
    states_df[[var]] <- collapsed[, var]
  }

  states_df
}

save_latent_states_csv <- function(states_df, out_dir, filename = "mfvar_latent_states.csv") {
  if (!"date" %in% names(states_df)) {
    stop("states_df must contain a 'date' column.")
  }
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  path <- file.path(out_dir, filename)
  readr::write_csv(states_df, path)
  path
}

prepare_latent_actual_plot_data <- function(states_df,
                                            qdat_orig,
                                            target_variables,
                                            transforms) {
  if (!"date" %in% names(states_df)) {
    stop("states_df must contain a 'date' column for plotting.")
  }

  required_cols <- c("qtr", target_variables)
  missing_cols <- setdiff(required_cols, names(qdat_orig))
  if (length(missing_cols)) {
    stop("qdat_orig missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  qdat_with_date <- qdat_orig
  qtr_yearqtr <- zoo::as.yearqtr(qdat_orig$qtr)
  qdat_with_date$date <- zoo::as.Date(qtr_yearqtr, frac = 1)

  qdat_for_plot <- qdat_with_date |>
    dplyr::select(date, tidyselect::all_of(target_variables))

  if (!is.null(transforms)) {
    for (var in target_variables) {
      if (var %in% names(qdat_for_plot) && !is.null(transforms[[var]])) {
        n_obs <- nrow(qdat_for_plot)
        indices <- seq_len(n_obs)
        trans <- transforms[[var]]

        trend_component <- trans$intercept + trans$slope * indices
        qdat_for_plot[[var]] <- qdat_for_plot[[var]] - trend_component

        if (!is.null(trans$has_seasonal) && isTRUE(trans$has_seasonal) && length(trans$seasonal)) {
          season_ids <- ((indices - 1L) %% trans$frequency) + 1L
          seasonal_component <- trans$seasonal[season_ids]
          qdat_for_plot[[var]] <- qdat_for_plot[[var]] - seasonal_component
        }
      }
    }
  }

  available_states <- intersect(target_variables, setdiff(names(states_df), "date"))
  if (!length(available_states)) {
    stop(
      "No target variables found in latent states. Available states: ",
      paste(setdiff(names(states_df), "date"), collapse = ", ")
    )
  }

  states_for_plot <- states_df |>
    dplyr::mutate(date = as.Date(.data$date)) |>
    dplyr::select(date, tidyselect::all_of(available_states))

  actuals_long <- qdat_for_plot |>
    dplyr::select(date, tidyselect::all_of(available_states)) |>
    tidyr::pivot_longer(-date, names_to = "variable", values_to = "value") |>
    dplyr::mutate(type = "Actual (Quarterly)")

  latent_long <- states_for_plot |>
    tidyr::pivot_longer(-date, names_to = "variable", values_to = "value") |>
    dplyr::mutate(type = "Latent State (MF-VAR)")

  label_map <- c(
    gdp_growth = "GDP Growth (%, detrended)",
    inflation = "Inflation (%, detrended)",
    exch_rate = "Exchange Rate (log CHF/EUR, detrended)"
  )

  list(
    actuals_long = actuals_long,
    latent_long = latent_long,
    available_states = available_states,
    label_map = label_map
  )
}

plot_latent_states <- function(states_df, out_dir, states = NULL, mode = c("facet", "heatmap"), filename = NULL) {
  if (!"date" %in% names(states_df)) {
    stop("states_df must contain a 'date' column for plotting.")
  }

  mode <- match.arg(mode)
  state_cols <- setdiff(names(states_df), "date")
  if (is.null(states)) {
    states <- state_cols
  } else {
    missing_states <- setdiff(states, state_cols)
    if (length(missing_states)) {
      stop("Requested states not present: ", paste(missing_states, collapse = ", "))
    }
  }

  states_long <- states_df |>
    dplyr::select(date, tidyselect::all_of(states)) |>
    tidyr::pivot_longer(-date, names_to = "state", values_to = "value")

  if (identical(mode, "facet")) {
    p <- ggplot2::ggplot(states_long, ggplot2::aes(x = date, y = value)) +
      ggplot2::geom_line(color = "#1b9e77") +
      ggplot2::facet_wrap(~ state, scales = "free_y", ncol = 2) +
      ggplot2::labs(
        title = "MF-VAR latent states (posterior mean)",
        x = "Date",
        y = "State value"
      ) +
      ggplot2::theme_minimal(base_size = 12)

    default_name <- "mfvar_latent_states_timeseries.png"
  } else {
    states_heatmap <- states_long |>
      dplyr::group_by(state) |>
      dplyr::mutate(
        value_mean = mean(value, na.rm = TRUE),
        value_sd = stats::sd(value, na.rm = TRUE),
        value = dplyr::if_else(value_sd > 0, (value - value_mean) / value_sd, value - value_mean)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-value_mean, -value_sd)

    p <- ggplot2::ggplot(states_heatmap, ggplot2::aes(x = date, y = state, fill = value)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c(option = "C") +
      ggplot2::labs(
        title = "MF-VAR latent states (heatmap)",
        x = "Date",
        y = "State",
        fill = "Z-score"
      ) +
      ggplot2::theme_minimal(base_size = 12)

    default_name <- "mfvar_latent_states_heatmap.png"
  }

  if (is.null(filename)) {
    filename <- default_name
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  plot_path <- file.path(out_dir, filename)
  ggplot2::ggsave(plot_path, plot = p, width = 8, height = if (identical(mode, "facet")) 6 else 4.5, dpi = 150)
  plot_path
}

plot_latent_states_with_actuals <- function(states_df, qdat_orig, out_dir, 
                                             target_variables = c("gdp_growth", "inflation", "exch_rate"),
                                             transforms = NULL,
                                             filename = NULL) {
  prep <- prepare_latent_actual_plot_data(states_df, qdat_orig, target_variables, transforms)

  combined <- dplyr::bind_rows(prep$actuals_long, prep$latent_long) |>
    dplyr::mutate(
      variable_label = dplyr::recode(.data$variable, !!!prep$label_map, .default = .data$variable)
    )

  p <- ggplot2::ggplot(combined, ggplot2::aes(x = date, y = value, color = type, linetype = type)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::facet_wrap(~ variable_label, scales = "free_y", ncol = 1) +
    ggplot2::scale_color_manual(
      values = c("Actual (Quarterly)" = "#d95f02", "Latent State (MF-VAR)" = "#1b9e77")
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Actual (Quarterly)" = "solid", "Latent State (MF-VAR)" = "dashed")
    ) +
    ggplot2::labs(
      title = "MF-VAR Latent States vs. Actual Quarterly Variables",
      subtitle = "Both series shown in detrended space (deterministic trend + seasonal removed)",
      x = "Date",
      y = "Detrended Value",
      color = NULL,
      linetype = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (is.null(filename)) {
    filename <- "mfvar_latent_vs_actual.png"
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  plot_path <- file.path(out_dir, filename)
  ggplot2::ggsave(plot_path, plot = p, width = 10, height = 8, dpi = 150)

  message("Saved latent states vs. actuals plot: ", plot_path)
  plot_path
}

plot_latent_actuals_with_kof <- function(states_df,
                                         qdat_orig,
                                         kof_ts,
                                         out_dir,
                                         target_variables = c("gdp_growth", "inflation", "exch_rate"),
                                         transforms = NULL,
                                         filename = NULL) {
  if (is.null(kof_ts) || !inherits(kof_ts, "ts")) {
    stop("kof_ts must be a monthly 'ts' object containing the KOF Barometer.")
  }

  prep <- prepare_latent_actual_plot_data(states_df, qdat_orig, target_variables, transforms)

  kof_dates <- zoo::as.yearmon(stats::time(kof_ts))
  kof_df <- tibble::tibble(
    date = zoo::as.Date(kof_dates, frac = 1),
    value = as.numeric(kof_ts)
  ) |>
    tidyr::drop_na()

  if (!nrow(kof_df)) {
    stop("KOF Barometer series is empty after removing missing values.")
  }

  date_min <- min(c(prep$actuals_long$date, prep$latent_long$date, kof_df$date), na.rm = TRUE)
  date_max <- max(c(prep$actuals_long$date, prep$latent_long$date, kof_df$date), na.rm = TRUE)

  kof_df <- kof_df |>
    dplyr::filter(.data$date >= date_min, .data$date <= date_max)

  if (!nrow(kof_df)) {
    stop("KOF Barometer has no observations within the latent/actual sample window.")
  }

  kof_long <- tidyr::crossing(kof_df, variable = prep$available_states) |>
    dplyr::mutate(type = "KOF Barometer (Monthly)")

  combined <- dplyr::bind_rows(prep$actuals_long, prep$latent_long, kof_long) |>
    dplyr::group_by(.data$variable) |>
    dplyr::mutate(
      value_std = {
        mean_val <- mean(.data$value, na.rm = TRUE)
        sd_val <- stats::sd(.data$value, na.rm = TRUE)
        centered <- .data$value - mean_val
        ifelse(is.na(sd_val) || sd_val == 0, centered, centered / sd_val)
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      variable_label = dplyr::recode(.data$variable, !!!prep$label_map, .default = .data$variable),
      type = factor(
        .data$type,
        levels = c("Actual (Quarterly)", "Latent State (MF-VAR)", "KOF Barometer (Monthly)")
      )
    )

  actual_points <- combined |>
    dplyr::filter(.data$type == "Actual (Quarterly)")

  p <- ggplot2::ggplot(combined, ggplot2::aes(x = date, y = value_std, color = type, linetype = type)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(
      data = actual_points,
      ggplot2::aes(x = date, y = value_std, color = type),
      size = 1.8,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_wrap(~ variable_label, ncol = 1) +
    ggplot2::scale_color_manual(
      values = c(
        "Actual (Quarterly)" = "#d95f02",
        "Latent State (MF-VAR)" = "#1b9e77",
        "KOF Barometer (Monthly)" = "#7570b3"
      )
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        "Actual (Quarterly)" = "solid",
        "Latent State (MF-VAR)" = "dashed",
        "KOF Barometer (Monthly)" = "dotdash"
      )
    ) +
    ggplot2::labs(
      title = "Latent states, actuals, and KOF Barometer",
      subtitle = "Each series is standardized (z-score) within its variable to highlight co-movement; KOF is monthly",
      x = "Date",
      y = "Standardized value",
      color = NULL,
      linetype = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (is.null(filename)) {
    filename <- "mfvar_latent_actuals_kof.png"
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  plot_path <- file.path(out_dir, filename)
  ggplot2::ggsave(plot_path, plot = p, width = 10, height = 9, dpi = 150)
  message("Saved latent states + actuals + KOF plot: ", plot_path)
  plot_path
}

