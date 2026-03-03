
# =========================================================
# FUNCTIONS: lower-bound projection engine + plotting
# =========================================================

# Decide which years to use for the slope (pre-COVID years, optionally include 2024)
choose_slope_years <- function(df,
                               disease_name,
                               anchor_year,
                               threshold_log_ratio = log(1.10),  # exactly +10%
                               covid_years = covid_years) {
  
  cfg <- trend_windows[[tolower(disease_name)]]
  if (is.null(cfg)) stop("No trend window config for disease: ", disease_name)
  
  # Base slope years
  if (isTRUE(cfg$censor)) {
    trend_years <- seq(cfg$start, cfg$end)          # TB/malaria: 2010–2019
    trend_years <- setdiff(trend_years, covid_years)
  } else {
    trend_years <- seq(cfg$recent_start, cfg$recent_end)  # HIV: 2015–2024
  }
  
  # Anchor value (e.g., 2024)
  y_anchor <- df$value_scaled[df$Year == anchor_year][1]
  if (!is.finite(y_anchor)) return(trend_years)
  
  # Reference year = last year in slope window (e.g., 2019)
  ref_year <- max(trend_years, na.rm = TRUE)
  y_ref <- df$value_scaled[df$Year == ref_year][1]
  if (!is.finite(y_ref) || y_ref <= 0) return(trend_years)
  
  # signed log ratio: >0 means anchor above ref
  log_ratio_signed <- log(y_anchor / y_ref)
  
  # include anchor unless it's MORE than +10% above ref
  if (!is.na(log_ratio_signed) && log_ratio_signed <= threshold_log_ratio) {
    sort(unique(c(trend_years, anchor_year)))
  } else {
    trend_years
  }
}

# Project from the anchor using exponential decline (if beta<0) or linear increase (if beta>0)
project_recent_trend <- function(df, slope_years, anchor_year, end_year,
                                 guard_linear_if_increasing = TRUE,
                                 disease_name = NA,
                                 metric_name = NA) {
  
  d_slope <- df %>%
    dplyr::filter(Year %in% slope_years, is.finite(value_scaled)) %>%
    dplyr::mutate(value_scaled = pmax(value_scaled, .eps))
  
  if (nrow(d_slope) < 2) return(NULL)
  
  # ---- estimate slopes ----
  fit_log <- lm(log(value_scaled) ~ Year, data = d_slope)
  beta <- coef(fit_log)["Year"]; if (is.na(beta)) beta <- 0
  
  fit_lin <- lm(value_scaled ~ Year, data = d_slope)
  slope_lin <- coef(fit_lin)["Year"]; if (is.na(slope_lin)) slope_lin <- 0
  
  # ---- anchor value (e.g. 2024) ----
  y_anchor <- df %>%
    dplyr::filter(Year == anchor_year) %>%
    dplyr::pull(value_scaled) %>%
    .[1]
  
  if (!is.finite(y_anchor)) return(NULL)
  
  use_linear <- guard_linear_if_increasing && (beta > 0)
  
  if (use_linear) {
    
    intercept_anchor <- y_anchor - slope_lin * anchor_year
    
    message("SLOPE FIT | disease=", disease_name,
            " | metric=", metric_name,
            " | slope_years=", min(slope_years), "-", max(slope_years),
            " | slope=", round(slope_lin, 6),
            " | intercept_anchor=", round(intercept_anchor, 6),
            " | model=LINEAR (anchored)")
    
    pred_fun <- function(yrs) {
      y_anchor + slope_lin * (yrs - anchor_year)
    }
    
  } else {
    
    alpha_anchor <- log(y_anchor) - beta * anchor_year
    
    message("SLOPE FIT | disease=", disease_name,
            " | metric=", metric_name,
            " | slope_years=", min(slope_years), "-", max(slope_years),
            " | log-slope(beta)=", round(beta, 6),
            " | log-intercept(alpha)=", round(alpha_anchor, 6),
            " | model=EXP (anchored)")
    
    pred_fun <- function(yrs) {
      y_anchor * exp(beta * (yrs - anchor_year))
    }
  }
  
  future_years <- seq(anchor_year + 1, end_year)
  
  tibble::tibble(
    Year       = c(df$Year[df$Year <= anchor_year], future_years),
    observed   = c(df$value_scaled[df$Year <= anchor_year],
                   rep(NA_real_, length(future_years))),
    projection = c(df$value_scaled[df$Year <= anchor_year],
                   pred_fun(future_years))
  )
}

# Long format (for lower-bound engine)
make_long_rates <- function(df) {
  df %>%
    tidyr::pivot_longer(c(incidence, mortality), names_to = "metric", values_to = "value") %>%
    dplyr::mutate(
      value_scaled = value,
      scale_label = metric
    )
}

# Country-level lower bound (expects *long* data with value_scaled)
run_lower_bound_country <- function(df_long,
                                    disease_name,
                                    anchor_year, end_year,
                                    include_2024_threshold = include_2024_if_within_log_ratio,
                                    guard_linear_if_increasing = TRUE) {
  
  df_long %>%
    dplyr::group_by(ISO3, metric, scale_label) %>%
    dplyr::group_modify(~{
      d <- .x %>%
        dplyr::arrange(Year) %>%
        dplyr::mutate(
          value_scaled = pmax(value_scaled, .eps),
          value_scaled = ifelse(is.finite(value_scaled), value_scaled, NA_real_)
        ) %>%
        dplyr::filter(!is.na(value_scaled))
      
      if (nrow(d) < 3) return(tibble::tibble())
      
      slope_yrs <- choose_slope_years(
        d,
        disease_name = disease_name,
        anchor_year = anchor_year,
        threshold_log_ratio = include_2024_threshold,
        covid_years = covid_years
      )
      
      out <- project_recent_trend(
        d,
        slope_yrs,
        anchor_year,
        end_year,
        guard_linear_if_increasing = guard_linear_if_increasing,
        disease_name = disease_name,
        metric_name  = unique(.x$metric)
      )
      
      if (is.null(out)) return(tibble::tibble())
      out
    }) %>%
    dplyr::ungroup()
}

# ---- Helper function to plot one country's incidence + mortality ----
# ---- Helper function to plot one country's metrics (cases/deaths/deathshivneg/etc.) ----
plot_country <- function(country_code, data, first_year_data, end_year) {
  df_c <- data %>% dplyr::filter(ISO3 == country_code)
  
  if (nrow(df_c) == 0) {
    message("No data for ", country_code)
    return(NULL)
  }
  
  metrics <- unique(df_c$metric)
  
  make_panel <- function(m) {
    df_panel <- df_c %>% dplyr::filter(metric == m) %>% dplyr::arrange(Year)
    if (nrow(df_panel) == 0) return(NULL)
    
    # safer y label
    y_lab <- df_panel$scale_label[which(!is.na(df_panel$scale_label))[1]]
    if (is.na(y_lab) || length(y_lab) == 0) y_lab <- m
    
    ggplot2::ggplot(df_panel, ggplot2::aes(x = Year, y = projection)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(y = observed), size = 1.5) +
      ggplot2::labs(
        title = paste0(country_code, " – ", unique(df_c$disease), " – ", m),
        x = "Year",
        y = y_lab
      ) +
      ggplot2::scale_x_continuous(breaks = seq(first_year_data, end_year, by = 1)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  
  plots <- lapply(metrics, make_panel)
  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(NULL)
  
  patchwork::wrap_plots(plots, ncol = min(2, length(plots)))
}


# ===================== PORTFOLIO (AGGREGATED) LOWER-BOUND =====================

# 1) Build aggregated long series for a disease (sum numerators/denominators, then rate)
build_agg_long <- function(df_wide, disease_name, end_year_data) {
  if (disease_name == "hiv") {
    agg <- df_wide %>%
      dplyr::filter(Year <= end_year_data) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        cases  = sum(hiv_cases_n_pip, na.rm = TRUE),
        deaths = sum(hiv_deaths_n_pip, na.rm = TRUE),
        hivneg = sum(HIVneg, na.rm = TRUE),
        pop    = sum(Population_n_pip, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::transmute(
        Year,
        incidence = dplyr::if_else(hivneg > 0, cases / hivneg, NA_real_),
        mortality = dplyr::if_else(pop    > 0, deaths / pop,   NA_real_)
      )
  } else if (disease_name == "tb") {
    # Chose the TB deaths 
    agg <- df_wide %>%
      dplyr::filter(Year <= end_year_data) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        cases  = sum(tb_cases_n_pip, na.rm = TRUE),
        deaths = sum(.data[[tb_deaths_portfolio]], na.rm = TRUE),
        pop    = sum(tb_pop_n_pip, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::transmute(
        Year,
        incidence = dplyr::if_else(pop > 0, cases / pop, NA_real_),
        mortality = dplyr::if_else(pop > 0, deaths / pop, NA_real_)
      )
  } else if (disease_name == "malaria") {
    agg <- df_wide %>%
      dplyr::filter(Year <= end_year_data) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        cases  = sum(malaria_cases_n_pip, na.rm = TRUE),
        deaths = sum(malaria_deaths_n_pip, na.rm = TRUE),
        par    = sum(malaria_par_n_pip,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::transmute(
        Year,
        incidence = dplyr::if_else(par > 0, cases / par,  NA_real_),
        mortality = dplyr::if_else(par > 0, deaths / par, NA_real_)
      )
  } else {
    stop("Unknown disease: ", disease_name)
  }
  
  # Long; add dummy 'country' to reuse group_modify()
  agg_long <- agg %>%
    tidyr::pivot_longer(c(incidence, mortality), names_to = "metric", values_to = "value") %>%
    dplyr::mutate(
      value_scaled = value,
      scale_label  = metric,
      country = disease_name
    )
  
  return(agg_long)
}

# 2) Apply lower-bound logic to the aggregated long series
run_lower_bound_portfolio <- function(df_wide, disease_name,
                                      anchor_year, end_year,
                                      include_2024_threshold = include_2024_if_within_log_ratio,
                                      guard_linear_if_increasing = TRUE) {
  
  agg_long <- build_agg_long(df_wide, disease_name, end_year_data)
  
  agg_long %>%
    dplyr::group_by(country, metric, scale_label) %>%
    dplyr::group_modify(~{
      d <- .x %>%
        dplyr::arrange(Year) %>%
        dplyr::mutate(
          value_scaled = pmax(value_scaled, .eps),
          value_scaled = dplyr::if_else(is.finite(value_scaled), value_scaled, NA_real_)
        ) %>%
        dplyr::filter(!is.na(value_scaled))
      
      if (nrow(d) < 3) {
        message("Portfolio: insufficient valid data for ", disease_name, " / ", unique(.x$metric))
        return(tibble::tibble())
      }
      
      slope_yrs <- choose_slope_years(
        d,
        disease_name = disease_name,
        anchor_year = anchor_year,
        threshold_log_ratio = include_2024_threshold,
        covid_years = covid_years
      )
      
      out <- project_recent_trend(
        d,
        slope_yrs,
        anchor_year,
        end_year,
        guard_linear_if_increasing = guard_linear_if_increasing,
      )
      
      if (is.null(out)) return(tibble::tibble())
      out
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(disease = disease_name)
}

# ==== PORTFOLIO-LEVEL PLOTS ====
plot_portfolio <- function(df, disease_name, start_year, end_year) {
  ggplot(df, aes(x = Year, y = projection)) +
    geom_line(color = "blue", linewidth = 1.2) +
    geom_point(aes(y = observed), color = "black", size = 1.5) +
    facet_wrap(~ metric, scales = "free_y") +
    labs(
      title = paste0(toupper(disease_name), " – Portfolio projection to ", end_year),
      subtitle = "Observed (black) vs. projected (blue)",
      x = "Year",
      y = unique(df$scale_label)
    ) +
    scale_x_continuous(breaks = seq(start_year, end_year, by = 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

