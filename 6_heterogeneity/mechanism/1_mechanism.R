library(tidyverse)
library(here)
library(stringr)
library(fixest)
library(purrr)
library(modelsummary)
library(boot)

setwd("~/Desktop/thesis")

iph_race_data <- read_csv("2_datacleaning_ucr-shr/data/shr_stateyear_race.csv") |> 
  filter(year >=2000)

all_iph_data <- iph_race_data |>
  filter(race_cat == "All") |>
  rename(covered_population = population)

violence_data_controls <- read_csv("3_baselineregression/data/violence_laws_with_controls.csv") |>
  select(-ip_homicide, -mf_ip_homicide, -fm_ip_homicide, -male_homicide, 
         -iph_rate, -mf_iph_rate, -fm_iph_rate, -malemale_homicide_rate, -shr_agencies)

all_data <- all_iph_data |> left_join(violence_data_controls, by = c("state", "year")) |>
  filter(nibrs_agencies > 0) |>
  mutate(
    log_iph = log(ip_homicide_rate + 0.1),
    log_strang = log(strang_rate + 0.1)
  )

all_data <- all_data |>
  group_by(state) |>
  mutate(
    adopt_year = suppressWarnings(min(year[strang_law == 1], na.rm = TRUE)),
    adopt_year = ifelse(is.infinite(adopt_year), NA, adopt_year),
    event_time = year - adopt_year
  ) |>
  ungroup() |>
  mutate( coverage_share = nibrs_population / census_population) 


delta_reporting <- all_data |>
  filter(!is.na(adopt_year)) |>
  mutate(
    period = case_when(
      event_time >= -3 & event_time <= -1 ~ "pre",
      event_time >= 1  & event_time <= 2  ~ "post",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(period)) |>
  group_by(state, period) |>
  summarize(
    avg_log_strang = weighted.mean(
      log_strang,
      w = nibrs_population,
      na.rm = TRUE
    ),
    n_years = sum(!is.na(log_strang)),
    avg_coverage = weighted.mean(
      coverage_share,
      w = nibrs_population,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  filter(n_years >= 1) |>
  select(state, period, avg_log_strang, avg_coverage) |>
  pivot_wider(
    names_from = period,
    values_from = c(avg_log_strang, avg_coverage)
  ) |>
  filter(!is.na(avg_log_strang_pre), !is.na(avg_log_strang_post)) |>
  mutate(
    delta_report = avg_log_strang_post - avg_log_strang_pre,
    z_delta_report = as.numeric(scale(delta_report)),
    high_delta_report = z_delta_report > 0
  )


all_data_mech <- all_data |>
  left_join(delta_reporting, by = "state") |>
  group_by(state) |>
  arrange(year) |>
  mutate(
    log_iph_lead1 = lead(log_iph, 1),
    log_iph_lead2 = lead(log_iph, 2)
  ) |>
  ungroup() |>
  group_by(year) |>
  mutate(high_male_homicide = male_male_nonip_homicide_rate > 
           median(male_male_nonip_homicide_rate, na.rm = TRUE)
  ) |>
  ungroup() 

# ------------------------------------------------------------
# Mechanism regressions
# ------------------------------------------------------------

m1 <- feols(
  log_iph ~ strang_law * z_delta_report |
    state + year,
  data = all_data_mech,
  cluster = ~state,
  weights = ~census_population
)

m2 <- feols(
  log_iph ~ strang_law * z_delta_report +
    poverty_rate + unemp_total + unemployment_gap |
    state + year,
  data = all_data_mech,
  cluster = ~state,
  weights = ~census_population
)

m3 <- feols(
  log_iph ~ strang_law * z_delta_report +
    high_male_homicide |
    state + year,
  data = all_data_mech,
  cluster = ~state,
  weights = ~census_population
)

feols(
  log_iph ~ strang_law * z_delta_report +
    poverty_rate + unemp_total + unemployment_gap + high_male_homicide |
    state + year,
  data = all_data_mech,
  cluster = ~state,
  weights = ~census_population
)

# ------------------------------------------------------------
# Table
# ------------------------------------------------------------

modelsummary(
  list(
    "(1)" = m1,
    "(2)" = m2,
    "(3)" = m3,
    "(4)" = m4
  ),
  coef_map = c(
    "strang_law" = "Strangulation law",
    "z_delta_report" = "$z_{\\Delta Report,s}$",
    "strang_law:z_delta_report" = "Strangulation law $\\times z_{\\Delta Report,s}$",
    "poverty_rate" = "Poverty rate",
    "unemp_total" = "Total unemployment rate",
    "unemployment_gap" = "Male-female unemployment gap",
    "high_male_homicideTRUE" = "High male non-IP homicide"
  ),
  stars = c("*" = .10, "**" = .05, "***" = .01),
  gof_omit = "IC|Log|Adj|Within|RMSE",
  add_rows = tibble::tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
    "State fixed effects", "Yes", "Yes", "Yes", "Yes",
    "Year fixed effects", "Yes", "Yes", "Yes", "Yes",
    "Economic controls", "No", "Yes", "No", "Yes",
    "High violence control", "No", "No", "Yes", "Yes"
  ),
  notes = "The dependent variable is the log intimate partner homicide rate. All models are weighted by state census population and include state and year fixed effects. Standard errors are clustered by state. The variable $z_{\\Delta Report,s}$ is the standardized change in reported strangulation around adoption.",
  output = "latex"
)

