library(tidyverse)
library(here)

setwd("~/Desktop/thesis/1_datacleaning_nibrs")

ip_core_combined <- readRDS("data/ip_core_combined.rds")

ASYX_CODE <- c("(850) Asphyxiation", "Asphyxiation")
IPH_CODE  <- c("(091) Murder/Nonnegligent Manslaughter", "Murder/Nonnegligent Manslaughter")
AGG_ASSAULT_CODE <- c("(131) Aggravated Assault", "Aggravated Assault")

POP_COLS <- paste0("pop", 1:5)

make_agency_pop <- function(df, pop_cols = POP_COLS) {
  df |>
    mutate(
      agency_pop_total = rowSums(across(all_of(pop_cols)), na.rm = TRUE)
    ) |>
    filter(is.finite(agency_pop_total), agency_pop_total > 0)
}

is_strangulation <- function(df) {
  df$weapon1 %in% ASYX_CODE | df$weapon2 %in% ASYX_CODE | df$weapon3 %in% ASYX_CODE
}

ip_core_combined <- ip_core_combined |>
  mutate(
    num_arrestees = suppressWarnings(as.numeric(num_arrestees)),
    arrest_flag = if_else(!is.na(num_arrestees) & num_arrestees > 0, 1L, 0L)
  ) |>
  make_agency_pop()



# Agency-year aggregation ------------------------------------------------------

agency_year_df <- ip_core_combined |>
  group_by(state, ori, year) |>
  summarise(
    dv_total = n(),
    
    dv_agg_assault = sum(offense1 %in% AGG_ASSAULT_CODE, na.rm = TRUE),
    dv_strangulation = sum(is_strangulation(cur_data_all()), na.rm = TRUE),
    dv_homicide = sum(offense1 %in% IPH_CODE, na.rm = TRUE),
    
    # arrests (all DV)
    arrested_incidents = sum(arrest_flag, na.rm = TRUE),
    
    # arrests among aggravated assault only
    arrested_agg_assault = sum(
      arrest_flag * (offense1 %in% AGG_ASSAULT_CODE),
      na.rm = TRUE
    ),
    
    # arrests among strangulation only
    arrested_strangulation = sum(
      arrest_flag * is_strangulation(cur_data_all()),
      na.rm = TRUE
    ),
    
    population = max(agency_pop_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(is.finite(population), population > 0) |>
  mutate(
    dv_rate = 1e5 * dv_total / population,
    dv_agg_assault_rate = 1e5 * dv_agg_assault / population,
    strang_rate = 1e5 * dv_strangulation / population,
    homicide_rate = 1e5 * dv_homicide / population,
    
    arrest_share = arrested_incidents / dv_total,
    
    dv_agg_arrest_share = if_else(
      dv_agg_assault > 0,
      arrested_agg_assault / dv_agg_assault,
      NA_real_
    ),
    
    arrest_share_strang = if_else(
      dv_strangulation > 0,
      arrested_strangulation / dv_strangulation,
      NA_real_
    )
  )

# State-year aggregation -------------------------------------------------------

state_year_df <- agency_year_df |>
  group_by(state, year) |>
  summarise(
    dv_total = sum(dv_total, na.rm = TRUE),
    dv_agg_assault = sum(dv_agg_assault, na.rm = TRUE),
    dv_strangulation = sum(dv_strangulation, na.rm = TRUE),
    dv_homicide = sum(dv_homicide, na.rm = TRUE),
    
    arrested_incidents = sum(arrested_incidents, na.rm = TRUE),
    arrested_agg_assault = sum(arrested_agg_assault, na.rm = TRUE),
    arrested_strangulation = sum(arrested_strangulation, na.rm = TRUE),
    
    population = sum(population, na.rm = TRUE),
    agencies = n_distinct(ori),
    .groups = "drop"
  ) |>
  mutate(
    dv_rate = 1e5 * dv_total / population,
    dv_agg_assault_rate = 1e5 * dv_agg_assault / population,
    strang_rate = 1e5 * dv_strangulation / population,
    homicide_rate = 1e5 * dv_homicide / population,
    
    arrest_share = arrested_incidents / dv_total,
    
    dv_agg_arrest_share = if_else(
      dv_agg_assault > 0,
      arrested_agg_assault / dv_agg_assault,
      NA_real_
    ),
    
    arrest_share_strang = if_else(
      dv_strangulation > 0,
      arrested_strangulation / dv_strangulation,
      NA_real_
    )
  ) |>
  mutate(
    state = str_extract(as.character(state), "[A-Z]{2}$")
  ) |>
  arrange(state, year)


saveRDS(state_year_df, "data/dv_stateyear_rates.rds")

## doing the same for male-male assault------
assault_control_combined <- readRDS("data/assault_control_combined.rds")


assault_control_combined <- assault_control_combined |>
  mutate(
    num_arrestees = suppressWarnings(as.numeric(num_arrestees)),
    arrest_flag = if_else(!is.na(num_arrestees) & num_arrestees > 0, 1L, 0L)
  ) |>
  make_agency_pop()

male_assault_agency_year <- assault_control_combined |>
  group_by(state, ori, year) |>
  summarise(
    assault_total = n(),
    
    agg_assault = sum(offense1 %in% AGG_ASSAULT_CODE, na.rm = TRUE),
    
    male_homicide = sum(offense1 %in% IPH_CODE, na.rm = TRUE),
    
    arrested_incidents = sum(arrest_flag, na.rm = TRUE),
    
    population = max(agency_pop_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(is.finite(population), population > 0) |>
  mutate(
    assault_rate = 1e5 * assault_total / population,
    agg_assault_rate = 1e5 * agg_assault / population,
    male_homicide_rate = 1e5 * male_homicide / population,
    assault_arrest_rate = arrested_incidents / assault_total
  )

# State-year aggregation -------------------------------------------------------

male_violence_state_year_df <- male_assault_agency_year |>
  group_by(state, year) |>
  summarise(
    assault_total = sum(assault_total, na.rm = TRUE),
    agg_assault = sum(agg_assault, na.rm = TRUE),
    male_homicide = sum(male_homicide, na.rm = TRUE),
    arrested_incidents = sum(arrested_incidents, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    agencies = n_distinct(ori),
    .groups = "drop"
  ) |>
  mutate(
    assault_rate = 1e5 * assault_total / population,
    agg_assault_rate = 1e5 * agg_assault / population,
    male_homicide_rate = 1e5 * male_homicide / population,
    assault_arrest_rate = arrested_incidents / assault_total
  ) |>
  mutate(
    state = str_extract(as.character(state), "[A-Z]{2}$")
  ) |>
  arrange(state, year)

saveRDS(male_violence_state_year_df, "data/maleviolence_stateyear_rates.rds")
