library(tidyverse)
library(here)
library(stringr)
library(fixest)

setwd("~/Desktop/thesis/6_heterogeneity")

ip_incidents <- readRDS("data/ip_core_combined.rds")
county_race_pop <- read_csv("data/county_year_race.csv")


ip_incidents_2023 |> filter( homicide == TRUE) |> count(victim_sex, offender_sex)

ip_incidents_2023 <- ip_incidents |>
  filter(year == 2023)|>
  filter(victim_race %in% c("(1) White", "(2) Black or African American", "(4) Asian"))

county_race_pop_2023 <- county_race_pop |> 
  filter(year == 2023) |>
  filter(race_cat %in% c("White", "Black", "Asian_PI")) |>
  mutate(
    state = str_trim(state),
    state = str_to_upper(state),
    state = if_else(state == "NB", "NE", state)
  )

covered_counties <- ip_incidents |>
  filter(year == 2023) |>
  mutate(
    county_fips = str_pad(as.character(fipscounty1), 3, pad = "0")
  ) |>
  distinct(state, year, county_fips)

county_pop_clean <- county_race_pop_2023 |>
  mutate(
    county_fips = str_pad(as.character(county_fips), 3, pad = "0")
  )

county_pop_covered <- county_pop_clean |>
  semi_join(
    covered_counties,
    by = c("state", "county_fips")
  )

# getting covered national demographic populations 2023

covered_pop_national_race <- county_pop_covered |>
  group_by(race_cat) |>
  summarise(
    covered_population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )


## aggregating counts by race------

ASYX_CODE <- c("(850) Asphyxiation", "Asphyxiation")
IPH_CODE  <- c("(091) Murder/Nonnegligent Manslaughter", "Murder/Nonnegligent Manslaughter")
AGG_ASSAULT <- c(
  "(131) Aggravated Assault", "Aggravated Assault")


ip_incidents_2023 <- ip_incidents_2023 |>
  mutate(
    strangulation = weapon1 %in% ASYX_CODE |
      weapon2 %in% ASYX_CODE |
      weapon3 %in% ASYX_CODE,
    homicide = offense1 %in% IPH_CODE, 
    agg_assault = offense1 %in% AGG_ASSAULT,
    arrest_flag = if_else(!is.na(num_arrestees) & num_arrestees > 0, 1L, 0L)
  )

p_race_counts <- ip_incidents_2023 |>
  group_by(victim_race) |>
  summarise(
    total_incidents = n(),
    strang_cases = sum(strangulation, na.rm = TRUE),
    agg_assault_cases = sum(agg_assault, na.rm = TRUE),
    homicides = sum(homicide, na.rm = TRUE),
    arrests = sum(arrest_flag, na.rm = TRUE),
    .groups = "drop"
  )

mean_age_by_race <- ip_incidents_2023 |>
  group_by(victim_race) |>
  summarise(
    mean_age_ipv = mean(victim_age, na.rm = TRUE),
    
    mean_age_strang = mean(victim_age[strangulation], na.rm = TRUE),
    
    mean_age_agg = mean(victim_age[agg_assault], na.rm = TRUE),
    
    mean_age_iph = mean(victim_age[homicide], na.rm = TRUE),
    
    n_ipv = n(),
    n_strang = sum(strangulation, na.rm = TRUE),
    n_iph = sum(homicide, na.rm = TRUE),
    
    .groups ="drop"
  )

arrest_rates <- ip_incidents_2023 |>
  group_by(victim_race) |>
  summarise(
    ipv_arrest_rate = mean(arrest_flag, na.rm = TRUE),
    
    strang_arrest_rate = mean(arrest_flag[strangulation], na.rm = TRUE),
    
    
    agg_ipv_arrest_rate = mean(arrest_flag[agg_assault], na.rm = TRUE),

    n_agg_assault = sum(agg_assault, na.rm = TRUE),
    
    .groups ="drop"
  )
