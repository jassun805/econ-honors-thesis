library(tidyverse)
library(here)
library(stringr)
library(fixest)
library(dplyr)

setwd("~/Desktop/thesis/4_descriptive_figures")

# read in data
nibrs_ip <- readRDS("data/ip_core_combined.rds")
shr_iph <- read_csv("data/ip_shr.csv")

ASYX_CODE <- c("(850) Asphyxiation", "Asphyxiation")
IPH_CODE  <- c("(091) Murder/Nonnegligent Manslaughter", "Murder/Nonnegligent Manslaughter")

POP_COLS <- paste0("pop", 1:5)

make_agency_pop <- function(df, pop_cols = POP_COLS) {
  df |>
    mutate(
      agency_pop_total = rowSums(across(all_of(pop_cols)), na.rm = TRUE)
    ) |>
    filter(is.finite(agency_pop_total), agency_pop_total > 0)
}

is_strangulation <- function(df) {
  df$weapon1 %in% ASYX_CODE |
    df$weapon2 %in% ASYX_CODE |
    df$weapon3 %in% ASYX_CODE
}

is_homicide <- function(df) {
  df$offense1 %in% IPH_CODE
}

## getting counts -----

nibrs_ip |> filter(year == 2023) |> count()

nibrs_ip |> filter(year == 2023) |> 
  filter(weapon1 %in% ASYX_CODE | weapon2 %in% ASYX_CODE | weapon3 %in% ASYX_CODE) |>
  count()

nibrs_ip |> filter(year == 2023) |>
  filter(offense1 %in% IPH_CODE) |> count()

## getting rates per 100,0000

# -------------------------------
# agency-year counts
# -------------------------------
nibrs_ip <- nibrs_ip |>
  mutate(
    agency_pop_total = rowSums(across(all_of(POP_COLS)), na.rm = TRUE),
    strang_flag = weapon1 %in% ASYX_CODE |
      weapon2 %in% ASYX_CODE |
      weapon3 %in% ASYX_CODE,
    iph_flag = offense1 %in% IPH_CODE
  ) |>
  filter(year == 2023)

agency_year_df <- nibrs_ip |>
  group_by(state, ori) |>
  summarise(
    ipv_total = n(),
    ipv_strangulation = sum(strang_flag, na.rm = TRUE),
    iph_total = sum(iph_flag, na.rm = TRUE),
    population = max(agency_pop_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(is.finite(population), population > 0)

# -------------------------------
# national-year counts + rates
# -------------------------------
national_year_df <- agency_year_df |>
  summarise(
    ipv_total = sum(ipv_total, na.rm = TRUE),
    ipv_strangulation = sum(ipv_strangulation, na.rm = TRUE),
    iph_total = sum(iph_total, na.rm = TRUE),
    nibrs_population = sum(population, na.rm = TRUE),
    agencies = n_distinct(ori)
  ) |>
  mutate(
    ipv_rate = 1e5 * ipv_total / nibrs_population,
    strangulation_rate = 1e5 * ipv_strangulation / nibrs_population,
    iph_rate = 1e5 * iph_total / nibrs_population
  )

national_year_df


## getting female victim and male victim percentages


nibrs_ip |>
  filter(!is.na(victim_sex)) |>
         filter(
           offense1 %in% IPH_CODE
         ) |>
  summarise(
    n = n(),
    n_male = sum(victim_sex == "(1) Male"),
    pct_male = n_male / n,
    n_female = sum(victim_sex == "(0) Female"),
    pct_female = n_female / n
  )


## getting relations

nibrs_ip |>
  filter(weapon1 %in% ASYX_CODE |weapon2 %in% ASYX_CODE | weapon3 %in% ASYX_CODE) |>
  count(vic_to_off_relationship) |>
  mutate(pct = n / sum(n))



nibrs_ip |>
  filter(offense1 %in% IPH_CODE) |>
  count(vic_to_off_relationship) |>
  mutate(pct = n / sum(n))

## mean age ------
nibrs_ip |>
  filter(!is.na(victim_age)) |>
  filter(offense1 %in% IPH_CODE) |>
  summarise(
    mean_age = mean(victim_age),
    pct_18_34 = mean(victim_age >= 18 & victim_age <= 34),
    pct_35_44 = mean(victim_age >= 35 & victim_age <= 44),
    pct_45_plus = mean(victim_age >= 45)
  )

nibrs_ip |>
  filter(!is.na(offender_age)) |>
  filter(offense1 %in% IPH_CODE) |>
  summarise(
    mean_age = mean(offender_age),
    pct_18_34 = mean(offender_age >= 18 & offender_age <= 34),
    pct_35_44 = mean(offender_age >= 35 & offender_age <= 44),
    pct_45_plus = mean(offender_age >= 45)
  )

## race and ethnicity-----
nibrs_ip |> count(offender_race, sort = TRUE)
nibrs_ip |> count(offender_ethnicity, sort = TRUE)


offender_race_wide <- nibrs_ip |>
  filter(offense1 %in% IPH_CODE) |>
  mutate(
      offender_race_cat = case_when(
        # Hispanic first
        offender_ethnicity == "(1) Hispanic or Latino" ~ "Hispanic",
        
        # Treat NA as non-Hispanic
        offender_race == "(1) White" &
          ((offender_ethnicity != "(1) Hispanic or Latino") | is.na(offender_ethnicity)) ~ "Non-Hispanic White",
        
        offender_race == "(2) Black or African American" &
          ((offender_ethnicity != "(1) Hispanic or Latino") | is.na(offender_ethnicity)) ~ "Non-Hispanic Black",
        
        offender_race %in% c("(4) Asian", "(5) Native Hawaiian or Other Pacific Islander") &
          ((offender_ethnicity != "(1) Hispanic or Latino") | is.na(offender_ethnicity)) ~ "Asian / Pacific Islander",
        
        offender_race == "(3) American Indian or Alaska Native" &
          ((offender_ethnicity != "(1) Hispanic or Latino") | is.na(offender_ethnicity)) ~ "American Indian / Alaska Native",
        
        TRUE ~ "Other / multiracial / unknown"
      )
    ) |>
  summarise(
    pct_off_white_nh = 100 * mean(offender_race_cat == "Non-Hispanic White"),
    pct_off_black_nh = 100 * mean(offender_race_cat == "Non-Hispanic Black"),
    pct_off_hispanic = 100 * mean(offender_race_cat == "Hispanic"),
    pct_off_api = 100 * mean(offender_race_cat == "Asian / Pacific Islander"),
    pct_off_aian = 100 * mean(offender_race_cat == "American Indian / Alaska Native"),
    pct_off_other = 100 * mean(offender_race_cat == "Other / multiracial / unknown")
  )


victim_race_wide <- nibrs_ip |>
  filter(offense1 %in% IPH_CODE) |>
  mutate(
    victim_race_cat = case_when(
      # Hispanic first
      victim_ethnicity == "(1) Hispanic or Latino" ~ "Hispanic",
      
      # Treat NA as non-Hispanic
      victim_race == "(1) White" &
        ((victim_ethnicity != "(1) Hispanic or Latino") | is.na(victim_ethnicity)) ~ "Non-Hispanic White",
      
      victim_race == "(2) Black or African American" &
        ((victim_ethnicity != "(1) Hispanic or Latino") | is.na(victim_ethnicity)) ~ "Non-Hispanic Black",
      
      victim_race %in% c("(4) Asian", "(5) Native Hawaiian or Other Pacific Islander") &
        ((victim_ethnicity != "(1) Hispanic or Latino") | is.na(victim_ethnicity)) ~ "Asian / Pacific Islander",
      
      victim_race == "(3) American Indian or Alaska Native" &
        ((victim_ethnicity != "(1) Hispanic or Latino") | is.na(victim_ethnicity)) ~ "American Indian / Alaska Native",
      
      TRUE ~ "Other / multiracial / unknown"
    )
  ) |>
  summarise(
    pct_vic_white_nh = 100 * mean(victim_race_cat == "Non-Hispanic White"),
    pct_vic_black_nh = 100 * mean(victim_race_cat == "Non-Hispanic Black"),
    pct_vic_hispanic = 100 * mean(victim_race_cat == "Hispanic"),
    pct_vic_api = 100 * mean(victim_race_cat == "Asian / Pacific Islander"),
    pct_vic_aian = 100 * mean(victim_race_cat == "American Indian / Alaska Native"),
    pct_vic_other = 100 * mean(victim_race_cat == "Other / multiracial / unknown")
  )
