################################################################################
# Heterogeneity panel: covered population and race-specific rates
################################################################################

library(tidyverse)
library(here)
library(stringr)
library(fixest)

setwd("~/Desktop/thesis")

# ------------------------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------------------------

ip_incidents   <- readRDS("1_datacleaning_nibrs/data/ip_core_combined.rds")
male_incidents <- readRDS("1_datacleaning_nibrs/data/assault_control_combined.rds")

setwd("~/Desktop/thesis/6_heterogeneity")

county_race_pop <- read_csv("data/county_year_race.csv")

# ------------------------------------------------------------------------------
# 2. Constants
# ------------------------------------------------------------------------------

ASYX_CODE <- c("(850) Asphyxiation", "Asphyxiation")

AGG_ASSAULT_CODES <- c(
  "(131) Aggravated Assault",
  "Aggravated Assault"
)

ASSAULT_CODES <- c(
  "(131) Aggravated Assault", "Aggravated Assault",
  "(132) Simple Assault", "Simple Assault"
)

KEEP_RACES <- c("White", "Black", "Asian_PI", "Nonwhite", "Native", "All")

# ------------------------------------------------------------------------------
# 3. Helper functions
# ------------------------------------------------------------------------------

fix_state_abbr <- function(x) {
  x <- str_trim(x)
  x <- str_to_upper(x)
  x <- if_else(x == "NB", "NE", x)
  x
}

expand_race_groups <- function(df) {
  bind_rows(
    df %>%
      filter(victim_race %in% c("(1) White", "White")) %>%
      mutate(victim_race_cat = "White"),
    
    df %>%
      filter(victim_race %in% c("(2) Black or African American", "Black or African American", "Black")) %>%
      mutate(victim_race_cat = "Black"),
    
    df %>%
      filter(victim_race %in% c("(4) Asian", "Asian", "Asian/Pacific Islander",
        "(5) Native Hawaiian or Other Pacific Islander",
        "Native Hawaiian or Other Pacific Islander")) %>%
      mutate(victim_race_cat = "Asian_PI"),
    
    df %>%
      filter(victim_race %in% c(
        "(2) Black or African American", "Black or African American", "Black",
        "(3) American Indian or Alaska Native", "American Indian or Alaska Native",
        "American Indian/Alaskan Native",
        "(4) Asian", "Asian", "Asian/Pacific Islander",
        "(5) Native Hawaiian or Other Pacific Islander",
        "Native Hawaiian or Other Pacific Islander"
      )) %>%
      mutate(victim_race_cat = "Nonwhite")
  )
}


# ------------------------------------------------------------------------------
# 4. Clean incident data
# ------------------------------------------------------------------------------

ip_incidents <- ip_incidents %>%
  filter(state != "GM") %>%
  mutate(
    state = fix_state_abbr(state),
    fipscounty1 = str_pad(as.character(fipscounty1), 3, pad = "0"),
    strangulation = weapon1 %in% ASYX_CODE |
      weapon2 %in% ASYX_CODE |
      weapon3 %in% ASYX_CODE,
    agg_assault = offense1 %in% AGG_ASSAULT_CODES,
    arrest_flag = if_else(!is.na(num_arrestees) & num_arrestees > 0, 1L, 0L)
  )

male_incidents <- male_incidents %>%
  filter(state != "GM") %>%
  mutate(
    state = fix_state_abbr(state),
    fipscounty1 = str_pad(as.character(fipscounty1), 3, pad = "0"),
    assault = offense1 %in% ASSAULT_CODES,
    agg_assault = offense1 %in% AGG_ASSAULT_CODES,
    arrest_flag = if_else(!is.na(num_arrestees) & num_arrestees > 0, 1L, 0L)
  )

# Optional diagnostics
ip_incidents %>% distinct(state) %>% arrange(state) %>% print(n = 60)
male_incidents %>% distinct(state) %>% arrange(state) %>% print(n = 60)
county_race_pop %>% distinct(state) %>% arrange(state) %>% print(n = 60)
# Optional diagnostics
ip_incidents %>% distinct(state) %>% arrange(state) %>% print(n = 60)
male_incidents %>% distinct(state) %>% arrange(state) %>% print(n = 60)
county_race_pop %>% distinct(state) %>% arrange(state) %>% print(n = 60)

# ------------------------------------------------------------------------------
# 5. Covered counties by state x county x year
# ------------------------------------------------------------------------------


covered_counties_year <- bind_rows(
  ip_incidents   %>% distinct(state, year, fipscounty1),
  male_incidents %>% distinct(state, year, fipscounty1)
) %>%
  distinct(state, year, fipscounty1)

# Diagnostic: number of covered counties by year
covered_counties_year %>%
  count(year, name = "n_covered_counties") %>%
  arrange(year) %>%
  print(n = Inf)

# ------------------------------------------------------------------------------
# 6. Clean county population data and keep only covered counties
# ------------------------------------------------------------------------------

county_race_pop_clean <- county_race_pop %>%
  mutate(
    state = fix_state_abbr(state),
    county_fips = str_pad(as.character(county_fips), 3, pad = "0")
  ) %>%
  filter(race_cat %in% KEEP_RACES)

county_race_pop_covered <- county_race_pop_clean %>%
  semi_join(
    covered_counties_year,
    by = c("state", "year", "county_fips" = "fipscounty1")
  )

# Diagnostic: covered counties that do not find a match in population data
not_in_race_pop_year <- covered_counties_year %>%
  anti_join(
    county_race_pop_clean,
    by = c("state", "year", "fipscounty1" = "county_fips")
  )

not_in_race_pop_year %>%
  count(year, state, name = "n_missing_county_years") %>%
  arrange(desc(n_missing_county_years), year, state) %>%
  print(n = Inf)

not_in_race_pop_year %>%
  arrange(year, state, fipscounty1) %>%
  print(n = Inf)

# ------------------------------------------------------------------------------
# 7. Covered population by state x year x race
# ------------------------------------------------------------------------------

covered_pop_stateyear_race <- county_race_pop_covered %>%
  group_by(state, year, race_cat) %>%
  summarise(
    covered_population = sum(population, na.rm = TRUE),
    n_covered_counties = n_distinct(county_fips),
    .groups = "drop"
  ) %>%
  arrange(state, year, race_cat)

covered_pop_stateyear_race %>% print(n = 100)

# ------------------------------------------------------------------------------
# 8. Incident counts by state x year x race
# ------------------------------------------------------------------------------

ip_counts_stateyear_race <- expand_race_groups(ip_incidents) %>%
  group_by(state, year, victim_race_cat) %>%
  summarise(
    dv_count = n(),
    strang_count = sum(strangulation, na.rm = TRUE),
    dv_agg_assault_count = sum(agg_assault, na.rm = TRUE),
    dv_arrest_count = sum(arrest_flag, na.rm = TRUE),
    strang_arrest_count = sum(arrest_flag[strangulation], na.rm = TRUE),
    dv_agg_assault_arrest_count = sum(arrest_flag[agg_assault], na.rm = TRUE),
    .groups = "drop"
  )

male_counts_stateyear_race <- expand_race_groups(male_incidents) %>%
  group_by(state, year, victim_race_cat) %>%
  summarise(
    male_assault_count = n(),
    male_agg_assault_count = sum(agg_assault, na.rm = TRUE),
    male_arrest_count = sum(arrest_flag, na.rm = TRUE),
    male_agg_assault_arrest_count = sum(arrest_flag[agg_assault], na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# 8. Incident counts by state x year x race
# ------------------------------------------------------------------------------

ip_counts_stateyear_race <- expand_race_groups(ip_incidents) %>%
  group_by(state, year, victim_race_cat) %>%
  summarise(
    dv_count = n(),
    strang_count = sum(strangulation, na.rm = TRUE),
    dv_agg_assault_count = sum(agg_assault, na.rm = TRUE),
    dv_arrest_count = sum(arrest_flag, na.rm = TRUE),
    strang_arrest_count = sum(arrest_flag[strangulation], na.rm = TRUE),
    dv_agg_assault_arrest_count = sum(arrest_flag[agg_assault], na.rm = TRUE),
    .groups = "drop"
  )

male_counts_stateyear_race <- expand_race_groups(male_incidents) %>%
  group_by(state, year, victim_race_cat) %>%
  summarise(
    male_assault_count = n(),
    male_agg_assault_count = sum(agg_assault, na.rm = TRUE),
    male_arrest_count = sum(arrest_flag, na.rm = TRUE),
    male_agg_assault_arrest_count = sum(arrest_flag[agg_assault], na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 9. Merge counts with covered population
# ------------------------------------------------------------------------------

race_stateyear_panel <- ip_counts_stateyear_race %>%
  full_join(
    male_counts_stateyear_race,
    by = c("state", "year", "victim_race_cat")
  ) %>%
  left_join(
    covered_pop_stateyear_race %>%
      rename(victim_race_cat = race_cat),
    by = c("state", "year", "victim_race_cat")
  ) %>%
  mutate(
    dv_count = replace_na(dv_count, 0),
    strang_count = replace_na(strang_count, 0),
    dv_agg_assault_count = replace_na(dv_agg_assault_count, 0),
    dv_arrest_count = replace_na(dv_arrest_count, 0),
    strang_arrest_count = replace_na(strang_arrest_count, 0),
    dv_agg_assault_arrest_count = replace_na(dv_agg_assault_arrest_count, 0),
    male_assault_count = replace_na(male_assault_count, 0),
    male_agg_assault_count = replace_na(male_agg_assault_count, 0),
    male_arrest_count = replace_na(male_arrest_count, 0),
    male_agg_assault_arrest_count = replace_na(male_agg_assault_arrest_count, 0)
  ) %>%
  arrange(state, year, victim_race_cat)

# ------------------------------------------------------------------------------
# 10. Construct rates and arrest shares
# ------------------------------------------------------------------------------

race_stateyear_panel <- race_stateyear_panel %>%
  mutate(
    dv_rate = 100000 * dv_count / covered_population,
    strang_rate = 100000 * strang_count / covered_population,
    dv_agg_assault_rate = 100000 * dv_agg_assault_count / covered_population,
    male_assault_rate = 100000 * male_assault_count / covered_population,
    male_agg_assault_rate = 100000 * male_agg_assault_count / covered_population,
    
    dv_arrest_share = if_else(dv_count > 0, dv_arrest_count / dv_count, NA_real_),
    strang_arrest_share = if_else(strang_count > 0, strang_arrest_count / strang_count, NA_real_),
    dv_agg_assault_arrest_share = if_else(dv_agg_assault_count > 0, dv_agg_assault_arrest_count / dv_agg_assault_count, NA_real_),
    
    male_arrest_share = if_else(male_assault_count > 0, male_arrest_count / male_assault_count, NA_real_),
    male_agg_assault_arrest_share = if_else(male_agg_assault_count > 0, male_agg_assault_arrest_count / male_agg_assault_count, NA_real_)
  )

# ------------------------------------------------------------------------------
# 11. Diagnostics
# ------------------------------------------------------------------------------

race_stateyear_panel %>%
  filter(is.na(covered_population)) %>%
  arrange(state, year, victim_race_cat) %>%
  print(n = Inf)

race_stateyear_panel %>% print(n = 100)

# ------------------------------------------------------------------------------
# 12. Save output
# ------------------------------------------------------------------------------

write_csv(race_stateyear_panel, "data/race_stateyear_panel_covered_pop.csv")
