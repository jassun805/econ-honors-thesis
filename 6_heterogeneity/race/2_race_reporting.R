library(tidyverse)
library(here)
library(stringr)
library(fixest)

setwd("~/Desktop/thesis/6_heterogeneity")

data <- read_csv("data/violence_laws_with_controls.csv") 


ip_incidents <- readRDS("data/ip_core_combined.rds") 
male_incidents <- readRDS("data/assault_control_combined.rds") 

HISPANIC_CODES <- c("(1) Hispanic or Latino", "Hispanic or Latino", "Hispanic Origin")
NONHIS_CODES   <- c("(0) Not Hispanic or Latino", "Not Hispanic or Latino", "Not of Hispanic Origin")
UNKNOWN_CODES  <- c("Undetermined", "Unknown/Missing/DNR", "Unknown/missing/DNR")

ASYX_CODE <- c("(850) Asphyxiation", "Asphyxiation")
IPH_CODE  <- c("(091) Murder/Nonnegligent Manslaughter", "Murder/Nonnegligent Manslaughter")
ASSAULT_CODES <- c(
  "(131) Aggravated Assault", "Aggravated Assault",
  "(132) Simple Assault", "Simple Assault"
)

KEEP_RACES <- c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic")

# =========================
# 3. HELPER FUNCTIONS
# =========================

make_ethnicity_3cat <- function(x) {
  case_when(
    x %in% HISPANIC_CODES ~ "Hispanic",
    x %in% NONHIS_CODES ~ "Non-Hispanic",
    x %in% UNKNOWN_CODES ~ "Unknown",
    is.na(x) ~ "Unknown",
    TRUE ~ "Unknown"
  )
}

make_race_cat <- function(victim_race, ethnicity_3cat) {
  case_when(
    ethnicity_3cat == "Hispanic" ~ "Hispanic",
    
    victim_race %in% c("(1) White", "White") &
      ethnicity_3cat == "Non-Hispanic" ~ "Non-Hispanic White",
    
    victim_race %in% c("(2) Black or African American", "Black or African American", "Black") &
      ethnicity_3cat == "Non-Hispanic" ~ "Non-Hispanic Black",
    
    victim_race %in% c("(4) Asian", "Asian", "Asian/Pacific Islander",
                       "(5) Native Hawaiian or Other Pacific Islander",
                       "Native Hawaiian or Other Pacific Islander") &
      ethnicity_3cat == "Non-Hispanic" ~ "Asian / Pacific Islander",
    
    victim_race %in% c("(3) American Indian or Alaska Native",
                       "American Indian or Alaska Native",
                       "American Indian/Alaskan Native") &
      ethnicity_3cat == "Non-Hispanic" ~ "American Indian / Alaska Native",
    
    TRUE ~ "Other / multiracial / unknown"
  )
}

make_race_short <- function(x) {
  case_when(
    x == "Non-Hispanic White" ~ "nh_white",
    x == "Non-Hispanic Black" ~ "nh_black",
    x == "Hispanic" ~ "hispanic",
    TRUE ~ NA_character_
  )
}

# =========================
# 4. ETHNICITY DIAGNOSTIC
# =========================

ip_incidents <- ip_incidents |>
  mutate(
    ethnicity_3cat = make_ethnicity_3cat(victim_ethnicity)
  )

# =========================
# 5. PREP INCIDENT-LEVEL DATA
# =========================

ip_race <- ip_incidents |>
  mutate(
    victim_race_cat = make_race_cat(victim_race, ethnicity_3cat),
    strangulation = weapon1 %in% ASYX_CODE |
      weapon2 %in% ASYX_CODE |
      weapon3 %in% ASYX_CODE,
    arrest_flag = if_else(!is.na(num_arrestees) & num_arrestees > 0, 1L, 0L)
  )

male_race <- male_incidents |>
  mutate(
    ethnicity_3cat = make_ethnicity_3cat(victim_ethnicity),
    victim_race_cat = make_race_cat(victim_race, ethnicity_3cat),
    assault = offense1 %in% ASSAULT_CODES,
    arrest_flag = if_else(!is.na(num_arrestees) & num_arrestees > 0, 1L, 0L)
  )


# =========================
# 6. AGGREGATE TO STATE x YEAR x RACE
# =========================

dv_race_stateyear <- ip_race |>
  group_by(state, year, victim_race_cat) |>
  summarise(
    dv_count = n(),
    dv_arrest_count = sum(arrest_flag, na.rm = TRUE),
    strang_count = sum(strangulation, na.rm = TRUE),
    .groups = "drop"
  )

male_race_stateyear <- male_race |>
  group_by(state, year, victim_race_cat) |>
  summarise(
    male_count = n(),
    male_arrest_count = sum(arrest_flag, na.rm = TRUE),
    assault_count = sum(assault, na.rm = TRUE),
    .groups = "drop"
  )

race_stateyear_panel <- full_join(
  dv_race_stateyear,
  male_race_stateyear,
  by = c("state", "year", "victim_race_cat")
) |>
  mutate(
    dv_count = coalesce(dv_count, 0L),
    dv_arrest_count = coalesce(dv_arrest_count, 0L),
    strang_count = coalesce(strang_count, 0L),
    male_count = coalesce(male_count, 0L),
    male_arrest_count = coalesce(male_arrest_count, 0L),
    assault_count = coalesce(assault_count, 0L)
  ) |>
  filter(victim_race_cat %in% KEEP_RACES)


# =========================
# 7. MERGE POPULATION + CONTROLS
# =========================

race_stateyear_panel <- race_stateyear_panel |>
  left_join(
    data |>
      select(
        state,
        year,
        nibrs_population,
        strang_law,
        poverty_rate,
        unemployment_gap,
        unemp_total
      ),
    by = c("state", "year")
  )

# =========================
# 8. CONSTRUCT RATES / PROPORTIONS / GAPS
# =========================

race_stateyear_panel <- race_stateyear_panel |>
  mutate(
    dv_arrest_prop = if_else(dv_count > 0, dv_arrest_count / dv_count, NA_real_),
    male_arrest_prop = if_else(male_count > 0, male_arrest_count / male_count, NA_real_),
    
    dv_rate = if_else(nibrs_population > 0, 1e5 * dv_count / nibrs_population, NA_real_),
    strang_rate = if_else(nibrs_population > 0, 1e5 * strang_count / nibrs_population, NA_real_),
    male_rate = if_else(nibrs_population > 0, 1e5 * male_count / nibrs_population, NA_real_),
    male_assault_rate = if_else(nibrs_population > 0, 1e5 * assault_count / nibrs_population, NA_real_),
    
    log_strang = log(strang_rate + 0.1), 
    log_male_assault = log(male_assault_rate + 0.1),
    log_ratio = log_strang - log_male_assault,
    
    log_arrest_ratio = log(dv_arrest_prop + 0.1) - log(male_arrest_prop + 0.1), 
    log_report_ratio = log(strang_count + 0.1) - log(assault_count + 0.1)
  ) |>
  arrange(state, year, victim_race_cat)


####################################
# DiD with hetereogeneity regression
####################################

race_stateyear_panel <- race_stateyear_panel |>
  mutate(
    victim_race_cat = factor(victim_race_cat),
    victim_race_cat = relevel(victim_race_cat, ref = "Non-Hispanic White")
  )

mod_A <- feols(
  log_ratio ~ strang_law * victim_race_cat +
    poverty_rate + unemp_total + unemployment_gap
  | state + year,
  data = race_stateyear_panel,
  cluster = ~state
)

mod_B <- feols(
  log_strang ~ strang_law * victim_race_cat +
    log_male_assault +
    poverty_rate + unemp_total + unemployment_gap
  | state + year,
  data = race_stateyear_panel,
  cluster = ~state
)


feols(
  log_report_ratio ~ strang_law*victim_race_cat + poverty_rate + unemp_total + unemployment_gap 
    | state + year,
  data = race_stateyear_panel, 
  cluster = ~state
)

##################
# Triple DDD Model
##################
ddd_data <- race_stateyear_panel |>
  select(
    state, year, victim_race_cat,
    strang_law,
    poverty_rate, unemp_total, unemployment_gap,
    strang_count, assault_count
  ) |>
  pivot_longer(
    cols = c(strang_count, assault_count),
    names_to = "outcome_type",
    values_to = "count"
  ) |>
  mutate(
    treated_outcome = if_else(outcome_type == "strang_count", 1, 0),
    log_count = log(count + 0.1)
  )

mod_DDD <- feols(
  log_count ~ 
    strang_law * treated_outcome * victim_race_cat +
    poverty_rate + unemp_total + unemployment_gap
  | state^victim_race_cat + year^victim_race_cat + outcome_type,
  data = ddd_data,
  cluster = ~state
)
