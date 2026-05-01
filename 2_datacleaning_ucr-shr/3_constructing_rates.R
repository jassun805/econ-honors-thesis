
library(tidyverse)
library(here)
library(stringr)
library(fixest)
library(dplyr)

setwd("~/Desktop/thesis/2_datacleaning_ucr-shr")

ip_relations <- c(
  "victim was boyfriend",
  "victim was common-law husband", 
  "victim was common-law wife", 
  "victim was ex-husband",
  "victim was ex-wife",
  "victim was girlfriend",
  "victim was wife",
  "victim was in a homosexual relationship with the offender"
)



## appending to current rates and counts dataset------
data <- read_csv("data/filtered_shr.csv") 

pop_raw <- read_fwf(
  "data/pop_by_race.txt",
  col_positions = fwf_widths(
    c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
    col_names = c(
      "year",
      "state_abbr",
      "state_fips",
      "county_fips",
      "filler",
      "race",
      "origin",
      "sex",
      "age",
      "population"
    )
  ),
  col_types = cols(
    year = col_integer(),
    state_abbr = col_character(),
    state_fips = col_character(),
    county_fips = col_character(),
    filler = col_character(),
    race = col_integer(),
    origin = col_integer(),
    sex = col_integer(),
    age = col_character(),
    population = col_double()
  )
)

pop_clean <- pop_raw |>
  mutate(
    state = state_abbr
  ) 



pop_expanded <- bind_rows(
  
  # --- ALL ---
  pop_clean |>
    mutate(race_cat = "All"),
  
  # --- WHITE ---
  pop_clean |>
    filter(race == 1) |>
    mutate(race_cat = "White"),
  
  # --- BLACK ---
  pop_clean |>
    filter(race == 2) |>
    mutate(race_cat = "Black"),
  
  # --- ASIAN / PACIFIC ISLANDER ---
  pop_clean |>
    filter(race == 4) |>
    mutate(race_cat = "Asian_PI"),
  
  #------ NATIVE AMERICAN-------
  pop_clean |>
    filter(race == 3) |>
    mutate(race_cat = "Native"),
  
  # --- NONWHITE (includes Black, AIAN, Asian_PI) ---
  pop_clean |>
    filter(race %in% c(2, 3, 4)) |>
    mutate(race_cat = "Nonwhite")
)

state_year_pop <- pop_expanded |>
  group_by(state, year, race_cat) |>
  summarise(
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )

state_year_pop <- state_year_pop |>
  mutate(
    race_cat = factor(
      race_cat,
      levels = c("All", "White", "Black", "Asian_PI", "Nonwhite", "Native")
    )
  )

write_csv(state_year_pop, "data/state_year_race_pop.csv")

########################
## getting counts-----
#######################

data <- data |>
  rename(state = state_abb)

data |> count(victim_1_race)

data <- data |> 
  rename(relationship = victim_1_relation_to_offender_1 )

shr_clean <- data |>
  mutate(
    victim_1_race = str_to_lower(str_trim(victim_1_race)),
    victim_1_sex  = str_to_lower(str_trim(victim_1_sex)),
    offender_1_sex = str_to_lower(str_trim(offender_1_sex))
  )

shr_race_expanded <- bind_rows(
  shr_clean |>
    mutate(victim_race_cat = "All"),
  
  shr_clean |>
    filter(victim_1_race == "white") |>
    mutate(victim_race_cat = "White"),
  
  shr_clean |>
    filter(victim_1_race == "black") |>
    mutate(victim_race_cat = "Black"),
  
  shr_clean |>
    filter(victim_1_race %in% c(
      "asian",
      "native hawaiian or other pacific islander"
    )) |>
    mutate(victim_race_cat = "Asian_PI"),
  
  shr_clean |>
    filter(victim_1_race %in% c(
      "american indian/alaskan native"
    )) |>
    mutate(victim_race_cat = "Native"),
  
  shr_clean |>
    filter(victim_1_race %in% c(
      "black",
      "american indian/alaskan native",
      "asian",
      "native hawaiian or other pacific islander"
    )) |>
    mutate(victim_race_cat = "Nonwhite")
)


shr_counts_stateyear_race <- shr_race_expanded |>
  group_by(state, year, victim_race_cat) |>
  summarise(
    total_homicide = n(),
    
    ip_homicide = sum(
      relationship %in% ip_relations,
      na.rm = TRUE
    ),
    
    male_offender_female_victim_ip_homicide = sum(
      offender_1_sex == "male" &
        victim_1_sex == "female" &
        relationship %in% ip_relations,
      na.rm = TRUE
    ),
    
    female_offender_male_victim_ip_homicide = sum(
      offender_1_sex == "female" &
        victim_1_sex == "male" &
        relationship %in% ip_relations,
      na.rm = TRUE
    ),
    
    male_male_nonip_homicide = sum(
      offender_1_sex == "male" &
        victim_1_sex == "male" &
        !(relationship %in% ip_relations),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  rename(race_cat = victim_race_cat)

# ------------------------------------------------------------------------------
# 3. Make race labels consistent before merge
# ------------------------------------------------------------------------------

state_year_pop <- state_year_pop |>
  mutate(race_cat = as.character(race_cat))

shr_counts_stateyear_race <- shr_counts_stateyear_race |>
  filter(year >=1990)

# ------------------------------------------------------------------------------
# 4. Merge homicide counts with state-year population
# ------------------------------------------------------------------------------

shr_stateyear_race <- shr_counts_stateyear_race |>
  left_join(
    state_year_pop,
    by = c("state", "year", "race_cat")
  ) |>
  arrange(state, year, race_cat)

# ------------------------------------------------------------------------------
# 5. Construct rates per 100,000
# ------------------------------------------------------------------------------

shr_stateyear_race <- shr_stateyear_race |>
  mutate(
    total_homicide_rate = 100000 * total_homicide / population,
    ip_homicide_rate = 100000 * ip_homicide / population,
    mofv_ip_homicide_rate = 100000 * male_offender_female_victim_ip_homicide / population,
    fovm_ip_homicide_rate = 100000 * female_offender_male_victim_ip_homicide / population,
    male_male_nonip_homicide_rate = 100000 * male_male_nonip_homicide / population
  )

shr_stateyear_race <- shr_stateyear_race |>
  filter(!state %in% c("GU"), !is.na(state))

write_csv(shr_stateyear_race, "data/shr_stateyear_race.csv")
