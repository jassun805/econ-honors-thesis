library(readr)
library(dplyr)
library(stringr)

setwd("~/Desktop/thesis/6_heterogeneity")

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

pop_clean <- pop_raw %>%
  mutate(
    state_county_fips = str_c(state_fips, county_fips)
  )

white_pop <- pop_clean %>%
  filter(race == 1) %>%
  mutate(race_cat = "White")

black_pop <- pop_clean %>%
  filter(race == 2) %>%
  mutate(race_cat = "Black")

aian_pop <- pop_clean %>%
  filter(race == 3) %>%
  mutate(race_cat = "Native")

asian_pi_pop <- pop_clean %>%
  filter(race == 4) %>%
  mutate(race_cat = "Asian_PI")

nonwhite_pop <- pop_clean %>%
  filter(race %in% c(2, 3, 4)) %>%
  mutate(race_cat = "Nonwhite")

all_pop <- pop_clean %>%
  filter(race %in% c(1,2, 3, 4)) %>%
  mutate(race_cat = "All")

pop_combined <- bind_rows(
  white_pop,
  black_pop,
  aian_pop,
  asian_pi_pop,
  nonwhite_pop,
  all_pop
)

county_year_race <- pop_combined %>%
  group_by(year, state_abbr, state_fips, county_fips, state_county_fips, race_cat) %>%
  summarise(
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(state = state_abbr) %>%
  filter(year >=1990)

write_csv(county_year_race, "data/county_year_race.csv")
