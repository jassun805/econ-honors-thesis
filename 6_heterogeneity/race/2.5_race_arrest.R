library(tidyverse)
library(here)
library(stringr)
library(fixest)
library(purrr)

setwd("~/Desktop/thesis/6_heterogeneity")

data <- read_csv("data/race_data_controls.csv")

race_split <- split(data, data$victim_race_cat)

map(race_split, ~ feols(
  dv_agg_assault_arrest_share ~ strang_law + poverty_rate + unemp_total + unemployment_gap + male_arrest_share
  | state + year,
  data = .x,
  weights = ~ covered_population,
  cluster = ~state
))

