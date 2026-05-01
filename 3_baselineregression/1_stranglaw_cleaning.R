library(tidyverse)
library(here)
library(stringr)
library(fixest)

setwd("~/Desktop/thesis/3_baselineregression")

# load in data
statelaws <- read_csv("data/state_binary.csv") |>
  rename(state = State) 

# coding state laws
law_panel <- statelaws |>
  transmute(
    state,
    Law = as.integer(Law)
  ) |>
  tidyr::expand_grid(year = 2000:2023) |>
  mutate(
    strang_law = if_else(!is.na(Law) & year >= Law, 1L, 0L),
  ) |>
  select(state, year, Law, strang_law) |>
  arrange(state, year)

# save
write_csv(law_panel, "data/law_panel_stateyear.csv")
