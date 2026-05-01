########################################
# Since UCR-SHR covers 90% of US Population
# Merging and cleaning census population data
############################################


library(tidyverse)
library(here)
library(stringr)
library(fixest)
library(dplyr)

setwd("~/Desktop/thesis/2_datacleaning_ucr-shr")

## reading in census population csvs-----
block1 <- read_csv("data/censuspop/pop2000-2010.csv") |>
  select(NAME, ORIGIN, starts_with("POPESTIMATE20")) |> 
  filter(ORIGIN == 0) |>
  filter(NAME != "United States") |> 
  rename_with(
    ~ str_replace(.x, "POPESTIMATE", "pop"),
    starts_with("POPESTIMATE20")
  ) |>
  mutate(state = state.abb[match(NAME, state.name)]) |>
  mutate(state = if_else(NAME == "District of Columbia", "DC", state)) |>
  select(-NAME, -pop2010, -ORIGIN)


block2 <- read_csv("data/censuspop/pop2010-2020.csv") |>
  mutate(NAME = str_remove(NAME, "^\\.")) |>
  mutate(state = state.abb[match(NAME, state.name)]) |>
  mutate(state = if_else(NAME == "District of Columbia", "DC", state)) |>
  select(-NAME, -pop2020)


block3 <- read_csv("data/censuspop/pop2020-2025.csv") |>
  mutate(NAME = str_remove(NAME, "^\\.")) |>
  mutate(state = state.abb[match(NAME, state.name)]) |>
  mutate(state = if_else(NAME == "District of Columbia", "DC", state)) |>
  filter(!is.na(state)) |>
  select(-NAME)
  

statepop <- left_join(block1, block2, by = "state") |> left_join(block3, by = "state")

statepop_long <- statepop |>
  pivot_longer(
    cols = starts_with("pop"),
    names_to = "year",
    values_to = "population"
  ) |>
  mutate(year = as.integer(str_extract(year, "\\d{4}")))


write_csv(statepop_long, "data/censuspop/statepop.csv")


