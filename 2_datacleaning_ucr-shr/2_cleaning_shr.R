library(tidyverse)
library(stringr)
library(here)
library(haven)

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

homicide_codes <- c(
  "murder and nonnegligent manslaughter",
  "murder and non-negligent manslaughter"
)


shr <- readRDS("data/ucr-shr/shr_1976_2023.rds") |>
  select(
    year, ori, state_abb, population, homicide_type, situation,
    victim_1_age, victim_1_sex, victim_1_race, victim_1_ethnic_origin,
    victim_1_relation_to_offender_1,
    offender_1_age, offender_1_sex, offender_1_race, offender_1_ethnic_origin,
    offender_1_weapon, offender_1_circumstance, offender_2_sex, victim_2_sex
  ) |>
  filter(year >= 1990)

shr <- shr |>
  filter(situation == "single victim/single offender") |> 
  filter(homicide_type %in% homicide_codes)

write_csv(shr, "data/filtered_shr.csv")
