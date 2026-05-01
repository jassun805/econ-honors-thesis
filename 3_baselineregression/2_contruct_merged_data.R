##########################################################################################
# making one dataset with reported strangulation, iph, and state law flag by state x year
##########################################################################################
library(tidyverse)
library(here)
library(stringr)
library(fixest)


dv_rates <- readRDS(here("1_datacleaning_nibrs/data/dv_stateyear_rates.rds"))
maleassault_rates <- readRDS(here("1_datacleaning_nibrs/data/maleviolence_stateyear_rates.rds"))
homicide_rates <- read_csv(here("2_datacleaning_ucr-shr/data/stateyear_homiciderates.csv"))

setwd("~/Desktop/thesis/3_baselineregression")

## data
law_panel <- read_csv("data/law_panel_stateyear.csv")

dv_small <- dv_rates |> select(-dv_homicide, -homicide_rate) |>
  rename(nibrs_population = population, nibrs_agencies = agencies, dv_arrest_share = arrest_share) 

maleassault_small <- maleassault_rates |>
  select(-male_homicide, -male_homicide_rate, - population, - agencies) |>
  rename (male_assault_total = assault_total, male_agg_assault = agg_assault,
          male_arrested_incidents = arrested_incidents, male_assault_rate = assault_rate, 
          male_agg_assault_rate = agg_assault_rate, male_assault_arrest_rate = assault_arrest_rate)

nibrs_violence_rates <- left_join(dv_small, maleassault_small, by = c("state", "year"))

nibrs_violence_rates <- nibrs_violence_rates |> mutate(
  state = if_else(state == "NB", "NE", state)
)
write_csv(nibrs_violence_rates, "data/nibrs_violence_rates.csv")

homicide_clean <- homicide_rates |> rename(census_population = population, shr_agencies = agencies)

all_violence <- full_join(nibrs_violence_rates, homicide_clean, by = c("state", "year")) |>
  filter(!state %in% c("GM", "GU"))


violence_with_laws <- full_join(all_violence,law_panel, by = c("state", "year"))
write_csv(violence_with_laws, "data/violence_with_laws.csv")
