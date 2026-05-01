library(tidyverse)
library(readr)
library(fixest)

setwd("~/Desktop/thesis/3_baselineregression")

data <- read_csv("data/violence_laws_with_controls.csv") |>
  mutate(log_strang_rate = log(strang_rate + 0.1))


data <- data |>
  group_by(year) |>
  mutate(
  male_assault_rate_std = (
    male_assault_rate - mean(male_assault_rate, na.rm = TRUE)
  ) / sd(male_assault_rate, na.rm = TRUE)
) |>
  mutate(
    male_arrest_median = median(male_assault_arrest_rate, na.rm = TRUE),
    high_male_arrest_share = as.integer(male_assault_arrest_rate > male_arrest_median)
  )|>
  ungroup()

## baseline TWFE
mod_base <- feols(
  log_strang_rate ~ strang_law | state + year,
  data = data,
  weights = ~nibrs_population,
  cluster = ~state
)

## spec A
mod_A <- feols(
  log_strang_rate ~ strang_law + unemp_total + poverty_rate | state + year,
  data = data,
  weights = ~nibrs_population,
  cluster = ~state
)

## spec B
mod_B <- feols(
  log_strang_rate ~ strang_law + poverty_rate + unemp_total + unemployment_gap | state + year,
  data = data,
  weights = ~nibrs_population,
  cluster = ~state
)

## spec C
mod_C <- feols(
  log_strang_rate ~ strang_law + poverty_rate + unemp_total + unemployment_gap + male_assault_rate_std + high_male_arrest_share | state + year,
  data = data,
  weights = ~nibrs_population,
  cluster = ~state
)


dv_mod <- feols(
  log(dv_rate + 0.1) ~ strang_law + poverty_rate + unemp_total + unemployment_gap + male_assault_rate_std + high_male_arrest_share| state + year,
  data = data,
  weights = ~nibrs_population,
  cluster = ~state
)

agg_dv_mod <- feols(
  log(dv_agg_assault_rate + 0.1) ~ strang_law + poverty_rate + unemp_total + unemployment_gap + male_assault_rate_std + high_male_arrest_share| state + year,
  data = data,
  weights = ~nibrs_population,
  cluster = ~state
)

etable(
  mod_base, mod_A, mod_B, mod_C,
  tex = TRUE,
  file = "Tables/baseline_did_controls.tex",
  replace = TRUE,
  
  title = "Effect of Strangulation Laws on Reported IPV Strangulation",
  label = "tab:baseline_did_controls",
  
  depvar = TRUE,
  headers = list(
    "Baseline" = 1,
    "Controls" = 3
  ),
  
  dict = c(
    log_strang_rate = "$\\log(\\text{Reported IPV strangulation rate}_{st} + 0.1)$",
    strang_law = "Strangulation law",
    unemp_total = "Total unemployment rate",
    unemployment_gap = "Male-Female unemployment gap",
    assault_rate = "Male-Male non-intimate partner assault rate",
    poverty_rate = "Poverty rate"
  ),
  
  drop = "Intercept",
  digits = 3,
  
  fixef.group = list(
    "State FE" = "state",
    "Year FE" = "year"
  ),
  
  fitstat = ~ n + r2,
  
  notes = c(
    "The dependent variable is $\\log(\\text{reported IPV strangulation rate}_{st} + 0.1)$, where the rate is measured per 100,000 population.",
    "All regressions include state and year fixed effects.",
    "Observations are weighted by NIBRS covered population.",
    "Standard errors are clustered at the state level."
  )
)

