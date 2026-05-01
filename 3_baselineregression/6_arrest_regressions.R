library(tidyverse)
library(readr)
library(fixest)

setwd("~/Desktop/thesis/3_baselineregression")

data <- read_csv("data/violence_laws_with_controls.csv")

data <- data |>
  group_by(year) |>
  mutate(
    male_arrest_median = median(male_assault_arrest_rate, na.rm = TRUE),
    high_male_arrest_share = as.integer(male_assault_arrest_rate > male_arrest_median)
  ) |>
  mutate(
    male_assault_rate_std = (
      male_assault_rate - mean(male_assault_rate, na.rm = TRUE)
    ) / sd(male_assault_rate, na.rm = TRUE)
    )|>
  mutate(
    male_assault_arrest_rate_std = (
      male_assault_arrest_rate - mean(male_assault_arrest_rate, na.rm = TRUE)
    ) / sd(male_assault_arrest_rate, na.rm = TRUE)
  ) |>
  ungroup()



# baseline
mod_base <- feols(
  dv_agg_arrest_share ~ strang_law | state + year,
  data = data,
  weights = ~nibrs_population,
  cluster = ~state
)


## spec A
mod_A <- feols(
  dv_agg_arrest_share ~ strang_law + unemp_total + poverty_rate | state + year,
  data = data,
  weights = ~nibrs_population,
  cluster = ~state
)

## spec B
mod_B <- feols(
  dv_agg_arrest_share ~ strang_law + poverty_rate + unemp_total + unemployment_gap | state + year,
  data = data,
  weights = ~nibrs_population,
  cluster = ~state
)

## spec C
mod_C <- feols(
  dv_agg_arrest_share ~ strang_law + poverty_rate + unemp_total + unemployment_gap + high_male_arrest_share | state + year,
  data = data,
  weights = ~nibrs_population,
  cluster = ~state
)


etable(
  mod_base, mod_A, mod_B, mod_C,
  tex = TRUE,
  file = "Tables/arrest_did_controls.tex",
  replace = TRUE,
  
  title = "Effect of Strangulation Laws on IPV Arrest",
  label = "tab:arrest_did_controls",
  
  depvar = TRUE,
  headers = list(
    "Baseline" = 1,
    "Controls" = 3
  ),
  
  dict = c(
    arrest_share = "IPV aggravated assault arrest share",
    strang_law = "Strangulation law",
    unemp_total = "Total unemployment rate",
    unemployment_gap = "Male--female unemployment gap",
    high_male_arrest_share = "High male assault arrest share",
    male_assault_rate_std = "Male assault rate",
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
    "The dependent variable is IPV aggravated assault arrest share, where the share is measured as the proportion of IPV cases where an arrest is made",
    "All regressions include state and year fixed effects.",
    "Observations are weighted by NIBRS covered population.",
    "Standard errors are clustered at the state level."
  )
)
