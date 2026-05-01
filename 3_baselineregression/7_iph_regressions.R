library(tidyverse)
library(readr)
library(fixest)
library(modelsummary)

setwd("~/Desktop/thesis/3_baselineregression")
dir.create("tables", showWarnings = FALSE)

data <- read_csv("data/violence_laws_with_controls.csv") |>
  mutate(
    log_ip_rate    = log(iph_rate + 0.1),
    log_fm_ip_rate = log(fm_iph_rate + 0.1),
    log_mf_ip_rate = log(mf_iph_rate + 0.1), 
    log_mm_homicide_rate = log(malemale_homicide_rate + 0.1), 
    log_assault_rate = log(male_assault_rate + 0.1)
  ) |>
  filter( year < 2021)

# Baseline
mod_base <- feols(
  iph_rate ~ strang_law | state + year,
  data = data,
  weights = ~census_population,
  cluster = ~state
)

mod_base_fm <- feols(
  log_fm_ip_rate ~ strang_law | state + year,
  data = data,
  weights = ~census_population,
  cluster = ~state
)

mod_base_mf <- feols(
  log_mf_ip_rate ~ strang_law | state + year,
  data = data,
  weights = ~census_population,
  cluster = ~state
)

# With controls
mod_ctrl <- feols(
  log_ip_rate ~ strang_law + poverty_rate +unemp_total + unemployment_gap + assault_rate | state + year,
  data = data,
  weights = ~census_population,
  cluster = ~state
)

mod_ctrl_fm <- feols(
  log_fm_ip_rate ~ strang_law + unemp_total + poverty_rate + unemployment_gap + assault_rate | state + year,
  data = data,
  weights = ~census_population,
  cluster = ~state
)

mod_ctrl_mf <- feols(
  log_mf_ip_rate ~ strang_law + unemp_total + unemployment_gap + poverty_rate + assault_rate | state + year,
  data = data,
  weights = ~census_population,
  cluster = ~state
)

mod_ppml_ctrl <- fepois(
  mf_ip_homicide ~ strang_law + unemp_total + poverty_rate + unemployment_gap + male_homicide | state + year,
  data = data,
  offset = ~log(census_population),
  cluster = ~state
)


data_long <- data |>
  select(state, year, strang_law, census_population,
         mf_iph_rate, malemale_homicide_rate) |>
  pivot_longer(
    cols = c(mf_iph_rate, malemale_homicide_rate),
    names_to = "group",
    values_to = "homicide_rate"
  ) |>
  mutate(
    # treatment group indicator
    ip = if_else(group == "mf_iph_rate", 1, 0),
    
    # log outcome
    log_rate = log(homicide_rate + 0.1)
  )


mod_ddd <- feols(
  homicide_rate ~ strang_law * ip | state + year + state^ip + year^ip,
  data = data_long,
  weights = ~census_population,
  cluster = ~state
)





models <- list(
  "(1)" = mod_base,
  "(2)" = mod_ctrl,
  "(3)" = mod_base_fm,
  "(4)" = mod_ctrl_fm,
  "(5)" = mod_base_mf,
  "(6)" = mod_ctrl_mf
)

coef_map <- c(
  "strang_law"   = "Strangulation law",
  "unemp_total"  = "Unemployment rate",
  "poverty_rate" = "Poverty rate"
)

gof_map <- tibble::tribble(
  ~raw,        ~clean,         ~fmt, ~omit,
  "nobs",      "Observations", 0,    FALSE,
  "r.squared", "R$^2$",        3,    FALSE
)

add_rows <- tibble::tribble(
  ~term,      ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`,
  "State FE", "Yes",  "Yes",  "Yes",  "Yes",  "Yes",  "Yes",
  "Year FE",  "Yes",  "Yes",  "Yes",  "Yes",  "Yes",  "Yes",
  "Controls", "No",   "Yes",  "No",   "Yes",  "No",   "Yes"
)

dir.create("tables", showWarnings = FALSE)

etable(
  mod_base, mod_ctrl,
  mod_base_fm, mod_ctrl_fm,
  mod_base_mf, mod_ctrl_mf,
  
  file = "tables/pre2021_ip_homicide_all_one_table.tex",
  replace = TRUE,
  
  title = "Effect of Strangulation Laws on Intimate Partner Homicide",
  
  headers = list(
    "All IP homicide" = 2,
    "Female-offender, male-victim IP homicide" = 2,
    "Male-offender, female-victim IP homicide" = 2
  ),
  
  dict = c(
    strang_law   = "Strangulation law",
    unemp_total  = "Unemployment rate",
    poverty_rate = "Poverty rate"
  ),
  
  drop = "Intercept",
  fitstat = ~ n + r2,
  
  extralines = list(
    "State fixed effects" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
    "Year fixed effects"  = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
    "Controls"            = c("No", "Yes", "No", "Yes", "No", "Yes")
  ),
  
  notes = c(
    "Dependent variable is log(homicide rate + 0.1).",
    "Standard errors are clustered at the state level.",
    "Observations are weighted by state population."
  )
)


