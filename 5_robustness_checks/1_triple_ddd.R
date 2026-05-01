library(tidyverse)
library(readr)
library(fixest)
library(modelsummary)

setwd("~/Desktop/thesis")

data <- read_csv("3_baselineregression/data/violence_laws_with_controls.csv") 
  

stacked <- data |>
  select(state, year, strang_law, Law,
         strang_rate, male_assault_rate,
         poverty_rate, unemp_total, unemployment_gap, nibrs_population) |>
  
  pivot_longer(
    cols = c(strang_rate, male_assault_rate),
    names_to = "group",
    values_to = "rate"
  ) |>
  
  mutate(
    # outcome
    log_rate = log(rate + 0.1),
    
    # treatment group indicator
    treated = as.integer(group == "strang_rate")
  )

ddd_model_nocontrols <- feols(
  log_rate ~ strang_law:treated |
    state^treated + year^treated,
  data = stacked,
  cluster = ~state, 
  weights = ~ nibrs_population
)


ddd_model <- feols(
  log_rate ~ strang_law:treated +
    poverty_rate + unemp_total + unemployment_gap |
    state^treated + year^treated,
  data = stacked,
  cluster = ~state, 
  weights = ~ nibrs_population
)

etable(
  ddd_model_nocontrols, ddd_model,
  tex = TRUE,
  file = "5_robustness_checks/ddd_robust.tex",
  replace = TRUE,
  
  title = "Effect of Strangulation Laws on Reported Strangulation: Triple-Difference Specification",
  label = "tab:ddd_strangulation",
  
  depvar = TRUE,
  se.below = TRUE,
  headers = list(
    "No Controls" = 1,
    "Controls" = 1
  ),
  
  dict = c(
    "strang_law:treated" = "Strangulation Law $\\times$ IPV Strangulation",
    "unemp_total" = "Total unemployment rate",
    "unemployment_gap" = "Male--female unemployment gap",
    "poverty_rate" = "Poverty rate"
  ),
  
  drop = "Intercept",
  digits = 3,
  
  fitstat = ~ n + r2,
  
  notes = c(
    "The dependent variable is $\\log(\\text{rate}_{gst} + 0.1)$.",
    "The sample stacks IPV strangulation and male non-intimate partner assault rates.",
    "The coefficient reports the difference in change in reported IPV strangulation relative to male non-intimate partner assault following adoption.",
    "All specifications include group-by-state and group-by-year fixed effects.",
    "Standard errors are clustered at the state level."
  )
)





# DDD event study 

ddd_es <- feols(
  log_rate ~ sunab(Law, year):treated +
    poverty_rate + unemp_total + unemployment_gap |
    state^treated + year^treated,
  data = stacked,
  cluster = ~state
)

es_df <- broom::tidy(ddd_es) |>
  filter(grepl("year::", term)) |>
  mutate(
    event_time = as.numeric(sub("year::", "", term))
  ) |>
  filter(event_time >= -5, event_time <= 5)
