library(tidyverse)
library(here)
library(stringr)
library(fixest)
library(purrr)
library(modelsummary)

setwd("~/Desktop/thesis")

iph_race_data <- read_csv("2_datacleaning_ucr-shr/data/shr_stateyear_race.csv")
violence_with_controls <- read_csv("3_baselineregression/data/violence_laws_with_controls.csv") |>
  select(state,year,poverty_rate, strang_law, Law, unemp_total, unemployment_gap, male_assault_rate, male_assault_arrest_rate) 

setwd("~/Desktop/thesis/6_heterogeneity")


iph_data_with_controls <- iph_race_data |>
  left_join(violence_with_controls, by = c("state", "year"))

iph_data_with_controls <- iph_data_with_controls |>
  group_by(race_cat, year) |>
  mutate(
    median_male_iph = median(male_male_nonip_homicide_rate, na.rm = TRUE),
    high_male_iph = as.integer(male_male_nonip_homicide_rate > median_male_iph)
  ) |>
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

iph_all <- iph_data_with_controls |>
  filter(race_cat == "All")

iph_white <- iph_data_with_controls |>
  filter(race_cat == "White")

iph_black<- iph_data_with_controls |>
  filter(race_cat == "Black")

iph_aapi <- iph_data_with_controls |>
  filter(race_cat == "Asian_PI")

iph_aian <- iph_data_with_controls |>
  filter(race_cat == "Native")

iph_bwan <- iph_data_with_controls |>
  filter(race_cat %in% c("White", "Black", "Asian_PI", "Native"))

iph_bwa <- iph_data_with_controls |>
  filter(race_cat %in% c("White", "Black", "Asian_PI"))

iph_nw <- iph_data_with_controls |>
  filter(race_cat %in% c("White", "Nonwhite"))



feols(
  ip_homicide_rate  ~ strang_law * i(race_cat, ref = "White")+ unemp_total + poverty_rate + 
    unemployment_gap + high_male_iph * i(race_cat, ref = "White")| state + year,
  data = iph_bwa,
  weights = ~population,
  cluster = ~state
)

feols(
  mofv_ip_homicide_rate ~ strang_law + unemp_total + poverty_rate + unemployment_gap + high_male_iph
  | state + year,
  data = iph_all,
  weights = ~population,
  cluster = ~state
)

feols(
  fovm_ip_homicide_rate ~ strang_law + unemp_total + poverty_rate + unemployment_gap + high_male_iph
  | state + year,
  data = iph_all,
  weights = ~population,
  cluster = ~state
)


###########################
# generating general model
########################
m1 <- feols(
  ip_homicide_rate ~ strang_law + unemp_total + poverty_rate + unemployment_gap | state + year,
  data = iph_all,
  weights = ~population,
  cluster = ~state
)

feols(
  log(ip_homicide_rate + 0.1) ~ strang_law + unemp_total + poverty_rate + unemployment_gap + high_male_iph + male_assault_rate_std | state + year,
  data = iph_all,
  weights = ~population,
  cluster = ~state
)

feols(
  log(fovm_ip_homicide_rate + 0.1) ~ strang_law + unemp_total + poverty_rate + unemployment_gap + high_male_iph | state + year,
  data = iph_all,
  weights = ~population,
  cluster = ~state
)

feols(
  log(mofv_ip_homicide_rate + 0.1) ~ strang_law + unemp_total + poverty_rate + unemployment_gap + male_assault_rate_std | state + year,
  data = iph_all,
  weights = ~population,
  cluster = ~state
)

models_baseline <- list(
  "IPH Rate" = m1,
  "Log IPH Rate" = m2,
  "Log F on M IPH" = m3,
  "Log M on F IPH" = m4
)

coef_map <- c(
  "strang_law" = "Strangulation Law",
  "unemp_total" = "Unemployment Rate",
  "poverty_rate" = "Poverty Rate",
  "unemployment_gap" = "Unemployment Gap"
)

gof_map <- tribble(
  ~raw,           ~clean,            ~fmt,
  "nobs",         "Observations",    0,
  "r.squared",    "R-squared",       3,
  "within.r.squared", "Within R-sq.", 3
)

modelsummary(
  models_baseline,
  coef_map = coef_map,
  gof_map = gof_map,
  stars = TRUE,
  fmt = 3,
  output = "tables/iph_baseline.tex",
  title = "Effects of Felony Strangulation Laws on Intimate Partner Homicide"
)

#######################
# Heterogeneity by race
#######################


m_iph_race <- feols(
  log(ip_homicide_rate + 0.1) ~
    strang_law * i(race_cat, ref = "White") +
    unemp_total + poverty_rate + unemployment_gap + + high_male_iph * i(race_cat, ref = "White") |
    state + year,
  data = iph_bwa,
  weights = ~population,
  cluster = ~state
)

m_mofv_race <- feols(
  log(mofv_ip_homicide_rate + 0.1) ~
    strang_law * i(race_cat, ref = "White") +
    unemp_total + poverty_rate + unemployment_gap + + high_male_iph * i(race_cat, ref = "White") |
    state + year,
  data = iph_bwa,
  weights = ~population,
  cluster = ~state
)

m_fovm_race <- feols(
  log(fovm_ip_homicide_rate + 0.1) ~
    strang_law * i(race_cat, ref = "White") +
    unemp_total + poverty_rate + unemployment_gap + high_male_iph * i(race_cat, ref = "White")|
    state + year,
  data = iph_bwa,
  weights = ~population,
  cluster = ~state
)

# optional: cleaner LaTeX numeric formatting
options(modelsummary_format_numeric_latex = "plain")

models_het <- list(
  "(1)" = m_iph_race,
  "(2)" = m_fovm_race,
  "(3)" = m_mofv_race
)

coef_map_het <- c(
  "strang_law" = "Strangulation Law",
  "race_cat::Black" = "Black",
  "race_cat::Asian_PI" = "Asian/Pacific Islander",
  "strang_law:race_cat::Black" = "Strangulation Law $\\times$ Black",
  "strang_law:race_cat::Asian_PI" = "Strangulation Law $\\times$ Asian/Pacific Islander"
)

gof_map <- tibble::tribble(
  ~raw,              ~clean,              ~fmt,
  "nobs",            "Observations",      0,
  "r.squared",       "$R^2$",             3,
  "within.r.squared","Within $R^2$",      3
)

add_rows_het <- tibble::tribble(
  ~term,                            ~`(1) All IPH`, ~`(2) F-Offender, M-Victim`, ~`(3) M-Offender, F-Victim`,
  "Economic controls",              "Yes",          "Yes",                      "Yes",
  "Race-specific high male IPH controls", "Yes",     "Yes",                      "Yes",
  "State FE",                       "Yes",          "Yes",                      "Yes",
  "Year FE",                        "Yes",          "Yes",                      "Yes",
  "Population weights",             "Yes",          "Yes",                      "Yes",
  "SE clustered by state",          "Yes",          "Yes",                      "Yes"
)

modelsummary(
  models_het,
  coef_map = coef_map_het,
  coef_omit = "unemp_total|poverty_rate|unemployment_gap",
  gof_map = gof_map,
  add_rows = add_rows_het,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  statistic = "({std.error})",
  fmt = 3,
  title = "Race Heterogeneity in the Effects of Felony Strangulation Laws on Intimate Partner Homicide",
  notes = c(
    "White is the omitted race category.",
    "All regressions include unemployment, poverty, and unemployment-gap controls.",
    "State and year fixed effects included in all specifications.",
    "Regressions are weighted by population and standard errors are clustered at the state level."
  ),
  output = "tables/iph_race_heterogeneity.tex"
)

