library(tidyverse)
library(here)
library(stringr)
library(fixest)
library(purrr)

setwd("~/Desktop/thesis/6_heterogeneity")

######################
# loading in datasets
#####################
race_data <- read_csv("data/race_data_with_controls.csv")

race_data <- race_data |>
  group_by(victim_race_cat) |>
  mutate(
    median_male_arrest_share = median(male_arrest_share, na.rm = TRUE),
    high_male_arrest_share = as.integer(male_arrest_share > median_arrest_share)
  ) |>
  ungroup()

data_bwa <- race_data |>
  filter(victim_race_cat %in% c("White", "Black", "Asian_PI"))

data_black <- race_data |>
  filter(victim_race_cat %in% c("Black"))

data_nw <- race_data |>
  filter(victim_race_cat %in% c("White", "Nonwhite"))

# Helper objects
# -----------------------------
econ_controls <- "poverty_rate + unemp_total + unemployment_gap"
race_term <- 'strang_law * i(victim_race_cat, ref = "White")'
male_race_term <- 'male_assault_rate * i(victim_race_cat, ref = "White")'

# -----------------------------
# Estimate models
# -----------------------------

m_dv <- feols(
  log(dv_rate + 0.1) ~
    strang_law * i(victim_race_cat, ref = "White") +
    poverty_rate + unemp_total + unemployment_gap +
    male_assault_rate * i(victim_race_cat, ref = "White") |
    state + year + victim_race_cat,
  data = data_bwa,
  weights = ~covered_population,
  cluster = ~state
)

m_agg <- feols(
  log(dv_agg_assault_rate + 0.1) ~
    strang_law * i(victim_race_cat, ref = "White") +
    poverty_rate + unemp_total + unemployment_gap +
    male_assault_rate * i(victim_race_cat, ref = "White") |
    state + year + victim_race_cat,
  data = data_bwa,
  weights = ~covered_population,
  cluster = ~state
)

m_str <- feols(
  log(strang_rate + 0.1) ~
    strang_law * i(victim_race_cat, ref = "White") +
    poverty_rate + unemp_total + unemployment_gap +
    male_assault_rate * i(victim_race_cat, ref = "White") |
    state + year + victim_race_cat,
  data = data_bwa,
  weights = ~covered_population,
  cluster = ~state
)

extract_row <- function(models, coef_name, label) {
  vals <- lapply(models, function(m) {
    ct <- summary(m)$coeftable
    rn <- rownames(ct)
    if (!(coef_name %in% rn)) return(c("", ""))
    est <- ct[coef_name, "Estimate"]
    se  <- ct[coef_name, "Std. Error"]
    p   <- ct[coef_name, "Pr(>|t|)"]
    
    stars <- ifelse(p < .01, "$^{***}$",
                    ifelse(p < .05, "$^{**}$",
                           ifelse(p < .1, "$^{*}$", "")))
    
    c(
      paste0(sprintf("%.3f", est), stars),
      paste0("(", sprintf("%.3f", se), ")")
    )
  })
  
  est_row <- sapply(vals, `[`, 1)
  se_row  <- sapply(vals, `[`, 2)
  
  c(
    paste0(label, " & ", paste(est_row, collapse = " & "), " \\\\"),
    paste0("& ", paste(se_row, collapse = " & "), " \\\\"),
    ""
  )
}

models <- list(m_dv, m_agg, m_str)

# Replace these names if names(coef(m_dv)) shows something different
coef_white <- "strang_law"
coef_black <- "strang_law:victim_race_cat::Black"
coef_aapi  <- "strang_law:victim_race_cat::Asian_PI"

rows_white <- extract_row(models, coef_white, "Strangulation law")
rows_black <- extract_row(models, coef_black, "Strangulation law $\\times$ Black")
rows_aapi  <- extract_row(models, coef_aapi,  "Strangulation law $\\times$ AAPI")

obs <- sapply(models, nobs)
r2  <- sapply(models, function(m) fitstat(m, "r2"))

tex_lines <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Effects of Strangulation Laws on Reported IPV Outcomes by Race}",
  "\\label{tab:race_mechanisms}",
  "\\begin{tabular}{lccc}",
  "\\toprule \\toprule",
  " & DV Rate & DV Aggravated Assault Rate & Strangulation Rate \\\\",
  "\\midrule",
  "\\emph{Variables} \\\\",
  "",
  rows_white,
  rows_black,
  rows_aapi,
  "\\midrule",
  "State fixed effects & Yes & Yes & Yes \\\\",
  "Year fixed effects & Yes & Yes & Yes \\\\",
  "Race fixed effects & Yes & Yes & Yes \\\\",
  "Economic controls & Yes & Yes & Yes \\\\",
  "Male assault $\\times$ race & Yes & Yes & Yes \\\\",
  "\\midrule",
  paste0("Observations & ", format(obs[1], big.mark = ","), " & ", format(obs[2], big.mark = ","), " & ", format(obs[3], big.mark = ","), " \\\\"),
  paste0("R$^2$ & ", sprintf("%.3f", r2[1]), " & ", sprintf("%.3f", r2[2]), " & ", sprintf("%.3f", r2[3]), " \\\\"),
  "\\bottomrule \\bottomrule",
  "\\end{tabular}",
  "",
  "\\vspace{0.3cm}",
  "\\begin{minipage}{0.9\\textwidth}",
  "\\footnotesize",
  "\\emph{Notes:} The dependent variables are $\\log(\\text{rate} + 0.1)$ for reported domestic violence, domestic violence aggravated assault, and strangulation. White is the omitted race category. All regressions include state, year, and race fixed effects; unemployment, poverty, and the unemployment gap; and race-specific controls for the male non-intimate partner assault rate. Standard errors clustered at the state level are reported in parentheses. Observations are weighted by covered population. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(tex_lines, "tables/race_mechanism_table.tex")
############################################################
# arrest share trends by race
############################################################

feols(
  dv_arrest_share ~ strang_law * i(victim_race_cat, ref = "White") + poverty_rate + unemp_total + unemployment_gap + male_arrest_share* i(victim_race_cat, ref = "White") | state + year,
  data = data_bwa, 
  weights = ~covered_population,
  cluster = ~state
)

feols(
  dv_agg_assault_arrest_share ~ strang_law * i(victim_race_cat, ref = "White") + poverty_rate + unemp_total + unemployment_gap | state + year,
  data = data_bwa, 
  weights = ~covered_population,
  cluster = ~state
)

feols(
  dv_arrest_share ~ strang_law * i(victim_race_cat, ref = "White") + poverty_rate + unemp_total + unemployment_gap + male_agg_assault_arrest_share | state + year,
  data = data_nw, 
  weights = ~covered_population,
  cluster = ~state
)

feols(
  dv_agg_assault_arrest_share ~ strang_law * i(victim_race_cat, ref = "White") + poverty_rate + unemp_total + unemployment_gap + high_male_arrest_share * i(victim_race_cat, ref = "White")| state + year,
  data = data_bwa, 
  weights = ~covered_population,
  cluster = ~state
)


