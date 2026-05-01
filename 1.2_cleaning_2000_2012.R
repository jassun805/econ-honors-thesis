########################################
# Aggregating incident-level IPV and male-male assault controls 
# From 2000-2012
##########################################

library(haven)
library(tidyverse)
library(dplyr)
library(here)

setwd("~/Desktop/thesis/1_datacleaning_nibrs")

ip_levels <- c(
  "Victim was Boyfriend/Girlfriend", 
  "Victim was Spouse", 
  "Victim was Common-Law Spouse", 
  "Victim was Ex-Spouse",
  "Homosexual Relationship")

pv_offenses <- c(
  "Murder/Nonnegligent Manslaughter", 
  "Kidnaping/Abduction", 
  "Forcible Rape", 
  "Rape", 
  "Forcible Sodomy",
  "Sodomy", 
  "Sexual Assault With An Object", 
  "Aggravated Assault", 
  "Simple Assault", 
  "Intimidation"
)

# Helper: extract year from filename like "2007.dta"
year_from_path <- function(path) {
  y <- stringr::str_match(basename(path), "^(\\d{4})\\.dta$")[, 2]
  if (is.na(y)) stop("Could not parse year from filename: ", path)
  as.integer(y)
}
process_year_dta <- function(path, ip_levels, pv_offenses) {
  year <- year_from_path(path)
  message("Processing year: ", year)
  
  df <- read_dta(path)

  core <- df |>
    transmute(
      year = year,
      state = STATE,
      ori = ORI,
      incnum = INCNUM,
      incdate = INCDATE,
      pop1 = B2005,
      pop2 = B2009,
      pop3 = B2013,
      pop4 = B2017,
      pop5 = B3005,
      months_reported = B3010,
      num_offenses = V1008,
      num_victims = V1009,
      num_offenders = V1010,
      num_arrestees = V1011,
      cleared_exception = V1013,
      offense1 = V4007,
      offense2 = V4008,
      offense3 = V4009,
      victim_age = V4018,
      victim_sex = V4019,
      victim_race = V4020,
      victim_ethnicity = V4021,
      victim_resident = V4022,
      injury1 = V4026,
      injury2 = V4027,
      injury3 = V4028,
      vic_to_off_relationship = V4032,
      incident_offense1 = V20061,
      incident_offense2 = V20062,
      incident_offense3 = V20063,
      weapon1 = V20171,
      weapon2 = V20181,
      weapon3 = V20191,
      offender_age = V50071,
      offender_sex = V50081,
      offender_race = V50091,
      offender_ethnicity = V60171,
      arrest_offense = V60111,
      arrestee_armed1 = V60121,
      arrestee_armed2 = V60131, 
      fipscounty1 = B3024
    ) |>
    mutate(
      across(
        c(
          state, ori, incnum,
          offense1, offense2, offense3,
          victim_sex, victim_race, victim_ethnicity, victim_resident,
          injury1, injury2, injury3,
          vic_to_off_relationship,
          incident_offense1, incident_offense2, incident_offense3,
          weapon1, weapon2, weapon3,
          offender_sex, offender_race, offender_ethnicity,
          arrest_offense, arrestee_armed1, arrestee_armed2, fipscounty1
        ),
        as_factor
      ),
      victim_age = as.numeric(haven::zap_labels(victim_age)),
      offender_age = as.numeric(haven::zap_labels(offender_age)),
      months_reported = suppressWarnings(as.integer(months_reported)),
      pop1 = suppressWarnings(as.integer(pop1)),
      pop2 = suppressWarnings(as.integer(pop2)),
      pop3 = suppressWarnings(as.integer(pop3)),
      pop4 = suppressWarnings(as.integer(pop4)),
      pop5 = suppressWarnings(as.integer(pop5)),
      num_offenses = suppressWarnings(as.integer(num_offenses)),
      num_victims = suppressWarnings(as.integer(num_victims)),
      num_offenderes = suppressWarnings(as.integer(num_offenderes)),
      num_arrestees = suppressWarnings(as.integer(num_arrestees)),
      incdate = as.character(incdate))
  
  rm(df)
  gc()

  
  ip_core <- core |>
    filter(
      vic_to_off_relationship %in% ip_levels,
      offense1 %in% pv_offenses
    )
  
  assault_control <- core |>
    filter(
      offense1 %in% c(
        "Aggravated Assault",
        "Simple Assault",
        "Murder/Nonnegligent Manslaughter"
      ),
      victim_sex == "Male",
      offender_sex == "Male"
    ) |>
    filter(!(vic_to_off_relationship %in% ip_levels))
  
  rm(core)
  gc()
  
  list(
    ip_core = ip_core,
    assault_control = assault_control
  )
}

# files 2000–2012 only
files <- list.files("data/annual_data", pattern = "^\\d{4}\\.dta$", full.names = TRUE)
years <- as.integer(sub("\\.dta$", "", basename(files)))
files <- files[years >= 2000 & years <= 2012]
files <- files[order(as.integer(sub("\\.dta$", "", basename(files))))]

ip_core_outfile <- "data/ip_core_2000_2012.rds"
assault_outfile <- "data/assault_control_2000_2012.rds"

ip_core_all <- tibble()
assault_all <- tibble()

for (f in files) {
  out <- process_year_dta(f, ip_levels, pv_offenses)
  
  ip_core_all <- bind_rows(ip_core_all, out$ip_core)
  assault_all <- bind_rows(assault_all, out$assault_control)
  
  saveRDS(ip_core_all, ip_core_outfile)
  saveRDS(assault_all, assault_outfile)
  
  rm(out)
  gc()
  
  message("Saved through: ", basename(f))
}


ip_core_all |> group_by(year) |> count(vic_to_off_relationship) |> print(n=200)

