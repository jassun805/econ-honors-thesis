########################################
# Aggregating incident-level IPV and male-male assault controls 
# From 2015-2023
##########################################


library(tidyverse)
library(dplyr)
library(here)

setwd("~/Desktop/thesis/1_datacleaning_nibrs")

loaded_obj <- load("data/annual_data/2023.rda")

data <- get(loaded_obj)

data |> count(V4032)


ip_levels <- c(
  "(01) Victim was Spouse", 
  "(02) Victim was Common-Law Spouse", 
  "(18) Victim was Boyfriend/Girlfriend", 
  "(21) Victim was Ex-Spouse", 
  "(26) Victim was Ex-relationship (Ex-boyfriend/ex-girlfriend)"
)

pv_offenses <- c(
  "(091) Murder/Nonnegligent Manslaughter", 
  "(092) Negligent Manslaughter", 
  "(100) Kidnaping/Abduction", 
  "(111) Rape", 
  "(112) Sodomy", 
  "(113) Sexual Assault With An Object", 
  "(131) Aggravated Assault", 
  "(132) Simple Assault", 
  "(133) Intimidation"
)

# Helper: load 1-object .rda into a data.frame/tibble
load_single_rda <- function(path) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  objs <- ls(env)
  if (length(objs) != 1) {
    stop("Expected exactly 1 object in ", path, " but found: ", paste(objs, collapse = ", "))
  }
  env[[objs[1]]]
}

# Helper: extract year from "....../2023.rda"
year_from_path <- function(path) {
  y <- str_match(basename(path), "^(\\d{4})\\.rda$")[,2]
  if (is.na(y)) stop("Could not parse year from filename: ", path)
  as.integer(y)
}

# Main: process one year's file -> list(ip_core=..., assault_control=...)
process_year_file <- function(path, ip_levels, pv_offenses) {
  year <- year_from_path(path)
  message("Processing year: ", year)
  df <- load_single_rda(path)
  
  core <- df |>
    mutate(year = year) |>
    rename(
      state = STATE,
      ori = ORI,
      incnum = INCNUM,
      incdate = INCDATE,
      ori_add_date = BH005,
      ori_nibrs_date = BH006,
      ori_city = BH007,
      state_abbr = BH008,
      ori_pop_group = BH009,
      country_div = BH010,
      country_region = BH011,
      agency_type = BH012,
      core_city = BH015,
      fed_jud_district = BH016,
      nibrs_active_flag = BH017,
      pop1 = BH019,
      county1 = BH020,
      msa1 = BH021,
      pop2 = BH023,
      county2 = BH024,
      msa2 = BH025,
      pop3 = BH027,
      county3 = BH028,
      msa3 = BH029,
      pop4 = BH031,
      county4 = BH032,
      msa4 = BH033,
      pop5 = BH035,
      county5 = BH036,
      msa5 = BH037,
      months_reported = BH040,
      fipscounty1 = BH054,
      fipscounty2 = BH055,
      fipscounty3 = BH056,
      fipscounty4 = BH057,
      fipscounty5 = BH058,
      num_offenses = V1008,
      num_victims = V1009,
      num_offenderes = V1010,
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
      offender_ethnicity = V50111,
      arrest_offense = V60111,
      arrestee_armed1 = V60121,
      arrestee_armed2 = V60131
    ) |>
    select(
      year, state, ori, incnum, incdate,
      ori_add_date, ori_nibrs_date, ori_city,
      state_abbr, ori_pop_group, country_div,
      country_region, agency_type, core_city,
      fed_jud_district, nibrs_active_flag, pop1,
      county1, msa1, pop2, county2, msa2, pop3,
      county3, msa3, pop4, county4, msa4, pop5,
      county5, msa5, months_reported, fipscounty1,
      fipscounty2, fipscounty3, fipscounty4,
      fipscounty5, num_offenses, num_victims,
      num_offenderes, num_arrestees, cleared_exception,
      offense1, offense2, offense3, victim_age, victim_sex,
      victim_race, victim_ethnicity, victim_resident,
      injury1, injury2, injury3, vic_to_off_relationship,
      incident_offense1, incident_offense2, incident_offense3,
      weapon1, weapon2, weapon3, offender_age, offender_sex,
      offender_race, offender_ethnicity, arrest_offense,
      arrestee_armed1, arrestee_armed2
    )
  
  core <- core |>
    mutate(
      incdate = as.character(incdate),
      ori_add_date = as.character(ori_add_date),
      ori_nibrs_date = as.character(ori_nibrs_date),
    ) |>
    mutate(
      # --- IDs / codes / labeled fields: force to character (avoid factor/double conflicts) ---
      across(
        c(
          state, ori, incnum, state_abbr,
          ori_city, country_div, country_region, agency_type, core_city, fed_jud_district,
          nibrs_active_flag, ori_pop_group,
          county1, county2, county3, county4, county5,
          msa1, msa2, msa3, msa4, msa5,
          fipscounty1, fipscounty2, fipscounty3, fipscounty4, fipscounty5,
          offense1, offense2, offense3,
          victim_sex, victim_race, victim_ethnicity, victim_resident,
          injury1, injury2, injury3,
          vic_to_off_relationship,
          incident_offense1, incident_offense2, incident_offense3,
          weapon1, weapon2, weapon3,
          offender_sex, offender_race, offender_ethnicity,
          arrest_offense, arrestee_armed1, arrestee_armed2
        ),
        ~ as.character(.x)
      )
    ) |>
    mutate(
      # --- numeric-ish fields: force to integer/numeric consistently ---
      across(
        c(
          months_reported,
          pop1, pop2, pop3, pop4, pop5,
          num_offenses, num_victims, num_offenderes, num_arrestees
        ),
        ~ suppressWarnings(as.integer(as.character(.x)))
      ),
      
      victim_age   = suppressWarnings(as.integer(as.character(victim_age))),
      offender_age = suppressWarnings(as.integer(as.character(offender_age)))
    )
  
  ip_core <- core |>
    filter(vic_to_off_relationship %in% ip_levels,
           offense1 %in% pv_offenses)
  
  assault_control <- core |>
    filter(offense1 %in% c("(131) Aggravated Assault", "(132) Simple Assault", "(091) Murder/Nonnegligent Manslaughter"),
           victim_sex == "(1) Male",
           offender_sex == "(1) Male") |>
    filter(!(vic_to_off_relationship %in% ip_levels))
  
  list(ip_core = ip_core, assault_control = assault_control)
}

# ---- Run across all years ----
files <- list.files("data", pattern = "^\\d{4}\\.rda$", full.names = TRUE)

out <- lapply(files, process_year_file, ip_levels = ip_levels, pv_offenses = pv_offenses)


ip_core_all <- bind_rows(lapply(out, `[[`, "ip_core"))
assault_control_all <- bind_rows(lapply(out, `[[`, "assault_control"))




saveRDS(ip_core_all, "data/ip_core_2015to2023.rds")
saveRDS(assault_control_all, "data/assault_control_2015to2023.rds")

