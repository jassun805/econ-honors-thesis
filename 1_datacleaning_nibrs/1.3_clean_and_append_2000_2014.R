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

core_2013 <- read_dta(
  "data/annual_data/2013.dta") |>
  transmute(
    state = STATE,
    ori = ORI,
    incnum = INCNUM,
    incdate = INCDATE,
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
    arrestee_armed2 = V60131, 
    fipscounty1 = BH054
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
    incdate = as.character(incdate), 
    
    year = 2013
    )

ip_core_2013 <- core_2013 |>
  filter(
    vic_to_off_relationship %in% ip_levels,
    offense1 %in% pv_offenses
  )

assault_control_2013 <- core_2013 |>
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

core_2014 <- read_dta(
  "data/annual_data/2014.dta") |>
  transmute(
    state = STATE,
    ori = ORI,
    incnum = INCNUM,
    incdate = INCDATE,
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
    arrestee_armed2 = V60131, 
    fipscounty1 = BH054
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
    incdate = as.character(incdate), 
    
    year = 2014)

ip_core_2014 <- core_2014 |>
  filter(
    vic_to_off_relationship %in% ip_levels,
    offense1 %in% pv_offenses
  )

assault_control_2014 <- core_2014 |>
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


ip_core_existing <- readRDS("data/ip_core_2000_2013.rds")
assault_control_existing <- readRDS("data/assault_control_2000_2013.rds")


ip_core_updated <- bind_rows(ip_core_existing, ip_core_2014)
assault_control_updated <- bind_rows(assault_control_existing, assault_control_2014)

saveRDS(ip_core_updated, "data/ip_core_2000_2014.rds")

saveRDS(assault_control_updated, "data/assault_control_2000_2014.rds")

