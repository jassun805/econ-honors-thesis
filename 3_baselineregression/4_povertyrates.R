library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

setwd("~/Desktop/thesis/3_baselineregression")

get_saipe_year <- function(year) {
  url <- paste0(
    "https://api.census.gov/data/timeseries/poverty/saipe?",
    "get=NAME,SAEPOVRTALL_PT&for=state:*&YEAR=", year
  )
  
  raw <- fromJSON(url)
  
  df <- as.data.frame(raw[-1, ], stringsAsFactors = FALSE)
  names(df) <- raw[1, ]
  
  df |>
    transmute(
      state_name = NAME,
      state = case_when(
        NAME == "District of Columbia" ~ "DC",
        TRUE ~ state.abb[match(NAME, state.name)]
      ),
      year = as.integer(year),
      poverty_rate = as.numeric(SAEPOVRTALL_PT)
    ) |>
    filter(!is.na(state))
}

poverty <- map_dfr(2000:2023, get_saipe_year)



write_csv(poverty, "data/poverty_rates.csv")



### append this to major dataset
poverty <- read_csv("data/poverty_rates.csv")

violence_with_laws <- read_csv("data/violence_with_laws.csv")
unemp <- read_csv("data/bls/stateyear_unemployment_rates.csv")



data_with_controls <- violence_with_laws |>
  left_join(poverty, by = c("state", "year")) |>
  left_join(unemp, by = c("state", "year"))

data_with_controls <- data_with_controls |>
  mutate(unemployment_gap = unemp_male - unemp_female) 

write_csv(data_with_controls, "data/violence_laws_with_controls.csv")

