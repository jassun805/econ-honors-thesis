library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)

setwd("~/Desktop/thesis/3_baselineregression")

input_dir  <- "data/bls"
output_csv <- "data/bls/stateyear_unemployment_rates.csv"

files <- list.files(
  path = input_dir,
  pattern = "\\.xlsx$",
  full.names = TRUE
)

extract_year <- function(file) {
  yy <- str_match(basename(file), "(\\d{2})\\.xlsx$")[, 2]
  
  if (is.na(yy)) {
    stop(paste("Could not extract year from:", basename(file)))
  }
  
  year <- as.integer(yy)
  
  if (year <= 30) {
    return(2000 + year)
  } else {
    return(1900 + year)
  }
}

clean_bls_file <- function(file) {
  year <- extract_year(file)
  
  raw <- read_excel(file, col_names = FALSE)
  names(raw) <- paste0("V", seq_len(ncol(raw)))
  
  df <- raw |>
    mutate(across(everything(), \(x) if (is.character(x)) str_trim(x) else x))
  
  out <- df |>
    transmute(
      state_name = as.character(V3),
      group = as.character(V4),
      unemployment_rate = suppressWarnings(as.numeric(V11)),
      year = year
    ) |>
    filter(group %in% c("Total", "Men", "Women")) |>
    mutate(
      state_name = str_to_title(state_name),
      state = case_when(
        state_name == "District Of Columbia" ~ "DC",
        TRUE ~ state.abb[match(state_name, state.name)]
      )
    ) |>
    filter(!is.na(state))
  
  return(out)
}


unemp_long <- map_dfr(files, clean_bls_file)

unemp_wide <- unemp_long |>
  select(state, year, group, unemployment_rate) |>
  distinct() |>
  pivot_wider(names_from = group, values_from = unemployment_rate) |>
  rename(
    unemp_total = Total,
    unemp_male = Men,
    unemp_female = Women
  ) |>
  arrange(state, year)

write_csv(unemp_wide, output_csv)

cat("Saved to:", output_csv, "\n")
print(head(unemp_wide, 10))
