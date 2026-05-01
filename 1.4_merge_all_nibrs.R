######################################
# Creating 1 dataset for IPV 2000-2023
# Creating 1 dataset for male-male assaults 2000-2023
######################################

library(tidyverse)
library(dplyr)
library(here)

setwd("~/Desktop/thesis/1_datacleaning_nibrs")

ip_core_old <- readRDS("data/ip_core_2000_2014.rds")
ip_core_new <- readRDS("data/ip_core_2015to2023.rds")

ip_core_new <- ip_core_new |>
  select(-state) |>
  rename(state = state_abbr)

ip_core_old <- ip_core_old |>
  mutate(cleared_exception = as_factor(cleared_exception)) 

union(
  setdiff(names(ip_core_old), names(ip_core_new)),
  setdiff(names(ip_core_new), names(ip_core_old))
)

common_vars <- intersect(names(ip_core_old), names(ip_core_new))
drop_vars <- c(
  "cleared_exception",
  "county1", "msa1",
  "county2", "msa2",
  "county3", "msa3",
  "county4", "msa4",
  "county5", "msa5"
)

common_vars <- setdiff(common_vars, drop_vars)

ip_core_combined <- bind_rows(
  ip_core_old[, common_vars],
  ip_core_new[, common_vars]
)


saveRDS(ip_core_combined, "data/ip_core_combined.rds")


#### doing the same for assault controls-------
assault_old <- readRDS("data/assault_control_2000_2014.rds")
assault_new <- readRDS("data/assault_control_2015to2023.rds")

assault_new <- assault_new |>
  select(-state) |>
  rename(state = state_abbr)

union(
  setdiff(names(assault_old), names(assault_new)),
  setdiff(names(assault_new), names(assault_old))
)

common_vars <- intersect(names(assault_old), names(assault_new))
drop_vars <- c(
  "cleared_exception",
  "county1", "msa1",
  "county2", "msa2",
  "county3", "msa3",
  "county4", "msa4",
  "county5", "msa5"
)

common_vars <- setdiff(common_vars, drop_vars)

assault_combined <- bind_rows(
  assault_old[, common_vars],
  assault_new[, common_vars]
)
saveRDS(assault_combined, "data/assault_control_combined.rds")


