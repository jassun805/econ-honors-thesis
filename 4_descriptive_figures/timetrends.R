library(tidyverse)
library(here)
library(stringr)
library(fixest)
library(dplyr)
library(ggplot)

setwd("~/Desktop/thesis/4_descriptive_figures")

# read in data
nibrs_ip <- readRDS("data/ip_core_combined.rds")

ASYX_CODE <- c("(850) Asphyxiation", "Asphyxiation")
IPH_CODE  <- c("(091) Murder/Nonnegligent Manslaughter", "Murder/Nonnegligent Manslaughter")

POP_COLS <- paste0("pop", 1:5)

make_agency_pop <- function(df, pop_cols = POP_COLS) {
  df |>
    mutate(
      agency_pop_total = rowSums(across(all_of(pop_cols)), na.rm = TRUE)
    ) |>
    filter(is.finite(agency_pop_total), agency_pop_total > 0)
}

is_strangulation <- function(df) {
  df$weapon1 %in% ASYX_CODE |
    df$weapon2 %in% ASYX_CODE |
    df$weapon3 %in% ASYX_CODE
}

is_homicide <- function(df) {
  df$offense1 %in% IPH_CODE
}

nibrs_ip <- nibrs_ip |>
  mutate(
    agency_pop_total = rowSums(across(all_of(POP_COLS)), na.rm = TRUE),
    strang_flag = weapon1 %in% ASYX_CODE |
      weapon2 %in% ASYX_CODE |
      weapon3 %in% ASYX_CODE,
    iph_flag = offense1 %in% IPH_CODE
  )

agency_year_df <- nibrs_ip |>
  group_by(state, ori, year) |>
  summarise(
    ipv_total = n(),
    ipv_strangulation = sum(strang_flag, na.rm = TRUE),
    iph_total = sum(iph_flag, na.rm = TRUE),
    population = max(agency_pop_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(is.finite(population), population > 0)

national_year_df <- agency_year_df |>
  group_by(year) |>
  summarise(
    ipv_total = sum(ipv_total, na.rm = TRUE),
    ipv_strangulation = sum(ipv_strangulation, na.rm = TRUE),
    iph_total = sum(iph_total, na.rm = TRUE),
    nibrs_population = sum(population, na.rm = TRUE),
    agencies = n_distinct(ori), 
    .groups = "drop"
  ) |>
  mutate(
    ipv_rate = 1e5 * ipv_total / nibrs_population,
    strangulation_rate = 1e5 * ipv_strangulation / nibrs_population,
    iph_rate = 1e5 * iph_total / nibrs_population
  )



scale_factor <- max(national_year_df$ipv_rate, na.rm = TRUE) / 
  max(national_year_df$strangulation_rate, na.rm = TRUE)

paper_purple <- "#6F479B"

p <- ggplot(national_year_df, aes(x = year)) +
  geom_line(
    aes(y = ipv_rate, color = "All IPV"),
    linewidth = 0.9, 
    linetype = "dashed"
  ) +
  geom_line(
    aes(y = strangulation_rate * scale_factor, color = "Strangulation IPV"),
    linewidth = 0.9, 
  ) +
  
  scale_y_continuous(
    name = "IPV (all) rate per 100,000",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "IPV Strangulation rate per 100,000"
    )
  ) +
  
  scale_color_manual(
    values = c(
      "All IPV" = "black",
      "Strangulation IPV" = paper_purple
    )
  ) +
  
  labs(
    x = "Year",
    y = NULL,
    color = NULL
  ) +
  
  theme_classic(base_family = "serif", base_size = 11) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    axis.title.y.left = element_text(size = 11, margin = margin(r = 8)),
    axis.title.y.right = element_text(size = 11, margin = margin(l = 8)),
    axis.text = element_text(size = 10)
  )
ggsave(
  filename = "figures/ipv_time_trends.png",
  plot = p,
  width = 7,
  height = 4.5,
  units = "in",
  dpi = 600
)


