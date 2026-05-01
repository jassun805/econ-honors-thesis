library(tidyverse)
library(usmap)
library(ggplot2)
library(viridis)

setwd("~/Desktop/thesis/4_descriptive_figures")

state_adoption_year <- read_csv("data/state_binary.csv")

law_data <- state_adoption_year %>%
  select(State, Law) %>%
  mutate(state = str_to_title(State))


state_adoption_year <- state_adoption_year |> rename(adoption_year = Law)

state_adoption_year_clean <- state_adoption_year |>
  transmute(
    state = as.character(State),
    adoption_year = as.numeric(adoption_year)
  )

years_vec <- seq(
  from = 2000,
  to   = 2023,
  by   = 1
)

adoption_path <- tibble(
  year = years_vec,
  states_adopted = sapply(
    years_vec,
    function(y) sum(state_adoption_year$adoption_year <= y, na.rm = TRUE)
  )
)


# ----------------------------
# 1. Extend adoption path to 2025
# ----------------------------
adoption_path_ext <- adoption_path |>
  arrange(year) |>
  add_row(
    year = 2025,
    states_adopted = dplyr::last(adoption_path$states_adopted)
  )

# ----------------------------
# 2. Step-shaped fill data
# ----------------------------
adoption_step <- adoption_path_ext |>
  arrange(year) |>
  mutate(year_next = lead(year, default = 2025)) |>
  rowwise() |>
  do(data.frame(
    x = c(.$year, .$year_next),
    y = c(.$states_adopted, .$states_adopted)
  )) |>
  ungroup()

# ----------------------------
# 3. Grid of boxed rectangles
# ----------------------------
x_cells <- tibble(
  xmin = 2000:2024,
  xmax = 2001:2025
)

y_cells <- tibble(
  ymin = c(seq(0, 45, 5), 50),
  ymax = c(seq(5, 50, 5), 51)
)

grid_boxes <- tidyr::crossing(x_cells, y_cells)

# ----------------------------
# 4. Plot
# ----------------------------
adoption_fig <- ggplot() +
  
  # light boxed background
  geom_rect(
    data = grid_boxes,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey96",
    color = "white",
    linewidth = 0.18
  ) +
  
  geom_ribbon(
    data = adoption_step,
    aes(x = x, ymin = 0, ymax = y),
    fill = "#7A6C8F",
    alpha = 0.88
  ) +
  
  geom_rect(
    data = grid_boxes,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA,
    color = "white",
    linewidth = 0.12
  ) +

  geom_rug(
    data = state_adoption_year,
    aes(x = adoption_year),
    sides = "b",
    alpha = 0.55,
    linewidth = 0.45
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 50, 10),
    minor_breaks = NULL,
    limits = c(0, 51),
    expand = c(0, 0),
    name = "Adopting jurisdictions"
  ) +
  
  scale_x_continuous(
    breaks = seq(2000, 2025, 5),
    minor_breaks = NULL,
    limits = c(2000, 2025.25),
    expand = c(0, 0),
    name = NULL
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_minimal(base_size = 11, base_family = "serif") +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    plot.margin = margin(t = 4, r = 6, b = 4, l = 6)
  )

ggsave(
  filename = "figures/adoption_trend.png",
  plot = adoption_fig,
  width = 7.0,
  height = 4.2,
  dpi = 400
)
