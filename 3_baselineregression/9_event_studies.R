#####################################################################################
# Event Studies: Strangulation Laws and Reported IPV Strangulation Rate and IPH
#####################################################################################

library(tidyverse)
library(readr)
library(fixest)

setwd("~/Desktop/thesis/3_baselineregression")

data <- read_csv("data/violence_laws_with_controls.csv") |>
  filter(year < 2021) |>
  mutate(
    log_strang_rate = log(strang_rate + 0.1)
  )



## reported strangulation -------------

data <- data |>
  mutate(
    strang_rate = ifelse(strang_rate < 0, NA, strang_rate),
    log_strang_rate = log(strang_rate + 0.1)
  )

es_mod_binned <- feols(
  log_strang_rate ~ sunab(Law, year) + poverty_rate + unemp_total + unemployment_gap + male_assault_rate| state + year,
  data = data,
  cluster = ~state
)

summary(es_mod_binned)


es_df <- broom::tidy(es_mod_binned) |>
  filter(grepl("^year::", term)) |>
  mutate(
    event_time = as.numeric(sub("year::", "", term)),
    conf_low   = estimate - 1.96 * std.error,
    conf_high  = estimate + 1.96 * std.error,
    post       = event_time >= 0
  ) |>
  filter(event_time >= -5, event_time <= 8)


es_df <- es_df |>
  bind_rows(
    data.frame(
      term = "baseline",
      estimate = 0,
      std.error = NA,
      conf_low = 0,
      conf_high = 0,
      event_time = -1
    )
  ) |>
  arrange(event_time)

es_df_ribbon <- es_df |>
  bind_rows(
    es_df |>
      filter(event_time == max(event_time)) |>
      mutate(event_time = 8.5)
  )

strang_p <-ggplot() +
  
  # ribbon (extended)
  geom_ribbon(
    data = es_df_ribbon,
    aes(x = event_time, ymin = conf_low, ymax = conf_high),
    fill = "#B7A7D6", alpha = 0.35
  ) +
  
  # line (original data only)
  geom_line(
    data = es_df_ribbon,
    aes(x = event_time, y = estimate),
    color = "#6F5A8A",
    linewidth = 1.2
  ) +
  
  # points (original data only)
  geom_point(
    data = es_df,
    aes(x = event_time, y = estimate),
    color = "#6F5A8A",
    size = 2.5
  ) +
  
  # reference lines
  geom_hline(yintercept = 0, color = "gray30", linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  
  scale_x_continuous(
    breaks = -5:8,
    limits = c(-5, 8.5),
    expand = c(0, 0)
  ) +
  
  coord_cartesian(ylim = c(-2, 2)) +
  
  labs(
    x = "Years Relative to Law Adoption",
    y = "Log Change in Strangulation Rate (per 100,000)"
  ) +
  
  theme_classic(base_size = 13, base_family = "serif") + 
  
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11, color = "black"),
    
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    
    panel.grid = element_blank(),
    
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  filename = "Figures/event_study_strang.png",
  plot = strang_p,
  width = 6.5,
  height = 4.5,
  dpi = 300
)

es_df <- es_df |>
  bind_rows(
    data.frame(
      term = "baseline",
      estimate = 0,
      std.error = NA,
      conf_low = 0,
      conf_high = 0,
      event_time = -1
    )
  ) |>
  arrange(event_time)

es_table <- es_df |>
  filter(event_time != -1) |>  
  mutate(
    term_label = case_when(
      event_time < 0 ~ paste0("Lead ", abs(event_time)),
      event_time == 0 ~ "Event year",
      event_time > 0 ~ paste0("Lag ", event_time)
    ),
    stars = case_when(
      is.na(std.error) ~ "",
      2 * pnorm(-abs(estimate / std.error)) < 0.01 ~ "***",
      2 * pnorm(-abs(estimate / std.error)) < 0.05 ~ "**",
      2 * pnorm(-abs(estimate / std.error)) < 0.10 ~ "*",
      TRUE ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    se_fmt = ifelse(is.na(std.error), "", sprintf("(%.3f)", std.error))
  ) |>
  select(term_label, estimate_fmt, se_fmt)

# add omitted baseline row back in for display
es_table <- bind_rows(
  es_table |> filter(str_detect(term_label, "Lead")),
  tibble(
    term_label = "Baseline: t = -1",
    estimate_fmt = "Omitted",
    se_fmt = ""
  ),
  es_table |> filter(!str_detect(term_label, "Lead") & term_label != "Baseline: t = -1")
)

library(kableExtra)

es_latex <- kable(
  es_table,
  format = "latex",
  booktabs = TRUE,
  align = c("l", "c", "c"),
  col.names = c("", "Estimate", "Std. Error"),
  caption = "Event Study Estimates for the Effect of Strangulation Laws",
  escape = FALSE
) |>
  kable_styling(latex_options = c("hold_position")) |>
  add_header_above(c(" " = 1, "Dependent variable: log(Strangulation rate + 0.1)" = 2)) |>
  footnote(
    general = "Entries report event-time coefficients from a Sun and Abraham (2021) event-study specification. The omitted category is one year prior to adoption (t = -1). Regressions include state and year fixed effects, economic controls, gender equality controls, and male assault rate. Standard errors are clustered at the state level. *** p<0.01, ** p<0.05, * p<0.10.",
    threeparttable = TRUE
  )

cat(es_latex)

## iph------------------



data <- data |>
  group_by(year) |>
  mutate(
    median_male_iph = median(malemale_homicide_rate, na.rm = TRUE),
    high_male_iph = as.integer(malemale_homicide_rate > median_male_iph)
  ) |>
  ungroup()

iph_levels <- feols(
  log(iph_rate + 0.1)  ~ sunab(Law, year) + poverty_rate + unemp_total + unemployment_gap + high_male_iph
  | state + year,
  data = data,
  cluster = ~state, 
  weights = ~census_population
)

summary(iph_levels)

iplot(
  iph_levels,
  ref.line = 0,
  xlab = "Years relative to law adoption",
  ylab = "Effect on IPH rate (per 100,000)",
  main = "Event Study: Strangulation Laws on Intimate Partner Homicide",
  xlim = c(-5, 8),
  ylim = c(-0.15,0.15) 
)

es_df <- broom::tidy(iph_levels) |>
  filter(grepl("^year::", term)) |>
  mutate(
    event_time = as.numeric(sub("year::", "", term)),
    conf_low   = estimate - 1.96 * std.error,
    conf_high  = estimate + 1.96 * std.error,
    post       = event_time >= 0
  ) |>
  filter(event_time >= -5, event_time <= 8)


es_df <- es_df |>
  bind_rows(
    data.frame(
      term = "baseline",
      estimate = 0,
      std.error = NA,
      conf_low = 0,
      conf_high = 0,
      event_time = -1
    )
  ) |>
  arrange(event_time)

es_df_ribbon <- es_df |>
  bind_rows(
    es_df |>
      filter(event_time == max(event_time)) |>
      mutate(event_time = 8.5)
  )

iph_p <- ggplot() +
  
  # ribbon (extended)
  geom_ribbon(
    data = es_df_ribbon,
    aes(x = event_time, ymin = conf_low, ymax = conf_high),
    fill = "#B7A7D6", alpha = 0.35
  ) +
  
  # line (original data only)
  geom_line(
    data = es_df_ribbon,
    aes(x = event_time, y = estimate),
    color = "#6F5A8A",
    linewidth = 1.2
  ) +
  
  # points (original data only)
  geom_point(
    data = es_df,
    aes(x = event_time, y = estimate),
    color = "#6F5A8A",
    size = 2.5
  ) +
  
  # reference lines
  geom_hline(yintercept = 0, color = "gray30", linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  
  scale_x_continuous(
    breaks = -5:8,
    limits = c(-5, 5.5),
    expand = c(0, 0)
  ) +
  
  coord_cartesian(ylim = c(-0.3, 0.3)) +
  
  labs(
    x = "Years Relative to Law Adoption",
    y = "Log change in IP Homicide Rate"
  ) +
  
  theme_classic(base_size = 13, base_family = "serif") + 
  
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11, color = "black"),
    
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    
    panel.grid = element_blank(),
    
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  filename = "Figures/event_study_iph.png",
  plot = p,
  width = 6.5,
  height = 4.5,
  dpi = 300
)

iph_table <- es_df |>
  filter(event_time != -1) |>  
  mutate(
    term_label = case_when(
      event_time < 0 ~ paste0("Lead ", abs(event_time)),
      event_time == 0 ~ "Event year",
      event_time > 0 ~ paste0("Lag ", event_time)
    ),
    stars = case_when(
      is.na(std.error) ~ "",
      2 * pnorm(-abs(estimate / std.error)) < 0.01 ~ "***",
      2 * pnorm(-abs(estimate / std.error)) < 0.05 ~ "**",
      2 * pnorm(-abs(estimate / std.error)) < 0.10 ~ "*",
      TRUE ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    se_fmt = ifelse(is.na(std.error), "", sprintf("(%.3f)", std.error))
  ) |>
  select(term_label, estimate_fmt, se_fmt)

iph_table <- bind_rows(
  iph_table |> filter(str_detect(term_label, "Lead")),
  tibble(
    term_label = "Baseline: t = -1",
    estimate_fmt = "Omitted",
    se_fmt = ""
  ),
  iph_table |> filter(!str_detect(term_label, "Lead"))
)


iph_latex <- kable(
  iph_table,
  format = "latex",
  booktabs = TRUE,
  align = c("l", "c", "c"),
  col.names = c("", "Estimate", "Std. Error"),
  caption = "Event Study Estimates for the Effect of Strangulation Laws on Intimate Partner Homicide",
  escape = FALSE
) |>
  kable_styling(latex_options = c("hold_position")) |>
  add_header_above(c(" " = 1, "Dependent variable: $\\log(\\text{IPH rate}_{st} + 0.1)$" = 2)) |>
  footnote(
    general = "Entries report event-time coefficients from a Sun and Abraham (2021) event-study specification. The omitted category is one year prior to adoption (t = -1). Regressions include state and year fixed effects, economic controls, and an indicator for high male non-intimate partner homicide. Standard errors are clustered at the state level. *** p<0.01, ** p<0.05, * p<0.10.",
    threeparttable = TRUE
  )

cat(iph_latex)

