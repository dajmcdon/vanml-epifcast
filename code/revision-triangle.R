
library(tidyverse)
if (!file.exists("data/dv-cli.rds")) {
  library(epidatr)
  dv_cli <- pub_covidcast(
    source = "doctor-visits",
    signals = "smoothed_cli",
    geo_type = "state",
    time_type = "day",
    geo_values = "wa",
    time_values = epirange(20211201, 20220201),
    issues = epirange(20211201, 20220201)
  ) |>
    select(time_value, value, issue)
  
  dv_cli_finalized <- pub_covidcast(
    source = "doctor-visits",
    signals = "smoothed_cli",
    geo_type = "state",
    time_type = "day",
    geo_values = "wa",
    time_values = epirange(20211201, 20220201),
    as_of = "2022-02-01"
  ) |>
    select(time_value, final_value = value)
  
  dv_cli <- left_join(dv_cli, dv_cli_finalized, by = "time_value")
  
  write_rds(dv_cli, "data/dv-cli.rds")
}

dv_cli <- read_rds("data/dv-cli.rds")

p1 <- dv_cli |>
  filter(time_value > ymd("2021-12-31"), issue >= "2021-12-31") |>
  ggplot(aes(time_value, issue, fill = value / final_value * 100)) +
  geom_tile() +
  scale_x_date(expand = expansion(), name = "Date") +
  scale_y_date(expand = expansion(), name = "Report date") +
  scale_fill_viridis_c(
    name = "% of final value", 
    option = "B",
    direction = -1
  ) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))

p2 <- dv_cli |>
  filter(issue >= "2021-12-31", time_value > ymd("2021-12-31")) |>
  ggplot(aes(time_value)) +
  geom_line(aes(y = value, colour = issue, group = issue)) +
  scale_x_date(expand = expansion(), name = "Date") +
  scale_y_continuous(
    expand = expansion(), 
    name = "% Outpatient visits due to CLI",
  ) +
  scale_colour_viridis_c(
    name = "Report Date", 
    direction = -1,
    trans = "date",
    labels = scales::label_date("%b %d"),
    option = "B"
  ) +
  geom_line(aes(y = final_value), color = "black") +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
