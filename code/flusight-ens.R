us <- epidatr::pub_covidcast(
  "hhs", "confirmed_admissions_influenza_1d",
  "nation", "day",
  geo_values = "*",
  time_values = epidatr::epirange(20231001, 20240221)
) |>
  as_epi_df() |>
  epi_slide(value = sum(value), before = 7L)

us_as_of <- epidatr::pub_covidcast(
  "hhs", "confirmed_admissions_influenza_1d",
  "nation", "day",
  geo_values = "*",
  time_values = epidatr::epirange(20231001, 20240221),
  as_of = 20240106
) |>
  as_epi_df() |>
  epi_slide(value = sum(value), before = 7L)

ens <- read_csv("data/2024-01-06-FluSight-ensemble.csv") |>
  filter(location == "US", output_type == "quantile") |>
  mutate(quantile_values = as.numeric(output_type_id)) |>
  select(forecast_date = reference_date, geo_value = location,
         ahead = horizon, target_date = target_end_date, quantile_values, value) |>
  group_by(forecast_date, target_date, ahead) |>
  nest(qv = quantile_values, v = value) |>
  ungroup() |>
  rowwise() |>
  mutate(qv = list(qv$quantile_values), v = list(v$value),
         .pred_distn = dist_quantiles(v, qv),
         .pred = median(.pred_distn)) |>
  select(forecast_date, target_date, ahead, .pred_distn, .pred) |>
  ungroup()
