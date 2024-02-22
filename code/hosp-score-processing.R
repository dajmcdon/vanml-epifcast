scores_summary <- read_rds("data/COVIDhub-hosp-scores-summary.rds")
# data comes from `smooth-multi-period/download-hosp-scores.R`
selected <- c("COVIDhub-4_week_ensemble", "CMU-TimeSeries", "Smooth-QMPF")

dl_models <- c(
  "GT-DeepCOVID",
  "CUB_PopCouncil-SLSTM",
  "Google_Harvard-CPF"
)
scores_summary <- scores_summary |>
  mutate(
    anon = case_when(
      forecaster %in% selected ~ forecaster,
      forecaster %in% dl_models ~ "Deep learners",
      TRUE ~ "other"),
    anon = case_when(
      anon == "COVIDhub-4_week_ensemble" ~ "CDC Ensemble",
      anon == "Smooth-QMPF" ~ "Smooth TS",
      anon == "CMU-TimeSeries" ~ "CMU TS",
      TRUE ~ anon)
  )
cols <- c(
  "other" = "grey",
  "Deep learners" = tertiary,
  "CDC Ensemble" = primary,
  "CMU TS" = fourth_colour,
  "Smooth TS" = secondary
)
