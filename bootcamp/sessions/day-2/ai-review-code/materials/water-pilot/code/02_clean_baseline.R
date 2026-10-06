# Project: Water conservation pilot
# Purpose: Clean baseline survey and attach treatment assignment

rm(list = ls())

library(dplyr)
library(readr)
library(haven)
library(fixest)

project <- "C:/Users/jdoe/Documents/water-pilot"

baseline <- read_csv(file.path(project, "data/raw/baseline.csv"))

baseline <- baseline |>
  filter(!is.na(district_id)) |>
  mutate(baseline_date = as.Date(survey_date)) |>
  select(-survey_date)

sum(duplicated(baseline$district_id))
baseline |> filter(district_id %in% district_id[duplicated(district_id)])
baseline <- baseline |> distinct(district_id, .keep_all = TRUE)

baseline <- baseline |>
  mutate(
    piped_water_share = piped_water_share * 100,
    water_per_household = baseline_water_use / households_surveyed,
    high_water_use = as.integer(baseline_water_use > 100)
  )

summary(baseline[, c("baseline_water_use", "baseline_productivity", "rainfall_mm")])
table(baseline$high_water_use)

# Treatment assignment comes from 01_randomize_treatment.do
assignment <- read_dta(file.path(project, "data/analysis_working.dta"))
baseline <- merge(baseline, assignment, by = "district_id")

prop.table(table(baseline$zone, baseline$treatment), 1)

feols(baseline_water_use ~ treatment, data = baseline, vcov = "hetero")
feols(baseline_productivity ~ treatment, data = baseline, vcov = "hetero")
feols(piped_water_share ~ treatment, data = baseline, vcov = "hetero")

baseline_means <- baseline |>
  group_by(treatment) |>
  summarise(
    observations = sum(!is.na(baseline_productivity)),
    across(c(baseline_water_use, baseline_productivity, piped_water_share,
             water_per_household, rainfall_mm), mean)
  )
write_csv(baseline_means, file.path(project, "outputs/baseline_means.csv"))

baseline <- baseline |>
  select(district_id, district_name, zone, district_type, treatment,
         baseline_date, everything()) |>
  arrange(district_id)

saveRDS(baseline, file.path(project, "data/analysis_working.rds"))
