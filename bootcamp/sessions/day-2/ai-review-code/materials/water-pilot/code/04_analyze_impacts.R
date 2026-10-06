# Project: Water conservation pilot
# Purpose: Merge survey rounds and estimate preliminary treatment effects

rm(list = ls())

library(dplyr)
library(readr)
library(fixest)
library(ggplot2)

project <- "C:/Users/jdoe/Documents/water-pilot"

analysis <- readRDS(file.path(project, "data/analysis_working.rds"))
endline <- readRDS(file.path(project, "data/endline_clean.rds"))
analysis <- merge(analysis, endline, by = "district_id", all = TRUE)

analysis$attrited <- as.integer(is.na(analysis$endline_productivity))

attrition <- analysis |>
  group_by(treatment) |>
  summarise(
    assigned = sum(!is.na(baseline_productivity)),
    attrition_rate = mean(attrited)
  )
write_csv(attrition, file.path(project, "outputs/attrition_by_treatment.csv"))

analysis <- analysis |> filter(!is.na(endline_productivity))

analysis <- analysis |>
  mutate(
    productivity_change = endline_productivity - baseline_productivity,
    water_use_change = endline_water_use - baseline_water_use,
    piped_water_change = endline_piped_water_share - piped_water_share
  )

summary(analysis[, c("baseline_productivity", "endline_productivity",
                     "productivity_change")])
analysis |>
  group_by(treatment) |>
  summarise(across(c(productivity_change, water_use_change),
                   list(n = length, mean = mean, sd = sd)))

outcome_means <- analysis |>
  group_by(treatment) |>
  summarise(
    observations = n(),
    across(c(baseline_productivity, endline_productivity, productivity_change,
             baseline_water_use, endline_water_use, water_use_change), mean)
  )
write_csv(outcome_means, file.path(project, "outputs/outcome_means.csv"))

m1 <- feols(productivity_change ~ treatment, data = analysis, vcov = "hetero")
m2 <- feols(endline_productivity ~ treatment + baseline_productivity + zone,
            data = analysis, vcov = "hetero")

ggplot(analysis, aes(x = factor(treatment), y = productivity_change)) +
  stat_summary(fun = mean, geom = "col") +
  labs(x = "treatment", y = "Mean change in productivity",
       title = "Productivity change by treatment status") +
  theme_minimal()

results <- data.frame(
  model = c("Unadjusted", "Adjusted"),
  treatment_effect = c(coef(m1)["treatment"], coef(m2)["treatment"]),
  standard_error = c(se(m1)["treatment"], se(m2)["treatment"])
)

write_csv(results, file.path(project, "outputs/regression_results.csv"))
