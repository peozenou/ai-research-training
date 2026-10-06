# Project: Water conservation pilot
# Purpose: Clean the endline district survey

rm(list = ls())

library(dplyr)
library(readr)

project <- "C:/Users/jdoe/Documents/water-pilot"

endline <- read_csv(file.path(project, "data/raw/endline.csv"))

endline <- endline |>
  filter(!is.na(district_id)) |>
  mutate(endline_date = as.Date(survey_date)) |>
  select(-survey_date)

sum(duplicated(endline$district_id))
endline <- endline |> distinct(district_id, .keep_all = TRUE)

endline <- endline |>
  mutate(piped_water_share = piped_water_share * 100) |>
  rename(
    endline_piped_water_share = piped_water_share,
    endline_households_surveyed = households_surveyed,
    endline_rainfall_mm = rainfall_mm
  ) |>
  mutate(
    endline_water_per_household =
      endline_water_use / endline_households_surveyed
  )

summary(endline[, c("endline_water_use", "endline_productivity", "endline_rainfall_mm")])
colSums(is.na(endline))

endline <- endline |>
  select(district_id, endline_date, endline_households_surveyed, everything()) |>
  arrange(district_id)

saveRDS(endline, file.path(project, "data/endline_clean.rds"))
