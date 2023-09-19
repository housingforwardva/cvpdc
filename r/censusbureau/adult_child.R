library(tidycensus)
library(tidyverse)

years <- 2015:2021

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes


b09021_vars <- load_variables(2021, "acs5") |>
  filter(str_sub(name, end = 6) %in% "B09021")

b09021_raw <- map_dfr(years, function(yr){
  b09021_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B09021",
    year = yr,
    survey = "acs5"
  ) |>
    mutate(year = yr)
})

b09021_raw <- b09021_raw |>
  subset(GEOID %in% cv)

b09021_vars_cleaned <- b09021_vars |>
  filter(name %in% c("B09021_009", "B09021_010", "B09021_011",
                     "B09021_012", "B09021_013", "B09021_014")) |>
  separate(label, into = c("est", "total", "age", "arrangement"), sep = "!!") |>
  select(variable = name, arrangement) |>
  mutate(arrangement = case_when(
    arrangement == "Householder living with spouse or spouse of householder" ~ "With spouse",
    arrangement == "Householder living with unmarried partner or unmarried partner of householder" ~ "With partner",
    TRUE ~ arrangement
  ))

b09021_data <- b09021_raw |>
  right_join(b09021_vars_cleaned, by = "variable") |>
  select(NAME, GEOID, year, arrangement, estimate) |>
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         NAME = case_when(
           NAME == "Bedford city" ~ "Bedford County", # Aggregate Bedford City and Bedford County
           TRUE ~ NAME)) 


write_rds(b09021_data, "data/b09021_data.rds")