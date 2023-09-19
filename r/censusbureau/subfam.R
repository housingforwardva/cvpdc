library(tidycensus)
library(tidyverse)

# Table B11013: Subfamily type by presence of own children

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes


years <- 2010:2021

b11013_vars <- load_variables(2021, "acs5") |>
  filter(str_sub(name, end = 6) %in% "B11013")

b11013_raw <- map_dfr(years, function(yr){
  b11013_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B11013",
    year = yr,
    survey = "acs5"
  ) |>
    mutate(year = yr)
})

b11013_raw <- b11013_raw |>
  subset(GEOID %in% cv)

b11013_vars_cleaned <- b11013_vars |>
  separate(label, into = c("est", "total", "subfamily", "children"), sep = "!!") |>
  select(variable = name, subfamily, children) |>
  filter(!variable %in% c("B11013_001", "B11013_002")) |>
  mutate(subfamily = case_when(
    subfamily == "Married-couple subfamily:" ~ "Married couple",
    subfamily == "Mother-child subfamily" ~ "Single mother",
    subfamily == "Father-child subfamily" ~ "Single father")) |>
  mutate(children = case_when(
    children == "With own children under 18 years" ~ "With children",
    children == "No own children under 18 years" ~ "No children",
    TRUE ~ "With children"))

b11013_data <- b11013_raw |>
  right_join(b11013_vars_cleaned, by = "variable") |>
  select(NAME, GEOID, year, subfamily, children, estimate) |>
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         NAME = case_when(
           NAME == "Bedford city" ~ "Bedford County", # Aggregate Bedford City and Bedford County
           TRUE ~ NAME))

write_rds(b11013_data, "data/b11013_data.rds")