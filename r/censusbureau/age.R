library(tidycensus)
library(tidyverse)

# Table B25007: Tenure by Age of Householder

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes


b25007_vars <- load_variables(2021, "acs5") |>
  filter(str_sub(name, end = 6) %in% "B25007")

b25007_raw <- map_dfr(years, function(yr){
  b25007_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25007",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |>
    mutate(year = yr)
})

b25007_raw <- b25007_raw |>
  subset(GEOID %in% cv)

b25007_vars_cleaned <- b25007_vars |>
  separate(label, into = c("est", "total", "tenure", "age"), sep = "!!") |>
  select(variable = name, tenure, age) |>
  drop_na() |>
  mutate(tenure = case_when(
    tenure == "Owner occupied:" ~ "Homeowner",
    tenure == "Renter occupied:" ~ "Renter"
  ))

b25007_data <- b25007_raw |>
  right_join(b25007_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, age, estimate, moe) |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         NAME = case_when(
           NAME == "Bedford city" ~ "Bedford County", # Aggregate Bedford City and Bedford County
           TRUE ~ NAME),
         age = case_when(
           age == "Householder 15 to 24 years" ~ "24 years and under",
           age == "Householder 25 to 34 years" ~ "25 to 44 years old",
           age == "Householder 35 to 44 years" ~ "25 to 44 years old",
           age == "Householder 45 to 54 years" ~ "45 to 64 years old",
           age == "Householder 55 to 59 years" ~ "45 to 64 years old",
           age == "Householder 60 to 64 years" ~ "45 to 64 years old",
           age == "Householder 65 to 74 years" ~ "65 years and over",
           age == "Householder 75 to 84 years" ~ "65 years and over",
           age == "Householder 85 years and over" ~ "65 years and over"
         ))

write_rds(b25007_data, "data/b25007_data.rds")