library(tidycensus)
library(tidyverse)

# Table B11001: Household type (including Living Alone)

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes


b11001_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 7) %in% "B11001_")

b11001_raw <- map_dfr(years, function(yr){
  b11001_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B11001",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |>
    mutate(year = yr)
})

b11001_raw <- b11001_raw |> 
  subset(GEOID %in% cv)

b11001_vars_cleaned <- b11001_vars |> 
  separate(label, into = c("est", "tot", "type", "relationship", "householder"),
           sep = "!!") |>
  select(variable = name, type, relationship, householder) |> 
  mutate(
    householder = case_when(
      relationship == "Married-couple family" ~ relationship,
      relationship == "Householder living alone" ~ relationship,
      relationship == "Householder not living alone" ~ relationship,
      TRUE ~ householder),
    relationship = case_when(
      relationship == "Householder living alone" ~ type,
      relationship == "Householder not living alone" ~ type,
      TRUE ~ relationship)
  ) |> 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) |> 
  drop_na()

b11001_data <- b11001_raw |>
  right_join(b11001_vars_cleaned, by = "variable") |>
  select(NAME, GEOID, year, "hhtype" = householder, relationship, type, estimate) |>
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         NAME = case_when(
           NAME == "Bedford city" ~ "Bedford County", # Aggregate Bedford City and Bedford County
           TRUE ~ NAME))

write_rds(b11001_data, "data/b11001_data.rds")
