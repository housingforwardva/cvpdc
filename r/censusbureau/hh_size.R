library(tidycensus)
library(tidyverse)

# Table B25009: Household size by tenure

b25009_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25009")

b25009_raw <- map_dfr(years, function(yr){
  b25009_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25009",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |>
    mutate(year = yr)
})

b25009_raw <- b25009_raw |> 
  subset(GEOID %in% cv)

b25009_vars_cleaned <- b25009_vars |> 
  separate(label, into = c("est", "total", "tenure", "size"), sep = "!!") |> 
  select(variable = name, tenure, size)  |> 
  drop_na() |>
  mutate(tenure = case_when(
    tenure == "Owner occupied:" ~ "Homeowner",
    tenure == "Renter occupied:" ~ "Renter"
  ))

b25009_data <- b25009_raw |> 
  right_join(b25009_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, size, estimate) |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         NAME = case_when(
           NAME == "Bedford city" ~ "Bedford County", # Aggregate Bedford City and Bedford County
           TRUE ~ NAME)) |> 
  mutate(size = case_when(
    size == "1-person household" ~ "1-person",
    size == "2-person household" ~ "2-person",
    size == "3-person household" ~ "3-person",
    TRUE ~ "4 or more persons"
  )) 

write_rds(b25009_data, "data/b25009_data.rds")

```