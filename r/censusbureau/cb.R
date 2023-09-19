library(tidyverse)
library(tidycensus)

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes


years <- 2010:2021

b25106_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25106")

b25106_raw <- map_dfr(years, function(yr) {
  b25042_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25106",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25106_raw <- b25106_raw |> 
  subset(GEOID %in% cv)

b25106_vars_cleaned <- b25106_vars |> 
  separate(label, into = c("est", "total", "tenure", "income", "cb"), sep = "!!") |> 
  select(variable = name, tenure, income, cb) |> 
  drop_na() |> 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) |> 
  mutate(tenure = case_when(
    tenure == "Owner-occupied housing units" ~ "Homeowner",
    tenure == "Renter-occupied housing units" ~ "Renter"),
    cb = case_when(
      cb == "30 percent or more" ~ "Cost-burdened",
      TRUE ~ "Not cost-burdened"))

b25106_raw <- b25106_raw |> 
  right_join(b25106_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, income, cb, estimate)

b25106_data <- b25106_raw |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         NAME = case_when(
           NAME == "Bedford city" ~ "Bedford County", # Aggregate Bedford City and Bedford County
           TRUE ~ NAME)) |>
  select(NAME, GEOID, year, tenure, income, cb, estimate) |> 
  group_by(NAME, GEOID, year, tenure, income, cb) |> 
  summarise(estimate = sum(estimate))


write_rds(b25106_data, "data/b25106_data.rds")

```