library(tidycensus)
library(tidyverse)

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes

years <- 2010:2021

b25042_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25042")

b25042_raw <- map_dfr(years, function(yr) {
  b25042_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25042",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25042_raw <- b25042_raw |> 
  subset(GEOID %in% cv)

b25042_vars_cleaned <- b25042_vars |> 
  separate(label, into = c("est", "total", "tenure", "br"), sep = "!!") |> 
  select(variable = name, tenure, br) |> 
  drop_na() |> 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  ))

b25042_raw <- b25042_raw |> 
  right_join(b25042_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, br, estimate)

b25042_data <- b25042_raw |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         NAME = case_when(
           NAME == "Bedford city" ~ "Bedford County", # Aggregate Bedford City and Bedford County
           TRUE ~ NAME)) |>
  select(NAME, GEOID, year, tenure, br, estimate) 


write_rds(b25042_data, "data/b25042_data.rds")
```
