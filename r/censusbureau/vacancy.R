library(tidycensus)
library(tidyverse)

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes



years <- 2010:2021

b25004_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25004") 

b25004_raw <- map_dfr(years, function(yr){
  b25004_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25004",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |>
    mutate(year = yr)
})

b25004_raw <- b25004_raw |> 
  subset(GEOID %in% cv)

b25004_vars_clean <- b25004_vars |> 
  separate(label, into = c("est", "total", "status"), sep = "!!") |> 
  select(variable = name, status)

b25004_data <- b25004_raw |> 
  left_join(b25004_vars_clean, by = "variable") |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         NAME = case_when(
           NAME == "Bedford city" ~ "Bedford County", # Aggregate Bedford City and Bedford County
           TRUE ~ NAME
         )) |> 
  select(GEOID, NAME, year, status, estimate) |> 
  drop_na()


write_rds(b25004_data, "data/b25004_data.rds")
