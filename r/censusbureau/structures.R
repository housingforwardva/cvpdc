library(tidyverse)
library(tidycensus)

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes

# Table B25127: Tenure by year structure built by units in structure

years <- 2010:2021

b25127_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25127")

b25127_raw <- map_dfr(years, function(yr){
  b25127_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25127",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25127_raw <- b25127_raw |> 
  subset(GEOID %in% cv) 

b25127_vars_cleaned <- b25127_vars |> 
  separate(label, into = c("est", "total", "tenure", "yrbuilt", "structure"), sep = "!!") |> 
  select(variable = name, tenure, yrbuilt, structure) |> 
  drop_na() |> 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  ))

b25127_raw <- b25127_raw |> 
  right_join(b25127_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, yrbuilt, structure, estimate, moe)

b25127_data <- b25127_raw |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         yrbuilt = str_remove_all(yrbuilt, "Built "),
         NAME = case_when(
           NAME == "Bedford city" ~ "Bedford County", # Aggregate Bedford City and Bedford County
           TRUE ~ NAME)) |>
  select(NAME, GEOID, year, tenure, yrbuilt, structure, estimate) |> 
  mutate(structure = case_when(
    structure == "1, detached  or attached" ~ "Single-family",
    structure == "2 to 4" ~ "2 to 4 units",
    structure == "5 to 19" ~ "5 to 19 units",
    structure == "20 to 49" ~ "20 or more units",
    structure == "50 or more" ~ "20 or more units",
    TRUE ~ "Other"
  )) 

write_rds(b25127_data, "data/b25127_data.rds")
