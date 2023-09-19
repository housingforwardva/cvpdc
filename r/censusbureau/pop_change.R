library(tidyverse)
library(tidycensus)

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes


pep_change_raw <- get_estimates(
  geography = "county",
  state = "VA",
  year = 2019,
  variables = c("NATURALINC", "DOMESTICMIG", "INTERNATIONALMIG"),
  time_series = TRUE
)

change_2020s <- get_estimates(
  geography = "county",
  state = "VA",
  year = 2022,
  variables = c("NATURALCHG", "DOMESTICMIG", "INTERNATIONALMIG"),
  time_series = TRUE
) |> 
  mutate(component = case_when(
    variable == "NATURALCHG" ~ "Natural increase",
    variable == "DOMESTICMIG" ~ "Domestic migration",
    variable == "INTERNATIONALMIG" ~ "International migration")) |>  
  select( # Simplify columns
    GEOID,
    year,
    component,
    value
  )

pep_change_clean <- pep_change_raw |>  
  mutate(GEOID = as.character(GEOID)) |>  
  filter(GEOID %in% cv) |> 
  mutate(year = # Translate date codes into years
           case_when(
             PERIOD == 1 ~ "2010",
             PERIOD == 2 ~ "2011",
             PERIOD == 3 ~ "2012",
             PERIOD == 4 ~ "2013",
             PERIOD == 5 ~ "2014",
             PERIOD == 6 ~ "2015",
             PERIOD == 7 ~ "2016",
             PERIOD == 8 ~ "2017",
             PERIOD == 9 ~ "2018",
             PERIOD == 10 ~ "2019")) |> 
  mutate(component = # Rename components of change
           case_when(
             variable == "NATURALINC" ~ "Natural increase",
             variable == "DOMESTICMIG" ~ "Domestic migration",
             variable == "INTERNATIONALMIG" ~ "International migration")) |>  
  select( # Simplify columns
    GEOID,
    year,
    component,
    value
  )

cv_comp_change <- pep_change_clean |> 
  rbind(change_2020s) |>
  filter(GEOID %in% cv) |>
  left_join(counties("VA", year = 2021), by = "GEOID") |>
  select(NAMELSAD, year, component, value)

write_rds(cv_comp_change, "data/comp_data.rds")