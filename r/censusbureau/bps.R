library(tidyverse)

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes


bps_years <- 2000:2022

header_rows <- read_csv("https://www2.census.gov/econ/bps/County/co2020a.txt", 
                        col_names = FALSE,
                        n_max = 2)

column_names <- header_rows |>
  select(X1:X18) |>
  t() |>
  as_tibble() |>
  mutate(group = rep(1:6, each = 3)) |>
  group_by(group) |>
  fill(V1, .direction = "updown") |>
  mutate(names = paste0(V1, ": ", V2)) |>
  pull(names)

library(glue)

cbps_raw <- map_df(bps_years, ~{
  raw <- read_csv(glue("https://www2.census.gov/econ/bps/County/co{.x}a.txt"), skip = 2,
                  col_names = FALSE) |>
    select(X1:X18) |>
    set_names(column_names)
  
  raw
  
})


# Read in latest 2023 cumulative data (thru July)

cbps_ytd <- read_csv("https://www2.census.gov/econ/bps/County/co2307y.txt", 
                     col_names = FALSE,
                     skip = 2) |> 
  select(X1:X18) |> 
  set_names(column_names) |> 
  mutate(`Survey: Date` = 2023)


cbps_data <- cbps_raw |> 
  bind_rows(cbps_ytd) |> 
  mutate(year = `Survey: Date`,
         GEOID = paste0(`FIPS: State`, `FIPS: County`)) |>
  select(`1-unit: Bldgs`:GEOID) |>
  filter(GEOID %in% cv) |>
  pivot_longer(`1-unit: Bldgs`:`5+ units: Value`,
               names_to = "type",
               values_to = "value") |>
  separate(type, into = c("Type", "col"), sep = ": ") |>
  pivot_wider(names_from = col,
              values_from = value) |>
  rename_with(tolower, Type:Value) |> 
  select(GEOID, year, type:units)

cv_cbps <- cbps_data |> 
  mutate(GEOID = case_when(
    GEOID == "51515" ~ "51019",
    TRUE ~ GEOID)) |> 
  left_join(counties("VA", year = 2021), by = "GEOID") |> 
  select(GEOID, NAMELSAD, year, type, units) |> 
  group_by(GEOID, NAMELSAD, year, type) |> 
  summarise(units = sum(units))


write_rds(cv_cbps, "data/cv_cbps.rds")