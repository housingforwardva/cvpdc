library(tidycensus)
library(tidyverse)

# B18105 Ambulatory Difficulty

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes

years <- 2012:2021 

b18105_vars <- load_variables(2021, "acs5") |>
  filter(str_sub(name, end = 6) %in% "B18105")

b18105_raw <- map_dfr(years, function(yr){
  b18105_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B18105",
    year = yr,
    survey = "acs5"
  ) |>
    mutate(year = yr)
})

b18105_raw <- b18105_raw |>
  subset(GEOID %in% cv)

b18105_vars_clean <- b18105_vars |> 
  separate(label, into = c("est", "total", "sex", "age", "ad"), sep = "!!") |> 
  select(variable = name, sex, age, ad) |> 
  drop_na() |> 
  mutate(across(.fns = ~str_remove_all(.x, ":")))

b18105_data <- b18105_raw  |> 
  right_join(b18105_vars_clean, by = "variable") |> 
  mutate(age_grp = case_when(
    age == "65 to 74 years" ~ "65 years and over",
    age == "75 years and over" ~ "65 years and over",
    TRUE ~ "Under 65 years old"
  )) |> 
  group_by(GEOID, NAME, year, age_grp, ad) |> 
  summarise(estimate = sum(estimate)) |>
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         NAME = case_when(
           NAME == "Bedford city" ~ "Bedford County", # Aggregate Bedford City and Bedford County
           TRUE ~ NAME
         ))

write_rds(b18105_data, "data/b18105_data.rds")

# B18107 Independent Living Difficulty

years <- 2012:2021 

b18107_vars <- load_variables(2021, "acs5") |>
  filter(str_sub(name, end = 6) %in% "B18107")

b18107_raw <- map_dfr(years, function(yr){
  b18107_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B18107",
    year = yr,
    survey = "acs5"
  ) |>
    mutate(year = yr)
})

b18107_raw <- b18107_raw |>
  subset(GEOID %in% cv)

b18107_vars_clean <- b18107_vars |> 
  separate(label, into = c("est", "total", "sex", "age", "id"), sep = "!!") |> 
  select(variable = name, sex, age, id) |> 
  drop_na() |> 
  mutate(across(.fns = ~str_remove_all(.x, ":")))

b18107_data <- b18107_raw  |> 
  right_join(b18107_vars_clean, by = "variable") |> 
  mutate(age_grp = case_when(
    age == "65 to 74 years" ~ "65 years and over",
    age == "75 years and over" ~ "65 years and over",
    TRUE ~ "Under 65 years old"
  )) |>
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         NAME = case_when(
           NAME == "Bedford city" ~ "Bedford County", # Aggregate Bedford City and Bedford County
           TRUE ~ NAME
         )) |> 
  group_by(GEOID, NAME, year, age_grp, id) |> 
  summarise(estimate = sum(estimate))

write_rds(b18107_data, "data/b18107_data.rds")
