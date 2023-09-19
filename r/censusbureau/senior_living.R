library(tidycensus)
library(tidyverse)

# Table B09020: Senior population by living arrangements

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes


years <- 2012:2021 # 5-year estimates only go back to 2012

b09020_vars <- load_variables(2021, "acs5") |>
  filter(str_sub(name, end = 6) %in% "B09020")

b09020_raw <- map_dfr(years, function(yr){
  b09020_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B09020",
    year = yr,
    survey = "acs5"
  ) |>
    mutate(year = yr)
})

b09020_raw <- b09020_raw |>
  subset(GEOID %in% cv)

b09020_vars_cleaned <- b09020_vars |>
  separate(label, into = c("est", "total", "hhgq", "family", "relationship", "sex", "alone"), sep = "!!") |>
  select(variable = name, hhgq, family, relationship, alone) |>
  filter(!variable %in% c("B09020_001", "B09020_002", "B09020_003", "B09020_005", "B09020_006",
                          "B09020_012", "B09020_013", "B09020_014", "B09020_017")) |>
  mutate(relationship = case_when(
    relationship == "Spouse" ~ "With spouse",
    relationship == "Nonrelatives" ~ "With nonrelatives",
    relationship %in% c("Parent", "Parent-in-law", "Other relatives") ~ "With other relative(s)",
    hhgq == "In group quarters" ~ "Group quarters",
    !is.na(alone) ~ alone,
    TRUE ~ relationship
  )) |>
  mutate(family = case_when(
    family == "In family households:" ~ "Family",
    family == "In nonfamily households:" ~ "Nonfamily",
    hhgq == "In group quarters" ~ "Group quarters"
  )) |>
  mutate(across(.fns = ~str_remove_all(.x, ":"))) |>
  select(1,3,4)

b09020_data <- b09020_raw |>
  right_join(b09020_vars_cleaned, by = "variable") |>
  select(NAME, GEOID, year, family, relationship, estimate) |>
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         NAME = case_when(
           NAME == "Bedford city" ~ "Bedford County", # Aggregate Bedford City and Bedford County
           TRUE ~ NAME))

write_rds(b09020_data, "data/b09020_data.rds")
