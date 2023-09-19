library(tidycensus)
library(tidyverse)

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes

years <- 2010:2021

b25118_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25118")

b25118_raw <- map_dfr(years, function(yr){
  b25118_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25118",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25118_raw <- b25118_raw |> 
  subset(GEOID %in% cv)

b25118_vars_cleaned <- b25118_vars |> 
  separate(label, into = c("est", "total", "tenure", "income"), sep = "!!") |>  
  select(variable = name, tenure, income) |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied:" ~ "Homeowner",
    tenure == "Renter occupied:" ~ "Renter"
  )) |> 
  drop_na()

b25118_data <- b25118_raw |> 
  right_join(b25118_vars_cleaned, by = "variable") |> 
  select(NAME, year, tenure, income, estimate) |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         NAME = case_when(
           NAME == "Bedford city" ~ "Bedford County", # Aggregate Bedford City and Bedford County
           TRUE ~ NAME)) |>
  mutate(income = case_when(
    income == "Less than $5,000" ~ "Less than $15,000",
    income == "$5,000 to $9,999" ~ "Less than $15,000",
    income == "$10,000 to $14,999" ~ "Less than $15,000",
    income == "$15,000 to $19,999" ~ "$15,000 to $24,999",
    income == "$20,000 to $24,999" ~ "$15,000 to $24,999",
    income == "$25,000 to $34,999" ~ "$25,000 to $49,999",
    income == "$35,000 to $49,999" ~ "$25,000 to $49,999",
    income == "$50,000 to $74,999" ~ "$50,000 to $74,999",
    TRUE ~ income
  ))

write_rds(b25118_data, "data/b25118_data.rds")

years <- 2010:2021 

b25119_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25119")

b25119_raw <- map_dfr(years, function(yr){
  b25119_pull <- get_acs(
    geography = "cbsa",
    table = "B25119",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |>
    mutate(year = yr)
})  


b25119_msa <- b25119_raw |> 
  filter(NAME == "Lynchburg, VA Metro Area")

b25119_vars_cleaned <- b25119_vars |> 
  separate(label, into = c("est", "income", "total", "tenure"), sep = "!!") |> 
  select(variable = name, tenure) |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied (dollars)" ~ "Homeowner",
    tenure == "Renter occupied (dollars)" ~ "Renter"
  ))

b25119_vars_cleaned$tenure <- b25119_vars_cleaned$tenure |>  
  replace_na('All households')

b25119_data <- b25119_msa |> 
  right_join(b25119_vars_cleaned, by = "variable") |> 
  select(NAME, year, tenure, estimate, moe)

library(readxl)

cpi <- read_excel("data/CPI_U_RS.xlsx")

cpi <- cpi |> 
  rename(year = Year,
         priceindex = Index) |> 
  transform(year = as.numeric(year))

b25119_cpi_msa <- b25119_data |> 
  left_join(cpi, by = 'year') |> 
  transform(dollars21 = ((399.2/priceindex)*estimate)) |> 
  select(NAME, year, tenure, dollars21, cdollars = estimate) |>
  filter(tenure != "All households") |> 
  select(year, NAME, tenure, dollars21, cdollars)

write_rds(b25119_cpi_msa, "data/b25119_cpi_msa.rds")

years <- 2010:2021 #Including Bedford City, but be mindful that you cannot aggregate medians.

b25119_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25119")

b25119_raw <- map_dfr(years, function(yr){
  b25119_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25119",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |>
    mutate(year = yr)
})

b25119_raw <- b25119_raw |> 
  subset(GEOID %in% cv)

b25119_vars_cleaned <- b25119_vars |> 
  separate(label, into = c("est", "income", "total", "tenure"), sep = "!!") |>  
  select(variable = name, tenure) |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied (dollars)" ~ "Homeowner",
    tenure == "Renter occupied (dollars)" ~ "Renter"
  ))

b25119_vars_cleaned$tenure <- b25119_vars_cleaned$tenure |>  replace_na('All households')
b25119_data <- b25119_raw |> 
  right_join(b25119_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, estimate, moe)


cpi <- read_excel("data/CPI_U_RS.xlsx")

cpi <- cpi |> 
  rename(year = Year,
         priceindex = Index) |> 
  transform(year = as.numeric(year))

b25119_cpi <- b25119_data |> 
  left_join(cpi, by = 'year') |> 
  transform(dollars21 = ((399.2/priceindex)*estimate)) |> 
  select(NAME, GEOID, year, tenure, dollars21, cdollars = estimate) |>
  filter(tenure != "All households") |> 
  select(year, NAME, tenure, dollars21, cdollars)

write_rds(b25119_cpi, "data/b25119_cpi.rds")

years <- 2010:2021 

b19013 <- paste0("B19013", LETTERS[2:9])

b19013_defns <- load_variables(2020, "acs5") |>
  filter(str_sub(name, end = 7) %in% b19013) |>
  filter(str_detect(name, "PR") == FALSE)

concept_to_race <- function(x) {
  out <- x |>
    str_remove_all("MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS \\(\\IN 2020 INFLATION-ADJUSTED DOLLARS\\)\\ \\(|\\)") |>
    str_to_title()
}

b19013_cleaned <- b19013_defns |>
  mutate(race = concept_to_race(concept)) |>
  separate(label, c("estimate", "medhhincome"), sep = "!!") |>
  select(variable = name, medhhincome, race) |>
  mutate(across(.fns = ~replace_na(.x, "All")),
         across(.fns = ~str_remove_all(.x, ":")))

b19013_raw_msa <- map_dfr(b19013, function(tb) {
  yearly_data <- map_dfr(years, function(yr) {
    acs_pull <- get_acs(
      geography = "cbsa",
      table = tb,
      year = yr
    ) |>
      left_join(b19013_cleaned, by = "variable")
    acs_rearranged <- acs_pull |>
      mutate(year = yr) |>
      select(variable, year, NAME, GEOID, race, medhhincome,
             estimate, moe)
    acs_rearranged
  })
  yearly_data
}) |> 
  filter(NAME == "Lynchburg, VA Metro Area")

cpi <- read_excel("data/CPI_U_RS.xlsx") |> 
  rename(year = Year,
         priceindex = Index) |> 
  transform(year = as.numeric(year))

b19013_msa <- b19013_raw_msa |> 
  mutate(across(.fns = ~str_remove_all(.x, "Alone Householder")),
         across(.fns = ~trimws(.x))) |> 
  select(year, NAME, race, estimate, moe) |> 
  transform(year = as.numeric(year),
            estimate = as.numeric(estimate))|> 
  left_join(cpi, by = 'year')|> 
  select(year, NAME, race, estimate, moe, priceindex) |> 
  transform(dollars21 = ((399.2/priceindex)*estimate)) |> 
  mutate(race = case_when(
    race == "Black Or African American" ~ "Black",
    race == "Two Or More Races Householder" ~ "Multiracial",
    race == "White Alone, Not Hispanic Or Latino Householder" ~ "White, non-Hispanic",
    race == "Hispanic Or Latino Householder" ~ "Hispanic or Latino",
    TRUE ~ race
  )) |> 
  filter(race %in% c("White, non-Hispanic", "Black", "Asian", "Multiracial", "Hispanic or Latino"))

write_rds(b19013_msa, "data/b19013_msa.rds")


years <- 2010:2021 

b19013 <- paste0("B19013", LETTERS[2:9])

b19013_defns <- load_variables(2020, "acs5") |>
  filter(str_sub(name, end = 7) %in% b19013) |>
  filter(str_detect(name, "PR") == FALSE)

concept_to_race <- function(x) {
  out <- x |>
    str_remove_all("MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS \\(\\IN 2020 INFLATION-ADJUSTED DOLLARS\\)\\ \\(|\\)") |>
    str_to_title()
}

b19013_cleaned <- b19013_defns |>
  mutate(race = concept_to_race(concept)) |>
  separate(label, c("estimate", "medhhincome"), sep = "!!") |>
  select(variable = name, medhhincome, race) |>
  mutate(across(.fns = ~replace_na(.x, "All")),
         across(.fns = ~str_remove_all(.x, ":")))

b19013_raw <- map_dfr(b19013, function(tb) {
  yearly_data <- map_dfr(years, function(yr) {
    acs_pull <- get_acs(
      geography = "county",
      table = tb,
      year = yr,
      state = "VA"
    ) |>
      left_join(b19013_cleaned, by = "variable")
    acs_rearranged <- acs_pull |>
      mutate(year = yr) |>
      select(variable, year, NAME, GEOID, race, medhhincome,
             estimate, moe)
    acs_rearranged
  })
  yearly_data
})

cpi <- read_excel("data/CPI_U_RS.xlsx") |> 
  rename(year = Year,
         priceindex = Index) |> 
  transform(year = as.numeric(year))

b19013_data <- b19013_raw |> 
  subset(GEOID %in% cv) |> 
  mutate(across(.fns = ~str_remove_all(.x, "Alone Householder")),
         across(.fns = ~str_remove_all(.x, ", Virginia")),
         across(.fns = ~trimws(.x))) |> 
  select(year, NAME, GEOID, race, estimate, moe) |> 
  transform(year = as.numeric(year),
            estimate = as.numeric(estimate))|> 
  left_join(cpi, by = 'year')|> 
  select(year, NAME, GEOID, race, estimate, moe, priceindex) |> 
  transform(dollars21 = ((399.2/priceindex)*estimate)) |> 
  mutate(race = case_when(
    race == "Black Or African American" ~ "Black",
    race == "Two Or More Races Householder" ~ "Multiracial",
    race == "White Alone, Not Hispanic Or Latino Householder" ~ "White, non-Hispanic",
    race == "Hispanic Or Latino Householder" ~ "Hispanic or Latino",
    TRUE ~ race
  )) |> 
  filter(race %in% c("White, non-Hispanic", "Black", "Asian", "Multiracial", "Hispanic or Latino"))

write_rds(b19013_data, "data/b19013_data.rds")