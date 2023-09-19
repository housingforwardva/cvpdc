library(tidyverse)

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes

years <- 2012:2019

sumlev <- "050"

dir.create(glue("data/{sumlev}"))

walk(years, ~{
  url <- glue("https://www.huduser.gov/PORTAL/datasets/cp/{.x - 4}thru{.x}-{sumlev}-csv.zip")
  file <- basename(url)
  path <- file.path("data", sumlev, file)
  if (!file.exists(path)) {
    GET(url, write_disk(path, overwrite = TRUE), progress(type = "down"))
  }
  print(glue("Unzipping {.x}..."))
  unzip(path, exdir = file.path("data", sumlev, .x))
})

# Tables to get
tables <- c(7, 9, paste0(18, LETTERS[1:3]))


# Go through and write out the various tables
walk(tables, function(table) {
  mytable <- purrr::map_df(years, function(year) {
    # Identify the year-specific folder
    path <- file.path("data", "050", year)
    # Find the file - it may be buried so use recursive = TRUE
    file <- list.files(path, pattern = glue("Table{table}.csv"), recursive = TRUE)
    # Read in the file quietly
    raw <- read_csv(file.path(path, file), col_types = cols())
    # Clean it up
    cleaned <- raw |>
      clean_names() |>
      mutate(fips = substr(geoid, 8, 12)) |>
      separate(name, into = c("county", "state"), sep = ",") |>
      filter(st == "51") |>
      pivot_longer(starts_with("T"),
                   names_to = "code",
                   values_to = "value") |>
      mutate(id = str_extract(code, "\\d+$"),
             type = str_extract(code, "est|moe")) |>
      select(-code) |>
      pivot_wider(names_from = type, values_from = value) |>
      rename(Estimate = est, MOE = moe) |>
      mutate(Code := glue("T{table}_est{id}"),
             Year = year) |>
      select(Code, Year, Estimate, MOE, everything(), -id) |>
      mutate(fips = case_when(
        fips == "51515" ~ "51019",
        TRUE ~ fips
      )) |>
      mutate(county = case_when(
        county == "Bedford city" ~ "Bedford County",
        TRUE ~ county
      )) |>
      subset(fips %in% cv)


    # Account for different data dictionaries
    # Find the data dictionary in the appropriate folder
    dict_path <- list.files(path, pattern = "dictionary", recursive = TRUE, full.names = TRUE)

    # Read in the data dictionary and merge
    dict <- read_excel(dict_path,
                       sheet = glue("Table {table}"))
    cleaned_with_dict <- cleaned |>
      left_join(dict, by = c("Code" = "Column Name"))
    cleaned_with_dict
  })
  file_name <- glue("Table{table}_2012to2019.csv")
  message(glue("Writing file {file_name}..."))
  write_csv(mytable, glue("data/{file_name}"))
})

t7_2020 <- read_csv("data/050/2020/2016thru2020-050-csv/Table7.csv") |>
  clean_names() |>
  mutate(fips = paste0(st, cnty)) |>
  separate(name, into = c("county", "state"), sep = ",") |>
  filter(st == "51") |>
  pivot_longer(starts_with("T"),
               names_to = "code",
               values_to = "value") |>
  mutate(id = str_extract(code, "\\d+$"),
         type = str_extract(code, "est|moe"))|>
  select(-code) |>
  pivot_wider(names_from = type, values_from = value) |>
  rename(Estimate = est, MOE = moe) |>
  mutate(Code := paste0("T7_est",id)) |>
  select(Code, Estimate, MOE, everything(), -id) |>
  mutate(fips = case_when(
    fips == "51515" ~ "51019",
    TRUE ~ fips
  )) |>
  mutate(county = case_when(
    county == "Bedford city" ~ "Bedford County",
    TRUE ~ county
  )) |>
  subset(fips %in% cv) |> 
  mutate(Year = 2020) 

dict_2020 <- read_excel("data/050/2020/CHAS-data-dictionary-16-20.xlsx",
                        sheet = "Table 7") 

t7_clean <- t7_2020 |> 
  left_join(dict_2020, by = c("Code" = "Column Name")) |> 
  clean_names() |> 
  filter(line_type == "Detail") |> 
  select(year, estimate, moe, county, fips, tenure, household_income, household_type, cost_burden) |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  )) |> 
  mutate(household_income = case_when(
    household_income == "household income is less than or equal to 30% of HAMFI" ~ "30% AMI or less",
    household_income == "household income is greater than 30% but less than or equal to 50% of HAMFI" ~ "31 to 50% AMI",
    household_income == "household income is greater than 50% but less than or equal to 80% of HAMFI" ~ "51 to 80% AMI",
    household_income == "household income is greater than 80% but less than or equal to 100% of HAMFI" ~ "81 to 100% AMI",
    household_income == "household income is greater than 100% of HAMFI" ~ "101% AMI or greater"
  )) |> 
  mutate(household_type = case_when(
    household_type == "household type is elderly family (2 persons, with either or both age 62 or over)" ~ "Elderly family",
    household_type == "household type is small family (2 persons, neither person 62 years or over, or 3 or 4 persons)" ~ "Small family",
    household_type == "household type is large family (5 or more persons)" ~ "Large family",
    household_type == "household type is elderly non-family" ~ "Elderly non-family",
    household_type == "other household type (non-elderly non-family)" ~ "Non-elderly non-family"
  ))  |> 
  mutate(cost_burden = case_when(
    cost_burden == "housing cost burden is less than or equal to 30%" ~ "Not cost-burdened",
    cost_burden == "housing cost burden is greater than 30% but less than or equal to 50%" ~ "Cost-burdened",
    cost_burden == "housing cost burden is greater than 50%" ~ "Severely cost-burdened",
    cost_burden == "housing cost burden not computed (household has no/negative income)" ~ "No or negative income"
  )) |> 
  mutate(cb_group = case_when(
    cost_burden == "Cost-burdened" ~ "Cost-burdened",
    cost_burden == "Severely cost-burdened" ~ "Cost-burdened",
    TRUE ~ cost_burden
  )) 
  

cb_7 <- read_csv("data/Table7_2012to2019.csv")|> 
  clean_names() |> 
  filter(line_type == "Detail") |> 
  select(year, estimate, moe, county, fips, tenure, household_income, household_type, cost_burden) |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  )) |> 
  mutate(household_income = case_when(
    household_income == "household income is less than or equal to 30% of HAMFI" ~ "30% AMI or less",
    household_income == "household income is greater than 30% but less than or equal to 50% of HAMFI" ~ "31 to 50% AMI",
    household_income == "household income is greater than 50% but less than or equal to 80% of HAMFI" ~ "51 to 80% AMI",
    household_income == "household income is greater than 80% but less than or equal to 100% of HAMFI" ~ "81 to 100% AMI",
    household_income == "household income is greater than 100% of HAMFI" ~ "101% AMI or greater"
  )) |> 
  mutate(household_type = case_when(
    household_type == "household type is elderly family (2 persons, with either or both age 62 or over)" ~ "Elderly family",
    household_type == "household type is small family (2 persons, neither person 62 years or over, or 3 or 4 persons)" ~ "Small family",
    household_type == "household type is large family (5 or more persons)" ~ "Large family",
    household_type == "household type is elderly non-family" ~ "Elderly non-family",
    household_type == "other household type (non-elderly non-family)" ~ "Non-elderly non-family"
  ))  |> 
  mutate(cost_burden = case_when(
    cost_burden == "housing cost burden is less than or equal to 30%" ~ "Not cost-burdened",
    cost_burden == "housing cost burden is greater than 30% but less than or equal to 50%" ~ "Cost-burdened",
    cost_burden == "housing cost burden is greater than 50%" ~ "Severely cost-burdened",
    cost_burden == "housing cost burden not computed (household has no/negative income)" ~ "No or negative income"
  )) |> 
  mutate(cb_group = case_when(
    cost_burden == "Cost-burdened" ~ "Cost-burdened",
    cost_burden == "Severely cost-burdened" ~ "Cost-burdened",
    TRUE ~ cost_burden
  )) 

cb_7 <- rbind(cb_7, t7_clean)

write_rds(cb_7, "data/cb_7.rds")

t9_2020 <- read_csv("data/050/2020/2016thru2020-050-csv/Table9.csv") |>
  clean_names() |>
  mutate(fips = paste0(st, cnty)) |>
  separate(name, into = c("county", "state"), sep = ",") |>
  filter(st == "51") |>
  pivot_longer(starts_with("T"),
               names_to = "code",
               values_to = "value") |>
  mutate(id = str_extract(code, "\\d+$"),
         type = str_extract(code, "est|moe"))|>
  select(-code) |>
  pivot_wider(names_from = type, values_from = value) |>
  rename(Estimate = est, MOE = moe) |>
  mutate(Code := paste0("T9_est",id)) |>
  select(Code, Estimate, MOE, everything(), -id) |>
  mutate(fips = case_when(
    fips == "51515" ~ "51019",
    TRUE ~ fips
  )) |>
  mutate(county = case_when(
    county == "Bedford city" ~ "Bedford County",
    TRUE ~ county
  )) |>
  subset(fips %in% cv) |> 
  mutate(Year = 2020) 

dict_2020 <- read_excel("data/050/2020/CHAS-data-dictionary-16-20.xlsx",
                        sheet = "Table 9") 

t9_clean <- t9_2020 |> 
  left_join(dict_2020, by = c("Code" = "Column Name")) |> 
  clean_names() |> 
  filter(line_type == "Detail")  |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  )) |> 
  mutate(race = case_when(
    race_ethnicity == "White alone, non-Hispanic" ~ "White, non-Hispanic",
    race_ethnicity == "Black or African-American alone, non-Hispanic" ~ "Black",
    race_ethnicity == "Asian alone, non-Hispanic" ~ "Asian",
    race_ethnicity == "Hispanic, any race" ~ "Hispanic or Latino",
    TRUE ~ "Another race, including multiracial"
  ))   |> 
  mutate(cost_burden = case_when(
    cost_burden == "less than or equal to 30%" ~ "Not cost-burdened",
    cost_burden == "greater than 30% but less than or equal to 50%" ~ "Cost-burdened",
    cost_burden == "greater than 50%" ~ "Severely cost-burdened",
    cost_burden == "not computed (no/negative income)" ~ "No or negative income"
  )) |> 
  mutate(cb_group = case_when(
    cost_burden == "Cost-burdened" ~ "Cost-burdened",
    cost_burden == "Severely cost-burdened" ~ "Cost-burdened",
    TRUE ~ cost_burden
  )) |> 
  select(county, fips, year, race, cost_burden, cb_group, estimate, moe)



cb_9 <- read_csv("data/Table9_2012to2019.csv") |> 
  clean_names() |> 
  filter(line_type == "Detail")  |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  )) |> 
  mutate(race = case_when(
    race_ethnicity == "White alone, non-Hispanic" ~ "White, non-Hispanic",
    race_ethnicity == "Black or African-American alone, non-Hispanic" ~ "Black",
    race_ethnicity == "Asian alone, non-Hispanic" ~ "Asian",
    race_ethnicity == "Hispanic, any race" ~ "Hispanic or Latino",
    TRUE ~ "Another race, including multiracial"
  ))   |> 
  mutate(cost_burden = case_when(
    cost_burden == "less than or equal to 30%" ~ "Not cost-burdened",
    cost_burden == "greater than 30% but less than or equal to 50%" ~ "Cost-burdened",
    cost_burden == "greater than 50%" ~ "Severely cost-burdened",
    cost_burden == "not computed (no/negative income)" ~ "No or negative income"
  )) |> 
  mutate(cb_group = case_when(
    cost_burden == "Cost-burdened" ~ "Cost-burdened",
    cost_burden == "Severely cost-burdened" ~ "Cost-burdened",
    TRUE ~ cost_burden
  )) |> 
  select(county, fips, year, race, cost_burden, cb_group, estimate, moe)

cb_9 <- rbind(cb_9, t9_clean)

write_rds(cb_9, "data/cb_9.rds")


t18c_2020 <- read_csv("data/050/2020/2016thru2020-050-csv/Table18C.csv") |>
  clean_names() |>
  mutate(fips = paste0(st, cnty)) |>
  separate(name, into = c("county", "state"), sep = ",") |>
  filter(st == "51") |>
  pivot_longer(starts_with("T"),
               names_to = "code",
               values_to = "value") |>
  mutate(id = str_extract(code, "\\d+$"),
         type = str_extract(code, "est|moe"))|>
  select(-code) |>
  pivot_wider(names_from = type, values_from = value) |>
  rename(Estimate = est, MOE = moe) |>
  mutate(Code := paste0("T18C_est",id)) |>
  select(Code, Estimate, MOE, everything(), -id) |>
  mutate(fips = case_when(
    fips == "51515" ~ "51019",
    TRUE ~ fips
  )) |>
  mutate(county = case_when(
    county == "Bedford city" ~ "Bedford County",
    TRUE ~ county
  )) |>
  subset(fips %in% cv) |> 
  mutate(Year = 2020) 

dict_2020 <- read_excel("data/050/2020/CHAS-data-dictionary-16-20.xlsx",
                        sheet = "Table 18C") 



t18c_clean <- t18c_2020 |> 
  left_join(dict_2020, by = c("Code" = "Column Name")) 

colnames(t18c_clean)[16] <- "cost"

t18c_2020_clean <-  t18c_clean |> 
  clean_names() |> 
  filter(line_type == "Detail") |> 
  select(county, fips, year, estimate, tenure, cost, household_income) %>% 
  group_by(county, fips, year, tenure, cost, household_income) %>% 
  summarise(estimate = sum(estimate))  %>% 
  filter(tenure == "Renter occupied") %>% 
  mutate(cost = case_when(
    cost == "greater than RHUD30 and less than or equal to RHUD50" ~ "31 to 50 percent AMI",
    cost == "greater than RHUD50 and less than or equal to RHUD80" ~ "51 to 80 percent AMI",
    cost == "greater than RHUD80" ~ "80 percent AMI or greater",
    cost == "less than or equal to RHUD30" ~ "30 percent AMI or below")) %>% 
  mutate(household_income = case_when(
    household_income == "greater than 100% of HAMFI" ~ "80 percent AMI or greater",
    household_income == "greater than 80% of HAMFI but less than or equal to 100% of HAMFI" ~ "80 percent AMI or greater",
    household_income == "greater than 50% of HAMFI but less than or equal to 80% of HAMFI" ~ "51 to 80 percent AMI",
    household_income == "greater than 30% of HAMFI but less than or equal to 50% of HAMFI" ~ "31 to 50 percent AMI",
    household_income == "less than or equal to 30% of HAMFI" ~ "30 percent AMI or below"
  )) |>
  group_by(county, fips, year, cost, household_income) %>% 
  mutate(fips = as.double(fips)) |> 
  summarise(estimate = sum(estimate)) %>% 
  mutate(match = case_when(
    cost == household_income ~ "Affordable",
    cost == "30 percent AMI or below" & household_income == "31 to 50 percent AMI" ~ "Very affordable",
    cost == "30 percent AMI or below" & household_income == "51 to 80 percent AMI" ~ "Very affordable",
    cost == "30 percent AMI or below" & household_income == "80 percent AMI or greater" ~ "Very affordable",
    cost == "31 to 50 percent AMI" & household_income == "31 to 50 percent AMI" ~ "Affordable",
    cost == "31 to 50 percent AMI" & household_income == "51 to 80 percent AMI" ~ "Very affordable",
    cost == "31 to 50 percent AMI" & household_income == "80 percent AMI or greater" ~ "Very affordable",
    cost == "31 to 50 percent AMI" & household_income == "30 percent AMI or below" ~ "Unaffordable",
    cost == "51 to 80 percent AMI" & household_income == "30 percent AMI or below" ~ "Unaffordable",
    cost == "51 to 80 percent AMI" & household_income == "31 to 50 percent AMI" ~ "Unaffordable",
    cost == "51 to 80 percent AMI" & household_income == "80 percent AMI or greater" ~ "Very affordable",
    cost == "80 percent AMI or greater" & household_income == "30 percent AMI or below" ~ "Unaffordable",
    cost == "80 percent AMI or greater" & household_income == "31 to 50 percent AMI" ~ "Unaffordable",
    cost == "80 percent AMI or greater" & household_income == "51 to 80 percent AMI" ~ "Unaffordable"
  )) %>% 
  mutate(gapcode = case_when(
    match == "Unaffordable" ~ "Gap",
    TRUE ~ "Matches or less than income"
  ))



tb_18c <- read_csv("data/Table18C_2012to2019.csv")

colnames(tb_18c)[16] <- "Cost"

renter <- tb_18c %>% 
  clean_names() %>% 
  filter(line_type == "Detail") %>% 
  select(county, fips, year, estimate, tenure, cost, household_income) %>% 
  group_by(county, fips, year, tenure, cost, household_income) %>% 
  summarise(estimate = sum(estimate))


tb_18_match <- renter %>% 
  filter(tenure == "Renter occupied") %>% 
  mutate(cost = case_when(
    cost == "greater than RHUD30 and less than or equal to RHUD50" ~ "31 to 50 percent AMI",
    cost == "greater than RHUD50 and less than or equal to RHUD80" ~ "51 to 80 percent AMI",
    cost == "greater than RHUD80" ~ "80 percent AMI or greater",
    cost == "less than or equal to RHUD30" ~ "30 percent AMI or below"
  )) %>% 
  mutate(household_income = case_when(
    household_income == "greater than 100% of HAMFI" ~ "80 percent AMI or greater",
    household_income == "greater than 80% of HAMFI but less than or equal to 100% of HAMFI" ~ "80 percent AMI or greater",
    household_income == "greater than 50% of HAMFI but less than or equal to 80% of HAMFI" ~ "51 to 80 percent AMI",
    household_income == "greater than 30% of HAMFI but less than or equal to 50% of HAMFI" ~ "31 to 50 percent AMI",
    household_income == "less than or equal to 30% of HAMFI" ~ "30 percent AMI or below"
  )) |>
  group_by(county, fips, year, cost, household_income) %>% 
  summarise(estimate = sum(estimate))


tb_18_match <- tb_18_match %>% 
  mutate(match = case_when(
    cost == household_income ~ "Affordable",
    cost == "30 percent AMI or below" & household_income == "31 to 50 percent AMI" ~ "Very affordable",
    cost == "30 percent AMI or below" & household_income == "51 to 80 percent AMI" ~ "Very affordable",
    cost == "30 percent AMI or below" & household_income == "80 percent AMI or greater" ~ "Very affordable",
    cost == "31 to 50 percent AMI" & household_income == "31 to 50 percent AMI" ~ "Affordable",
    cost == "31 to 50 percent AMI" & household_income == "51 to 80 percent AMI" ~ "Very affordable",
    cost == "31 to 50 percent AMI" & household_income == "80 percent AMI or greater" ~ "Very affordable",
    cost == "31 to 50 percent AMI" & household_income == "30 percent AMI or below" ~ "Unaffordable",
    cost == "51 to 80 percent AMI" & household_income == "30 percent AMI or below" ~ "Unaffordable",
    cost == "51 to 80 percent AMI" & household_income == "31 to 50 percent AMI" ~ "Unaffordable",
    cost == "51 to 80 percent AMI" & household_income == "80 percent AMI or greater" ~ "Very affordable",
    cost == "80 percent AMI or greater" & household_income == "30 percent AMI or below" ~ "Unaffordable",
    cost == "80 percent AMI or greater" & household_income == "31 to 50 percent AMI" ~ "Unaffordable",
    cost == "80 percent AMI or greater" & household_income == "51 to 80 percent AMI" ~ "Unaffordable"
  )) %>% 
  mutate(gapcode = case_when(
    match == "Unaffordable" ~ "Gap",
    TRUE ~ "Matches or less than income"
  ))

tb_18_match <- rbind(t18c_2020_clean, tb_18_match)

write_rds(tb_18_match, "data/tb_18_match.rds")


