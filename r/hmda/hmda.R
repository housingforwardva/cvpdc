library(tidyverse)
library(httr)
library(jsonlite)

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes


col_spec <- cols_only(
  "activity_year" = col_double(),
  "lei" = col_character(),
  "county_code" = col_character(),
  "census_tract" = col_character(),
  "derived_loan_product_type" = col_character(),
  "derived_dwelling_category" = col_character(),
  "derived_ethnicity" = col_character(),
  "derived_race" = col_character(),
  "derived_sex" = col_character(),
  "action_taken" = col_integer(),
  "purchaser_type" = col_integer(),
  "loan_type" = col_integer(),
  "loan_purpose" = col_integer(),
  "reverse_mortgage" = col_integer(),
  "loan_amount" = col_character(),
  "loan_to_value_ratio" = col_character(),
  "interest_rate" = col_character(),
  "total_loan_costs" = col_character(),
  "loan_term" = col_character(),
  "property_value" = col_character(),
  "construction_method" = col_integer(),
  "occupancy_type" = col_integer(),
  "manufactured_home_secured_property_type" = col_integer(),
  "manufactured_home_land_property_interest" = col_integer(),
  "total_units" = col_character(),
  "applicant_age" = col_character(),
  "income" = col_double(),
  "debt_to_income_ratio" = col_character(),
  "denial_reason-1" = col_integer(),
  "denial_reason-2" = col_integer(),
  "denial_reason-3" = col_integer(),
  "denial_reason-4" = col_integer(),
  "tract_minority_population_percent" = col_double(),
  "ffiec_msa_md_median_family_income" = col_double(),
  "tract_to_msa_income_percentage" = col_double(),
  "tract_owner_occupied_units" = col_double()
)

hmda_pull <- map_dfr(2018:2021, ~{
  GET("https://ffiec.cfpb.gov/v2/data-browser-api/view/csv", 
      query = list(
        states = "VA",
        years = .x
      ), 
      progress()) %>%
    content(as = "text") %>%
    read_csv(col_types = col_spec) 
})


cv_hmda <- hmda_pull |> 
  mutate(GEOID = as.character(county_code)) |>
  subset(GEOID %in% cv) |>
  select(GEOID,
         activity_year,
         loan = derived_loan_product_type,
         hometype = derived_dwelling_category,
         ethnicity = derived_ethnicity,
         race = derived_race,
         action_taken,
         loan_type,
         loan_purpose,
         loan_amount,
         occupancy_type,
         applicant_age
  ) |> 
  mutate(action_taken = case_when(
    action_taken == 1 ~ "Loan originated",
    action_taken == 2 ~ "Application approved but not accepted",
    action_taken == 3 ~ "Application denied",
    action_taken == 4 ~ "Application withdrawn by applicant",
    action_taken == 5 ~ "File closed for incompleteness",
    action_taken == 6 ~ "Purchased loan",
    action_taken == 7 ~ "Preapproval request denied",
    action_taken == 8 ~ "Preapproval request approved but not accepted"),
    loan_type = case_when(
      loan_type == 1 ~ "Conventional (not insured or guaranteed by FHA, VA, RHS, or FSA)",
      loan_type == 2 ~ "Federal Housing Administration insured (FHA)",
      loan_type == 3 ~ "Veterans Affairs guaranteed (VA)",
      loan_type == 4 ~ "USDA Rural Housing Service or Farm Service Agency guaranteed (RHS or FSA)"),
    loan_purpose = case_when(
      loan_purpose == 1 ~ "Home purchase",
      loan_purpose == 2 ~ "Home improvement",
      loan_purpose == 31 ~ "Refinancing",
      loan_purpose == 32 ~ "Cash-out refinancing",
      loan_purpose == 4 ~ "Other purpose",
      loan_purpose == 5 ~ "Not applicable"),
    occupancy_type = case_when(
      occupancy_type == 1 ~ "Principal residence",
      occupancy_type == 2 ~ "Second residence",
      occupancy_type == 3 ~ "Investment property"))


write_rds(cv_hmda, "data/cv_hmda.rds") 