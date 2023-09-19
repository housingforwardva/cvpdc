library(readxl)
library(tidyverse)


# Load OEWS files filtered to Lynchburg, VA MSA

oews_2019 <- read_xlsx("data/oews_va_19.xlsx",
                       sheet = "m2019",
                       na = c("*", "**", "#")) |> 
  filter(area_title == "Lynchburg, VA")

oews_2022 <- read_xlsx("data/oews_va_22.xlsx",
                       sheet = "m2022",
                       na = c("*", "**", "#")) |> 
  janitor::clean_names(case = "snake") |> 
  filter(area_title == "Lynchburg, VA")

# Find 5 most common major occupation categories from 2022

oews_occ <- oews_2022 |> 
  filter(o_group == "major") |> 
  slice_max(tot_emp, n = 5) |> 
  select(9, 10, tot_emp_2022 = 12, a_median_2022 = 28)

# Add 2019 wage data for those occupations

oews_occ <- oews_occ |> 
  left_join(oews_2019, by = "occ_code")|> 
  select(1, occ_title = 2, 3, 4, tot_emp_2019 = 14, a_median_2019 = 29)

# Calculate wage percent change and add median for all occupations

oews_occ <- oews_occ |> 
  mutate(pct_change = (a_median_2022 - a_median_2019) / a_median_2019) |> 
  add_row(occ_title = "All Occupations", pct_change = as.numeric(oews_pct[oews_pct$wage == "Median",5]))

write_rds(oews_occ, "data/oews_occ.rds")

```