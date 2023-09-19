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


# Pull out annual wage percentiles for each year

percentiles_2019 <- oews_2019 |> 
  filter(occ_code == "00-0000") |> 
  select(9, 24:28) |> 
  mutate(year = "y2019", .before = 1)

percentiles_2022 <- oews_2022 |> 
  filter(occ_code == "00-0000") |> 
  select(10, 26:30) |> 
  mutate(year = "y2022", .before = 1)

# Combine years, rearrange, and calculate percent change over time

percentiles <- bind_rows(percentiles_2019, percentiles_2022) |> 
  pivot_longer(3:7) |> 
  pivot_wider(names_from = year,
              values_from = value) |> 
  mutate(pct_change = (y2022 - y2019)/y2019,
         wage = case_when(
           name == "a_pct10" ~ "10th percentile",
           name == "a_pct25" ~ "25th percentile",
           name == "a_median" ~ "Median",
           name == "a_pct75" ~ "75th percentile",
           name == "a_pct90" ~ "90th percentile"
         )) |> 
  mutate(wage = fct_relevel(wage,
                            "10th percentile",
                            "25th percentile",
                            "Median",
                            "75th percentile",
                            "90th percentile"))

write_rds(percentiles, "data/oews_pct.rds")
