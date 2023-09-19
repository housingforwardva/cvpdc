library(tidyverse)


cv_var <- read_csv("data/home-sales.csv") |> 
  subset(name %in% c("Amherst County", "Appomattox County", "Bedford County", "Campbell County", "Lynchburg City")) |> 
  mutate(period = as.yearqtr(quarter, format = "%Y Q%q"), frac = 1) |> 
  mutate(year = substr(period, 1, 4)) |> 
  mutate(year = as.numeric(year))

library(fredr)
library(lubridate)
library(zoo)

cpi_ls <- fredr(
  series_id = "CUUR0000SA0L2"
) %>% 
  select(date, value) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(year = year(date)) |> 
  mutate(period = as.yearqtr(date, format = "%Y Q%q", frac = 1)) |> 
  group_by(period) |> 
  summarise(cpi = mean(value, na.rm = TRUE))

cv_var <- cv_var |> 
  left_join(cpi_ls, by = "period") |> 
  transform(adj_price = (278.47967/cpi)*med_price)

write_rds(cv_var, "data/cv_var.rds")
