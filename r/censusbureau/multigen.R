library(tidyverse)
library(tidycensus)
library(sf)
# Load PUMA geographies for Virginia
pumas <- pumas(state = "VA")

# Filter PUMAs to Virginia Region 2000 (North and South)

pumas_cv <- pumas |>
  filter(str_detect(NAMELSAD10, "Lynchburg|Bedford"))

# Pull each PUMS year separately because data structure is not same across 2016 to 2020

multigen_2016 <- get_pums(
  variables = "MULTG",
  state = "VA",
  year = 2016,
  puma = pumas_cv$PUMACE10)

multigen_2016 <- multigen_2016 |>
  mutate(YEAR = "2016",
         SPORDER = as.numeric(SPORDER),
         PUMA = case_when(
           str_length(PUMA) == 4 ~ paste0("0", PUMA),
           TRUE ~ as.character(PUMA))) |>
  select(YEAR, SERIALNO, PUMA, SPORDER, WGTP, PWGTP, MULTG)

multigen_2017 <- get_pums(
  variables = "MULTG",
  state = "VA",
  year = 2017,
  puma = pumas_cv$PUMACE10)

multigen_2017 <- multigen_2017 |>
  mutate(SPORDER = as.numeric(SPORDER),
         YEAR = "2017") |>
  select(YEAR, SERIALNO, PUMA, SPORDER, WGTP, PWGTP, MULTG)

multigen_2018 <- get_pums(
  variables = "MULTG",
  state = "VA",
  year = 2018,
  puma = pumas_cv$PUMACE10)

multigen_2018 <- multigen_2018 |>
  mutate(SPORDER = as.numeric(SPORDER),
         YEAR = "2018") |>
  select(YEAR, SERIALNO, PUMA, SPORDER, WGTP, PWGTP, MULTG)

multigen_2019 <- get_pums(
  variables = "MULTG",
  state = "VA",
  year = 2019,
  puma = pumas_cv$PUMACE10)

multigen_2019 <- multigen_2019 |>
  mutate(YEAR = "2019") |>
  select(YEAR, SERIALNO, PUMA, SPORDER, WGTP, PWGTP, MULTG)

multigen_2020 <- get_pums(
  variables = "MULTG",
  state = "VA",
  year = 2020,
  puma = pumas_cv$PUMACE10)

multigen_2020 <- multigen_2020 |>
  mutate(YEAR = "2020") |>
  select(YEAR, SERIALNO, PUMA, SPORDER, WGTP, PWGTP, MULTG)

# Bind years together and recode

multigen_raw <- bind_rows(multigen_2016, multigen_2017, multigen_2018,
                          multigen_2019, multigen_2020) |>
  filter(!MULTG %in% c("b", "0")) |>
  mutate(geography = case_when(
    PUMA == "51096" ~ "Lynchburg city and Campbell County",
    PUMA == "51095" ~ "Amherst, Appomattox, and Bedford Counties"
  )) |> 
  mutate(MULTG = case_when(
    MULTG == "1" ~ "Single generation",
    MULTG == "2" ~ "Multiple generations"
  ))

# Summarize percent of multigenerational households by year
multigen_data <- multigen_raw |>
  group_by(geography, YEAR) |>
  summarise(pct = sum(PWGTP[MULTG == "Multiple generations"]) / sum(PWGTP))|>
  mutate(geography = fct_reorder(geography, pct, .desc = TRUE))

write_rds(multigen_data, "data/multigen_data.rds")