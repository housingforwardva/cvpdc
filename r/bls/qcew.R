library(glue)
library(tidyverse)

years <- 2015:2022

cnty <- c(51009, 51011, 51019, 51031, 51680)

qcewGetAreaData <- function(year, qtr, area) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}


qcew_data <- map_dfr(cnty, function(cnty){
  yearly_data <- map_dfr(years, function(yr){
    qcew_pull <- qcewGetAreaData(yr, "a", cnty) |> 
      filter(own_code == 0) |> 
      filter(industry_code == 10) |> 
      select(area_fips, annual_avg_emplvl, annual_avg_wkly_wage,avg_annual_pay) |> 
      mutate(year = yr)
  })
})

cpi <- read_excel("data/CPI_U_RS.xlsx")

cpi <- cpi |> 
  rename(year = Year,
         priceindex = Index) |> 
  transform(year = as.numeric(year))

qcew_data_cpi <- qcew_data |> 
  left_join(cpi, by = 'year') |> 
  transform(adj_pay = ((399.2/priceindex)*avg_annual_pay))


# amherst <- qcewGetAreaData(2021, "a", 51009) |> 
#   filter(own_code == 0) |> 
#   filter(industry_code == 10) |> 
#   select(area_fips, annual_avg_emplvl, annual_avg_wkly_wage,avg_annual_pay)


write_rds(qcew_data_cpi, "data/qcew_data.rds")


