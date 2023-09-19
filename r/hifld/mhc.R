library(tidyverse)
library(sf)

mhc <- st_read("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/Mobile_Home_Parks/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson", quiet = TRUE) |> 
  filter(STATE == "VA") |> 
  filter(str_detect(COUNTY, "AMHERST|APPOMATTOX|BEDFORD|CAMPBELL"))

write_rds(mhc, "data/mhc_cnty.rds")

