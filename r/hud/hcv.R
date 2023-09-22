library(tidyverse)
library(sf)

hcv <- st_read("https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/Housing_Choice_Vouchers_by_Tract/FeatureServer/4/query?f=json&where=(STATE%20IN%20('51'))%20AND%20(COUNTY%20IN%20('680'%2C%20'009'%2C%20'011'%2C%20'019'%2C%20'031'))&outFields=*", quiet = TRUE)

write_rds(hcv, "data/hcv.rds")
