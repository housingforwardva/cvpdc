library(tidyverse)
library(sf)

hcv <- st_read("https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/Housing_Choice_Vouchers_by_Tract/FeatureServer/4/query?f=json&where=(COUNTY%20IN%20('680'))%20AND%20(STATE%20IN%20('51'))&outFields=*", quiet = TRUE)

write_rds(hcv, "data/hcv.rds")
