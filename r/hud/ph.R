library(tidyverse)
library(sf)

ph <- st_read("https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/Public_Housing_Developments/FeatureServer/11/query?f=json&where=(FORMAL_PARTICIPANT_NAME%20IN%20('Lynchburg%20Redevelopment%20%26%20Housing%20Authority'))&outFields=*", quiet = TRUE)

write_rds(ph, "data/ph.rds")