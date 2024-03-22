library(tidyverse)
library(sf)
library(mapboxapi)
library(tigris)
library(ggspatial)

# Create a vector of FIPS codes of the region in order to easily pull localities from data sets.

cv <- c("51009", "51011", "51019", "51031", "51680", "51515") # CVPDC FIPS codes

lb <- "51680"

cnty_code <- c("009", "011", "019", "031", "515", "680")

lb_hex_count <- read_rds("data/lb_hex_count.rds")

sf_use_s2(TRUE)

options(tigris_use_cache = TRUE)

cv_boundary <- counties("VA") |> 
  filter(GEOID %in% cv)

cv_basemap <- layer_static_mapbox(
  location = cv_boundary,
  style_id = "light-v10",
  username = "mapbox"
)

lb_boundary <- counties("VA") |> 
  filter(GEOID %in% lb)

lb_basemap <- layer_static_mapbox(
  location = lb_boundary,
  style_id = "light-v10",
  username = "mapbox"
)

lb_hex_basemap <- layer_static_mapbox(
  location = lb_hex_count,
  style_id = "light-v10",
  username = "mapbox"
)

write_rds(cv_basemap, "data/cv_basemap.rds")

write_rds(lb_basemap, "data/lb_basemap.rds")

write_rds(lb_hex_basemap, "data/lb_hex_basemap.rds")

write_rds(cv_boundary, "data/cv_boundary.rds")

