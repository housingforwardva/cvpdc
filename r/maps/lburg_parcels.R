library(sf)
library(tidyverse)

parcels <- st_read("https://mapviewer.lynchburgva.gov/ArcGIS/rest/services/OpenData/ODPDynamic/MapServer/41/query?outFields=*&where=1%3D1&f=geojson", quiet = TRUE) %>% 
  st_point_on_surface()

write_rds(parcels, "data/parcels.rds")

# The following investigates the number of single-family detached homes that are being utilized as rentals. There is a growing concern over homeownership rates in the city. The current market incentivizes the rental of single-family homes over the sale.

# wards <- st_read("https://mapviewer.lynchburgva.gov/ArcGIS/rest/services/OpenData/ODPStatic/MapServer/62/query?outFields=*&where=1%3D1&f=geojson", quiet = TRUE)

# write_rds(wards, "data/wards.rds")

wards <- read_rds("data/wards.rds")

parcels <- read_rds("data/parcels.rds")

residential <- parcels %>%
  clean_names() %>% 
  select(parcel_id, loc_addr, loc_city, loc_zip, owner1, mail_addr, mail_city, mail_stat, mail_zip, pc_desc, yr_built, current_land, current_imp) %>% 
  filter(str_detect(pc_desc, "DETACHED SNG FAM")) %>% 
  st_join(left = FALSE, wards["Designation"])

library(stringdist)

residential$match_score <- stringsim(residential$loc_addr, residential$mail_addr)

# ggplot(residential,
#        aes(x = match_score)) +
#   geom_histogram(bins = 30) +
#   theme_minimal()
# 
# set.seed(1983)
# 
# fuzzy_matches <- residential %>%
#   filter(match_score >= 0.75 & match_score <= 0.9) %>%
#   st_drop_geometry() %>%
#   slice_sample(n = 100)

res_own <- residential %>% 
  mutate(address_num = str_extract(loc_addr, "[0-9]+"),
         owner_num = str_extract(mail_addr, "[0-9]+")) %>% 
  filter(current_imp > 0) %>% 
  mutate(ownership = case_when(
    match_score > 0.6 & address_num == owner_num ~ "Owner-occupied SFD",
    TRUE ~ "SFR")
  )

sfr <- res_own %>% 
  filter(ownership == "SFR")

hex_grid <- st_make_grid(sfr, n = c(25,25),
                         what = "polygons", square = FALSE)

hex_grid_sf <- st_sf(hex_grid) %>% 
  mutate(grid_id = 1:length(lengths(hex_grid)))

hex_grid_sf$n_pts <- lengths(st_intersects(hex_grid_sf, sfr))

hex_count <- filter(hex_grid_sf, n_pts > 4)


write_rds(hex_count, "data/hex_count.rds")