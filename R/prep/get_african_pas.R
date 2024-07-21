rm(list = ls())

source("R/functions/get.heterogeneity.R")



library(sf)
library(tidyverse)
library(data.table)
library(terra)
library(rnaturalearth)



world <- rnaturalearth::ne_countries() %>% dplyr::select(continent, sovereignt, geometry)
africa <- world %>% filter(continent == "Africa")

### download all PAs from: https://www.protectedplanet.net/en/search-areas?geo_type=site

shp1.raw <- read_sf("../../../../../resources/spatial/WDPA/WDPA_WDOECM_May2024_Public_all_shp/WDPA_WDOECM_May2024_Public_all_shp_0/WDPA_WDOECM_May2024_Public_all_shp-polygons.shp")

test <- st_intersects(shp1.raw, africa)


table(shp1.raw$IUCN_CAT)
shp1 <- shp1.raw %>%
  filter(st_intersects(geometry, africa, sparse = FALSE) %>% rowSums() > 0) %>%
  filter(!grepl("arine", DESIG_ENG)) %>%
  filter(!grepl("Hunting Area", DESIG_ENG))

mapview::mapview(shp1)

shp2.raw <- read_sf("../../../../../resources/spatial/WDPA/WDPA_WDOECM_May2024_Public_all_shp/WDPA_WDOECM_May2024_Public_all_shp_1/WDPA_WDOECM_May2024_Public_all_shp-polygons.shp")

shp2 <- shp2.raw %>% 
  filter(st_intersects(geometry, africa, sparse = FALSE) %>% rowSums() > 0) %>%
  filter(!grepl("arine", DESIG_ENG)) %>%
  filter(!grepl("Hunting Area", DESIG_ENG))


shp3.raw <- read_sf("../../../../../resources/spatial/WDPA/WDPA_WDOECM_May2024_Public_all_shp/WDPA_WDOECM_May2024_Public_all_shp_2/WDPA_WDOECM_May2024_Public_all_shp-polygons.shp")

shp3 <- shp3.raw %>% 
  filter(st_intersects(geometry, africa, sparse = FALSE) %>% rowSums() > 0) %>%
  filter(!grepl("arine", DESIG_ENG)) %>%
  filter(!grepl("Hunting Area", DESIG_ENG))

pas <- rbind(shp1, shp2, shp3) %>% 
  filter(MARINE == 0)

pas.cont <- st_join(pas, world)


pas <- pas[!duplicated(data.frame(pas)),]

st_write(pas, "data/clean_data/african_pas.gpkg")
