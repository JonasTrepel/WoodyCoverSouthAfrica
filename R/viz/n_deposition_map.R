### N deposition maps 

library(rnaturalearth)
library(sf)
library(terra)
library(tidyverse)
### extract N deposition -----------
#from: https://acp.copernicus.org/articles/23/7091/2023/
ndep <- rast("../../../../../resources/spatial/N_deposition_Rubin_etal/total_N_dep.tif")
plot(ndep)

sa.sh <- ne_countries(country = "south africa", type = "map_units")

## files too large to upload on github. please contact me (jonas.trepel@bio.au.dk) and I'll send them
pa.shapes <- read_sf("data/spatial/sub_saharan_african_pas_w_covariates.shp")



sa.ext <- ext(sa.sh)

ndep.cropped <- crop(ndep, sa.ext)
plot(ndep.cropped)

dt.ndep.cropped <- as.data.frame(ndep.cropped, xy = T)

sa.sh <- ne_countries(country = "south africa", type = "map_units")


# Convert the dataframe to an sf object
sa.points <- st_as_sf(dt.ndep.cropped, coords = c("x", "y"), crs = 4326)
sa.cent <- st_centroid(sa.points)
# Create a bounding box for the grid
bbox <- st_bbox(sa.points)

# Generate a grid over the bounding box
grid <- st_make_grid(sa.sh, cellsize = c(1, 1), what = "polygons") %>% st_sf() %>% st_join(sa.cent) %>% st_intersection(sa.sh[, "geometry"])
# Convert the grid to an sf object (if not already)
mapview::mapview(grid)



p.n.sa <- ggplot() +
  geom_sf(data = grid, aes(fill = SelfBand, color = SelfBand)) +
  geom_sf(data = sa.sh, fill = "transparent", color = "white") +
  scale_fill_viridis_c(option = "E") +
  scale_color_viridis_c(option = "E") +
  labs(fill = "N deposition\n(kg/km2/y)", color = "N deposition\n(kg/km2/y)", y = "Latitude", y = "Longitude", title = "b)") +
  theme_void() +
  theme(legend.position = "bottom")
p.n.sa

## whole of africa 


africa <- ne_countries(continent = "Africa", type = "map_units")


africa.ext <- ext(africa)

ndep.cropped.africa <- crop(ndep, africa.ext)
plot(ndep.cropped.africa)

dt.ndep.cropped.africa <- as.data.frame(ndep.cropped.africa, xy = T)


# Convert the dataframe to an sf object
africa.points <- st_as_sf(dt.ndep.cropped.africa, coords = c("x", "y"), crs = 4326)
africa.cent <- st_centroid(africa.points)
# Generate a grid over the bounding box
sf_use_s2(FALSE)
grid.africa <- st_make_grid(africa, cellsize = c(1, 1), what = "polygons") %>% st_sf() %>% st_join(africa.cent) %>% st_intersection(africa[, "geometry"])
# Convert the grid to an sf object (if not already)
mapview::mapview(grid.africa)

p.n.africa <- ggplot() +
  geom_sf(data = grid.africa, aes(fill = SelfBand, color = SelfBand)) +
  geom_sf(data = pa.shapes, fill = "transparent", color = "white") +
  scale_fill_viridis_c(option = "E") +
  scale_color_viridis_c(option = "E") +
  labs(fill = "N deposition\n(kg/km2/y)", color = "N deposition\n(kg/km2/y)", y = "Latitude", y = "Longitude", title = "a)") +
  theme_void() +
  theme(legend.position = "none")
p.n.africa

library(gridExtra)
p.ndep <- grid.arrange(p.n.africa, p.n.sa, ncol = 2)

ggsave(plot = p.ndep, "builds/plots/september/n_deposition_maps.png", dpi = 600, height = 6, width = 9)

p.n.africa.leg <- ggplot() +
  geom_sf(data = grid.africa, aes(fill = SelfBand, color = SelfBand)) +
  geom_sf(data = pa.shapes, fill = "transparent", color = "white") +
  scale_fill_viridis_c(option = "E") +
  scale_color_viridis_c(option = "E") +
  labs(fill = "N deposition\n(kg km⁻² y⁻¹)", 
       color = "N deposition\n(kg km⁻² y⁻¹)") +
  theme_void() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.2, 0.2))

p.n.africa.leg
ggsave(plot = p.n.africa.leg, "builds/plots/september/n_deposition_africa.png", dpi = 600, height = 6, width = 5)

