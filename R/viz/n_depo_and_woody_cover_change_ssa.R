### N deposition maps 

library(rnaturalearth)
library(sf)
library(terra)
library(tidyverse)
library(gridExtra)



dt <- fread("data/clean_data/final_sub_saharan_african_pa_data.csv") %>% 
  filter(complete.cases(across(
    c(woody_cover_change, venter_woody_cover_trend, woody_cover_sd_ha_coef, woody_cover_sd_km_coef, 
      mat_coef, prec_coef, n_deposition, fire_frequency, burned_area_coef)
  )))
setDT(dt)
nrow(dt[woody_cover_change > 0])/nrow(dt)

pa_shapes <- read_sf("data/spatial/pa_shapes/sub_saharan_african_pas.gpkg") %>% 
  filter(WDPA_PID %in% unique(dt$unique_id))

africa <- rnaturalearth::ne_countries(scale = 50, continent = c("Africa"),
                                      returnclass = "sf") 


##### DW Woody cover Change --------------------
# Sub saharan Africa
v_africa <- vect(africa)

r_wcc <- rast("data/spatial/covariates/woody_cover_coef_km.tif")

r_wcc <- mask(r_wcc, v_africa)
africa_cropped <- st_crop(africa, st_bbox(r_wcc))


dt_wcc_raw <- as.data.frame(r_wcc, xy = TRUE)
(uq <- quantile(dt_wcc_raw$woody_cover_coef, .975))
(lq <- quantile(dt_wcc_raw$woody_cover_coef, .025))
dt_wcc <- dt_wcc_raw %>% 
  mutate(woody_cover_coef = case_when(
    .default = woody_cover_coef, 
    woody_cover_coef > uq ~ uq, 
    woody_cover_coef < lq ~ lq
  ))


p_wcc <- ggplot() +
  geom_raster(data = dt_wcc, aes(x = x, y = y, color = woody_cover_coef, fill = woody_cover_coef)) +
  geom_sf(data = africa_cropped, color = "grey50", fill = "transparent") +
  # scale_color_scico(palette = "bam", midpoint = 0) +
  scale_fill_scico(palette = "bam", midpoint = 0) +
  labs(color = "DW woody cover\nchange (%/year)", fill = "DW woody cover\nchange (%/year)") +
  theme_void() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.2, 0.2),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))
p_wcc

##### Venter woody cover trend #######
r_vwct <- rast("data/spatial/covariates/venter_woody_cover_trend_900m.tif")

r_vwct <- mask(r_vwct, v_africa)


dt_vwct_raw <- as.data.frame(r_vwct, xy = TRUE)
(uq <- quantile(dt_vwct_raw$venter_woody_cover_trend, .99, na.rm = T))
(lq <- quantile(dt_vwct_raw$venter_woody_cover_trend, .01, na.rm = T))
dt_vwct <- dt_vwct_raw %>% 
  mutate(venter_woody_cover_trend = case_when(
    .default = venter_woody_cover_trend, 
    venter_woody_cover_trend > uq ~ uq, 
    venter_woody_cover_trend < lq ~ lq
  ))

p_vwct <- ggplot() +
  geom_raster(data = dt_vwct, aes(x = x, y = y, color = venter_woody_cover_trend, fill = venter_woody_cover_trend)) +
  geom_sf(data = africa_cropped, color = "grey50", fill = "transparent") +
  geom_sf(data = sa, color = "black", fill = "transparent") +  scale_fill_scico(palette = "bam", midpoint = 0) +
  labs(color = "Venter's woody cover\nchange (%/year)", fill = "Venter's woody cover\nchange (%/year)") +
  theme_void() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.2, 0.2),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))
p_vwct


### extract N deposition -----------
#from: https://acp.copernicus.org/articles/23/7091/2023/
r_ndep <- rast("data/spatial/covariates/total_N_dep.tif")
plot(r_ndep)

r_ndep_cropped <- crop(r_ndep, africa_cropped)
plot(r_ndep_cropped)

dt_ndep <- as.data.frame(r_ndep_cropped, xy = T)


# Convert the dataframe to an sf object
africa_points <- st_as_sf(dt_ndep, coords = c("x", "y"), crs = 4326)
africa_cent <- st_centroid(africa_points)
# Generate a grid over the bounding box
sf_use_s2(FALSE)
grid_africa <- st_make_grid(africa_cropped, cellsize = c(1, 1), what = "polygons") %>%
  st_sf() %>%
  st_join(africa_cent) %>% 
  st_intersection(africa_cropped[, "geometry"])
# Convert the grid to an sf object (if not already)
mapview::mapview(grid_africa)

p_ndep <- ggplot() +
  geom_sf(data = grid_africa, aes(fill = SelfBand, color = SelfBand)) +
  geom_sf(data = pa_shapes, fill = "transparent", color = "white") +
  scale_fill_viridis_c(option = "E") +
  scale_color_viridis_c(option = "E") +
  labs(fill = "N deposition\n(kg km⁻² y⁻¹)", 
       color = "N deposition\n(kg km⁻² y⁻¹)") +
  theme_void() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.2, 0.2),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))
p_ndep

library(patchwork)

p_maps <- p_ndep | p_vwct | p_wcc


ggsave(plot = p_maps, "builds/plots/revision/wcc_and_ndep_ssa.png", dpi = 600, height = 8, width = 16)

