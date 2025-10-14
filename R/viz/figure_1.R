### figure 1 


library(sf)
library(tidyverse)
library(data.table)
library(ggridges)
library(mapview)
library(exactextractr)
library(terra)
library(MetBrewer)
library(gridExtra)
library(viridis)
library(scales)
library(rnaturalearth)
library(terra)
library(ggspatial)
library(scico)

dt <- fread("data/clean_data/final_reserve_data.csv")

########### RESERVE DISTRIBUTION ################################

## Biome -----------

veg <- sf::read_sf("data/spatial/covariates/Vegetation_Map_SA_2018/NVM2018_AEA_V22_7_16082019_final.shp")

veg <- st_transform(veg, crs = 4326)
veg <- st_sf(veg) %>% 
  mutate(BIOMEID_18 = ifelse(veg$BIOME_18 == "Albany Thicket", 5, veg$BIOMEID_18))
# Perform a spatial join to associate "pa" polygons with "veg" polygons

table(veg$BIOMEID_18)

# Get the extent of the "veg" data
veg_bbox <- st_bbox(veg)

# Create a raster with the matching extent and resolution
r_temp1k <- rast(
  extent = veg_bbox, res = 0.01
)

# Rasterize the "veg" polygons into the matching raster
table(veg$BIOME_18)
biome_r <- rasterize(veg, r_temp1k, field = "BIOMEID_18")
plot(biome_r)

africa <- rnaturalearth::ne_countries(scale = 50, continent = c("Africa"),
                                      returnclass = "sf") 

sa <- africa %>% filter(name == "South Africa") %>% st_transform(crs = 4326)


biome_r.mask <- mask(biome_r, sa)


leg <- unique(veg %>% dplyr::select(c("BIOME_18", "BIOMEID_18")))
leg$geometry <- NULL
setDT(leg)
leg <- unique(leg)
leg <- leg[!BIOMEID_18 == 0]
leg

dt_biome <- as.data.frame(biome_r.mask, xy = TRUE) %>% left_join(leg)

### other data ----------------------
## We're not allowed to upload this file. Please contact me and we'll find a solution: jonas.trepel@gmail.com
dt_shape <- st_read("data/spatial/pa_shapes/reserve_shapes.gpkg")

sf_use_s2(FALSE)
dt.cent <- dt_shape %>%
  left_join(dt) %>% st_centroid()
mapview(dt.cent)
sf_use_s2(TRUE)

africa <- rnaturalearth::ne_countries(scale = 50, continent = c("Africa"),
                                      returnclass = "sf") 

sa <- africa %>% filter(name == "South Africa") %>% st_transform(crs = 4326)


grid <- st_make_grid(dt_shape, cellsize = c(1, 1), what = "polygons", square = F) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  st_intersection(sa) %>% 
  mutate(grid_ID = paste0("Grid_", 1:nrow(.))) %>% 
  dplyr::select(grid_ID) %>% 
  mutate(n_reserves = lengths(st_intersects(.,dt.cent)))
#remove duplicates 
grid <- grid[!duplicated(data.frame(grid)),]

sf_use_s2(FALSE)
sa <- st_intersection(grid, sa) %>% group_by(name) %>% summarise()
mapview(sa)

mapview::mapview(grid, zcol = "n_reserves")



grid_cent <- st_centroid(grid)
unique(dt_biome$BIOME_18)
scales::show_col(viridis(12))
library(rcartocolor)

n_color <- 12
scales::show_col(carto_pal(n_color, "Safe"))

pal = c("Albany Thicket" = "#6699CC", "Azonal Vegetation" = "#888888", "Savanna" = "#999933", 
        "Forests" = "#332288", "Grassland" = "#117733", 
        "Nama-Karoo" = "#AA4499", "Indian Ocean Coastal Belt" = "#44AA99", 
        "Desert" = "#DDCC77", "Succulent Karoo" = "#882255", 
        "Fynbos" = "#661100")

p_cent <- ggplot() +
  geom_tile(data = dt_biome[!is.na(dt_biome$BIOME_18),], aes(x = x, y = y, color = BIOME_18, fill = BIOME_18), alpha = 1) + 
  geom_sf(data = sa, fill = "white", linewidth = 0.1, alpha = 0.15) +
  geom_sf(data = grid_cent[!grid_cent$n_reserves == 0,], aes(size = n_reserves), alpha = 1) +
  labs(size = "Number of\nreserves") +
  labs(color = "Biome", fill = "Biome") +
 # ylim(35, 22) +
#  xlim(17, 33) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  annotation_scale(location = "tr", bar_cols = c("wheat3", "white")) +
  theme_void() +
  theme(
    legend.text = element_text(size = 12),  # Adjust the size of the legend text
    legend.title = element_text(size = 14), # Adjust the size of the legend title
    legend.key.size = unit(0.5, "cm"),
    legend.position = "right"# Adjust the size of the legend keys
  )
p_cent

range(dt_biome$x)
range(dt_biome$y)
range(dt_biome$y)

library(viridis)
#viridis(12)
############################## Woody cover chaneg ################################

#South Africa
r_wcc <- rast("data/spatial/figure_1_tifs/sa_woody_cover_coef_km.tif")

r_wcc_sa <- mask(r_wcc, sa)


dt_wcc_sa_raw <- as.data.frame(r_wcc_sa, xy = TRUE)
(uq <- quantile(dt_wcc_sa_raw$woody_cover_coef, .975))
(lq <- quantile(dt_wcc_sa_raw$woody_cover_coef, .025))
dt_wcc_sa <- dt_wcc_sa_raw %>% 
  mutate(woody_cover_coef = case_when(
    .default = woody_cover_coef, 
    woody_cover_coef > uq ~ uq, 
    woody_cover_coef < lq ~ lq
  ))

p_wcc_sa <- ggplot() +
  geom_raster(data = dt_wcc_sa, aes(x = x, y = y, color = woody_cover_coef, fill = woody_cover_coef)) +
 # scale_color_scico(palette = "bam", midpoint = 0) +
  scale_fill_scico(palette = "bam", midpoint = 0) +
  labs(color = "Woody cover change (%/year)", fill = "Woody cover change (%/year)") +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))
p_wcc_sa


# Sub saharan Africa
v_africa <- vect(africa)

r_wcc <- rast("data/spatial/covariates/woody_cover_coef_km.tif")

r_wcc <- mask(r_wcc, africa)

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
  # scale_color_scico(palette = "bam", midpoint = 0) +
  scale_fill_scico(palette = "bam", midpoint = 0) +
  labs(color = "Woody cover change (%/year)", fill = "Woody cover change (%/year)") +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))
p_wcc

############################## Venter's Woody Cover Change ################################

r_vwct_sa <- rast("data/spatial/figure_1_tifs/sa_venter_woody_cover_trend_900m.tif")

r_vwct_sa <- mask(r_vwct_sa, sa)


dt_vwct_sa_raw <- as.data.frame(r_vwct_sa, xy = TRUE)
(uq <- quantile(dt_vwct_sa_raw$venter_woody_cover_trend, .99, na.rm = T))
(lq <- quantile(dt_vwct_sa_raw$venter_woody_cover_trend, .01, na.rm = T))
dt_vwct_sa_sa <- dt_vwct_sa_raw %>% 
  mutate(venter_woody_cover_trend = case_when(
    .default = venter_woody_cover_trend, 
    venter_woody_cover_trend > uq ~ uq, 
    venter_woody_cover_trend < lq ~ lq
  ))

p_vwct_sa <- ggplot() +
  geom_raster(data = dt_vwct_sa_sa, aes(x = x, y = y, color = venter_woody_cover_trend, fill = venter_woody_cover_trend)) +
  # scale_color_scico(palette = "bam", midpoint = 0) +
  scale_fill_scico(palette = "bam", midpoint = 0) +
  labs(color = "Venter's woody cover\nchange (%/year)", fill = "Venter's woody cover\nchange (%/year)") +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))
p_vwct_sa

############################## Woody Cover Heterogeneity ################################

r_wcsd <- rast("data/spatial/figure_1_tifs/sa_woody_cover_sd_coef_km.tif")

r_wcsd_sa <- mask(r_wcsd, sa)


dt_wcsd_sa_raw <- as.data.frame(r_wcsd_sa, xy = TRUE)
(uq <- quantile(dt_wcsd_sa_raw$woody_cover_coef, .99, na.rm = T))
(lq <- quantile(dt_wcsd_sa_raw$woody_cover_coef, .01, na.rm = T))
dt_wcsd_sa <- dt_wcsd_sa_raw %>% 
  mutate(woody_cover_coef = case_when(
    .default = woody_cover_coef, 
    woody_cover_coef > uq ~ uq, 
    woody_cover_coef < lq ~ lq
  ))

p_wcsd <- ggplot() +
  geom_raster(data = dt_wcsd_sa, aes(x = x, y = y, color = woody_cover_coef, fill = woody_cover_coef)) +
  # scale_color_scico(palette = "bam", midpoint = 0) +
  scale_fill_scico(palette = "cork", midpoint = 0) +
  labs(color = "Woody cover\nheterogeneity change", fill = "Woody cover\nheterogeneity change") +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))
p_wcsd


####################### Density plots ###########################################

dt$cat <- ifelse(dt$source %in% c("SANParks"), "no", "yes")


p_dens_map <- ggplot() +
  geom_density_line(data = dt, aes(x = prec_change, fill = prec_change),
                    linewidth = 1, fill = "wheat2", color = "wheat2") +
  scale_fill_viridis_c() +
  labs(fill = "MAP change/n(%/year)", x = "Annual precipitation change (%/year)", y = "") +
  theme_classic() +
  theme(axis.ticks.y = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.y = element_blank(),
        legend.position = "none")
p_dens_map

p_dens_mat <- ggplot() +
  geom_density_line(data = dt, aes(x = mat_change, fill = mat_change),
                    linewidth = 1, fill = "wheat2", color = "wheat2") +
  scale_fill_viridis_c() +
  labs(fill = "MAT change/n(%/year)", x = "Mean annual temperature change (%/year)", y = "") +
  theme_classic() +
  theme(axis.ticks.y = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.y = element_blank(),
        legend.position = "none")
p_dens_mat


p_dens_ndep <- ggplot() +
  geom_density_line(data = dt, aes(x = n_deposition, fill = n_deposition), 
                    linewidth = 1, fill = "wheat2", color = "wheat2") +
  scale_fill_viridis_c() +
  labs(fill = "MAT/n(°C)", x = bquote("Atmospheric Nitrogen Deposition ([kg/"~km^2*"])/year"), y = "") +
  theme_classic() +
  theme(axis.ticks.y = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.y = element_blank(),
        legend.position = "none")
p_dens_ndep

p_dens_hbm <- ggplot() +
  geom_density_line(data = dt, aes(x = herbi_biomass_kgkm2, fill = MAT), 
                    linewidth = 1, fill = "wheat2", color = "wheat2") +
  scale_fill_viridis_c() +
  labs(fill = "/n(°C)", x = bquote("Herbivore Biomass (kg/"~km^2*")"), y = "") +
  theme_classic() +
  theme(axis.ticks.y = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.y = element_blank(),
        legend.position = "none")
p_dens_hbm


p_dens_hsp <- ggplot() +
  geom_density_line(data = dt, aes(x = n_herbi_sp_reserve, fill = MAT),
                    linewidth = 1, fill = "wheat2", color = "wheat2") +
  scale_fill_viridis_c() +
  labs(fill = "/n(°C)", x = "Herbivore Species Richness", y = "") +
  theme_classic() +
  theme(axis.ticks.y = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.y = element_blank(),
        legend.position = "none")
p_dens_hsp


#Combine

p_dens <- grid.arrange(p_dens_ndep, p_dens_mat, p_dens_map, p_dens_hbm, p_dens_hsp, ncol = 1)

p_upper <- grid.arrange(p_dens, p_cent, widths = c(1, 2.5))

p_lower <- grid.arrange(p_wcc, p_vwct, p_wcsd,
                        ncol = 3)

p_fig1 <- grid.arrange(p_upper, p_lower, heights = c(2, 1.25))

ggsave(plot = p_fig1, "builds/plots/revision/figure_1.png", dpi = 600, height = 12, width = 14)
