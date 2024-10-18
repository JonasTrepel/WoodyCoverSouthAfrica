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

dt <- fread("data/ReserveDataSouthAfricaFinal.csv")




########### RESERVE DISTRIBUTION ################################

## Biome -----------

veg <- sf::read_sf("../../../../../resources/spatial/Vegetation_Map_SA_2018/NVM2018_AEA_V22_7_16082019_final.shp")

veg <- st_transform(veg, crs = 4326)
veg <- st_sf(veg) %>% 
  mutate(BIOMEID_18 = ifelse(veg$BIOME_18 == "Albany Thicket", 5, veg$BIOMEID_18))
# Perform a spatial join to associate "pa" polygons with "veg" polygons

table(veg$BIOMEID_18)

# Get the extent of the "veg" data
veg_bbox <- st_bbox(veg)

# Create a raster with the matching extent and resolution
r.temp1k <- rast(
  extent = veg_bbox, res = 0.01
)

# Rasterize the "veg" polygons into the matching raster
table(veg$BIOME_18)
biome.r <- rasterize(veg, r.temp1k, field = "BIOMEID_18")
plot(biome.r)

africa <- rnaturalearth::ne_countries(scale = 50, continent = c("Africa"),
                                      returnclass = "sf") 

sa <- africa %>% filter(name == "South Africa") %>% st_transform(crs = 4326)


biome.r.mask <- mask(biome.r, sa)


leg <- unique(veg %>% dplyr::select(c("BIOME_18", "BIOMEID_18")))
leg$geometry <- NULL
setDT(leg)
leg <- unique(leg)
leg <- leg[!BIOMEID_18 == 0]
leg

dt.biome <- as.data.frame(biome.r.mask, xy = TRUE) %>% left_join(leg)

### other data ----------------------
## can't be uploaded. Please contact me and we'll find a solution: jonas.trepel@bio.au.dk
dt.shape <- st_read("data/reserve_shapes.gpkg")

sf_use_s2(FALSE)
dt.cent <- dt.shape %>%
  left_join(dt) %>% st_centroid()
mapview(dt.cent)
sf_use_s2(TRUE)

africa <- rnaturalearth::ne_countries(scale = 50, continent = c("Africa"),
                                      returnclass = "sf") 

sa <- africa %>% filter(name == "South Africa") %>% st_transform(crs = 4326)


grid <- st_make_grid(dt.shape, cellsize = c(1, 1), what = "polygons", square = F) %>% 
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



grid.cent <- st_centroid(grid)
unique(dt.biome$BIOME_18)
scales::show_col(viridis(12))
library(rcartocolor)

nColor <- 12
scales::show_col(carto_pal(nColor, "Safe"))

pal = c("Albany Thicket" = "#6699CC", "Azonal Vegetation" = "#888888", "Savanna" = "#999933", 
        "Forests" = "#332288", "Grassland" = "#117733", 
        "Nama-Karoo" = "#AA4499", "Indian Ocean Coastal Belt" = "#44AA99", 
        "Desert" = "#DDCC77", "Succulent Karoo" = "#882255", 
        "Fynbos" = "#661100")

p.cent <- ggplot() +
  geom_tile(data = dt.biome[!is.na(dt.biome$BIOME_18),], aes(x = x, y = y, color = BIOME_18, fill = BIOME_18), alpha = 1) + 
  geom_sf(data = sa, fill = "white", linewidth = 0.1, alpha = 0.15) +
  geom_sf(data = grid.cent[!grid.cent$n_reserves == 0,], aes(size = n_reserves), alpha = 1) +
  labs(size = "Number of/nreserves") +
  labs(color = "Biome", fill = "Biome") +
 # ylim(35, 22) +
#  xlim(17, 33) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme_void() +
  theme(
    legend.text = element_text(size = 12),  # Adjust the size of the legend text
    legend.title = element_text(size = 14), # Adjust the size of the legend title
    legend.key.size = unit(0.5, "cm"),
    legend.position = "right"# Adjust the size of the legend keys
  )
p.cent

range(dt.biome$x)
range(dt.biome$y)
range(dt.biome$y)

library(viridis)
viridis(12)
############################## Woody cover ################################

tc <- rast("../../../../../resources/spatial/meta_canopy_height/sa_tree_cover_100mmean.tif")

tc.1k <- resample(tc, r.temp1k)

sa.tc <- mask(tc.1k, sa)


dt.tc <- as.data.frame(sa.tc, xy = TRUE)

p.tc <- ggplot() +
  geom_tile(data = dt.tc, aes(x = x, y = y, color = lyr.1, fill = lyr.1)) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  labs(color = "Woody cover (%)", fill = "Woody cover (%)") +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))
p.tc


############################## Woody cover SD ################################

tc <- rast("../../../../../resources/spatial/meta_canopy_height/sa_tree_cover_100mmean.tif")

sd_agg <- function(x, ...) {
  return(sd(x, na.rm = TRUE))
}
tc.1k.sd <- aggregate(tc, fact=10, fun=sd_agg)

sa.tc.sc <- mask(tc.1k.sd, sa)


dt.tc.sd <- as.data.frame(sa.tc.sc, xy = TRUE)

p.tc.sd <- ggplot() +
  geom_tile(data = dt.tc.sd, aes(x = x, y = y, color = lyr.1, fill = lyr.1)) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  labs(color = "Woody cover heterogeneity", fill = "Woody cover heterogeneity") +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))
p.tc.sd


############################# Woody cover trend #######################################
## mask data: 

wct <- rast("../../../../../resources/spatial/ZanderVenterData/woody_cover_trend_venter2019_250m.tif")

lc <- rast("../../../../../resources/spatial/LandCover/GlobalLandCoverCopernicus2019.tif")
saLc <- mask(lc, sa)
plot(saLc)
lcMask <- saLc
undesiredClasses <- c(40, 50) #urban and forest
lcMask <- classify(saLc, rcl = cbind(undesiredClasses, 1), others = 0)
plot(lcMask)

lcMask <- crop(lcMask, wct) 
wctPreMask <- exactextractr::exact_resample(x = wct, y = lcMask, fun = "mean")

wctMasked <- mask(wctPreMask, lcMask)

plot(wctMasked)
plot(lcMask)

wct1k <- exactextractr::exact_resample(x = wctMasked, y = r.temp1k, fun = "mean")
#lcMask1k <- exactextractr::exact_resample(x = lcMask, y = r.temp1k, fun = "max")

sa.wct <- mask(wct1k, sa)

dt.wct <- as.data.frame(sa.wct, xy = TRUE) %>% as.data.table()
dt.mask <- as.data.frame(lcMask, xy = TRUE) %>% as.data.table() %>% 
  filter(`2019_discrete_classification` == 1)


p.wct <- ggplot() +
  geom_tile(data = dt.wct[lyr.1 > -2 & lyr.1 < 2,], aes(x = x, y = y, color = lyr.1, fill = lyr.1)) +
  geom_tile(data = dt.mask, aes(x = x, y = y), color = "grey") +
  scale_color_met_c(name = "Isfahan1", direction = -1) +
  scale_fill_met_c(name = "Isfahan1", direction = -1) +
  labs(color = "Woody cover change (%/year)", fill = "Woody cover change (%/year)") +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))
p.wct


####################### Density plots ###########################################

dt$cat <- ifelse(dt$source %in% c("SANParks"), "no", "yes")


p.dens.map <- ggplot() +
  geom_density_line(data = dt, aes(x = MAP, fill = MAP), linewidth = 1.5, fill = "grey90") +
  scale_fill_viridis_c() +
  labs(fill = "MAP/n(mm)", x = "Mean annual precipitation (mm)", y = "") +
  theme_classic() +
  theme(axis.ticks.y = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.y = element_blank(),
        legend.position = "none")
p.dens.map

p.dens.mat <- ggplot() +
  geom_density_line(data = dt, aes(x = MAT, fill = MAT), linewidth = 1.5, fill = "grey90") +
  scale_fill_viridis_c() +
  labs(fill = "MAT/n(°C)", x = "Mean Annual Temperature (°C)", y = "") +
  theme_classic() +
  theme(axis.ticks.y = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.y = element_blank(),
        legend.position = "none")
p.dens.mat

p.dens.ndep <- ggplot() +
  geom_density_line(data = dt, aes(x = n_deposition, fill = n_deposition), linewidth = 1.5, fill = "grey90") +
  scale_fill_viridis_c() +
  labs(fill = "MAT/n(°C)", x = bquote("Atmospheric Nitrogen Deposition ([kg/"~km^2*"])/year"), y = "") +
  theme_classic() +
  theme(axis.ticks.y = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.y = element_blank(),
        legend.position = "none")
p.dens.ndep

p.dens.hbm <- ggplot() +
  geom_density_line(data = dt, aes(x = herbi_biomass_kgkm2, fill = MAT), linewidth = 1.5, fill = "grey90") +
  scale_fill_viridis_c() +
  labs(fill = "/n(°C)", x = bquote("Herbivore Biomass (kg/"~km^2*")"), y = "") +
  theme_classic() +
  theme(axis.ticks.y = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.y = element_blank(),
        legend.position = "none")
p.dens.hbm


p.dens.hsp <- ggplot() +
  geom_density_line(data = dt, aes(x = n_herbi_sp_reserve, fill = MAT), linewidth = 1.5, fill = "grey90") +
  scale_fill_viridis_c() +
  labs(fill = "/n(°C)", x = "Herbivore Species Richness", y = "") +
  theme_classic() +
  theme(axis.ticks.y = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.y = element_blank(),
        legend.position = "none")
p.dens.hsp


p.dens <- grid.arrange(p.dens.map, p.dens.ndep, p.dens.hbm, p.dens.hsp, ncol = 1)

p.upper <- grid.arrange(p.dens, p.cent, widths = c(1, 2.5))

p.lower <- grid.arrange(p.tc, p.wct, p.tc.sd, ncol = 3)

p.fig1 <- grid.arrange(p.upper, p.lower, heights = c(2, 1))

ggsave(plot = p.fig1, "builds/plots/september/figure1.png", dpi = 600, height = 12, width = 14)
#ggsave(plot = p.fig1, "builds/plots/july/figure1.svg", dpi = 600, height = 12, width = 14)


