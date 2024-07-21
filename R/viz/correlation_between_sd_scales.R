
source("R/functions/get.heterogeneity.R")


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

range(dt$MAP)
mean(dt$MAP)

range(dt$MAT)
mean(dt$MAT)

range(dt$n_herbi_sp_reserve)
mean(dt$n_herbi_sp_reserve)

range(dt$herbi_biomass_ha)
mean(dt$herbi_biomass_ha)

range(dt$tree_cover_mean)
mean(dt$tree_cover_mean)

range(dt$woody_cover_trend_venter2019)
mean(dt$woody_cover_trend_venter2019)

nrow(dt[woody_cover_trend_venter2019 > 0,])/nrow(dt)

## can't be shared unfortunately. Contact jonas.trepel@bio.au.dk for any questions
dt.shape <- st_read("data/reserve_shapes.gpkg")

### file to big to upload. Contact jonas.trepel@bio.au.dk for any questions! 
### get woody cover SD 
tc <- rast("../../../../../resources/spatial/meta_canopy_height/sa_tree_cover_100mmean.tif")


sd_agg <- function(x, ...) {
  return(sd(x, na.rm = TRUE))
}

tc.500m.sd <- aggregate(tc, fact=5, fun=sd_agg)

tc.1k.sd <- aggregate(tc, fact=10, fun=sd_agg)


#### extract -------

sd500.extr <- get.heterogeneity(vector = dt.shape, grid = NULL, id.col = "reserve_name", raster = tc.500m.sd)
setnames(sd500.extr, c("mean"), 
         c("tree_cover_sd_100_500"))

sd1k.extr <- get.heterogeneity(vector = dt.shape, grid = NULL, id.col = "reserve_name", raster = tc.1k.sd)
setnames(sd1k.extr, c("mean"), 
         c("tree_cover_sd_100_1k"))


dt.plot <- dt %>% 
  left_join(sd1k.extr) %>% 
  left_join(sd500.extr)

############ plots

cor.test(dt.plot$tree_cover_sd_100, dt.plot$tree_cover_sd_500)
p1 <- ggplot(data = dt.plot, aes(x = tree_cover_sd_100, y = tree_cover_sd_500)) +
  geom_point() +
  labs(x = "Tree cover SD (10m pixels on 100m scale)", y = "Tree cover SD (10m pixels on 500m scale)") +
  annotate("text", label = 'Cor = 0.96\np = <0.0001', y = 30, x = 5) +
  theme_classic()
p1  


cor.test(dt.plot$tree_cover_sd_100_500, dt.plot$tree_cover_sd_100_1k)
p2 <- ggplot(data = dt.plot, aes(x = tree_cover_sd_100_500, y = tree_cover_sd_100_1k)) +
  geom_point() +
  labs(x = "Tree cover SD (100m pixels on 500m scale)", y = "Tree cover SD (100m pixels on 1000m scale)") +
  annotate("text", label = 'Cor = 0.98\np = <0.0001', y = 30, x = 5) +
  theme_classic()
p2  

cor.test(dt.plot$tree_cover_sd_100, dt.plot$tree_cover_sd_100_500)
p3 <- ggplot(data = dt.plot, aes(x = tree_cover_sd_100, y = tree_cover_sd_100_500)) +
  geom_point() +
  labs(x = "Tree cover SD (10m pixels on 100m scale)", y = "Tree cover SD (100m pixels on 500m scale)") +
  annotate("text", label = 'Cor = 0.87\np = <0.0001', y = 30, x = 5) +
  theme_classic()
p3  


cor.test(dt.plot$tree_cover_sd_500, dt.plot$tree_cover_sd_100_500)
p4 <- ggplot(data = dt.plot, aes(x = tree_cover_sd_500, y = tree_cover_sd_100_500)) +
  geom_point() +
  labs(x = "Tree cover SD (10m pixels on 500m scale)", y = "Tree cover SD (100m pixels on 500m scale)") +
  annotate("text", label = 'Cor = 0.97\np = <0.0001', y = 30, x = 5) +
  theme_classic()
p4  

cor.test(dt.plot$tree_cover_sd_100, dt.plot$tree_cover_sd_100_1k)
p5 <- ggplot(data = dt.plot, aes(x = tree_cover_sd_100, y = tree_cover_sd_100_1k)) +
  geom_point() +
  labs(x = "Tree cover SD (10m pixels on 100m scale)", y = "Tree cover SD (100m pixels on 1000m scale)") +
  annotate("text", label = 'Cor = 0.84\np = <0.0001', y = 30, x = 5) +
  theme_classic()
p5 

cor.test(dt.plot$tree_cover_sd_500, dt.plot$tree_cover_sd_100_1k)
p6 <- ggplot(data = dt.plot, aes(x = tree_cover_sd_500, y = tree_cover_sd_100_1k)) +
  geom_point() +
  labs(x = "Tree cover SD (10m pixels on 500m scale)", y = "Tree cover SD (100m pixels on 1000m scale)") +
  annotate("text", label = 'Cor = 0.95\np = <0.0001', y = 30, x = 5) +
  theme_classic()
p6 

p <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
ggsave(plot = p, "builds/plots/july/heterogeneity_scale_plot.png", dpi = 600, height = 10.5, width = 12)
