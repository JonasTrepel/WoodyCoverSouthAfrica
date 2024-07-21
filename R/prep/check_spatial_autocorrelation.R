
########################################################################## 
######## REQUIRES DATA THAT CANNOT BE PUBLISHED ##########################
###### please contact jonas.trepel@bio.au.dk for any questions ###########
########################################################################## 
library(tidyverse)
library(spatialRF)
library(terra)
library(sf)
library(data.table)
library(gbm)
library(gstat)
library(gridExtra)
library(grid)

dt <- fread("data/ReserveDataSouthAfricaFinal.csv") 



### test spatial autocorrelation -----

#Load locations

dt.shape <-  st_read("data/clean_data/reserve_shapes.gpkg")

sf_use_s2(FALSE)
x <- st_coordinates(st_centroid(dt.shape))[,1]
y <- st_coordinates(st_centroid(dt.shape))[,2]
reserve_name <- dt.shape$reserve_name

dt.coords.raw <- data.table(reserve_name = reserve_name, x = x, y = y)

dt.coords <- dt %>% left_join(dt.coords.raw)


sa.sh <- rnaturalearth::ne_countries(country = "south africa", type = "map_units")
sa.sh <- st_transform(sa.sh, crs(dt.shape))
sa.sh.buf <- st_buffer(sa.sh, dist = 1) #1 degree buffer because some centroids fall a bit out of the range (argh)


#check if coordinates align with expectations
ggplot() + geom_sf(data = sa.sh.buf, fill = "black") + geom_sf(data = sa.sh, fill = "grey") +
  geom_point(data = dt.coords[source == "SANParks", ], aes(x = x, y = y)) +
  geom_label(data = dt.coords[source == "SANParks", ], aes(x = x, y = y, label = reserve_name), nudge_y = 0.4)#beaut


crds <- st_coordinates(sa.sh.buf[,c(1,2)]) # get coordinates for SA shape
bound <- list(list(x = crds[,1], y = crds[,2], f = rep(0, nrow(crds)))) # define boundary (I guess)
knots <-unique(dt.coords[,c("x","y")]) #reserve locations as knots


#### fit models

### CURRENT WOODY COVER
dt.cwc <- dt %>%
  dplyr::select(tree_cover_mean,
                MAT, MAP,
                CW_mean_species_body_mass,  herbi_biomass_ha,
                fire_events_since_2001, prop_burned_area)

gbm.cwc <- gbm(
  formula = tree_cover_mean ~ .,
  distribution = "gaussian",
  data = dt.cwc,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.005,
  cv.folds = 5, #cross validation folds (how often training/test data split)
  n.cores = 2, # will use all cores by default
  verbose = TRUE
)


response.observed.cwc <- dt.cwc$tree_cover_mean
response.predicted.cwc <- predict(gbm.cwc, newdata = dt.cwc, n.trees = gbm.cwc$n.trees)
residuals_cwc <- response.observed.cwc - response.predicted.cwc

#dt.coords$residuals_cwc <- response.observed.cwc - response.predicted.cwc

# ## check autocorrelation using variograms
# cwc.coords.var <- gstat::variogram(residuals_cwc ~ 1, locations = ~ x + y, data = as.data.frame(dt.coords))
# cwc.coords.var.mod <- fit.variogram(cwc.coords.var, vgm(1, "Sph"))
# plot(cwc.coords.var, model = cwc.coords.var.mod) #
#
# png(file="builds/plots/july/vario_cwc.png",
#     width=600, height=350)
# plot(cwc.coords.var, model = cwc.coords.var.mod,
#      frame = FALSE, pch = 16,col = "#2E9FDF",  main = "Variogram for current woody cover") #
# dev.off()
# #dev.new()


# same for all (in theory)
distance.matrix <- as.matrix(dist(cbind(dt.coords$x, dt.coords$y)))
distance.thresholds <- unname(round(quantile(distance.matrix, c(seq(0.05, .95, 0.05))), 1))

#Moran's I test of the residuals
moran.test.cwc <- spatialRF::moran_multithreshold(
  x = residuals_cwc,
  distance.matrix = distance.matrix, 
  verbose = T,
  distance.thresholds = distance.thresholds
)
p.cwc <- moran.test.cwc$plot
p.cwc
p.cwc.top <- grid.arrange(p.cwc, top = textGrob("Current woody cover" ,gp=gpar(fontsize=14)))


### WOODY COVER TREND 
dt.wct  <- dt %>% 
  dplyr::select(woody_cover_trend_venter2019, 
                MAT, MAP, n_deposition, 
                CW_mean_species_body_mass, 
                herbi_fun_div_distq1, n_herbi_sp_reserve,
                grazer_biomass_ha, browser_biomass_ha, mixed_feeder_biomass_ha, herbi_biomass_ha,
                fire_events_since_2001, prop_burned_area)


gbm.wct <- gbm(
  formula = woody_cover_trend_venter2019 ~ .,
  distribution = "gaussian",
  data = dt.wct,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.005,
  cv.folds = 5, #cross validation folds (how often training/test data split)
  n.cores = 2, # will use all cores by default
  verbose = TRUE
)  

response.observed.wct <- dt.wct$woody_cover_trend_venter2019
response.predicted.wct <- predict(gbm.wct, newdata = dt.wct, n.trees = gbm.wct$n.trees)
residuals_wct <- response.observed.wct - response.predicted.wct
dt.coords$residuals_wct <- response.observed.wct - response.predicted.wct

## check autocorrelation using variograms
wct.coords.var <- gstat::variogram(residuals_wct ~ 1, locations = ~ x + y, data = as.data.frame(dt.coords))
wct.coords.var.mod <- fit.variogram(wct.coords.var, vgm(1, "Sph"))
plot(wct.coords.var, model = wct.coords.var.mod) #


png(file="builds/plots/july/vario_wct.png",
    width=600, height=350)
plot(wct.coords.var, model = wct.coords.var.mod,
     frame = FALSE, pch = 16,col = "#2E9FDF",  main = "Variogram for woody cover change") #
dev.off()
#dev.new()
# same for all (in theory)
distance.matrix <- as.matrix(dist(cbind(dt.coords$x, dt.coords$y)))
distance.thresholds <- unname(round(quantile(distance.matrix, c(seq(0.05, .95, 0.05))), 1))


#Moran's I test of the residuals
moran.test.wct <- spatialRF::moran_multithreshold(
  x = residuals_wct,
  distance.matrix = distance.matrix, 
  distance.thresholds = distance.thresholds,
  verbose = T
)
p.wct <- moran.test.wct$plot
p.wct
p.wct.top <- grid.arrange(p.wct, top = textGrob("Woody cover change" ,gp=gpar(fontsize=14)))


### WOODY COVER SD 
dt.wcsd <- dt %>% 
  dplyr::select(tree_cover_sd_100, 
                MAT, MAP, elevation_sd_1000, 
                CW_mean_species_body_mass, 
                herbi_fun_div_distq1, n_herbi_sp_reserve,
                grazer_biomass_ha, browser_biomass_ha, mixed_feeder_biomass_ha, herbi_biomass_ha,
                fire_events_since_2001, prop_burned_area)

gbm.wcsd <- gbm(
  formula = tree_cover_sd_100 ~ .,
  distribution = "gaussian",
  data = dt.wcsd,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.005,
  cv.folds = 5, #cross validation folds (how often training/test data split)
  n.cores = 2, # will use all cores by default
  verbose = TRUE
)  


response.observed.wcsd <- dt.wcsd$tree_cover_sd_100
response.predicted.wcsd <- predict(gbm.wcsd, newdata = dt.wcsd, n.trees = gbm.wcsd$n.trees)
residuals_wcsd <- response.observed.wcsd - response.predicted.wcsd
dt.coords$residuals_wcsd <- response.observed.wcsd - response.predicted.wcsd

## check autocorrelation using variograms
wcsd.coords.var <- gstat::variogram(residuals_wcsd ~ 1, locations = ~ x + y, data = as.data.frame(dt.coords))
wcsd.coords.var.mod <- fit.variogram(wcsd.coords.var, vgm(1, "Sph"))
plot(wcsd.coords.var, model = wcsd.coords.var.mod) #

png(file="builds/plots/july/vario_wcsd.png",
    width=600, height=350)
plot(wcsd.coords.var, model = wcsd.coords.var.mod,
     frame = FALSE, pch = 16,col = "#2E9FDF",  main = "Variogram for woody cover SD") #
dev.off()
dev.new()

# same for all (in theory)
distance.matrix <- as.matrix(dist(cbind(dt.coords$x, dt.coords$y)))
distance.thresholds <- unname(round(quantile(distance.matrix, c(seq(0.05, .95, 0.05))), 1))


#Moran's I test of the residuals
moran.test.wcsd <- spatialRF::moran_multithreshold(
  x = residuals_wcsd,
  distance.matrix = distance.matrix, #careful <- was build in another dt
  distance.thresholds = distance.thresholds,
  verbose = T
)
p.wcsd <- moran.test.wcsd$plot
p.wcsd
print(p.wcsd)
p.wcsd.top <- grid.arrange(p.wcsd, top = textGrob("Woody cover SD" ,gp=gpar(fontsize=14)))


### CANOPY HEIGHT SD 
dt.chsd <- dt %>% 
  dplyr::select(canopy_height_sd_100, 
                MAT, MAP, elevation_sd_1000, 
                CW_mean_species_body_mass, 
                herbi_fun_div_distq1, n_herbi_sp_reserve,
                grazer_biomass_ha, browser_biomass_ha, mixed_feeder_biomass_ha, herbi_biomass_ha,
                fire_events_since_2001, prop_burned_area)

gbm.chsd <- gbm(
  formula = canopy_height_sd_100 ~ .,
  distribution = "gaussian",
  data = dt.chsd,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.005,
  cv.folds = 5, #cross validation folds (how often training/test data split)
  n.cores = 2, # will use all cores by default
  verbose = TRUE
)  


response.observed.chsd <- dt.chsd$canopy_height_sd_100
response.predicted.chsd <- predict(gbm.chsd, newdata = dt.chsd, n.trees = gbm.chsd$n.trees)
residuals_chsd <- response.observed.chsd - response.predicted.chsd
dt.coords$residuals_chsd <- response.observed.chsd - response.predicted.chsd

## check autocorrelation using variograms
chsd.coords.var <- gstat::variogram(residuals_chsd ~ 1, locations = ~ x + y, data = as.data.frame(dt.coords))
chsd.coords.var.mod <- fit.variogram(chsd.coords.var, vgm(1, "Sph"))
plot(chsd.coords.var, model = chsd.coords.var.mod) #

png(file="builds/plots/july/vario_chsd.png",
    width=600, height=350)
plot(chsd.coords.var, model = chsd.coords.var.mod,
     frame = FALSE, pch = 16,col = "#2E9FDF",  main = "Variogram for canopy height SD") #
dev.off()
dev.new()

# same for all (in theory)
distance.matrix <- as.matrix(dist(cbind(dt.coords$x, dt.coords$y)))
distance.thresholds <- unname(round(quantile(distance.matrix, c(seq(0.05, .95, 0.05))), 1))
#Moran's I test of the residuals
moran.test.chsd <- spatialRF::moran_multithreshold(
  x = residuals_chsd,
  distance.matrix = distance.matrix, 
  distance.threshold = distance.thresholds,
  verbose = T
)
p.chsd <- moran.test.chsd$plot
p.chsd.top <- grid.arrange(p.chsd, top = textGrob("Canopy height SD" ,gp=gpar(fontsize=14)))

## combine morans I plots --- 

p.moran.nc <- grid.arrange(p.cwc.top, p.wct.top, p.wcsd.top, p.chsd.top, ncol = 2,  top = textGrob("Without spatial predictor" ,gp=gpar(fontsize=14)))
ggsave(plot=p.moran.nc, "builds/plots/july/morans_i_no_correction.png", dpi = 600, height = 10, width = 14)


###########################################################  
#########  INCLUDE SPATIAL PREDICTOR ########### ----------
###########################################################  

#### fit models 

### CURRENT WOODY COVER 
dt.cwcsp1 <- dt %>% 
  dplyr::select(tree_cover_mean, 
                MAT, MAP,  
                CW_mean_species_body_mass,  herbi_biomass_ha,
                fire_events_since_2001, prop_burned_area, sp1)

gbm.cwcsp1 <- gbm(
  formula = tree_cover_mean ~ .,
  distribution = "gaussian",
  data = dt.cwcsp1,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.005,
  cv.folds = 5, #cross validation folds (how often training/test data split)
  n.cores = 2, # will use all cores by default
  verbose = TRUE
)  


response.observed.cwcsp1 <- dt.cwcsp1$tree_cover_mean
response.predicted.cwcsp1 <- predict(gbm.cwcsp1, newdata = dt.cwcsp1, n.trees = gbm.cwcsp1$n.trees)
residuals_cwcsp1 <- response.observed.cwcsp1 - response.predicted.cwcsp1
dt.coords$residuals_cwcsp1 <- response.observed.cwcsp1 - response.predicted.cwcsp1

## check autocorrelation using variograms
cwcsp1.coords.var <- gstat::variogram(residuals_cwcsp1 ~ 1, locations = ~ x + y, data = as.data.frame(dt.coords))
cwcsp1.coords.var.mod <- fit.variogram(cwcsp1.coords.var, vgm(1, "Sph"))
plot(cwcsp1.coords.var, model = cwcsp1.coords.var.mod) #

png(file="builds/plots/july/vario_cwcsp1.png",
    width=600, height=350)
plot(cwcsp1.coords.var, model = cwcsp1.coords.var.mod,
     frame = FALSE, pch = 16,col = "#2E9FDF",  main = "Variogram for current woody cover") #
dev.off()
dev.new()

# same for all (in theory)
distance.matrix <- as.matrix(dist(cbind(dt.coords$x, dt.coords$y)))
distance.thresholds <- unname(round(quantile(distance.matrix, c(seq(0.05, .95, 0.05))), 1))

#Moran's I test of the residuals
moran.test.cwcsp1 <- spatialRF::moran_multithreshold(
  x = residuals_cwcsp1,
  distance.matrix = distance.matrix, #careful <- was build in another dt
  distance.thresholds = distance.thresholds,
  verbose = T
)
p.cwcsp1 <- moran.test.cwcsp1$plot
p.cwcsp1
p.cwcsp1.top <- grid.arrange(p.cwcsp1, top = textGrob("Current woody cover" ,gp=gpar(fontsize=14)))


### WOODY COVER TREND 
dt.wctsp1  <- dt %>% 
  dplyr::select(woody_cover_trend_venter2019, 
                MAT, MAP, n_deposition, 
                CW_mean_species_body_mass, 
                herbi_fun_div_distq1, n_herbi_sp_reserve,
                grazer_biomass_ha, browser_biomass_ha, mixed_feeder_biomass_ha, herbi_biomass_ha,
                fire_events_since_2001, prop_burned_area, sp1)


gbm.wctsp1 <- gbm(
  formula = woody_cover_trend_venter2019 ~ .,
  distribution = "gaussian",
  data = dt.wctsp1,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.005,
  cv.folds = 5, #cross validation folds (how often training/test data split)
  n.cores = 2, # will use all cores by default
  verbose = TRUE
)  

response.observed.wctsp1 <- dt.wctsp1$woody_cover_trend_venter2019
response.predicted.wctsp1 <- predict(gbm.wctsp1, newdata = dt.wctsp1, n.trees = gbm.wctsp1$n.trees)
residuals_wctsp1 <- response.observed.wctsp1 - response.predicted.wctsp1
dt.coords$residuals_wctsp1 <- response.observed.wctsp1 - response.predicted.wctsp1

## check autocorrelation using variograms
wctsp1.coords.var <- gstat::variogram(residuals_wctsp1 ~ 1, locations = ~ x + y, data = as.data.frame(dt.coords))
wctsp1.coords.var.mod <- fit.variogram(wctsp1.coords.var, vgm(1, "Sph"))
plot(wctsp1.coords.var, model = wctsp1.coords.var.mod) #

png(file="builds/plots/july/vario_wctsp1.png",
    width=600, height=350)
plot(wctsp1.coords.var, model = wctsp1.coords.var.mod,
     frame = FALSE, pch = 16,col = "#2E9FDF",  main = "Variogram for woody cover change") #
dev.off()
dev.new()

# same for all (in theory)
distance.matrix <- as.matrix(dist(cbind(dt.coords$x, dt.coords$y)))
distance.thresholds <- unname(round(quantile(distance.matrix, c(seq(0.05, .95, 0.05))), 1))



#Moran's I test of the residuals
moran.test.wctsp1 <- spatialRF::moran_multithreshold(
  x = residuals_wctsp1,
  distance.matrix = distance.matrix, 
  distance.thresholds = distance.thresholds,
  verbose = T
)
p.wctsp1 <- moran.test.wctsp1$plot
p.wctsp1
p.wctsp1.top <- grid.arrange(p.wctsp1, top = textGrob("Woody cover change" ,gp=gpar(fontsize=14)))


### WOODY COVER SD 
dt.wcsdsp1 <- dt %>% 
  dplyr::select(tree_cover_sd_100, 
                MAT, MAP, elevation_sd_1000, 
                CW_mean_species_body_mass, 
                herbi_fun_div_distq1, n_herbi_sp_reserve,
                grazer_biomass_ha, browser_biomass_ha, mixed_feeder_biomass_ha, herbi_biomass_ha,
                fire_events_since_2001, prop_burned_area)

gbm.wcsdsp1 <- gbm(
  formula = tree_cover_sd_100 ~ .,
  distribution = "gaussian",
  data = dt.wcsdsp1,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.005,
  cv.folds = 5, #cross validation folds (how often training/test data split)
  n.cores = 2, # will use all cores by default
  verbose = TRUE
)  


response.observed.wcsdsp1 <- dt.wcsdsp1$tree_cover_sd_100
response.predicted.wcsdsp1 <- predict(gbm.wcsdsp1, newdata = dt.wcsdsp1, n.trees = gbm.wcsdsp1$n.trees)
residuals_wcsdsp1 <- response.observed.wcsdsp1 - response.predicted.wcsdsp1
dt.coords$residuals_wcsdsp1 <- response.observed.wcsdsp1 - response.predicted.wcsdsp1

## check autocorrelation using variograms
wcsdsp1.coords.var <- gstat::variogram(residuals_wcsdsp1 ~ 1, locations = ~ x + y, data = as.data.frame(dt.coords))
wcsdsp1.coords.var.mod <- fit.variogram(wcsdsp1.coords.var, vgm(1, "Sph"))
plot(wcsdsp1.coords.var, model = wcsdsp1.coords.var.mod) #

png(file="builds/plots/july/vario_wcsdsp1.png",
    width=600, height=350)
plot(wcsdsp1.coords.var, model = wcsdsp1.coords.var.mod,
     frame = FALSE, pch = 16,col = "#2E9FDF",  main = "Variogram for woody cover SD") #
dev.off()
dev.new()

# same for all (in theory)
distance.matrix <- as.matrix(dist(cbind(dt.coords$x, dt.coords$y)))
distance.thresholds <- unname(round(quantile(distance.matrix, c(seq(0.05, .95, 0.05))), 1))


#Moran's I test of the residuals
moran.test.wcsdsp1 <- spatialRF::moran_multithreshold(
  x = residuals_wcsdsp1,
  distance.matrix = distance.matrix, 
  distance.thresholds = distance.thresholds,
  verbose = T
)
p.wcsdsp1 <- moran.test.wcsdsp1$plot
p.wcsdsp1

p.wcsdsp1.top <- grid.arrange(p.wcsdsp1, top = textGrob("Woody cover SD" ,gp=gpar(fontsize=14)))


### CANOPY HEIGHT SD 
dt.chsdsp1 <- dt %>% 
  dplyr::select(canopy_height_sd_100, 
                MAT, MAP, elevation_sd_1000, 
                CW_mean_species_body_mass, 
                herbi_fun_div_distq1, n_herbi_sp_reserve,
                grazer_biomass_ha, browser_biomass_ha, mixed_feeder_biomass_ha, herbi_biomass_ha,
                fire_events_since_2001, prop_burned_area, sp1)

gbm.chsdsp1 <- gbm(
  formula = canopy_height_sd_100 ~ .,
  distribution = "gaussian",
  data = dt.chsdsp1,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.005,
  cv.folds = 5, #cross validation folds (how often training/test data split)
  n.cores = 2, # will use all cores by default
  verbose = TRUE
)  


response.observed.chsdsp1 <- dt.chsdsp1$canopy_height_sd_100
response.predicted.chsdsp1 <- predict(gbm.chsdsp1, newdata = dt.chsdsp1, n.trees = gbm.chsdsp1$n.trees)
residuals_chsdsp1 <- response.observed.chsdsp1 - response.predicted.chsdsp1
dt.coords$residuals_chsdsp1 <- response.observed.chsdsp1 - response.predicted.chsdsp1

## check autocorrelation using variograms
chsdsp1.coords.var <- gstat::variogram(residuals_chsdsp1 ~ 1, locations = ~ x + y, data = as.data.frame(dt.coords))
chsdsp1.coords.var.mod <- fit.variogram(chsdsp1.coords.var, vgm(1, "Sph"))
plot(chsdsp1.coords.var, model = chsdsp1.coords.var.mod) #

png(file="builds/plots/july/vario_chsdsp1.png",
    width=600, height=350)
plot(chsdsp1.coords.var, model = chsdsp1.coords.var.mod,
     frame = FALSE, pch = 16,col = "#2E9FDF",  main = "Variogram for canopy height SD") #
dev.off()
dev.new()

# same for all (in theory)
distance.matrix <- as.matrix(dist(cbind(dt.coords$x, dt.coords$y)))
distance.thresholds <- unname(round(quantile(distance.matrix, c(seq(0.05, .95, 0.05))), 1))


#Moran's I test of the residuals
moran.test.chsdsp1 <- spatialRF::moran_multithreshold(
  x = residuals_chsdsp1,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  verbose = T
)
p.chsdsp1 <- moran.test.chsdsp1$plot
p.chsdsp1.top <- grid.arrange(p.chsdsp1, top = textGrob("Canopy height SD" ,gp=gpar(fontsize=14)))


## combine morans I plots --- 

p.moran.nc <- grid.arrange(p.cwcsp1.top, p.wctsp1.top, p.wcsdsp1.top, p.chsdsp1.top, ncol = 2,  top = textGrob("Including spatial predictor" ,gp=gpar(fontsize=14)))
ggsave(plot=p.moran.nc, "builds/plots/july/morans_i_w_sp.png", dpi = 600, height = 10, width = 14)
