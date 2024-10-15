


source("R/functions/get.heterogeneity.R")

library(terra)
library(tidyverse)
library(data.table)
library(sf)
library(tidylog)
library(mapview)
library(exactextractr)
library(grid)
library(MuMIn)
library(gridExtra)
library(rnaturalearth)

### load reserve data to check something real quick 
all.pas <- st_read("data/spatial/african_pas.gpkg") ## output from script: "get_african_pas.R".
unique(all.pas$IUCN_CAT)
africa <- ne_countries(continent = "Africa", type = "map_units")

dt.desig <- as.data.frame(table(all.pas$DESIG_ENG)) %>% arrange(-Freq)

dt.desig
#remove the biosphere reserves
strict.pas.raw <- all.pas %>% filter(!grepl("Biosphere Reserve", DESIG_ENG) 
                        & !WDPA_PID %in% c("555563456", "555512160", "903059")
                        & !grepl("Protected Environment", DESIG_ENG)
                        & !grepl("World Heritage Site", DESIG_ENG)
                        & !grepl("Botanical Garden", DESIG_ENG)
                        & !grepl("Mountain Catchment Area", DESIG_ENG)
                        & !grepl("Marine Protected Area", DESIG_ENG) & !grepl("Ramsar Site", DESIG_ENG) & !grepl("Forest", DESIG_ENG)
                        & !grepl("Buffer", DESIG_ENG)) %>% 
  filter(DESIG_ENG %in% c("National Park", "National Reserve", "Natural Reserve", "Nature Reserve", "Total Wildlife Reserve", 
                          "Regional Nature Park", "Nature Conservation Reserve", "Wilderness Area", "Private Nature Reserve", 
                          "Area of Absolute Protection", "Nature Forest Reserve", "Biological Reserve", "Protected area", 
                          "National park", "Conservation Area", "Private Protected Area", "Terrestrial protected area", "Protected Area",
                          "New Protected Area", "Nature reserve", "Private Reserve", "National Park") | IUCN_CAT %in% c("Ia","Ib", "II")) %>%
  dplyr::select(WDPA_PID, DESIG_ENG, GIS_AREA, NAME)
mapview(strict.pas.raw)


#get the ones that were included in the south african scale but are for some reason missing here: 
## output from script: "prepare_sa_pas"
sa.pa.ids <- fread("data/south_african_pas_w_covariates.csv") %>% dplyr::select(WDPA_PID) %>% pull()

alin <- strict.pas.raw %>% dplyr::select(WDPA_PID) %>% as.data.table() %>% mutate(geom = NULL) %>% pull()

## output from script: "prepare_sa_pas"
sa.pa.shp <- st_read("data/spatial/south_african_pas_w_covariates.shp") %>% rename(WDPA_PID = WDPA_PI,
                                                                            DESIG_ENG = DESIG_E, 
                                                                            GIS_AREA = GIS_ARE, 
                                                                            geom = geometry) %>% dplyr::select(NAME, WDPA_PID, DESIG_ENG, GIS_AREA)

missing <- sa.pa.shp %>% filter(!WDPA_PID %in% alin) 


strict.pas <- rbind(strict.pas.raw, missing)
nrow(strict.pas %>% filter(WDPA_PID %in% sa.pa.ids))
table(strict.pas$DESIG_ENG)

#### extract covariates -------------------------


## MAT---------------

mat <- rast("../../../../../resources/spatial/Chelsa_Climate/CHELSA_bio1_1981-2010_V.2.1.tif") 


mat.extr <- get.heterogeneity(vector = strict.pas, grid = NULL, id.col = "WDPA_PID", raster = mat)
setnames(mat.extr, c("mean"), 
         c("MAT"))


## MAP---------------
map <- rast("../../../../../resources/spatial/Chelsa_Climate/CHELSA_bio12_1981-2010_V.2.1.tif") 
plot(map)

map.extr <- get.heterogeneity(vector = strict.pas, grid = NULL, id.col = "WDPA_PID", raster = map)
setnames(map.extr, c("mean"), 
         c("MAP"))



## Fire frequency ---------------
fir <- rast("../../../../../resources/spatial/Fire/FireEventsBetween2001And2021Africa.tif")
range(values(fir), na.rm = T)


values(fir) <- ifelse(is.na(values(fir)), 0, values(fir))


fir.extr <- get.heterogeneity(vector = strict.pas, grid = NULL, id.col = "WDPA_PID", raster = fir)
setnames(fir.extr, c("mean"), 
         c("fire_events_since_2001"))




## extract N deposition -----------

ndep <- rast("../../../../../resources/spatial/N_deposition_Rubin_etal/total_N_dep.tif")
plot(ndep)

ndep.extr <- get.heterogeneity(vector = strict.pas, grid = NULL, id.col = "WDPA_PID", raster = ndep)
setnames(ndep.extr, c("mean"), 
         c("n_deposition"))


## Proportion burned area ---------------
fir <- rast("../../../../../resources/spatial/Fire/FireEventsBetween2001And2021Africa.tif")
#plot(dsf)

dsf.burned <- fir
#values(dsf.burned) <- ifelse(values(dsf) < 3650 & values(dsf) > 1, 1, 0)
values(dsf.burned) <- ifelse(is.na(values(dsf.burned)), 0, 1)

plot(dsf.burned)


dsf.burned.extr <- get.heterogeneity(vector = strict.pas, grid = NULL, id.col = "WDPA_PID", raster = dsf.burned)
setnames(dsf.burned.extr, c("mean"), 
         c("prop_burned_area"))


### import GEE stuff
venter.trend.raw <- fread("data/data_fragments/VenterWoodyStatsMeanAfricanPas.csv") %>% 
  dplyr::select(WDPA_PID, woody_cover_trend_venter2019) %>% 
  mutate(WDPA_PID = as.character(WDPA_PID)) %>% 
  rename(woody_trend_venter2019 = woody_cover_trend_venter2019)

sa.venter.raw <- fread("data/south_african_pas_w_covariates.csv") %>% dplyr::select(WDPA_PID, woody_trend_venter2019) %>% mutate(WDPA_PID = as.character(WDPA_PID))

sa.venter <-  sa.venter.raw %>% filter(!WDPA_PID %in% c(unique(venter.trend.raw$WDPA_PID)))

venter.trend <- rbind(sa.venter, venter.trend.raw) %>% unique()
n_distinct(venter.trend$WDPA_PID)

quantile(venter.trend$woody_trend_venter2019)


pa.cov.0.1 <- strict.pas %>% 
  as.data.table() %>% 
  mutate(geometry = NULL) %>% 
  dplyr::select(NAME, WDPA_PID, DESIG_ENG, GIS_AREA) %>% 
  left_join(map.extr) %>% 
  left_join(mat.extr) %>% 
  left_join(fir.extr) %>% 
  left_join(ndep.extr) %>% 
  left_join(dsf.burned.extr) %>% 
  left_join(venter.trend) %>% 
  unique() %>% 
  filter(complete.cases(.))

nrow(pa.cov.0.1 %>% filter(WDPA_PID %in% sa.pa.ids))

pa.cov.shp <- pa.cov.0.1 %>% left_join(strict.pas[, c("NAME", "WDPA_PID", "GIS_AREA")]) %>% st_as_sf()

## spatial predictors: ------
library(spatialRF)

#The euclidian distance matrix:
sf_use_s2(FALSE)
c1 <- st_centroid(pa.cov.shp)
coords <- st_coordinates(c1)
c2 <- cbind(c1, coords) 

distance.matrix <-as.matrix(dist(cbind(c2$X, c2$Y)))
diag(distance.matrix) <- 0 #ged rid of diagonal 
distance.thresholds <- unname(round(quantile(distance.matrix, c(seq(0.05, .75, 0.1))), 1))

library(spatialRF)


#several distances
mems <- spatialRF::mem_multithreshold(
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds
)


# rank by moran's I

mem.rank <- spatialRF::rank_spatial_predictors(
  distance.matrix = distance.matrix,
  spatial.predictors.df = mems,
  ranking.method = "moran"
)

#order the data frame 
mems2 <- mems[, mem.rank$ranking]
head(mems2)

## add spatial predictors
pa.cov.shp$spatial_predictor1 <- mems2[,1]
pa.cov.shp$spatial_predictor2 <- mems2[,2]
pa.cov.shp$spatial_predictor3 <- mems2[,3]
pa.cov.shp$spatial_predictor4 <- mems2[,4]
pa.cov.shp$spatial_predictor5 <- mems2[,5]
# pa.cov.shp$sp6 <- mems2[,6]
# pa.cov.shp$sp7 <- mems2[,7]
# pa.cov.shp$sp8 <- mems2[,8]
# pa.cov.shp$sp9 <- mems2[,9]
# pa.cov.shp$sp10 <- mems2[,10]
# pa.cov.shp$sp11 <- mems2[,11]
# pa.cov.shp$sp12 <- mems2[,12]
# pa.cov.shp$sp13 <- mems2[,13]
# pa.cov.shp$sp14 <- mems2[,14]
# pa.cov.shp$sp15 <- mems2[,15]
# pa.cov.shp$sp16 <- mems2[,16]
# pa.cov.shp$sp17 <- mems2[,17]
# pa.cov.shp$sp18 <- mems2[,18]
# pa.cov.shp$sp19 <- mems2[,19]
# pa.cov.shp$sp20 <- mems2[,20]


s_preds <- pa.cov.shp %>% as.data.table() %>% mutate(geometry = NULL, source = NULL, geom = NULL)

pa.cov <- pa.cov.0.1 %>% 
  left_join(s_preds) %>%
  mutate(fire_events_since_2001 = ifelse(is.na(fire_events_since_2001), 0, fire_events_since_2001)) %>% 
  filter(complete.cases(.))



fwrite(pa.cov, "data/sub_saharan_african_pas_w_covariates.csv")
pa.cov.shp <- pa.cov %>% left_join(strict.pas[, c("NAME", "WDPA_PID", "GIS_AREA")]) %>% st_as_sf()
write_sf(pa.cov.shp, "data/spatial/sub_saharan_african_pas_w_covariates.shp", append = FALSE)
summary(pa.cov)

#### test if spatial predictor is even necessary 



dt.gbm <- pa.cov %>% 
  dplyr::select(woody_trend_venter2019, 
                MAT, MAP, n_deposition,
                fire_events_since_2001, prop_burned_area) %>% filter(complete.cases(.))
library(gbm)
gbmFit <- gbm(
  formula = woody_trend_venter2019 ~ .,
  distribution = "gaussian",
  data = dt.gbm,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.005,
  cv.folds = 5, #cross validation folds (how often training/test data split)
  n.cores = 2, # will use all cores by default
  verbose = TRUE
)  

sf_use_s2(FALSE)
c1 <- st_centroid(pa.cov.shp)
coords <- st_coordinates(c1)
dt.coords <- cbind(c1, coords) 

response.observed <- dt.gbm$woody_trend_venter2019
response.predicted <- predict(gbmFit, newdata = dt.gbm, n.trees = gbmFit$n.trees)
residuals <- response.observed - response.predicted
dt.coords$residuals <- response.observed - response.predicted



# same for all (in theory)
distance.matrix <- as.matrix(dist(cbind(dt.coords$X, dt.coords$Y)))
distance.thresholds <- unname(round(quantile(distance.matrix, c(seq(0.05, .95, 0.05))), 1))

mean(distance.matrix, na.rm = T)
#Moran's I test of the residuals
moran.test <- spatialRF::moran_multithreshold(
  x = residuals,
  distance.matrix = distance.matrix, 
  verbose = T,
  distance.thresholds = distance.thresholds
)
moran.p.nc <- moran.test$plot
moran.p.nc
moran.p.nc.top <- grid.arrange(moran.p.nc, top = textGrob("Woody cover change" ,gp=gpar(fontsize=14)))

### include spatial pred 

dt.gbm.sp <- pa.cov %>% 
  dplyr::select(woody_trend_venter2019, 
                MAT, MAP, n_deposition,
                fire_events_since_2001, prop_burned_area, spatial_predictor1, spatial_predictor2, spatial_predictor3,
                spatial_predictor4, spatial_predictor5) %>% filter(complete.cases(.))
library(gbm)
gbmFitSp <- gbm(
  formula = woody_trend_venter2019 ~ .,
  distribution = "gaussian",
  data = dt.gbm.sp,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.005,
  cv.folds = 5, #cross validation folds (how often training/test data split)
  n.cores = 2, # will use all cores by default
  verbose = TRUE
)  


response.observed.sp <- dt.gbm.sp$woody_trend_venter2019
response.predicted.sp <- predict(gbmFitSp, newdata = dt.gbm.sp, n.trees = gbmFitSp$n.trees)
residuals.sp <- response.observed.sp - response.predicted.sp
dt.coords$residuals_sp <- response.observed.sp - response.predicted.sp



# same for all 
distance.matrix <- as.matrix(dist(cbind(dt.coords$X, dt.coords$Y)))
distance.thresholds <- unname(round(quantile(distance.matrix, c(seq(0.05, .95, 0.05))), 1))

#Moran's I test of the residuals
moran.test.sp <- spatialRF::moran_multithreshold(
  x = residuals.sp,
  distance.matrix = distance.matrix, 
  verbose = T,
  distance.thresholds = distance.thresholds
)
moran.p.nc.sp <- moran.test.sp$plot
moran.p.nc.sp
moran.p.nc.sp.top <- grid.arrange(moran.p.nc.sp, top = textGrob("Woody cover change" ,gp=gpar(fontsize=14)))

