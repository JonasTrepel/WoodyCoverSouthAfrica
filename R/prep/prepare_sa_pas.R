
# N deposition sensitivity analysis 1

## all strict PAs in South Africa 

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


#dt <- fread("data/ReserveDataSouthAfricaFinal.csv") 



#load and combine PA data -------
sf_use_s2(FALSE)


##### FILES NOT INCLUDED #####
## all South African PAs 
pa_1 <- read_sf("data/spatial/pa_shapes/raw/WDPA_WDOECM_Sep2023_Public_ZAF_shp/WDPA_WDOECM_Sep2023_Public_ZAF_shp_0/WDPA_WDOECM_Sep2023_Public_ZAF_shp-polygons.shp")
pa_2 <- read_sf("data/spatial/pa_shapes/raw/WDPA_WDOECM_Sep2023_Public_ZAF_shp/WDPA_WDOECM_Sep2023_Public_ZAF_shp_1/WDPA_WDOECM_Sep2023_Public_ZAF_shp-polygons.shp")
pa_3 <- read_sf("data/spatial/pa_shapes/raw/WDPA_WDOECM_Sep2023_Public_ZAF_shp/WDPA_WDOECM_Sep2023_Public_ZAF_shp_2/WDPA_WDOECM_Sep2023_Public_ZAF_shp-polygons.shp")

pa <- rbind(pa_1, pa_2, pa_3)

#subset to terrestrial South African borders 
africa <- rnaturalearth::ne_countries(scale = 50, continent = c("Africa"),
                                      returnclass = "sf") 

sa <- africa %>% filter(name == "South Africa")
#mapview(sa)



sa_t <- st_transform(sa, st_crs(crs(pa)))

pa <- pa %>% st_intersection(sa_t)

str(pa)

#remove the biosphere reserves
pa <- pa %>% filter(!grepl("Biosphere Reserve", pa$DESIG_ENG) 
                    & !WDPA_PID %in% c("555563456", "555512160", "903059")
                    & !grepl("Protected Environment", pa$DESIG)
                    &
                      !grepl("World Heritage Site", pa$DESIG)
                    & !grepl("Botanical Garden", pa$DESIG)
                    & !grepl("Mountain Catchment Area", pa$DESIG)
                    & !grepl("Marine Protected Area", pa$DESIG) & !grepl("Ramsar Site", pa$DESIG) & !grepl("Forest", pa$DESIG)) 

#mapview(pa) #WDPAID: 555563456

table(pa$DESIG_ENG)


## Biome -----------

veg <- sf::read_sf("data/spatial/covariates/Vegetation_Map_SA_2018/NVM2018_AEA_V22_7_16082019_final.shp")


veg <- st_transform(veg, crs = 4326)
veg <- st_sf(veg) %>% 
  mutate(BIOMEID_18 = ifelse(veg$BIOME_18 == "Albany Thicket", 5, veg$BIOMEID_18))
# Perform a spatial join to associate "pa" polygons with "veg" polygons

table(veg$BIOMEID_18)

# Get the extent of the "veg" data
veg_bbox <- st_bbox(veg)

# Calculate the desired resolution in degrees
desired_resolution <- c(0.01, 0.01)  # Approximately 1 km at the equator

# Calculate the number of rows and columns based on the desired resolution
num_rows <- as.integer((32.9-16.4) / desired_resolution[2])
num_cols <- as.integer((34.8-22.1) / desired_resolution[1])

# Create a raster with the matching extent and resolution
matching_raster <- rast(
  extent = veg_bbox,
  nrows = num_rows,
  ncols = num_cols,
  crs = crs(veg)
)

# Rasterize the "veg" polygons into the matching raster
table(veg$BIOME_18)
veg_raster <- rasterize(veg, matching_raster, field = "BIOMEID_18")

leg <- unique(veg %>% dplyr::select(c("BIOME_18", "BIOMEID_18")))
leg$geometry <- NULL
setDT(leg)
leg <- unique(leg)
leg <- leg[!BIOMEID_18 == 0]
leg

v_trans <- pa %>%
  st_transform(crs(veg_raster))

Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


veg_extraction <- terra::extract(veg_raster,
                                 v_trans,
                                 fun = Mode, na.rm = T)# 

veg_df <- as.data.frame(veg_raster, xy = T)

veg_final <- veg_extraction %>% 
  as.data.table() %>% 
  cbind(pa[, "WDPA_PID"]) %>% 
  mutate(geometry = NULL) %>% 
  left_join(leg, by = "BIOMEID_18") %>% 
  rename(Biome = BIOME_18) %>% 
  dplyr::select(WDPA_PID, Biome) 


#Get spatial predictors --------
library(spatialRF)

#The euclidian distance matrix:
sf_use_s2(FALSE)
c1 <- st_centroid(pa)
coords <- st_coordinates(c1)
c2 <- cbind(c1, coords) 

distance_matrix <-as.matrix(dist(cbind(c2$X, c2$Y)))
diag(distance_matrix) <- 0 #ged rid of diagonal 
distance_thresholds <- unname(round(quantile(distance_matrix, c(seq(0.05, .95, 0.05))), 1))


#several distances
mems <- spatialRF::mem_multithreshold(
  distance.matrix = distance_matrix,
  distance.thresholds = distance_thresholds
)


# rank by moran's I

mem_rank <- spatialRF::rank_spatial_predictors(
  distance.matrix = distance_matrix,
  spatial.predictors.df = mems,
  ranking.method = "moran"
)

#order the data frame 
mems2 <- mems[, mem_rank$ranking]
head(mems2)

## add spatial predictors
pa$spatial_predictor1 <- mems2[,1]
pa$spatial_predictor2 <- mems2[,2]
pa$spatial_predictor3 <- mems2[,3]
pa$spatial_predictor4 <- mems2[,4]
pa$spatial_predictor5 <- mems2[,5]


s_preds <- pa %>% as.data.table() %>% mutate(geometry = NULL, source = NULL)
names(pa)
pa_fin <- pa %>% 
  left_join(veg_final) %>%
  dplyr::select(WDPAID, WDPA_PID, PA_DEF, NAME, DESIG_ENG, IUCN_CAT, GIS_AREA, 
                spatial_predictor1, spatial_predictor2, spatial_predictor3, spatial_predictor4, spatial_predictor5, 
                Biome)

write_sf(pa_fin, "data/spatial/pa_shapes/south_african_pas.gpkg", append = FALSE)




# 
# 
# 
# 
# 
# 
# 
# 
# 
# #### extract covariates -------------------------
# 
# 
# ## MAT---------------
# 
# mat <- rast("../../../../../resources/spatial/Chelsa_Climate/CHELSA_bio1_1981-2010_V_2_1.tif") 
# 
# 
# mat.extr <- get.heterogeneity(vector = pa, grid = NULL, id.col = "WDPA_PID", raster = mat)
# setnames(mat.extr, c("mean"), 
#          c("MAT"))
# 
# 
# ## MAP---------------
# map <- rast("../../../../../resources/spatial/Chelsa_Climate/CHELSA_bio12_1981-2010_V_2_1.tif") 
# plot(map)
# 
# map.extr <- get.heterogeneity(vector = pa, grid = NULL, id.col = "WDPA_PID", raster = map)
# setnames(map.extr, c("mean"), 
#          c("MAP"))
# 
# ## elevation --------
# 
# ele <- rast("../../../../../resources/spatial/Elevation_ZAF/Elevation_SA_90m.tif")
# 
# 
# ele.extr <- get.heterogeneity(vector = pa, grid = NULL, id.col = "WDPA_PID", raster = ele)
# 
# setnames(ele.extr, c("mean"), 
#          c("elevation_mean"))
# ele.extr$x <- NULL
# ele.extr$elevation_mean_1000 <- NULL
# 
# 
# ## Biome -----------
# 
# veg <- sf::read_sf("../../../../../resources/spatial/Vegetation_Map_SA_2018/NVM2018_AEA_V22_7_16082019_final.shp")
# 
# 
# veg <- st_transform(veg, crs = 4326)
# veg <- st_sf(veg) %>% 
#   mutate(BIOMEID_18 = ifelse(veg$BIOME_18 == "Albany Thicket", 5, veg$BIOMEID_18))
# # Perform a spatial join to associate "pa" polygons with "veg" polygons
# 
# table(veg$BIOMEID_18)
# 
# # Get the extent of the "veg" data
# veg_bbox <- st_bbox(veg)
# 
# # Calculate the desired resolution in degrees
# desired_resolution <- c(0.01, 0.01)  # Approximately 1 km at the equator
# 
# # Calculate the number of rows and columns based on the desired resolution
# num_rows <- as.integer((32.9-16.4) / desired_resolution[2])
# num_cols <- as.integer((34.8-22.1) / desired_resolution[1])
# 
# # Create a raster with the matching extent and resolution
# matching_raster <- rast(
#   extent = veg_bbox,
#   nrows = num_rows,
#   ncols = num_cols,
#   crs = crs(veg)
# )
# 
# # Rasterize the "veg" polygons into the matching raster
# table(veg$BIOME_18)
# veg_raster <- rasterize(veg, matching_raster, field = "BIOMEID_18")
# 
# leg <- unique(veg %>% dplyr::select(c("BIOME_18", "BIOMEID_18")))
# leg$geometry <- NULL
# setDT(leg)
# leg <- unique(leg)
# leg <- leg[!BIOMEID_18 == 0]
# leg
# 
# v_trans <- pa %>%
#   st_transform(crs(veg_raster))
# 
# Mode <- function(x, na.rm = FALSE) {
#   if(na.rm){
#     x = x[!is.na(x)]
#   }
#   
#   ux <- unique(x)
#   return(ux[which.max(tabulate(match(x, ux)))])
# }
# 
# 
# veg.extraction <- terra::extract(veg_raster,
#                                  v_trans,
#                                  fun = Mode, na.rm = T)# 
# 
# veg.df <- as.data.frame(veg_raster, xy = T)
# 
# veg.final <- veg.extraction %>% 
#   as.data.table() %>% 
#   cbind(pa[, "WDPA_PID"]) %>% 
#   mutate(geometry = NULL) %>% 
#   left_join(leg, by = "BIOMEID_18") %>% 
#   rename(Biome = BIOME_18) %>% 
#   dplyr::select(WDPA_PID, Biome) 
# 
# ## Fire frequency ---------------
# fir <- rast("../../../../../resources/spatial/Fire/FireEventsBetween2001And2021Africa.tif")
# range(values(fir), na.rm = T)
# 
# 
# #values(fir) <- ifelse(is.na(values(fir)), 0, values(fir))
# 
# 
# fir.extr <- get.heterogeneity(vector = pa, grid = NULL, id.col = "WDPA_PID", raster = fir)
# setnames(fir.extr, c("mean"), 
#          c("fire_events_since_2001"))
# 
# 
# 
# 
# ## extract N deposition -----------
# 
# ndep <- rast("../../../../../resources/spatial/N_deposition_Rubin_etal/total_N_dep.tif")
# plot(ndep)
# 
# ndep.extr <- get.heterogeneity(vector = pa, grid = NULL, id.col = "WDPA_PID", raster = ndep)
# setnames(ndep.extr, c("mean"), 
#          c("n_deposition"))
# 
# 
# ## Proportion burned area ---------------
# fir <- rast("../../../../../resources/spatial/Fire/FireEventsBetween2001And2021Africa.tif")
# #plot(dsf)
# 
# dsf.burned <- fir
# #values(dsf.burned) <- ifelse(values(dsf) < 3650 & values(dsf) > 1, 1, 0)
# values(dsf.burned) <- ifelse(is.na(values(dsf.burned)), 0, 1)
# 
# plot(dsf.burned)
# 
# dsf.burned.extr <- get.heterogeneity(vector = pa, grid = NULL, id.col = "WDPA_PID", raster = dsf.burned)
# setnames(dsf.burned.extr, c("mean"), 
#          c("prop_burned_area"))
# 
# 
# pa.cov_0_1 <- pa %>% 
#   as.data.table() %>% 
#   mutate(geometry = NULL) %>% 
#   dplyr::select(NAME, WDPAID, WDPA_PID,PA_DEF, ORIG_NAME, DESIG_ENG, DESIG_TYPE, GIS_AREA, STATUS, VERIF) %>% 
#   left_join(map.extr) %>% 
#   left_join(mat.extr) %>% 
#   left_join(ele.extr) %>% 
#   left_join(veg.final) %>% 
#   left_join(fir.extr) %>% 
#   left_join(ndep.extr) %>% 
#   left_join(dsf.burned.extr) %>% 
#   mutate(fire_events_since_2001 = ifelse(is.na(fire_events_since_2001), 0, fire_events_since_2001)) %>% 
#   unique() %>% 
#   filter(complete.cases(.))
# 
# #### needed to export to get the GEE vars. 
# pa.shp <- pa.cov_0_1 %>% left_join(pa) %>% dplyr::select(WDPA_PID, geometry)
# write_sf(pa.shp, "data/spatial/sa_pa_shapes.shp")
# 
# ### import GEE stuff
# venter.trend <- fread("data/data_fragments/saPasVenterWoodyTrend.csv") %>% 
#   dplyr::select(WDPA_PID, trend) %>% 
#   rename(woody_trend_venter2019 = trend) %>% 
#   mutate(WDPA_PID = as.character(WDPA_PID))
# meta.tc <- fread("data/data_fragments/saPasMetaTreeCover.csv") %>% 
#   dplyr::select(mean, WDPA_PID) %>% 
#   rename(tree_cover_mean_gee = mean) %>% 
#   mutate(WDPA_PID = as.character(WDPA_PID))
# 
# 
# pa.cov_0_2 <- pa.cov_0_1 %>% 
#   left_join(venter.trend) %>% 
#   mutate(
#          Biome = as.factor(Biome))
# 
# pa.cov_shp <- pa.cov_0_2 %>% left_join(pa[, c("NAME", "WDPA_PID", "GIS_AREA")]) %>% st_as_sf()
# 
# 
# ## spatial predictors: ------
# library(spatialRF)
# 
# #The euclidian distance matrix:
# sf_use_s2(FALSE)
# c1 <- st_centroid(pa.cov_shp)
# coords <- st_coordinates(c1)
# c2 <- cbind(c1, coords) 
# 
# distance_matrix <-as.matrix(dist(cbind(c2$X, c2$Y)))
# diag(distance_matrix) <- 0 #ged rid of diagonal 
# distance_thresholds <- unname(round(quantile(distance_matrix, c(seq(0.05, .95, 0.05))), 1))
# 
# library(spatialRF)
# 
# 
# #several distances
# mems <- spatialRF::mem_multithreshold(
#   distance_matrix = distance_matrix,
#   distance_thresholds = distance_thresholds
# )
# 
# 
# # rank by moran's I
# 
# mem_rank <- spatialRF::rank_spatial_predictors(
#   distance_matrix = distance_matrix,
#   spatial.predictors.df = mems,
#   ranking.method = "moran"
# )
# 
# #order the data frame 
# mems2 <- mems[, mem_rank$ranking]
# head(mems2)
# 
# ## add spatial predictors
# pa.cov_shp$spatial_predictor1 <- mems2[,1]
# pa.cov_shp$spatial_predictor2 <- mems2[,2]
# pa.cov_shp$spatial_predictor3 <- mems2[,3]
# pa.cov_shp$spatial_predictor4 <- mems2[,4]
# pa.cov_shp$spatial_predictor5 <- mems2[,5]
# # pa.cov_shp$sp6 <- mems2[,6]
# # pa.cov_shp$sp7 <- mems2[,7]
# # pa.cov_shp$sp8 <- mems2[,8]
# # pa.cov_shp$sp9 <- mems2[,9]
# # pa.cov_shp$sp10 <- mems2[,10]
# # pa.cov_shp$sp11 <- mems2[,11]
# # pa.cov_shp$sp12 <- mems2[,12]
# # pa.cov_shp$sp13 <- mems2[,13]
# # pa.cov_shp$sp14 <- mems2[,14]
# # pa.cov_shp$sp15 <- mems2[,15]
# # pa.cov_shp$sp16 <- mems2[,16]
# # pa.cov_shp$sp17 <- mems2[,17]
# # pa.cov_shp$sp18 <- mems2[,18]
# # pa.cov_shp$sp19 <- mems2[,19]
# # pa.cov_shp$sp20 <- mems2[,20]
# 
# 
# s_preds <- pa.cov_shp %>% as.data.table() %>% mutate(geometry = NULL, source = NULL)
# 
# pa.cov <- pa.cov_0_2 %>% 
#   left_join(s_preds) %>%
#   mutate(fire_events_since_2001 = ifelse(is.na(fire_events_since_2001), 0, fire_events_since_2001)) %>% 
#   filter(complete.cases(.))
# 
# 
# 
# 
# fwrite(pa.cov, "data/south_african_pas_w_covariates.csv")
# pa.cov_shp <- pa.cov %>% left_join(pa[, c("NAME", "WDPA_PID", "GIS_AREA")]) %>% st_as_sf()
# write_sf(pa.cov_shp, "data/spatial/south_african_pas_w_covariates.shp", append = FALSE)
# summary(pa.cov)
# 
# #### test if spatial predictor is even necessary 
# 
# 
# 
# dt.gbm <- pa.cov %>% 
#   dplyr::select(woody_trend_venter2019, 
#                 MAT, MAP, n_deposition,
#                 fire_events_since_2001, prop_burned_area) %>% filter(complete.cases(.))
# library(gbm)
# gbmFit <- gbm(
#   formula = woody_trend_venter2019 ~ .,
#   distribution = "gaussian",
#   data = dt.gbm,
#   n.trees = 5000,
#   interaction.depth = 2,
#   shrinkage = 0.005,
#   cv_folds = 5, #cross validation folds (how often training/test data split)
#   n.cores = 2, # will use all cores by default
#   verbose = TRUE
# )  
# 
# sf_use_s2(FALSE)
# c1 <- st_centroid(pa.cov_shp)
# coords <- st_coordinates(c1)
# dt.coords <- cbind(c1, coords) 
# 
# response.observed <- dt.gbm$woody_trend_venter2019
# response.predicted <- predict(gbmFit, newdata = dt.gbm, n.trees = gbmFit$n.trees)
# residuals <- response.observed - response.predicted
# dt.coords$residuals <- response.observed - response.predicted
# 
# 
# 
# # same for all (in theory)
# distance_matrix <- as.matrix(dist(cbind(dt.coords$X, dt.coords$Y)))
# distance_thresholds <- unname(round(quantile(distance_matrix, c(seq(0.05, .95, 0.05))), 1))
# 
# #Moran's I test of the residuals
# moran.test <- spatialRF::moran_multithreshold(
#   x = residuals,
#   distance_matrix = distance_matrix, 
#   verbose = T,
#   distance_thresholds = distance_thresholds
# )
# moran.p.nc <- moran.test$plot
# moran.p.nc
# moran.p.nc.top <- grid.arrange(moran.p.nc, top = textGrob("Woody cover change" ,gp=gpar(fontsize=14)))
# 
# ### include spatial pred 
# 
# dt.gbm.sp <- pa.cov %>% 
#   dplyr::select(woody_trend_venter2019, 
#                 MAT, MAP, n_deposition,
#                 fire_events_since_2001, prop_burned_area, spatial_predictor1, spatial_predictor2, spatial_predictor3) %>% filter(complete.cases(.))
# library(gbm)
# gbmFitSp <- gbm(
#   formula = woody_trend_venter2019 ~ .,
#   distribution = "gaussian",
#   data = dt.gbm.sp,
#   n.trees = 5000,
#   interaction.depth = 2,
#   shrinkage = 0.005,
#   cv_folds = 5, #cross validation folds (how often training/test data split)
#   n.cores = 2, # will use all cores by default
#   verbose = TRUE
# )  
# 
# 
# response.observed.sp <- dt.gbm.sp$woody_trend_venter2019
# response.predicted.sp <- predict(gbmFitSp, newdata = dt.gbm.sp, n.trees = gbmFitSp$n.trees)
# residuals.sp <- response.observed.sp - response.predicted.sp
# dt.coords$residuals_sp <- response.observed.sp - response.predicted.sp
# 
# 
# 
# # same for all (in theory)
# distance_matrix <- as.matrix(dist(cbind(dt.coords$X, dt.coords$Y)))
# distance_thresholds <- unname(round(quantile(distance_matrix, c(seq(0.05, .95, 0.05))), 1))
# 
# #Moran's I test of the residuals
# moran.test.sp <- spatialRF::moran_multithreshold(
#   x = residuals.sp,
#   distance_matrix = distance_matrix, 
#   verbose = T,
#   distance_thresholds = distance_thresholds
# )
# moran.p.nc.sp <- moran.test.sp$plot
# moran.p.nc.sp
# moran.p.nc.sp.top <- grid.arrange(moran.p.nc.sp, top = textGrob("Woody cover change" ,gp=gpar(fontsize=14)))
