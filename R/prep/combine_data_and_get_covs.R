### new cleaning and combining script 

#### June 2024 


rm(list = ls())

source("functions/get.heterogeneity.R")


library(terra)
library(tidyverse)
library(data.table)
library(sf)
library(tidylog)
library(mapview)
library(exactextractr)

exclude.hard.to.count.sp = FALSE

## SANParks

dt.sanp <- fread("data/clean_data/data_fragments/sanparks_cleanish.csv")
sf.sanp <- st_read("data/clean_data/data_fragments/sanparks_shapes.gpkg")

# sf.sanp <- sf.sanp %>% 
#   st_cast("POLYGON") %>% 
#   group_by(NAME) %>%
#   summarize(geometry = st_union(geometry))

sanp <- dt.sanp %>% 
  dplyr::select(c( 
    ## columns unique to the dataset 
    NAME, area_ha, 
    ## common columns (i.e., trait and diversity measures)
    max_species_body_mass, mean_species_body_mass, median_species_body_mass, 
    CW_max_species_body_mass, CW_mean_species_body_mass, CW_median_species_body_mass, 
    herbivore_fg_own, carnivore_fg_own, hempson_fg, trophic_groups, trophic_groups_hempson, carnivore_fg_own_simple, 
    herbivore_fg_own_simple, total_biomass_ha,  total_m_biomass_ha, m_herbi_biomass_ha,
    n_species_reserve, n_herbi_sp_reserve, elephant_biomass_ha, white_rhino_biomass_ha, white_rhino_yn,
    browser_biomass_ha, grazer_biomass_ha, mixed_feeder_biomass_ha,
    grazer_browser_ratio, grazer_mixed_ratio, browser_mixed_ratio,
    herbi_biomass_ha, prop_biomass_w_occasional_predator, prop_biomass_w_regular_predator,
    herbi_fun_red, herbi_fun_div_distq1, herbi_fun_ent,
    prop_species_w_occasional_predator, prop_species_w_regular_predator,
    predator_biomass_ha
  )) %>% 
  left_join(sf.sanp, by = "NAME") %>% 
  rename(reserve_name = NAME) %>% 
  mutate(irrigate = "not_part_of_dataset", 
         prescribed_fire = "not_part_of_dataset", 
         clear_veg = "not_part_of_dataset", 
         establishment_year = NA, 
         mean_rainfall = NA, 
         source = "SANParks"
  ) %>% 
  st_as_sf()  %>% st_transform(crs = 4326)



## HC 
dt.hc <- fread("data/clean_data/data_fragments/hc_cleanish.csv")
names(dt.hc)
sf.hc <- st_read("data/clean_data/data_fragments/hc_shapes.gpkg") 

hc <- dt.hc %>% 
  dplyr::select(c(
    ## columns unique to the dataset
    FARMNAME, irrigate,
    establishment_year, clear_veg, farm_size_ha, prescribed_fire,
    ## common columns (i.e., trait and diversity measures)
    max_species_body_mass, mean_species_body_mass, median_species_body_mass, 
    CW_max_species_body_mass, CW_mean_species_body_mass, CW_median_species_body_mass, 
    herbivore_fg_own, carnivore_fg_own, hempson_fg, trophic_groups, trophic_groups_hempson, carnivore_fg_own_simple, 
    herbivore_fg_own_simple, total_biomass_ha, total_m_biomass_ha, m_herbi_biomass_ha,
    n_species_reserve, n_herbi_sp_reserve, elephant_biomass_ha, white_rhino_biomass_ha, white_rhino_yn,
    browser_biomass_ha, grazer_biomass_ha, mixed_feeder_biomass_ha,
    grazer_browser_ratio, grazer_mixed_ratio, browser_mixed_ratio,
    herbi_biomass_ha, prop_biomass_w_occasional_predator, prop_biomass_w_regular_predator,
    prop_species_w_occasional_predator, prop_species_w_regular_predator,
    herbi_fun_red, herbi_fun_div_distq1, herbi_fun_ent,
    predator_biomass_ha)) %>% 
  left_join(sf.hc, by = "FARMNAME") %>% 
  rename(reserve_name = FARMNAME, 
         area_ha = farm_size_ha) %>% 
  mutate( mean_rainfall = NA, 
          clear_veg = case_when(
            clear_veg == "" ~ NA, 
            clear_veg %in% c("< once/5yrs", "< once/yr", "irregularly", "regularly") ~ "yes", 
            clear_veg == "never" ~ "no"
          ), 
          prescribed_fire = case_when(
            prescribed_fire == "" ~ NA, 
            prescribed_fire %in% c("< once/5yrs", "< once/yr", "irregularly", "regularly") ~ "yes", 
            prescribed_fire == "never" ~ "no"
          ), 
          source = "HC") %>% 
  st_as_sf()  %>% st_transform(crs = 4326)

## EWT 
dt.ewt <- fread("data/clean_data/data_fragments/ewt_cleanish.csv")
names(dt.ewt)
sf.ewt <- st_read("data/clean_data/data_fragments/ewt_shapes.gpkg")

ewt <- dt.ewt %>% 
  dplyr::select(c(
    ## columns unique to the dataset
    FarmID, mean_rainfall, farm_size_ha, burn_never, bush_clearing_no,
    how_long_has_your_farm_been_used_for_wildlife_how_long_has_your_farm_been_used_for_wildlife,
    ## common columns (i.e., trait and diversity measures)
    max_species_body_mass, mean_species_body_mass, median_species_body_mass, 
    CW_max_species_body_mass, CW_mean_species_body_mass, CW_median_species_body_mass, 
    herbivore_fg_own, carnivore_fg_own, hempson_fg, trophic_groups, trophic_groups_hempson, carnivore_fg_own_simple, 
    herbivore_fg_own_simple, total_biomass_ha,
    total_m_biomass_ha, m_herbi_biomass_ha,
    n_species_reserve, n_herbi_sp_reserve, elephant_biomass_ha, white_rhino_biomass_ha, white_rhino_yn,
    browser_biomass_ha, grazer_biomass_ha, mixed_feeder_biomass_ha,
    grazer_browser_ratio, grazer_mixed_ratio, browser_mixed_ratio,
    herbi_biomass_ha, prop_biomass_w_occasional_predator, prop_biomass_w_regular_predator,
    prop_species_w_occasional_predator, prop_species_w_regular_predator,
    herbi_fun_red, herbi_fun_div_distq1, herbi_fun_ent,
    predator_biomass_ha, elephant_biomass_ha)) %>% 
  left_join(sf.ewt, by = "FarmID") %>% 
  rename(reserve_name = FarmID, 
         area_ha = farm_size_ha, 
         prescribed_fire = burn_never, 
         clear_veg = bush_clearing_no, 
         establishment_year = how_long_has_your_farm_been_used_for_wildlife_how_long_has_your_farm_been_used_for_wildlife) %>% 
  mutate(prescribed_fire = ifelse(prescribed_fire == 1, "no", "yes"),
         clear_veg = ifelse(clear_veg == 1, "no", "yes"), 
         establishment_year = ifelse(establishment_year > 1800, establishment_year, 2016 - establishment_year), 
         establishment_year = ifelse(establishment_year < 3000, establishment_year, NA), 
         irrigate = "not_part_of_dataset", 
         source = "EWT"
  ) %>%
  st_as_sf()  %>% st_transform(crs = 4326)
## SWEP 

dt.swep <- fread("data/clean_data/data_fragments/swep_cleanish.csv")
names(dt.swep)
sf.swep <- st_read("data/clean_data/data_fragments/swep_shapes.gpkg")

swep <- dt.swep %>% 
  dplyr::select(c(
    ## columns unique to the dataset
    SiteID, farm_size_ha,year_to_wildlife,
    ## common columns (i.e., trait and diversity measures)
    max_species_body_mass, mean_species_body_mass, median_species_body_mass, 
    CW_max_species_body_mass, CW_mean_species_body_mass, CW_median_species_body_mass, 
    herbivore_fg_own, carnivore_fg_own, hempson_fg, trophic_groups, trophic_groups_hempson, carnivore_fg_own_simple, 
    herbivore_fg_own_simple, total_biomass_ha, elephant_biomass_ha, white_rhino_biomass_ha, white_rhino_yn,
    total_m_biomass_ha, m_herbi_biomass_ha,
    n_species_reserve, n_herbi_sp_reserve,
    browser_biomass_ha, grazer_biomass_ha, mixed_feeder_biomass_ha,
    grazer_browser_ratio, grazer_mixed_ratio, browser_mixed_ratio,
    herbi_biomass_ha, prop_biomass_w_occasional_predator, prop_biomass_w_regular_predator,
    prop_species_w_occasional_predator, prop_species_w_regular_predator,
    herbi_fun_red, herbi_fun_div_distq1, herbi_fun_ent,
    predator_biomass_ha)) %>% 
  unique() %>%
  left_join(sf.swep, by = "SiteID") %>% 
  rename(reserve_name = SiteID, 
         establishment_year = year_to_wildlife, 
         area_ha = farm_size_ha) %>% 
  mutate(prescribed_fire = "not_part_of_dataset",
         mean_rainfall = "not_part_of_dataset",
         clear_veg = "not_part_of_dataset", 
         establishment_year = ifelse(establishment_year > 1800, establishment_year, 2016 - establishment_year), 
         establishment_year = ifelse(establishment_year < 3000, establishment_year, NA), 
         irrigate = "not_part_of_dataset", 
         source = "SWEP"
  ) %>%
  st_as_sf() %>% st_transform(crs = 4326)

table(swep$white_rhino_yn)

dt.sf <- rbind(sanp, ewt, hc, swep)

## clean the spatial data --------------------------

sf_use_s2(FALSE)
#i think SWEP is the most recent and they came w actual borders, so let's keep them when in doubt 

dt.sub1 <- dt.sf[!dt.sf$source == "EWT", ]

dt.int1 <- st_intersection(ewt, dt.sub1 %>% dplyr::select(c(geom)))

#mapview(dt.int1, col.regions = "red" ) + mapview(swep)


exclude.from.ewt <- unique(dt.int1$reserve_name)

dt.sub2 <- dt.sf[!dt.sf$source == "HC", ]
dt.int2 <- st_intersection(hc, dt.sub2 %>% dplyr::select(c(geom)))

#mapview(dt.int2, col.regions = "red" ) + mapview(swep)

exclude.from.hc <- unique(dt.int2$reserve_name)


dt.sub3 <- dt.sf[!dt.sf$source == "SANParks", ]
dt.int3 <- st_intersection(dt.sub3, sanp %>% dplyr::select(c(geom)))
exclude.from.swep <- unique(dt.int3$reserve_name)


#mapview(dt.int2, col.regions = "red" ) + mapview(swep)

exclude.from.hc <- unique(dt.int2$reserve_name)

sf_use_s2(TRUE)

## exclude the duplicates and one breeding camp w insanely high densities (LIM77)

dt.sf <- dt.sf %>%
  filter(!reserve_name %in% c("LIM77", "Farm_47", exclude.from.ewt, exclude.from.hc, exclude.from.swep))


n_distinct(dt.sf$reserve_name) #currently (April 4), 461 different reserves


sf_use_s2(FALSE)

source <- dt.sf[, c("reserve_name", "source")]
source$geom <- NULL
source <- unique(source)

dt.shape <- dt.sf %>%
  dplyr::select(c(geom, reserve_name, source)) %>%
  st_sf() %>% 
  group_by(reserve_name) %>%
  summarize() %>% left_join(source) %>% filter(!st_is_empty(.))

st_write(obj = dt.shape, "data/clean_data/reserve_shapes.gpkg", append = FALSE)
st_write(obj = dt.shape, "data/clean_data/reserve_shapes.shp", append = FALSE)

#mapview(dt.shape)

n_distinct(dt.shape$reserve_name)

mapview(dt.shape, zcol = "source")

## EXTRACT COVARIATES ----------
dt.utm <- st_transform(dt.shape, crs = 22235)


sf_use_s2(FALSE)
grid1000 <- st_make_grid(dt.utm, cellsize = c(1000, 1000), what = "polygons", square = TRUE) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  st_join(dt.sf) %>% 
  filter(!is.na(reserve_name)) 

grid1000 <- grid1000 %>%
  mutate(grid_ID = paste0("Grid_", 1:nrow(grid1000))) %>% dplyr::select(c("grid_ID"))

## elevation --------

ele <- rast("../../../../../resources/spatial/Elevation_ZAF/Elevation_SA_90m.tif")
crs(ele) # 
res(ele)

ele.extr <- get.heterogeneity(vector = dt.shape, grid = grid1000, id.col = "reserve_name", raster = ele)

setnames(ele.extr, c("id.col", "grid.cv", "grid.sd", "grid.mean", "mean"), 
         c("reserve_name", "elevation_cv_1000", "elevation_sd_1000", "elevation_mean_1000", "elevation_mean"))
ele.extr$x <- NULL
ele.extr$elevation_mean_1000 <- NULL


## MAT---------------

mat <- rast("../../../../../resources/spatial/Chelsa_Climate/CHELSA_bio1_1981-2010_V.2.1.tif") 


mat.extr <- get.heterogeneity(vector = dt.shape, grid = NULL, id.col = "reserve_name", raster = mat)
setnames(mat.extr, c("mean"), 
         c("MAT"))

## MAP---------------
map <- rast("../../../../../resources/spatial/Chelsa_Climate/CHELSA_bio12_1981-2010_V.2.1.tif") 
plot(map)

map.extr <- get.heterogeneity(vector = dt.shape, grid = NULL, id.col = "reserve_name", raster = map)
setnames(map.extr, c("mean"), 
         c("MAP"))

## Biome -----------

veg <- sf::read_sf("../../../../../resources/spatial/Vegetation_Map_SA_2018/NVM2018_AEA_V22_7_16082019_final.shp")


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
#plot(veg_raster)


leg <- unique(veg %>% dplyr::select(c("BIOME_18", "BIOMEID_18")))
leg$geometry <- NULL
setDT(leg)
leg <- unique(leg)
leg <- leg[!BIOMEID_18 == 0]
leg

v.trans <- dt.shape %>%
  st_transform(crs(veg_raster))

Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


veg.extraction <- terra::extract(veg_raster,
                                 v.trans,
                                 fun = Mode, na.rm = T)# 


table(veg.extraction$BIOMEID_18)

veg.df <- as.data.frame(veg_raster, xy = T)

veg.final <- veg.extraction %>% 
  # mutate(BIOMEID_18 = ifelse(BIOMEID_18 %in% c(5.5, 7.5, 8.5), NA, BIOMEID_18)) %>% 
  as.data.table() %>% 
  cbind(dt.shape[, "reserve_name"]) %>% 
  mutate(geometry = NULL) %>% 
  left_join(leg, by = "BIOMEID_18") %>% 
  rename(Biome = BIOME_18) %>% 
  dplyr::select(reserve_name, Biome) 



## Days since last fire ---------------
dsf <- rast("../../../../../resources/spatial/Fire/DaysSinceLastFire2020Africa.tif")
plot(dsf)

values(dsf) <- ifelse(values(dsf) == 0, max(values(dsf)) + 1, values(dsf))



dsf.extr <- get.heterogeneity(vector = dt.shape, grid = NULL, id.col = "reserve_name", raster = dsf)
setnames(dsf.extr, c("mean"), 
         c("days_since_last_fire"))


## Fire frequency ---------------
fir <- rast("../../../../../resources/spatial/Fire/FireEventsBetween2001And2021Africa.tif")
plot(fir)
range(values(fir), na.rm = T)


#values(fir) <- ifelse(is.na(values(fir)), 0, values(fir))


fir.extr <- get.heterogeneity(vector = dt.shape, grid = NULL, id.col = "reserve_name", raster = fir)
setnames(fir.extr, c("mean"), 
         c("fire_events_since_2001"))

## Proportion burned area ---------------
#dsf <- rast("../../../../../resources/spatial/Fire/DaysSinceLastFire2020Africa.tif")
#plot(dsf)

dsf.burned <- fir
#values(dsf.burned) <- ifelse(values(dsf) < 3650 & values(dsf) > 1, 1, 0)
values(dsf.burned) <- ifelse(is.na(values(dsf.burned)), 0, 1)

plot(dsf.burned)


dsf.burned.extr <- get.heterogeneity(vector = dt.shape, grid = NULL, id.col = "reserve_name", raster = dsf.burned)
setnames(dsf.burned.extr, c("mean"), 
         c("prop_burned_area"))

### HFI 2009 --------------

hfi <- rast("../../../../../resources/spatial/Human_Footprint_Index_2009_2018/wildareas-v3-2009-human-footprint.tif")
plot(hfi)

hfi.extr <- get.heterogeneity(vector = dt.shape, grid = NULL, id.col = "reserve_name", raster = hfi)
setnames(hfi.extr, c("mean"), 
         c("hfi"))


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


## extract N deposition -----------

ndep <- rast("../../../../../resources/spatial/N_deposition_Rubin_etal/total_N_dep.tif")
plot(ndep)

ndep.extr <- get.heterogeneity(vector = dt.shape, grid = NULL, id.col = "reserve_name", raster = ndep)
setnames(ndep.extr, c("mean"), 
         c("n_deposition"))


# add tree cover and canopy height -------------

tc.ch.het <- fread("data/clean_data/data_fragments/dt_with_ch_and_tc_9june2024_server.csv")

# add Venter variables  -------------

## Woody cover 
v.mean <- fread("data/clean_data/data_fragments/VenterWoodyStatsMean.csv") %>% 
  dplyr::select(rsrv_nm, woody_cover_mean_venter2019, woody_cover_trend_significance_venter2019, woody_cover_trend_venter2019) %>%
  rename(reserve_name = rsrv_nm)  %>% unique()

v.sd <- fread("data/clean_data/data_fragments/VenterWoodyStatsSD.csv") %>% 
  dplyr::select(rsrv_nm, woody_cover_sd_venter2019, woody_cover_trend_significance_sd_venter2019, woody_cover_trend_sd_venter2019) %>%
  rename(reserve_name = rsrv_nm)  %>% unique()


 ## spatial predictors: ------
 library(spatialRF)
 
 #The euclidian distance matrix:
 sf_use_s2(FALSE)
 c1 <- st_centroid(dt.shape)
 coords <- st_coordinates(c1)
 c2 <- cbind(c1, coords) 
 
 distance.matrix <-as.matrix(dist(cbind(c2$X, c2$Y)))
 diag(distance.matrix) <- 0 #ged rid of diagonal 
 distance.thresholds <- unname(round(quantile(distance.matrix, c(seq(0.05, .95, 0.05))), 1))
 
 library(spatialRF)
 
 
 #single distance (0km by default)
 memsingle <- spatialRF::mem(distance.matrix = distance.matrix)
 
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
 dt.shape$sp1 <- mems2[,1]
 dt.shape$sp2 <- mems2[,2]
 dt.shape$sp3 <- mems2[,3]
 dt.shape$sp4 <- mems2[,4]
 dt.shape$sp5 <- mems2[,5]
 # dt.shape$sp6 <- mems2[,6]
 # dt.shape$sp7 <- mems2[,7]
 # dt.shape$sp8 <- mems2[,8]
 # dt.shape$sp9 <- mems2[,9]
 # dt.shape$sp10 <- mems2[,10]
 # dt.shape$sp11 <- mems2[,11]
 # dt.shape$sp12 <- mems2[,12]
 # dt.shape$sp13 <- mems2[,13]
 # dt.shape$sp14 <- mems2[,14]
 # dt.shape$sp15 <- mems2[,15]
 # dt.shape$sp16 <- mems2[,16]
 # dt.shape$sp17 <- mems2[,17]
 # dt.shape$sp18 <- mems2[,18]
 # dt.shape$sp19 <- mems2[,19]
 # dt.shape$sp20 <- mems2[,20]
 
 
s_preds <- dt.shape %>% as.data.table() %>% mutate(geom = NULL, source = NULL)
 
 

## combine 1 --------

dt.cov1 <- dt.sf %>% 
  as.data.table() %>% 
  mutate(geom = NULL) %>% 
  left_join(ele.extr)  %>%
  #left_join(lc.extraction)  %>%
  left_join(mat.extr)  %>%
  left_join(hfi.extr) %>% 
  left_join(map.extr)  %>%
  left_join(veg.final)  %>%
  left_join(dsf.extr)  %>%
  left_join(fir.extr)  %>%
  left_join(dsf.burned.extr)  %>%
 # left_join(evi.1000)  %>%
 #  left_join(elm.extr.fin)  %>%
  left_join(ndep.extr)  %>%
#  left_join(evi.2019.extr)  %>%
 # left_join(venter.evi.extr)  %>%
#  left_join(robust.evi.extr) %>% 
  left_join(tc.ch.het)  %>%
  left_join(v.sd)  %>%
  left_join(v.mean)  %>%
#  left_join(evi.venter.trends.gee)  %>%
   left_join(s_preds) %>% 
  unique() %>% 
  filter(!is.na(MAT)) %>%   
  left_join(sd1k.extr) %>% 
  left_join(sd500.extr) %>% 
  mutate(one_per_row = 1) 


dt.cov2 <- dt.cov1[, n_reserves_biome := sum(one_per_row), by = "Biome"]
unique(dt.cov2[, .(Biome, n_reserves_biome)])

## fix sanpark age -------
unique(dt.cov2[source == "SANParks", reserve_name])

dt.cov3 <- dt.cov2 %>% 
  mutate(
    establishment_year = case_when(
      .default = establishment_year, 
      reserve_name == "Mokala National Park" ~ 2007, 
      reserve_name == "Agulhas National Park" ~ 1998, 
      reserve_name == "Marakele National Park" ~ 1994, 
      reserve_name == "Mapungupwe National Park" ~ 1995, 
      reserve_name == "West Coast National Park" ~ 1985, 
      reserve_name == "Addo-Elephant National Park" ~ 1931, 
      reserve_name == "Bontebok National Park" ~ 1931, 
      reserve_name == "Kruger National Park" ~ 1926, 
      reserve_name == "Augrabies Falls National Park" ~ 1966, 
      reserve_name == "Namaqua National Park" ~ 1999, 
      reserve_name == "Mountain Zebra National Park" ~ 1937, 
      reserve_name == "Golden Gate Highlands National Park" ~ 1963, 
      reserve_name == "Vaalbos National Park" ~ NA, 
      reserve_name == "Meerkat National Park" ~ 2020, 
      reserve_name == "Garden Route National Park" ~ 2009, 
      reserve_name == "Camdeboo National Park" ~ 2005, 
      reserve_name == "Karoo National Park" ~ 1979, 
      reserve_name == "Kalahari Gemsbok National Park" ~ 1931, 
      reserve_name == "Tankwa-Karoo National Park" ~ 1986, 
      
    )
  ) %>% filter(n_reserves_biome >= 3) %>% 
  unique() %>% 
  filter(!Biome == "") %>% 
  filter(!is.na(Biome)) %>% 
  filter(!is.na(n_herbi_sp_reserve)) %>% 
  mutate(grazer_biomass_ha = ifelse(is.na(grazer_biomass_ha), 0, grazer_biomass_ha), 
         mixed_feeder_biomass_ha = ifelse(is.na(mixed_feeder_biomass_ha), 0, mixed_feeder_biomass_ha), 
         browser_biomass_ha = ifelse(is.na(browser_biomass_ha), 0, browser_biomass_ha), 
         fire_events_since_2001 = ifelse(is.na(fire_events_since_2001), 0, fire_events_since_2001), 
         herbi_biomass_kgkm2 = herbi_biomass_ha*100,
         grazer_biomass_kgkm2 = grazer_biomass_ha*100,
         mixed_feeder_biomass_kgkm2 = mixed_feeder_biomass_ha*100,
         browser_biomass_kgkm2 = browser_biomass_ha*100,
         elephant_biomass_kgkm2 = elephant_biomass_ha*100
  ) %>% 
  rename(spatial_predictor1 = sp1,
         spatial_predictor2 = sp2,
         spatial_predictor3 = sp3,
         spatial_predictor4 = sp4,
         spatial_predictor5 = sp5) %>% 
  mutate(reserve_name_new = ifelse(source == "SANParks", reserve_name, paste0("reserve_", 1:(nrow(.)-19))))
names(dt.cov3)

reserve_name_stuff <- dt.cov3 %>% dplyr::select(reserve_name, reserve_name_new)
dt.shape.fin <- dt.shape %>% left_join(reserve_name_stuff) %>% mutate(reserve_name = reserve_name_new) %>% dplyr::select(reserve_name)
st_write(obj = dt.shape.fin, "data/clean_data/reserve_shapes.gpkg", append = FALSE)
st_write(obj = dt.shape.fin, "data/clean_data/reserve_shapes.shp", append = FALSE)

dt.cov4 <- dt.cov3 %>% dplyr::select(- c(hempson_fg, trophic_groups, carnivore_fg_own, herbivore_fg_own, 
                                         trophic_groups_hempson,carnivore_fg_own_simple, herbivore_fg_own_simple,
                                         total_biomass_ha, total_m_biomass_ha, m_herbi_biomass_ha, white_rhino_biomass_ha, 
                                         white_rhino_yn, prop_biomass_w_occasional_predator, prop_species_w_occasional_predator, 
                                         prop_species_w_regular_predator, prop_biomass_w_regular_predator, irrigate, 
                                         prescribed_fire, clear_veg, mean_rainfall, elevation_cv_1000, one_per_row)) %>%
  mutate(reserve_name = reserve_name_new) %>% dplyr::select(-reserve_name_new)

names(dt.cov4)


if(exclude.hard.to.count.sp == TRUE){
fwrite(dt.cov4, "data/clean_data/dt_with_covs_july2024_hard_sp_excluded.csv")}else{
  fwrite(dt.cov4, "data/ReserveDataSouthAfricaFinal.csv")
}

