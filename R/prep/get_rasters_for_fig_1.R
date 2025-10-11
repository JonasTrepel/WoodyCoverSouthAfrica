library(terra)
library(remotePARTS)
library(rnaturalearth)
library(future)
library(furrr)
library(data.table)
library(tidyverse)

tc_files <- list.files("data/spatial/time_series", pattern = "woody_cover_ha", full.names = T)

terraOptions(memfrac = 0.5)


plan(multisession, workers = 5)

future_walk(1:length(tc_files),
            .progress = TRUE,
            function(i){
              
              sa_sf <- ne_countries(country = "South Africa", returnclass = "sf")
              sa_vect <- vect(sa_sf)
              
              file <- tc_files[i]
              dw_r <- rast(file)
             # plot(dw_r)
              dw_r <- crop(dw_r, ext(sa_vect))
              #plot(dw_r)
              
              years <- gsub("data/spatial/time_series/woody_cover_ha_", "", file)
              years <- gsub(".tif", "", years)
              
              tree_cover_km <- aggregate(
                dw_r,
                fact = 10,
                fun = "mean",
                cores = 1,
                filename = paste0("data/spatial/time_series/sa_woody_cover_km_", years, ".tif"),
                overwrite = TRUE
              )
              

            })

print(paste0("all done ", Sys.time()))
plan(sequential)


#Aggregate SD ---------
tc_files <- list.files("data/spatial/time_series", pattern = "woody_cover_sd_ha", full.names = T)

terraOptions(memfrac = 0.5)


plan(multisession, workers = 5)

future_walk(1:length(tc_files),
            .progress = TRUE,
            function(i){
              
              sa_sf <- ne_countries(country = "South Africa", returnclass = "sf")
              sa_vect <- vect(sa_sf)
              
              file <- tc_files[i]
              dw_r <- rast(file)
              # plot(dw_r)
              dw_r <- crop(dw_r, ext(sa_vect))
              #plot(dw_r)
              
              years <- gsub("data/spatial/time_series/woody_cover_sd_ha_", "", file)
              years <- gsub(".tif", "", years)
              
              tree_cover_km <- aggregate(
                dw_r,
                fact = 10,
                fun = "mean",
                cores = 1,
                filename = paste0("data/spatial/time_series/sa_woody_cover_sd_agg_ha_km_", years, ".tif"),
                overwrite = TRUE
              )
              
              
            })

print(paste0("all done ", Sys.time()))
plan(sequential)

# crop and aggregate venter 

venter_r <- rast("data/spatial/covariates/venter_woody_cover_trend.tif")
venter_r <- crop(venter_r, ext(sa_vect))
#plot(dw_r)
venter_r_km <- aggregate(
  venter_r,
  fact = 10,
  fun = "mean",
  cores = 1,
  filename = paste0("data/spatial/figure_1_tifs/sa_venter_woody_cover_trend_900m.tif"),
  overwrite = TRUE
)


##### Calculate trends -----------


### Woody cover --------

tc_files <- list.files("data/spatial/time_series", pattern = "sa_woody_cover_km", full.names = T)

for(i in 1:length(tc_files)){
  
  r <- rast(tc_files[i])
  
  dt_tmp <- as.data.frame(r, xy = T)
  dt_tmp <- setnames(dt_tmp, old = names(dt_tmp), new = c("x", "y", paste0("woody_cover_", i)))
  
  if(i == 1){
   dt_tc <- dt_tmp
  } else {
    dt_tc <- left_join(dt_tc, dt_tmp)
  }
  print(i)
}


dt_tc_sub <- dt_tc %>% 
  filter(complete.cases(.)) %>% as.data.frame()

Y_tc <- as.matrix(dt_tc_sub %>% dplyr::select(contains("woody_cover")))
coords_tc <- as.matrix(dt_tc_sub[, c("x", "y")])

ar_results_tc <- fitAR_map(Y = Y_tc, coords = coords_tc)

dt_tc_sub$woody_cover_coef <- coefficients(ar_results_tc)[, "t"] 
dt_tc_sub$woody_cover_p_val <- ar_results_tc$pvals[, 2]


dt_tc_res <- dt_tc %>% 
  left_join(dt_tc_sub) %>% 
  dplyr::select(x, y, woody_cover_coef)

r_woody_cover_coef <- rast(dt_tc_res, type = "xyz")
crs(r_woody_cover_coef) <- crs(rast(tc_files[1]))
plot(r_woody_cover_coef)
writeRaster(r_woody_cover_coef, "data/spatial/figure_1_tifs/sa_woody_cover_coef_km.tif")

# Woody cover SD -------------

tc_sd_files <- list.files("data/spatial/time_series", pattern = "sa_woody_cover_sd_agg_ha_km_", full.names = T)

for(i in 1:length(tc_sd_files)){
  
  r <- rast(tc_sd_files[i])
  
  dt_tmp <- as.data.frame(r, xy = T)
  dt_tmp <- setnames(dt_tmp, old = names(dt_tmp), new = c("x", "y", paste0("woody_cover_sd_", i)))
  
  if(i == 1){
    dt_tc_sd <- dt_tmp
  } else {
    dt_tc_sd <- left_join(dt_tc_sd, dt_tmp)
  }
  print(i)
}


dt_tc_sd_sd_sub <- dt_tc_sd %>% 
  filter(complete.cases(.)) %>% as.data.frame()

Y_tc_sd <- as.matrix(dt_tc_sd_sd_sub %>% dplyr::select(contains("woody_cover_sd")))
coords_tc_sd <- as.matrix(dt_tc_sd_sd_sub[, c("x", "y")])

ar_results_tc_sd <- fitAR_map(Y = Y_tc_sd, coords = coords_tc_sd)

dt_tc_sd_sd_sub$woody_cover_coef <- coefficients(ar_results_tc_sd)[, "t"] 
dt_tc_sd_sd_sub$woody_cover_p_val <- ar_results_tc_sd$pvals[, 2]


dt_tc_sd_sd_res <- dt_tc_sd %>% 
  left_join(dt_tc_sd_sd_sub) %>% 
  dplyr::select(x, y, woody_cover_coef)

r_woody_cover_coef <- rast(dt_tc_sd_sd_res, type = "xyz")
crs(r_woody_cover_coef) <- crs(rast(tc_sd_files[1]))
plot(r_woody_cover_coef)
writeRaster(r_woody_cover_coef, "data/spatial/figure_1_tifs/sa_woody_cover_sd_coef_km.tif")

