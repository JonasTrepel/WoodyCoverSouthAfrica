library(terra)
library(remotePARTS)
library(rnaturalearth)
library(future)
library(furrr)
library(data.table)
library(tidyverse)

tc_files <- list.files("data/spatial/time_series", pattern = "woody_cover_ha", full.names = T)

terraOptions(memfrac = 0.25)


plan(multisession, workers = 5)

future_walk(1:length(tc_files),
            .progress = TRUE,
            function(i){
              
              file <- tc_files[i]
              dw_r <- rast(file)
              # plot(dw_r)
              #plot(dw_r)
              
              years <- gsub("data/spatial/time_series/woody_cover_ha_", "", file)
              years <- gsub(".tif", "", years)
              
              tree_cover_km <- aggregate(
                dw_r,
                fact = 10,
                fun = "mean",
                cores = 1,
                filename = paste0("data/spatial/time_series/woody_cover_km_", years, ".tif"),
                overwrite = TRUE
              )
              
              
            })

print(paste0("all done ", Sys.time()))
plan(sequential)


### Woody cover --------

tc_files <- list.files("data/spatial/time_series", pattern = "woody_cover_km", full.names = T)
tc_files <- tc_files[!grepl("sa_", tc_files)]

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
writeRaster(r_woody_cover_coef, "data/spatial/covariates/woody_cover_coef_km.tif")
