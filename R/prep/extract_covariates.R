#library(terra)
library(tidyverse)
library(data.table)
library(sf)
library(tidylog)
library(sf)
library(tictoc)
library(furrr)
library(terra)
library(exactextractr)
### define if we want to run it for control or PA 

param <- "reserves"
#param = "sa_pas"
#param = "sub_saharan_pas"

if(param == "reserves"){
  vect_raw <- read_sf("data/spatial/pa_shapes/reserve_shapes.gpkg")
  
} else if(param == "sa_pas"){
  vect_raw <- read_sf("data/spatial/pa_shapes/south_african_pas.gpkg") %>% 
    mutate(unique_id = WDPA_PID)
  
} else if(param == "sub_saharan_pas"){
  vect_raw <- read_sf("data/spatial/pa_shapes/sub_saharan_african_pas.gpkg") %>% 
    mutate(unique_id = WDPA_PID)
  
} 

sf_use_s2(FALSE)
coords <- vect %>% st_transform(4326) %>% 
  st_centroid() %>% #
  st_coordinates()
vect$lon <- coords[,1]
vect$lat <- coords[,2]

## get time series file paths sorted ---------------------

#Precipitation
prec_files <- data.table(file_path = list.files("data/spatial/time_series/",
                                                           pattern = "precipitation_sum", 
                                                           full.names = TRUE), 
                                     file_name = list.files("data/spatial/time_series/",
                                                           pattern = "precipitation_sum", 
                                                           full.names = FALSE)
) %>% 
 # filter(grepl("100m", file_name)) %>% 
  mutate(col_name =  gsub(".tif", "", file_name))

#MAT
mat_files <- data.table(file_path = list.files("data/spatial/time_series/",
                                               pattern = "mat_", 
                                               full.names = TRUE), 
                         file_name = list.files("data/spatial/time_series/",
                                               pattern = "mat_", 
                                               full.names = FALSE)
) %>% 
  # filter(grepl("100m", file_name)) %>% 
  mutate(col_name =  gsub(".tif", "", file_name))


#Burned area
burned_area_files <- data.table(file_path = list.files("data/spatial/time_series/",
                                              pattern = "burned_area", 
                                              full.names = TRUE), 
                        file_name = list.files("data/spatial/time_series/",
                                              pattern = "burned_area", 
                                              full.names = FALSE)
) %>% 
  # filter(grepl("100m", file_name)) %>% 
  mutate(col_name =  gsub(".tif", "", file_name))

#Woody cover ha
wc_files <- data.table(file_path = list.files("data/spatial/time_series/",
                                              pattern = "woody_cover_ha", 
                                              full.names = TRUE), 
                        file_name = list.files("data/spatial/time_series/",
                                              pattern = "woody_cover_ha", 
                                              full.names = FALSE)
) %>% 
  # filter(grepl("100m", file_name)) %>% 
  mutate(col_name =  gsub(".tif", "", file_name))

#Woody cover SD ha
wc_sd_ha_files <- data.table(file_path = list.files("data/spatial/time_series/",
                                             pattern = "woody_cover_sd_ha", 
                                             full.names = TRUE), 
                       file_name = list.files("data/spatial/time_series/",
                                             pattern = "woody_cover_sd_ha", 
                                             full.names = FALSE)
) %>% 
  # filter(grepl("100m", file_name)) %>% 
  mutate(col_name =  gsub(".tif", "", file_name))

#Woody cover SD km
wc_sd_km_files <- data.table(file_path = list.files("data/spatial/time_series/",
                                                   pattern = "woody_cover_sd_km", 
                                                   full.names = TRUE), 
                             file_name = list.files("data/spatial/time_series/",
                                                   pattern = "woody_cover_sd_km", 
                                                   full.names = FALSE)
) %>% 
  # filter(grepl("100m", file_name)) %>% 
  mutate(col_name =  gsub(".tif", "", file_name))


time_series_covs <- rbind(prec_files, 
              mat_files,
              burned_area_files, 
              wc_files,
              wc_sd_ha_files,
              wc_sd_km_files)


#get other file paths sorted -------------
col_name <- c(
  
  #### Continuous ####
  "elevation", ## Elevation
  "chelsa_map", ## MAP
  "chelsa_mat", ## MAT
  "n_deposition", ## Nitrogen depo
  "fire_frequency", #average number of fires per year
  "venter_woody_cover_trend"
)

cov_paths <- c(
  
  #### Continuous ####
  "data/spatial/covariates/Elevation_SA_90m.tif", ## Elevation
  "data/spatial/covariates/CHELSA_bio12_1981-2010_V.2.1.tif", ## MAP
  "data/spatial/covariates/CHELSA_bio1_1981-2010_V.2.1.tif", ## MAT
  
  "data/spatial/covariates/total_N_dep.tif", ## Nitrogen depo
  "data/spatial/covariates/n_fires_500m_2001_2024.tif", #Fires 
  "data/spatial/covariates/venter_woody_cover_trend.tif"

)

other_covs <- data.table(
  col_name = col_name, 
  file_path = cov_paths, 
  file_name = col_name
) %>% filter(!is.na(cov_paths))

covs <- rbind(time_series_covs, other_covs)

################################## LOOOOOOOOOOOOP ############################            
options(future.globals.maxSize = 10 * 1024^3)  # 10 GB
plan(multisession, workers = 21)
tic()


  
dt_covs_list <- future_map(1:nrow(covs),
                             .progress = TRUE,
                             .options = furrr_options(seed = TRUE),
                             function(i) {
                               
                               #for(i in 1:nrow(covs)){
                               
                               cov_r <- rast(covs[i, ]$file_path)
                               
                               vect_trans <- st_transform(vect, crs = st_crs(cov_r))
                               
                               extr <- exactextractr::exact_extract(cov_r, 
                                                                    append_cols = c("unique_id"),
                                                                    vect_trans, 
                                                                    fun = "mean")
                               
                               setnames(extr, "mean", covs[i, ]$col_name)
                               
                               dt_extr_fin <- extr %>% 
                                 as.data.table() %>%
                                 mutate(geom = NULL) %>% 
                                 unique()
                               
                               print(paste0(covs[i, ]$col_name,"; i = ", i))
                               
                               return(dt_extr_fin)
                               
                             }
  )
  

toc()
plan(sequential)
Sys.time()
dt_covs <- dt_covs_list %>%
  reduce(~ left_join(.x, .y, by = "unique_id"))

#combine
vect_covs <- vect %>% 
  as.data.table() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  left_join(dt_covs) %>% 
  as.data.table() %>% 
  mutate(x = NULL, 
         geom = NULL,
         geometry = NULL) %>% 
  mutate(mean_burned_area = rowMeans(select(., contains("burned_area")), na.rm = TRUE), 
         mean_prec = rowMeans(select(., contains("precipitation")), na.rm = TRUE),
         mean_mat = rowMeans(select(., contains("mat_")), na.rm = TRUE),
         mean_woody_cover = rowMeans(select(., contains("woody_cover_ha")), na.rm = TRUE), 
         mean_woody_cover_sd_ha = rowMeans(select(., contains("woody_cover_sd_ha")), na.rm = TRUE), 
         mean_woody_cover_sd_km = rowMeans(select(., contains("woody_cover_sd_km")), na.rm = TRUE)) 


if(param == "reserves"){

  fwrite(vect_covs, "data/data_fragments/reserves_with_time_series.csv")
  
} else if(param == "sa_pas"){
  
  fwrite(vect_covs, "data/data_fragments/south_african_pas_with_time_series.csv")
  
} else if(param == "sub_saharan_pas"){
  
  fwrite(vect_covs, "data/data_fragments/sub_saharan_pas_with_time_series.csv")
  
} 
gc()
