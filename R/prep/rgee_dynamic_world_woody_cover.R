library(rgee)
library(data.table)
library(tidyverse)
library(googledrive)
library(terra)

source("R/functions/monitor_gee_task.R")
#  
rgee_env_dir <- c("C:\\Users\\au713983\\.conda\\envs\\rgee_env")
reticulate::use_python(rgee_env_dir, required=T)
#ee_clean_user_credentials()
#ee$Authenticate(auth_mode='notebook')
ee$Initialize(project = "jonas-trepel")
drive_auth(email = "jonas.trepel@gmail.com")
ee$String('Hello from the Earth Engine servers!')$getInfo()


#define area of interest
aoi <- ee$Geometry$Rectangle(
  coords = c(-17.5, -35.0, 51.0, 16.0), # xmin, ymin, xmax, ymax-->Sub Saharan Africa 
 # coords = c(27.68, -24.18, 27.91, -23.99), #Kaingo
  proj = "EPSG:4326",
  geodesic = FALSE
)

Map$addLayer(aoi)
# Define years and dates for Landsat image collection

years <- c(2015:2024)

for(year in years){ 
  
  year_1 = year+1
  
  start_date <- paste0(year, "-07-01")
  end_date <- paste0(year_1, "-6-30")
  
  
  dw_ic <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
    filterDate(start_date, end_date)$
    filterBounds(aoi)$
    select("label")
  
  # Compute mode across time
  dynamic_world_img <- dw_ic$reduce(ee$Reducer$mode())
  
  
  # Display - not smart when doing all over subsaharan africa
  # Map$centerObject(aoi)
  # Map$addLayer(
  #   dynamic_world_img,
  #   list(min = 0, max = 8, palette = c(
  #     "#419BDF", # Water
  #     "#397D49", # Trees
  #     "#88B053", # Grass
  #     "#7A87C6", # Flooded vegetation
  #     "#E49635", # Crops
  #     "#DFC35A", # Shrub & Scrub
  #     "#C4281B", # Built-up
  #     "#A59B8F", # Bare ground
  #     "#B39FE1"  # Snow/Ice
  #   )),
  #   paste("DW", year)
  # )
  # 
  #make woody cover = 1, else 0 / NA
  dynamic_world_img$projection()$getInfo()
  
  
  binary_img <- dynamic_world_img$
    eq(1)$
    rename("binary_label")$
    setDefaultProjection(
      crs = "EPSG:4326",  
      scale = 10   
    )
  
  binary_img$projection()$getInfo()
  
  dw_projection <- binary_img$projection()

  fract_cov_img <- binary_img$reduceResolution(
    reducer = ee$Reducer$mean(),
    maxPixels = 255
  )$reproject(
    crs = dw_projection,
    scale = 100
  )$rename("woody_cover")$
    multiply(100)$
    round()
  

  ## get woody cover SD 
  cov_sd_img_100m <- binary_img$reduceResolution(
    reducer = ee$Reducer$stdDev(),
    maxPixels = 255
  )$reproject(
    crs = dw_projection,
    scale = 100
  )$rename("woody_cover_sd_ha")$
    multiply(100)$
    round()
  
  # ## get woody cover SD at 1 km
  # cov_sd_img_1000m <- fract_cov_img$reduceResolution(
  #   reducer = ee$Reducer$stdDev(),
  #   maxPixels = 255
  # )$reproject(
  #   crs = dw_projection,
  #   scale = 1000
  # )$rename("woody_cover_sd_km")$
  #   round()
  
  
  #### EXPORTS 
  
  #fractional woody cover 
  export_woody_cover <- ee_image_to_drive(
    image = fract_cov_img,
    region = aoi,
    folder = "rgee_backup_woody_cover_ha",
    description = "woody_cover_ha",
    scale = 100,
    timePrefix = FALSE,
    maxPixels = 1e13
  )

  #woody cover SD ha
  export_woody_cover_sd_ha <- ee_image_to_drive(
    image = cov_sd_img_100m,
    region = aoi,
    folder = "rgee_backup_woody_cover_sd_ha",
    description = "woody_cover_sd_ha",
    scale = 100,
    timePrefix = FALSE,
    maxPixels = 1e13
  )

  # #woody cover SD km
  # export_woody_cover_sd_km <- ee_image_to_drive(
  #   image = cov_sd_img_1000m,
  #   region = aoi,
  #   folder = "rgee_backup_woody_cover_sd_km",
  #   description = "woody_cover_sd_km",
  #   scale = 1000,
  #   timePrefix = FALSE,
  #   maxPixels = 1e13
  # )
  # 
  
  # export_woody_cover_sd_km$start()
  export_woody_cover$start()
  export_woody_cover_sd_ha$start()
  
  Sys.sleep(300)
  
  ### Now download actual woody cover -------------
  monitor_gee_task(pattern = "woody_cover_ha", path = "rgee_backup_woody_cover_ha",
                   last_sleep_time = 600, mail = "jonas.trepel@gmail.com")
  
  Sys.sleep(600)
  (woody_cover_ha_drive_files <- drive_ls(path = "rgee_backup_woody_cover_ha",
                                          pattern = "woody_cover_ha") %>%
      dplyr::select(name) %>% 
      unique())
  
  for(filename in unique(woody_cover_ha_drive_files$name)){
    
    path_name = paste0("data/spatial/raw_tiles/", filename)
    drive_download(file = filename, path = path_name, overwrite = TRUE)
  }
  
  
  woody_cover_ha_files <- list.files("data/spatial/raw_tiles",
                                     full.names = T, pattern = "woody_cover_ha")
  
  woody_cover_ha_raster_list <- lapply(woody_cover_ha_files, rast)
  
  woody_cover_ha_file_name_merge <- paste0("data/spatial/time_series/woody_cover_ha_", year,"_",year_1,".tif")
  
  data_type_woody_cover_ha <- terra::datatype(woody_cover_ha_raster_list[[1]])
  
  woody_cover_ha_r <- merge(sprc(woody_cover_ha_raster_list),
                            filename = woody_cover_ha_file_name_merge,
                            overwrite = TRUE,
                            datatype = data_type_woody_cover_ha)
  plot(woody_cover_ha_r)
  
  
  file.remove(woody_cover_ha_files)
  googledrive::drive_rm(unique(woody_cover_ha_drive_files$name))
  googledrive::drive_rm("rgee_backup_woody_cover_ha")
  
  print(paste0(year, " done. Time: ", Sys.time()))
  
  agg_km_r <- terra::aggregate(woody_cover_ha_r,
                                fact = 10,
                                fun = "sd", 
                                na.rm=TRUE,
                                filename = paste0(
                                  "data/spatial/time_series/woody_cover_sd_km_", year,"_",year_1, ".tif"),  
                                cores = 10, 
                                overwrite = T)
  
  plot(agg_km_r)
  
  #### Now SD at ha scale as it should already be running since a while --------
  monitor_gee_task(pattern = "woody_cover_sd_ha", path = "rgee_backup_woody_cover_sd_ha",
                   last_sleep_time = 600, mail = "jonas.trepel@gmail.com")
  
  Sys.sleep(600)
  (woody_cover_sd_ha_drive_files <- drive_ls(path = "rgee_backup_woody_cover_sd_ha",
                                         pattern = "woody_cover_sd_ha") %>%
      dplyr::select(name) %>% 
      unique())
  
  for(filename in unique(woody_cover_sd_ha_drive_files$name)){
    
    path_name = paste0("data/spatial/raw_tiles/", filename)
    drive_download(file = filename, path = path_name, overwrite = TRUE)
  }
  
  
  woody_cover_sd_ha_files <- list.files("data/spatial/raw_tiles",
                                    full.names = T, pattern = "woody_cover_sd_ha")
  
  woody_cover_sd_ha_raster_list <- lapply(woody_cover_sd_ha_files, rast)
  
  woody_cover_sd_ha_file_name_merge <- paste0("data/spatial/time_series/woody_cover_sd_ha_", year,"_",year_1,".tif")
  
  data_type_woody_cover_sd_ha <- terra::datatype(woody_cover_sd_ha_raster_list[[1]])
  
  woody_cover_sd_ha_r <- merge(sprc(woody_cover_sd_ha_raster_list),
                           filename = woody_cover_sd_ha_file_name_merge,
                           overwrite = TRUE,
                           datatype = data_type_woody_cover_sd_ha)
  plot(woody_cover_sd_ha_r)
  
  
  file.remove(woody_cover_sd_ha_files)
  googledrive::drive_rm(unique(woody_cover_sd_ha_drive_files$name))
  googledrive::drive_rm("rgee_backup_woody_cover_sd_ha")
  
 
  print(paste0(year, " SD done. Time: ", Sys.time()))
  
}
