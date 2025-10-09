#Download MAT

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

years <- c(2000:2024)

for(year in years){
  
  print(paste0("Starting with: ", year))
  
  year_1 = year+1
  
  start_date <- paste0(year, "-07-01")
  end_date <- paste0(year_1, "-6-30")
  
  
  annual_img <- ee$
    ImageCollection('ECMWF/ERA5_LAND/MONTHLY_AGGR')$
    select('temperature_2m')$
    filterDate(start_date, end_date)$
    mean()$subtract(273.15)
  
  Map$addLayer(annual_img)
  

  export_task <- ee_image_to_drive(image = annual_img,
                                   region = aoi,
                                   folder = "rgee_backup_mat",
                                   description = "annual_temp",
                                   scale = 11132, 
                                   timePrefix = FALSE, 
                                   maxPixels = 1e13
  )
  export_task$start()
  
  Sys.sleep(30)
  monitor_gee_task(pattern = "annual_temp", path = "rgee_backup_mat",
                   last_sleep_time = 10, mail = "jonas.trepel@gmail.com")
  
  (drive_files <- drive_ls(path = "rgee_backup_mat",
                           pattern = "annual_temp") %>%
      dplyr::select(name) %>% 
      unique())
  
  
  path_name <- paste0("data/spatial/time_series/mat_", year, ".tif")
  
  
  drive_download(file = drive_files$name, path = path_name, overwrite = TRUE)
  
  
  googledrive::drive_rm(unique(drive_files$name))
  googledrive::drive_empty_trash()
  
  r <- rast(path_name)
  plot(r, main = paste0(year))
  
  print(paste0(year, " done"))
  
}
