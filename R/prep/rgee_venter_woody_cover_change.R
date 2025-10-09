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


img <- ee$
  Image('users/lukiejohn/WPE_Venter_etal_2018')$
  select("trend")


img$projection()$getInfo()

img_projection <- img$projection()

img_90m <- img$reduceResolution(
  reducer = ee$Reducer$mean(),
  maxPixels = 255
)$reproject(
  crs = img_projection,
  scale = 90
)$rename("venter_woody_cover_trend")

vis_params <- list(
  min = -1,
  max = 1,
  palette = c('#D7191C', '#FDAE61', '#FFFFC0', '#A6D96A', '#1A9641')
)

Map$addLayer(
  img_90m,
  visParams = vis_params,
  name = 'trend % p yr'
)

#woody cover SD ha
export_woody_cover_trend <- ee_image_to_drive(
  image = img_90m,
  region = aoi,
  folder = "rgee_backup_venter_woody_cover_trend",
  description = "venter_woody_cover_trend",
  scale = 90,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_woody_cover_trend$start()

Sys.sleep(60)
monitor_gee_task(pattern = "venter_woody_cover_trend", path = "rgee_backup_venter_woody_cover_trend",
                 last_sleep_time = 600, mail = "jonas.trepel@gmail.com")

Sys.sleep(600)
(woody_cover_trend_drive_files <- drive_ls(path = "rgee_backup_venter_woody_cover_trend",
                                        pattern = "venter_woody_cover_trend") %>%
    dplyr::select(name) %>% 
    unique())

for(filename in unique(woody_cover_trend_drive_files$name)){
  
  path_name = paste0("data/spatial/raw_tiles/", filename)
  drive_download(file = filename, path = path_name, overwrite = TRUE)
}


woody_cover_trend_files <- list.files("data/spatial/raw_tiles",
                                   full.names = T, pattern = "venter_woody_cover_trend")

woody_cover_trend_raster_list <- lapply(woody_cover_trend_files, rast)

woody_cover_trend_file_name_merge <- paste0("data/spatial/covariates/venter_woody_cover_trend.tif")

data_type_woody_cover_trend <- terra::datatype(woody_cover_trend_raster_list[[1]])

woody_cover_trend_r <- merge(sprc(woody_cover_trend_raster_list),
                          filename = woody_cover_trend_file_name_merge,
                          overwrite = TRUE,
                          datatype = data_type_woody_cover_trend)
plot(woody_cover_trend_r)


file.remove(woody_cover_trend_files)
googledrive::drive_rm(unique(woody_cover_trend_drive_files$name))
googledrive::drive_rm("rgee_backup_venter_woody_cover_trend")

print(paste0("Done. Time: ", Sys.time()))


