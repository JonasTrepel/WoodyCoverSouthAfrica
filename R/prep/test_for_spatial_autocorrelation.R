
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
library(caret)
library(gbm)
library(patchwork)

#### Reserves ----------------------------------

dt <- fread("data/clean_data/final_reserve_data.csv") %>% 
  filter(complete.cases(across(
    c(woody_cover_change, venter_woody_cover_trend, woody_cover_sd_ha_coef, woody_cover_sd_km_coef, 
      mat_change, prec_change, n_deposition,  mean_mat, mean_prec,
      CW_mean_species_body_mass, herbi_fun_div_distq1, n_herbi_sp_reserve,
      grazer_biomass_ha, browser_biomass_ha, 
      herbi_biomass_ha, fire_frequency, burned_area_coef)
  )))

summary(dt)

### test spatial autocorrelation -----

#Load locations

dt_shape <-  st_read("data/spatial/pa_shapes/reserve_shapes.gpkg")

sf_use_s2(FALSE)
x <- st_coordinates(st_centroid(dt_shape))[,1]
y <- st_coordinates(st_centroid(dt_shape))[,2]
unique_id <- dt_shape$unique_id

dt_coords_raw <- data.table(unique_id = unique_id, x = x, y = y)

dt_coords <- dt %>% left_join(dt_coords_raw)



responses <- c("woody_cover_change", "venter_woody_cover_trend", "woody_cover_sd_ha_coef", "woody_cover_sd_km_coef")

#### fit models without spatial predictor

p_list_no_sp <- list()

for(response in unique(responses)){


dt_mod <- dt_coords %>% 
  dplyr::select(all_of(response), 
                mat_change, prec_change, n_deposition, 
                CW_mean_species_body_mass,  mean_mat, mean_prec,
                herbi_fun_div_distq1, n_herbi_sp_reserve,
                grazer_biomass_ha, browser_biomass_ha, herbi_biomass_ha,
                fire_frequency, burned_area_coef, x, y) %>% 
  filter(complete.cases(.))

form <- as.formula(paste0(response, 
                          " ~ mat_change + prec_change + n_deposition + 
                          CW_mean_species_body_mass + herbi_fun_div_distq1 +
                          n_herbi_sp_reserve + grazer_biomass_ha +
                          browser_biomass_ha + herbi_biomass_ha +
                          fire_frequency + burned_area_coef"))

gbm_fit <- gbm(
  formula = form,
  data = dt_mod,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.005,
  cv.folds = 5, #cross validation folds (how often training/test data split)
  n.cores = 2, # will use all cores by default
  verbose = TRUE
)  

response_observed <- dt_mod %>% pull(response)
response_predicted <- predict(gbm_fit, newdata = dt_mod, n.trees = gbm_fit$n.trees)
residuals <- response_observed - response_predicted

distance_matrix <- as.matrix(dist(cbind(dt_mod$x, dt_mod$y)))
distance_thresholds <- unname(round(quantile(distance_matrix, c(seq(0.05, .95, 0.05))), 1))


#Moran's I test of the residuals
moran_test_res <- spatialRF::moran_multithreshold(
  x = residuals,
  distance.matrix = distance_matrix, 
  distance.thresholds = distance_thresholds,
  verbose = T
)
p_mor <- moran_test_res$plot

p_lab <- case_when(
  response == "venter_woody_cover_trend" ~ "Venter Woody Cover Change", 
  response == "woody_cover_change" ~ "Woody Cover Change", 
  response == "woody_cover_sd_ha_coef" ~ "Woody Cover SD Change (ha)", 
  response == "woody_cover_sd_km_coef" ~ "Woody Cover SD Change (km²)"
)

p_mor + labs(title = p_lab, subtitle = "Without Spatial Predictor")

p_list_no_sp[[response]] <- p_mor + labs(title = p_lab, subtitle = "Without Spatial Predictor")

}

p_mor_no_sp <- patchwork::wrap_plots(p_list_no_sp, ncol = 2) +
  plot_annotation(title = "No Spatial Predictor")
p_mor_no_sp

#### fit models without spatial predictor

p_list_sp <- list()

for(response in unique(responses)){
  
  
  dt_mod <- dt_coords %>% 
    dplyr::select(all_of(response), 
                  mat_change, prec_change, n_deposition, 
                  CW_mean_species_body_mass, 
                  herbi_fun_div_distq1, n_herbi_sp_reserve,
                  grazer_biomass_ha, browser_biomass_ha, herbi_biomass_ha,
                  fire_frequency, burned_area_coef, x, y, spatial_predictor1) %>% 
    filter(complete.cases(.))
  
  form <- as.formula(paste0(response, 
                            " ~ mat_change + prec_change +  n_deposition + 
                          CW_mean_species_body_mass + herbi_fun_div_distq1 +
                          n_herbi_sp_reserve + grazer_biomass_ha +
                          browser_biomass_ha + herbi_biomass_ha +
                          fire_frequency + burned_area_coef + spatial_predictor1"))
  
  gbm_fit <- gbm(
    formula = form,
    data = dt_mod,
    n.trees = 5000,
    interaction.depth = 2,
    shrinkage = 0.005,
    cv.folds = 5, #cross validation folds (how often training/test data split)
    n.cores = 2, # will use all cores by default
    verbose = TRUE
  )  
  
  response_observed <- dt_mod %>% pull(response)
  response_predicted <- predict(gbm_fit, newdata = dt_mod, n.trees = gbm_fit$n.trees)
  residuals <- response_observed - response_predicted
  
  distance_matrix <- as.matrix(dist(cbind(dt_mod$x, dt_mod$y)))
  distance_thresholds <- unname(round(quantile(distance_matrix, c(seq(0.05, .95, 0.05))), 1))
  
  
  #Moran's I test of the residuals
  moran_test_res <- spatialRF::moran_multithreshold(
    x = residuals,
    distance.matrix = distance_matrix, 
    distance.thresholds = distance_thresholds,
    verbose = T
  )
  p_mor <- moran_test_res$plot
  
  p_lab <- case_when(
    response == "venter_woody_cover_trend" ~ "Venter Woody Cover Change", 
    response == "woody_cover_change" ~ "Woody Cover Change", 
    response == "woody_cover_sd_ha_coef" ~ "Woody Cover SD Change (ha)", 
    response == "woody_cover_sd_km_coef" ~ "Woody Cover SD Change (km²)"
  )
  
  p_mor + labs(title = p_lab, subtitle = "With Spatial Predictor")
  
  p_list_sp[[response]] <- p_mor + labs(title = p_lab, subtitle = "With Spatial Predictor")
  
}

p_mor_sp <- patchwork::wrap_plots(p_list_sp, ncol = 2) +
  plot_annotation(title = "With Spatial Predictor")
p_mor_sp

p_empty <- ggplot() + theme_void()

p_res_comb <- p_mor_no_sp / p_empty / p_mor_sp +
  plot_layout(heights = c(1, 0.1, 1))
p_res_comb

ggsave(plot = p_res_comb, "builds/plots/revision/supplement/reserves_spatial_autocorrelation.png", dpi = 600, height = 12, width = 9)

##### South African PAs ----------------------------


dt_sap <- fread("data/clean_data/final_south_african_pa_data.csv") %>% 
  filter(complete.cases(across(
    c(woody_cover_change, venter_woody_cover_trend, woody_cover_sd_ha_coef, woody_cover_sd_km_coef, 
      mat_change, prec_change, n_deposition, 
     fire_frequency, burned_area_coef)
  ))) %>% 
  mutate(unique_id = as.character(WDPA_PID))

summary(dt_sap)

### test spatial autocorrelation -----

#Load locations

dt_sap_shape <-  st_read("data/spatial/pa_shapes/south_african_pas.gpkg") %>% 
  mutate(unique_id = as.character(WDPA_PID))

sf_use_s2(FALSE)
x <- st_coordinates(st_centroid(dt_sap_shape))[,1]
y <- st_coordinates(st_centroid(dt_sap_shape))[,2]
unique_id <- dt_sap_shape$unique_id

dt_sap_coords_raw <- data.table(unique_id = unique_id, x = x, y = y)

dt_sap_coords <- dt_sap %>% left_join(dt_sap_coords_raw)



responses <- c("woody_cover_change", "venter_woody_cover_trend", "woody_cover_sd_ha_coef", "woody_cover_sd_km_coef")

#### fit models without spatial predictor

p_list_no_sp_sap <- list()

for(response in unique(responses)){
  
  
  dt_sap_mod <- dt_sap_coords %>% 
    dplyr::select(all_of(response), 
                  mat_change, prec_change, n_deposition, 
                  fire_frequency, burned_area_coef, x, y) %>% 
    filter(complete.cases(.))
  
  form <- as.formula(paste0(response, 
                            " ~ mat_change + prec_change + n_deposition +
                          fire_frequency + burned_area_coef"))
  
  gbm_fit <- gbm(
    formula = form,
    data = dt_sap_mod,
    n.trees = 5000,
    interaction.depth = 2,
    shrinkage = 0.005,
    cv.folds = 5, #cross validation folds (how often training/test data split)
    n.cores = 2, # will use all cores by default
    verbose = TRUE
  )  
  
  response_observed <- dt_sap_mod %>% pull(response)
  response_predicted <- predict(gbm_fit, newdata = dt_sap_mod, n.trees = gbm_fit$n.trees)
  residuals <- response_observed - response_predicted
  
  distance_matrix <- as.matrix(dist(cbind(dt_sap_mod$x, dt_sap_mod$y)))
  distance_thresholds <- unname(round(quantile(distance_matrix, c(seq(0.05, .95, 0.05))), 1))
  
  
  #Moran's I test of the residuals
  moran_test_res <- spatialRF::moran_multithreshold(
    x = residuals,
    distance.matrix = distance_matrix, 
    distance.thresholds = distance_thresholds,
    verbose = T
  )
  p_mor <- moran_test_res$plot
  
  p_lab <- case_when(
    response == "venter_woody_cover_trend" ~ "Venter Woody Cover Change", 
    response == "woody_cover_change" ~ "Woody Cover Change", 
    response == "woody_cover_sd_ha_coef" ~ "Woody Cover SD Change (ha)", 
    response == "woody_cover_sd_km_coef" ~ "Woody Cover SD Change (km²)"
  )
  
  p_mor + labs(title = p_lab, subtitle = "Without Spatial Predictor")
  
  p_list_no_sp_sap[[response]] <- p_mor + labs(title = p_lab, subtitle = "Without Spatial Predictor")
  
}

p_mor_no_sp_sap <- patchwork::wrap_plots(p_list_no_sp_sap, ncol = 2) +
  plot_annotation(title = "No Spatial Predictor")
p_mor_no_sp_sap

#### fit models without spatial predictor

p_list_sp_sap <- list()

for(response in unique(responses)){
  
  
  dt_sap_mod <- dt_sap_coords %>% 
    dplyr::select(all_of(response), 
                  mat_change, prec_change, n_deposition, 
                  fire_frequency, burned_area_coef, x, y,
                  spatial_predictor1, spatial_predictor2, spatial_predictor3, spatial_predictor4, spatial_predictor5) %>% 
    filter(complete.cases(.))
  
  form <- as.formula(paste0(response, 
                            " ~ mat_change + prec_change + n_deposition + 
                          fire_frequency + burned_area_coef +
                            spatial_predictor1 + spatial_predictor2 + spatial_predictor3 + spatial_predictor4 + spatial_predictor5"))
  
  gbm_fit <- gbm(
    formula = form,
    data = dt_sap_mod,
    n.trees = 5000,
    interaction.depth = 2,
    shrinkage = 0.005,
    cv.folds = 5, #cross validation folds (how often training/test data split)
    n.cores = 2, # will use all cores by default
    verbose = TRUE
  )  
  
  response_observed <- dt_sap_mod %>% pull(response)
  response_predicted <- predict(gbm_fit, newdata = dt_sap_mod, n.trees = gbm_fit$n.trees)
  residuals <- response_observed - response_predicted
  
  distance_matrix <- as.matrix(dist(cbind(dt_sap_mod$x, dt_sap_mod$y)))
  distance_thresholds <- unname(round(quantile(distance_matrix, c(seq(0.05, .95, 0.05))), 1))
  
  
  #Moran's I test of the residuals
  moran_test_res <- spatialRF::moran_multithreshold(
    x = residuals,
    distance.matrix = distance_matrix, 
    distance.thresholds = distance_thresholds,
    verbose = T
  )
  p_mor <- moran_test_res$plot
  
  p_lab <- case_when(
    response == "venter_woody_cover_trend" ~ "Venter Woody Cover Change", 
    response == "woody_cover_change" ~ "Woody Cover Change", 
    response == "woody_cover_sd_ha_coef" ~ "Woody Cover SD Change (ha)", 
    response == "woody_cover_sd_km_coef" ~ "Woody Cover SD Change (km²)"
  )
  
  p_mor + labs(title = p_lab, subtitle = "With Spatial Predictor")
  
  p_list_sp_sap[[response]] <- p_mor + labs(title = p_lab, subtitle = "With Spatial Predictor")
  
}

p_mor_sp_sap <- patchwork::wrap_plots(p_list_sp_sap, ncol = 2) +
  plot_annotation(title = "With Spatial Predictor")
p_mor_sp_sap

p_empty <- ggplot() + theme_void()

p_res_comb <- p_mor_no_sp_sap / p_empty / p_mor_sp_sap +
  plot_layout(heights = c(1, 0.1, 1))
p_res_comb

ggsave(plot = p_res_comb, "builds/plots/revision/supplement/south_african_pa_spatial_autocorrelation.png", 
       dpi = 600, height = 12, width = 9)


### Sub Saharan African PAs ---------------

dt_ssa <- fread("data/clean_data/final_sub_saharan_african_pa_data.csv") %>% 
  filter(complete.cases(across(
    c(woody_cover_change, venter_woody_cover_trend, woody_cover_sd_ha_coef, woody_cover_sd_km_coef, 
      mat_change, prec_change, n_deposition, 
      fire_frequency, burned_area_coef)
  ))) %>% 
  mutate(unique_id = as.character(WDPA_PID))

summary(dt_ssa)

### test spatial autocorrelation -----

#Load locations

dt_ssa_shape <-  st_read("data/spatial/pa_shapes/sub_saharan_african_pas.gpkg") %>% 
  mutate(unique_id = as.character(WDPA_PID))

sf_use_s2(FALSE)
x <- st_coordinates(st_centroid(dt_ssa_shape))[,1]
y <- st_coordinates(st_centroid(dt_ssa_shape))[,2]
unique_id <- dt_ssa_shape$unique_id

dt_ssa_coords_raw <- data.table(unique_id = unique_id, x = x, y = y)

dt_ssa_coords <- dt_ssa %>% left_join(dt_ssa_coords_raw)



responses <- c("woody_cover_change", "venter_woody_cover_trend", "woody_cover_sd_ha_coef", "woody_cover_sd_km_coef")

#### fit models without spatial predictor

p_list_no_sp_ssa <- list()

for(response in unique(responses)){
  
  
  dt_ssa_mod <- dt_ssa_coords %>% 
    dplyr::select(all_of(response), 
                  mat_change, prec_change, n_deposition, 
                  fire_frequency, burned_area_coef, x, y) %>% 
    filter(complete.cases(.))
  
  form <- as.formula(paste0(response, 
                            " ~ mat_change + prec_change + n_deposition +
                          fire_frequency + burned_area_coef"))
  
  gbm_fit <- gbm(
    formula = form,
    data = dt_ssa_mod,
    n.trees = 5000,
    interaction.depth = 2,
    shrinkage = 0.005,
    cv.folds = 5, #cross validation folds (how often training/test data split)
    n.cores = 2, # will use all cores by default
    verbose = TRUE
  )  
  
  response_observed <- dt_ssa_mod %>% pull(response)
  response_predicted <- predict(gbm_fit, newdata = dt_ssa_mod, n.trees = gbm_fit$n.trees)
  residuals <- response_observed - response_predicted
  
  distance_matrix <- as.matrix(dist(cbind(dt_ssa_mod$x, dt_ssa_mod$y)))
  distance_thresholds <- unname(round(quantile(distance_matrix, c(seq(0.05, .95, 0.05))), 1))
  
  
  #Moran's I test of the residuals
  moran_test_res <- spatialRF::moran_multithreshold(
    x = residuals,
    distance.matrix = distance_matrix, 
    distance.thresholds = distance_thresholds,
    verbose = T
  )
  p_mor <- moran_test_res$plot
  
  p_lab <- case_when(
    response == "venter_woody_cover_trend" ~ "Venter Woody Cover Change", 
    response == "woody_cover_change" ~ "Woody Cover Change", 
    response == "woody_cover_sd_ha_coef" ~ "Woody Cover SD Change (ha)", 
    response == "woody_cover_sd_km_coef" ~ "Woody Cover SD Change (km²)"
  )
  
  p_mor + labs(title = p_lab, subtitle = "Without Spatial Predictor")
  
  p_list_no_sp_ssa[[response]] <- p_mor + labs(title = p_lab, subtitle = "Without Spatial Predictor")
  
}

p_mor_no_sp_ssa <- patchwork::wrap_plots(p_list_no_sp_ssa, ncol = 2) +
  plot_annotation(title = "No Spatial Predictor")
p_mor_no_sp_ssa

#### fit models without spatial predictor

p_list_sp_ssa <- list()

for(response in unique(responses)){
  
  
  dt_ssa_mod <- dt_ssa_coords %>% 
    dplyr::select(all_of(response), 
                  mat_change, prec_change, n_deposition,
                  fire_frequency, burned_area_coef, x, y,
                  spatial_predictor1, spatial_predictor2, spatial_predictor3, spatial_predictor4, spatial_predictor5) %>% 
    filter(complete.cases(.))
  
  form <- as.formula(paste0(response, 
                            " ~ mat_change + prec_change + n_deposition +
                          fire_frequency + burned_area_coef +
                            spatial_predictor1 + spatial_predictor2 + spatial_predictor3 + spatial_predictor4 + spatial_predictor5"))
  
  gbm_fit <- gbm(
    formula = form,
    data = dt_ssa_mod,
    n.trees = 5000,
    interaction.depth = 2,
    shrinkage = 0.005,
    cv.folds = 5, #cross validation folds (how often training/test data split)
    n.cores = 2, # will use all cores by default
    verbose = TRUE
  )  
  
  response_observed <- dt_ssa_mod %>% pull(response)
  response_predicted <- predict(gbm_fit, newdata = dt_ssa_mod, n.trees = gbm_fit$n.trees)
  residuals <- response_observed - response_predicted
  
  distance_matrix <- as.matrix(dist(cbind(dt_ssa_mod$x, dt_ssa_mod$y)))
  distance_thresholds <- unname(round(quantile(distance_matrix, c(seq(0.05, .95, 0.05))), 1))
  
  
  #Moran's I test of the residuals
  moran_test_res <- spatialRF::moran_multithreshold(
    x = residuals,
    distance.matrix = distance_matrix, 
    distance.thresholds = distance_thresholds,
    verbose = T
  )
  p_mor <- moran_test_res$plot
  
  p_lab <- case_when(
    response == "venter_woody_cover_trend" ~ "Venter Woody Cover Change", 
    response == "woody_cover_change" ~ "Woody Cover Change", 
    response == "woody_cover_sd_ha_coef" ~ "Woody Cover SD Change (ha)", 
    response == "woody_cover_sd_km_coef" ~ "Woody Cover SD Change (km²)"
  )
  
  p_mor + labs(title = p_lab, subtitle = "With Spatial Predictor")
  
  p_list_sp_ssa[[response]] <- p_mor + labs(title = p_lab, subtitle = "With Spatial Predictor")
  
}

p_mor_sp_ssa <- patchwork::wrap_plots(p_list_sp_ssa, ncol = 2) +
  plot_annotation(title = "With Spatial Predictor")
p_mor_sp_ssa

p_empty <- ggplot() + theme_void()

p_res_comb <- p_mor_no_sp_ssa / p_empty / p_mor_sp_ssa +
  plot_layout(heights = c(1, 0.1, 1))
p_res_comb

ggsave(plot = p_res_comb, "builds/plots/revision/supplement/south_african_pa_spatial_autocorrelation.png", 
       dpi = 600, height = 12, width = 9)

