library(remotePARTS)
library(tidyverse)
library(data.table)
library(future)
library(furrr)
library(tictoc)
library(robustbase)
library(performance)

#param <- "reserves"
#param = "sa_pas"
param = "sub_saharan_pas"

if(param == "reserves"){
 
  dt <- fread("data/data_fragments/reserves_with_time_series.csv") %>% 
    as.data.frame() 
  
} else if(param == "sa_pas"){
  
  dt <- fread("data/data_fragments/south_african_pas_with_time_series.csv") %>% 
    as.data.frame() 
  
} else if(param == "sub_saharan_pas"){

  dt <- fread("data/data_fragments/sub_saharan_pas_with_time_series.csv") %>% 
    as.data.frame() 
  
} 


#dt <- dt %>% sample_n(1000)

# Define a helper functions to process trends
parts_trend <- function(cols_pattern, trend_name, dt) {
  
  cols <- grep(cols_pattern, names(dt), value = TRUE)
  
  dt_subset <- dt %>% dplyr::select(all_of(cols), lon, lat, unique_id) %>% 
    filter(complete.cases(.)) %>% as.data.frame()
  
  Y <- as.matrix(dt_subset[, cols])
  coords <- as.matrix(dt_subset[, c("lon", "lat")])
  
  ar_results <- fitAR_map(Y = Y, coords = coords)
  
  dt_subset[[paste0(trend_name, "_coef")]] <- coefficients(ar_results)[, "t"] 
  dt_subset[[paste0(trend_name, "_p_value")]] <- ar_results$pvals[, 2]
  
  
  dt_subset <- dt_subset %>% 
    dplyr::select(paste0(trend_name, "_coef"),
                  paste0(trend_name, "_p_value"),
                  unique_id)
  
  return(dt_subset)
  rm(dt_subset)
  gc()
}

rlm_trend <- function(cols_pattern, trend_name, dt) {
  
  library(robustbase)
  library(performance)
  
  cols <- grep(cols_pattern, names(dt), value = TRUE)
  trend_res <- data.frame()
  
  for(id in unique(dt$unique_id)){
    
    resp_v <- dt %>% 
      filter(unique_id == id) %>% 
      dplyr::select(all_of(cols)) %>% 
      filter(complete.cases(.)) %>%
      as.numeric()
    
    if(any(is.na(resp_v))){next}
    
    time_v <- c(1:length(resp_v))
    
    m <- lmrob(resp_v ~ time_v)
    
    if(!m$coefficients[2] == 0){
     
      m_tidy <- broom::tidy(m) %>% 
      filter(term == "time_v")
      estimate <- m_tidy$estimate
      p_val <- m_tidy$p.value
      r2 <- as.numeric(r2(m)$R2_adjusted)
      
    } else {
      estimate <- 0
      p_val <- 0
      r2 <- 0
    }
    

    sub_res <-  data.frame(estimate = estimate, 
                           p_val = p_val,
                           r2 = r2, 
                           unique_id = id
                           )
    setnames(sub_res, old = c("estimate", "p_val", "r2", "unique_id"), 
             new = c(paste0(trend_name, "_rlm_est"), 
                     paste0(trend_name, "_rlm_p_val"),
                     paste0(trend_name, "_rlm_r2"), 
                     "unique_id"))


    trend_res <- rbind(sub_res, trend_res)
      
    }

  return(trend_res)
  rm(trend_res)
  rm(dt_sub)
  
  gc()
}


# List of trends
trend_configs <- data.frame(
  pattern = c("mat_", "precipitation_", "burned_area_",
              "woody_cover_ha_", "woody_cover_sd_ha_", "woody_cover_sd_km_"),
  name = c("mat", "prec", "burned_area",
           "woody_cover_ha", "woody_cover_sd_ha", "woody_cover_sd_km"),
  stringsAsFactors = FALSE
)



################################## LOOOOOOOOOOOOP ############################            
options(future.globals.maxSize = 10 * 1024^3)  # 10 GB
plan(multisession, workers = 6)
tic()

dt_trend_list <- future_map(1:nrow(trend_configs),
                                  .progress = TRUE,
                                  .options = furrr_options(seed = TRUE),
                                  function(i) {
                                    
                                    #for(i in 1:nrow(trend_configs)){
                                    config <- trend_configs[i, ]
                                    
                                    dt_sub_parts <- parts_trend(config$pattern, config$name, dt)
                                    
                                  #  dt_sub_rlm <- rlm_trend(config$pattern, config$name, dt)
                                    dt_sub <- dt_sub_parts
                                  #  dt_sub <- left_join(dt_sub_parts, dt_sub_rlm)
                                    
                                    return(dt_sub)
                                    
                                    print(paste0(config$name, " done! ", Sys.time()))
                                    
                                  }
)



toc()
plan(sequential)
Sys.time()
dt_trend <- dt_trend_list %>%
  reduce(~ left_join(.x, .y, by = "unique_id"))

#
ctk <- dt %>% dplyr::select(unique_id,
                            mean_woody_cover, mean_woody_cover_sd_ha,
                            mean_woody_cover_sd_km,
                            mean_mat, mean_prec, 
                            mean_burned_area)

dt_res <- dt %>% 
  as.data.table() %>% 
  left_join(dt_trend) %>%
  dplyr::select(-all_of(grep("woody_cover_ha", names(dt), value = T)),
                -all_of(grep("woody_cover_sd_ha", names(dt), value = T)),
                -all_of(grep("woody_cover_sd_km", names(dt), value = T)),
                -all_of(grep("mat_", names(dt), value = T)),
                -all_of(grep("precipitation_", names(dt), value = T)),
                -all_of(grep("burned_area_", names(dt), value = T))) %>% 
  left_join(ctk) %>% 
  mutate(mat_change = (mat_coef/mean_mat)*100,
         prec_change = (prec_coef /mean_prec)*100) %>% 
  rename(woody_cover_change = woody_cover_ha_coef)
hist(dt_res$mat_change)
hist(dt_res$mat_coef)
hist(dt_res$woody_cover_change)


if(param == "reserves"){
  dt_res$lon <- NULL
  dt_res$lat <- NULL
  dt_res$reserve_name <- NULL
  
  dt_mega <- fread("data/data_fragments/reserve_megafauna_data.csv")
  
  fwrite(dt_res %>% 
           left_join(dt_mega), "data/clean_data/final_reserve_data.csv")
  
} else if(param == "sa_pas"){
  
  fwrite(dt_res, "data/clean_data/final_south_african_pa_data.csv")
  
} else if(param == "sub_saharan_pas"){
  fwrite(dt_res, "data/clean_data/final_sub_saharan_african_pa_data.csv")
} 
