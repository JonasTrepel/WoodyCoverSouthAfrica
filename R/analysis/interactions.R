#### Make interaction plots ####
library("hstats")
library(caret)
library(caretEnsemble)
library(data.table)
library(tidyverse)


dt <- fread("data/ReserveDataSouthAfricaFinal.csv")  %>% 
  mutate(
    Biome = as.factor(Biome), 
    elephant_yn = ifelse(elephant_biomass_ha > 0, "elephants", "no_elephants")) 

################################## CREATE MODEL GUIDE ########################         
##############################################################################    


subsets <- c("!is.na(reserve_name)")

tier_labels<- c("Full model")

tiers <- c("main")

responses <- c("tree_cover_mean", "woody_cover_trend_venter2019", "tree_cover_sd_100", "canopy_height_sd_100")

dt.tier.raw <- data.table(
  subset = subsets, 
  tier = tiers, 
  tier_label = tier_labels, 
  n = 0)

tier.resp <- CJ(response = responses, tier = tiers) %>% 
  mutate(terms = case_when(
    response == "tree_cover_mean" ~ "'MAT', 'MAP', 'CW_mean_species_body_mass', 'herbi_biomass_kgkm2', 'fire_events_since_2001', 'prop_burned_area', 'spatial_predictor1'",
    response == "woody_cover_trend_venter2019" ~ "'MAT', 'MAP', 'n_deposition', 'CW_mean_species_body_mass', 'herbi_fun_div_distq1', 'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 'browser_biomass_kgkm2', 'mixed_feeder_biomass_kgkm2', 'herbi_biomass_kgkm2', 'fire_events_since_2001', 'prop_burned_area', 'spatial_predictor1'",
    response == "tree_cover_sd_100" ~ "'MAT', 'MAP', 'elevation_sd_1000', 'CW_mean_species_body_mass', 'herbi_fun_div_distq1', 'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 'browser_biomass_kgkm2', 'mixed_feeder_biomass_kgkm2', 'herbi_biomass_kgkm2', 'fire_events_since_2001', 'prop_burned_area', 'spatial_predictor1'",
    response == "canopy_height_sd_100" ~ "'MAT', 'MAP', 'elevation_sd_1000', 'CW_mean_species_body_mass', 'herbi_fun_div_distq1', 'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 'browser_biomass_kgkm2', 'mixed_feeder_biomass_kgkm2', 'herbi_biomass_kgkm2', 'fire_events_since_2001', 'prop_burned_area', 'spatial_predictor1'"
  )) %>% mutate(
    response_tier = paste0(response, "_", tier)
  ) %>% 
  mutate(terms = case_when(
    .default = terms,
    response_tier == "tree_cover_mean_elephants" ~ "MAT, MAP, CW_mean_species_body_mass, elephant_biomass_kgkm2, herbi_biomass_kgkm2, fire_events_since_2001, spatial_predictor1",
    response_tier == "woody_cover_trend_venter2019_elephants" ~ "MAT, MAP, n_deposition, CW_mean_species_body_mass, herbi_fun_div_distq1, n_herbi_sp_reserve, elephant_biomass_kgkm2, herbi_biomass_kgkm2, fire_events_since_2001, spatial_predictor1",
    response_tier == "tree_cover_sd_100_elephants" ~ "MAT, MAP, CW_mean_species_body_mass, herbi_fun_div_distq1, n_herbi_sp_reserve, elephant_biomass_kgkm2, herbi_biomass_kgkm2, fire_events_since_2001, spatial_predictor1",
    response_tier == "canopy_height_sd_100_elephants" ~ "MAT, MAP, CW_mean_species_body_mass, herbi_fun_div_distq1, n_herbi_sp_reserve, elephant_biomass_kgkm2, herbi_biomass_kgkm2, fire_events_since_2001, spatial_predictor1"
  ))

dt.tier <- dt.tier.raw %>% left_join(tier.resp)

for(i in 1:nrow(dt.tier)){
  subset <- dt.tier[i, ]$subset
  dt.sub <- dt %>% dplyr::filter(eval(parse(text = subset)))
  
  nr <- nrow(dt.sub)
  dt.tier[i, ]$n <- nr
  
}
unique(dt.tier$n)


################ Define parameters etc #################
dt.names <- data.table(
  term = c("tree_cover_sd_100", "MAT", "MAP", "elevation_sd_1000",
           "CW_mean_species_body_mass", "herbi_fun_div_distq1", "n_herbi_sp_reserve", 
           "grazer_biomass_kgkm2", "browser_biomass_kgkm2", "mixed_feeder_biomass_kgkm2", 
           "herbi_biomass_kgkm2", "fire_events_since_2001", "prop_burned_area", "n_deposition", "elephant_biomass_kgkm2"),
  clean_term = c("Woody cover heterogeneity", "MAT (°C)", "MAP (mm)", "Elevation SD (m)",
                 "Mean body mass (kg; cwm)", "Herbivore functional diversity", 
                 "Herbivore species richness", "Grazer biomass (kg/km2)", 
                 "Browser biomass (kg/km2)", "Mixed feeder biomass (kg/km2)",
                 "Herbivore biomass (kg/km2)", "Fire frequency", "Proportion burned area", "N deposition ([kg/km2]/year)", "Elephant biomass (kg/km2)"))



### create Tune Grids 

### GBM 
tuneGridGbm <- expand.grid(
  shrinkage = c( .001, .005, 0.01),
  interaction.depth = c(1,2,3),
  n.minobsinnode = c(5, 10, 15), 
  n.trees = seq(1000, 10000, 250)
)

### Random forest 
tuneGridRf <- expand.grid(
  mtry = c(1:10))

### XGBoost

tuneGridXgbTree <- expand.grid(
  nrounds = seq(50, 200, 50), 
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,
  max_depth = c(1, 3, 5, 7),
  min_child_weight = c(5, 10, 15), 
  colsample_bytree = 0.9, 
  subsample = 0.8
)

### Fit Control 

fitControl <- trainControl(## 10 fold cross validation
  method = "repeatedcv",
  number = 5, #number of splits 
  repeats = 5, #repeat 10 times
  savePredictions = "final", #keep final model predictions, would otherwise be dumped
  returnResamp = "final")

############## Model Woody Cover ###################


for(i in 1:nrow(dt.tier)){
  
  
  tier <- dt.tier[i, ]$tier
  subset <- dt.tier[i, ]$subset
  tier_label <- dt.tier[i, ]$tier_label
  response <- dt.tier[i, ]$response
  response_tier <- dt.tier[i, ]$response_tier
  
  terms <- dt.tier[i, ]$terms
  
  terms.vector <- strsplit(gsub("'", "", terms), ", ")[[1]]
  
  
  dt.sub <- dt %>% dplyr::filter(eval(parse(text = subset)))
  
  print(paste0("starting ", response,"; tier: ", tier, " (", i, "/", nrow(dt.tier), ")"))
  
  dt.gbm <- dt.sub %>% 
    dplyr::select(all_of(response), all_of(terms.vector)) %>% dplyr::filter(complete.cases(.)) # %>% sample_n(40)
  
  ##############################################################################            
  ################################## RUN ALL MODELS ############################            
  ##############################################################################            
  
  formula <- as.formula(paste(response, "~ ."))
  
  listFit <- caretList(formula,
                       data = dt.gbm, 
                       # methodList = c("gbm", "rf", "xgbTree"),
                       trControl = fitControl,
                       verbose = FALSE,
                       tuneList=list(
                         gbm=caretModelSpec(method="gbm", tuneGrid=tuneGridGbm),
                         rf=caretModelSpec(method="rf", tuneGrid=tuneGridRf),
                         xgbTree=caretModelSpec(method="xgbTree", tuneGrid = tuneGridXgbTree) ))
  
  ## get best models and ensemble 
  greedyEnsemble <- caretEnsemble(
    listFit,
    metric="RMSE",
    trControl = trainControl(method = "repeatedcv", number = 5, repeats = 5))
  
  ### GBM 
  bestGBM <- listFit$gbm
  
  ### RF 
  
  bestRF <- listFit$rf
  
  ### xGBTREE
  
  bestXGB <- listFit$xgbTree
  
  if(dt.tier[i, ]$response == "tree_cover_mean"){
    bestCurrentWC <- bestRF
    
    greedyEnsembleCWC <- greedyEnsemble
    bestGbmCWC <- bestGBM
    bestRfCWC <- bestRF
    bestXgbCWC <- bestXGB
  }
  
  if(dt.tier[i, ]$response == "woody_cover_trend_venter2019"){
    bestWCChange <- greedyEnsemble
    
    greedyEnsembleWCC <- greedyEnsemble
    bestGbmWCC <- bestGBM
    bestRfWCC <- bestRF
    bestXgbWCC <- bestXGB
  }
  
  if(dt.tier[i, ]$response == "tree_cover_sd_100"){
    bestWCSD <- greedyEnsemble
    
    greedyEnsembleWCSD <- greedyEnsemble
    bestGbmWCSD  <- bestGBM
    bestRfCWCSD  <- bestRF
    bestXgbWCSD  <- bestXGB
  }
  
  if(dt.tier[i, ]$response == "canopy_height_sd_100"){
    bestCHSD <- greedyEnsemble
    
    greedyEnsembleCHSD <- greedyEnsemble
    bestGbmCHSD <- bestGBM
    bestRfCHSD <- bestRF
    bestXgbCHSD <- bestXGB
  }

  print(paste0(response, " done; ", i, "/", nrow(dt.tier)))
}


##### Interaction Plots and Computing #####