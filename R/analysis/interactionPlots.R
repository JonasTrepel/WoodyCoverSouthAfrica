#### Make interaction plots ####
library("hstats")
library(caret)
library(caretEnsemble)
library(data.table)
library(tidyverse)
library(gridExtra)

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
  clean_term = c("Woody\nCover\nHetero-\ngeneity", "MAT (°C)", "MAP (mm)", "Elevation SD (m)",
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
    
    
    dtCWC <- dt.gbm
    greedyEnsembleCWC <- greedyEnsemble
    bestGbmCWC <- bestGBM
    bestRfCWC <- bestRF
    bestXgbCWC <- bestXGB
  }
  
  if(dt.tier[i, ]$response == "woody_cover_trend_venter2019"){
    bestWCChange <- greedyEnsemble
    
    dtWCC <- dt.gbm
    greedyEnsembleWCC <- greedyEnsemble
    bestGbmWCC <- bestGBM
    bestRfWCC <- bestRF
    bestXgbWCC <- bestXGB
  }
  
  if(dt.tier[i, ]$response == "tree_cover_sd_100"){
    bestWCSD <- greedyEnsemble
    
    dtWCSD <- dt.gbm
    greedyEnsembleWCSD <- greedyEnsemble
    bestGbmWCSD  <- bestGBM
    bestRfCWCSD  <- bestRF
    bestXgbWCSD  <- bestXGB
  }
  
  if(dt.tier[i, ]$response == "canopy_height_sd_100"){
    bestCHSD <- greedyEnsemble
    
    dtWCSD
    greedyEnsembleCHSD <- greedyEnsemble
    bestGbmCHSD <- bestGBM
    bestRfCHSD <- bestRF
    bestXgbCHSD <- bestXGB
  }

  print(paste0(response, " done; ", i, "/", nrow(dt.tier)))
}


##### Interaction Plots and Computing #####


######## Load functions ###########
source("R/functions/Mode.R")
source("R/functions/plotInteractionTile.R")



##### Current woody cover ######
hstatsCWC <- hstats(bestCurrentWC, X = dt.sub, v = c('MAT', 'MAP', 'CW_mean_species_body_mass', 'herbi_biomass_kgkm2', 'fire_events_since_2001', 'prop_burned_area'))
h2_pairwise(hstatsCWC)

### plot interactions of 4 most important variables 
#MAP, MAT, Herbivore Biomass, Mean Body Mass

## MAP:MAT
cwcP1 <- plotInteractionTile(mod = bestCurrentWC, var1 = "MAP", var2 = "MAT", data = dt.sub) +
  labs(fill = "Current\nWoody\nCover (%)", x = "Mean Annual Precipitation (mm)",  y = "Mean Annual Temperature (°C)") 
cwcP1

## MAP:HB
cwcP2 <- plotInteractionTile(mod = bestCurrentWC, var1 = "MAP", var2 = "herbi_biomass_kgkm2", data = dt.sub) +
  labs(fill = "Current\nWoody\nCover (%)", x = "Mean Annual Precipitation (mm)" , y = "Herbivore Biomass (kg/km2)") 
cwcP2

## MAP:MBM
cwcP3 <- plotInteractionTile(mod = bestCurrentWC, var1 = "MAP", var2 = "CW_mean_species_body_mass", data = dt.sub) +
  labs(fill = "Current\nWoody\nCover (%)", x = "Mean Annual Precipitation (mm)", y = "Mean Body Mass (kg; cwm)") 
cwcP3

## MAT:HB
cwcP4 <- plotInteractionTile(mod = bestCurrentWC, var1 = "MAT", var2 = "herbi_biomass_kgkm2", data = dt.sub) +
  labs(fill = "Current\nWoody\nCover (%)", x = "Mean Annual Temperature (°C)", y = "Herbivore Biomass (kg/km2)") 
cwcP4

## MAT:MBM
cwcP5 <- plotInteractionTile(mod = bestCurrentWC, var1 = "MAP", var2 = "CW_mean_species_body_mass", data = dt.sub) +
  labs(fill = "Current\nWoody\nCover (%)", x = "Mean Annual Temperature (°C)", y = "Mean Body Mass (kg; cwm)") 
cwcP5

## HB:MBM
cwcP6 <- plotInteractionTile(mod = bestCurrentWC, var1 = "herbi_biomass_kgkm2", var2 = "CW_mean_species_body_mass", data = dt.sub) +
  labs(fill = "Current\nWoody\nCover (%)", x = "Herbivore Biomass (kg/km2)", y = "Mean Body Mass (kg; cwm)") 
cwcP6

### summarize 

cwcIntPlot <- grid.arrange(cwcP1, cwcP2, cwcP3, cwcP4, cwcP5, cwcP6, ncol = 3)
ggsave(plot = cwcIntPlot, "builds/plots/september/interactionPlots/currentWCInteractions.png", dpi = 600, width = 12, height = 5.5)

##### Woody Cover Change ######

hstatsWCC <- hstats(bestWCChange, X = dt.sub, v = c('MAT', 'MAP', 'n_deposition',
                                                    'CW_mean_species_body_mass', 'herbi_fun_div_distq1',
                                                    'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 
                                                    'browser_biomass_kgkm2', 'mixed_feeder_biomass_kgkm2',
                                                    'herbi_biomass_kgkm2', 'fire_events_since_2001', 'prop_burned_area'))
h2_pairwise(hstatsWCC)

### plot interactions of 4 most important variables 
#MAP, MAT, Herbivore Biomass, Mean Body Mass

## Ndepo:MAT
wccP1 <- plotInteractionTile(mod = bestWCChange, var1 = "n_deposition", var2 = "MAP", data = dt.sub) +
  labs(fill = "Woody\nCover\nChange\n(%/year)", x = "N deposition ([kg/km2]/year)",  y = "Mean Annual Precipitation (mm)") 
wccP1

## Ndepo:MAP
wccP2 <- plotInteractionTile(mod = bestWCChange, var1 = "n_deposition", var2 = "MAT", data = dt.sub) +
  labs(fill = "Woody\nCover\nChange\n(%/year)", x = "N deposition ([kg/km2]/year)" , y = "Mean Annual Temperature (°C)") 
wccP2

## Ndepo:HFD
wccP3 <- plotInteractionTile(mod = bestWCChange, var1 = "n_deposition", var2 = "herbi_fun_div_distq1", data = dt.sub) +
  labs(fill = "Woody\nCover\nChange\n(%/year)", x = "N deposition ([kg/km2]/year)", y = "Herbivore Functional Diversity") 
wccP3

## MAP:MAT
wccP4 <- plotInteractionTile(mod = bestWCChange, var1 = "MAT", var2 = "MAP", data = dt.sub) +
  labs(fill = "Woody\nCover\nChange\n(%/year)", x = "Mean Annual Temperature (°C)", y = "Mean Annual Precipitation (mm)") 
wccP4

## MAP:HFD
wccP5 <- plotInteractionTile(mod = bestWCChange, var1 = "MAP", var2 = "herbi_fun_div_distq1", data = dt.sub) +
  labs(fill = "Woody\nCover\nChange\n(%/year)", x = "Mean Annual Precipitation (mm)", y = "Herbivore Functional Diversity") 
wccP5

## MAT:HFD
wccP6 <- plotInteractionTile(mod = bestWCChange, var1 = "MAT", var2 = "herbi_fun_div_distq1", data = dt.sub) +
  labs(fill = "Woody\nCover\nChange\n(%/year)", x = "Mean Annual Temperature (°C)", y = "Herbivore Functional Diversity") 
wccP6

## HBM:HDF
wccP7 <- plotInteractionTile(mod = bestWCChange, var1 = "herbi_biomass_kgkm2", var2 = "herbi_fun_div_distq1", data = dt.sub) +
  labs(fill = "Woody\nCover\nChange\n(%/year)", x = "Herbivore Biomass (kg/km2)", y = "Herbivore Functional Diversity") 
wccP7

### summarize 

wccIntPlot <- grid.arrange(wccP1, wccP2, wccP3, wccP4, wccP5, wccP6, ncol = 3)
ggsave(plot = wccIntPlot, "builds/plots/september/interactionPlots/wcChangeInteractions.png", dpi = 600, width = 12, height = 5.5)

##### Woody\nCover\nHetero-\ngeneity ######

hstatsWCSD <- hstats(bestWCSD, X = dt.sub, v = c('MAT', 'MAP', 'elevation_sd_1000',
                                                    'CW_mean_species_body_mass', 'herbi_fun_div_distq1',
                                                    'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 
                                                    'browser_biomass_kgkm2', 'mixed_feeder_biomass_kgkm2',
                                                    'herbi_biomass_kgkm2', 'fire_events_since_2001', 'prop_burned_area'))
h2_pairwise(hstatsWCSD)

### plot interactions of 4 most important variables 
#MAP, MAT, Herbivore Biomass, Mean Body Mass

## MAT:MAP
wcsdP1 <- plotInteractionTile(mod = bestWCSD, var1 = "MAT", var2 = "MAP", data = dt.sub) +
  labs(fill = "Woody\nCover\nHetero-\ngeneity", x = "Mean Annual Temperature (°C)",  y = "Mean Annual Precipitation (mm)") 
wcsdP1

## MAT:Elev
wcsdP2 <- plotInteractionTile(mod = bestWCSD, var1 = "MAT", var2 = "elevation_sd_1000", data = dt.sub) +
  labs(fill = "Woody\nCover\nHetero-\ngeneity", x = "Mean Annual Temperature (°C)" , y = "Elevation SD") 
wcsdP2

## MAT:cwmbm
wcsdP3 <- plotInteractionTile(mod = bestWCSD, var1 = "MAT", var2 = "CW_mean_species_body_mass", data = dt.sub) +
  labs(fill = "Woody\nCover\nHetero-\ngeneity", x = "Mean Annual Temperature (°C)", y = "Mean Body Mass (kg; cwm)") 
wcsdP3

## MAP:elev
wcsdP4 <- plotInteractionTile(mod = bestWCSD, var1 = "MAP", var2 = "elevation_sd_1000", data = dt.sub) +
  labs(fill = "Woody\nCover\nHetero-\ngeneity", x = "Mean Annual Precipitation (mm)", y = "Elevation SD") 
wcsdP4

## MAP:cwmbm
wcsdP5 <- plotInteractionTile(mod = bestWCSD, var1 = "MAP", var2 = "CW_mean_species_body_mass", data = dt.sub) +
  labs(fill = "Woody\nCover\nHetero-\ngeneity", x = "Mean Annual Precipitation (mm)", y = "Mean Body Mass (kg; cwm)") 
wcsdP5

## elev:cwmbm
wcsdP6 <- plotInteractionTile(mod = bestWCSD, var1 = "elevation_sd_1000", var2 = "CW_mean_species_body_mass", data = dt.sub) +
  labs(fill = "Woody\nCover\nHetero-\ngeneity", x = "Elevation SD", y = "Mean Body Mass (kg; cwm)") 
wcsdP6

## HBM:HDF
wcsdP7 <- plotInteractionTile(mod = bestWCSD, var1 = "herbi_biomass_kgkm2", var2 = "herbi_fun_div_distq1", data = dt.sub) +
  labs(fill = "Woody\nCover\nHetero-\ngeneity", x = "Herbivore Biomass (kg/km2)", y = "Herbivore Functional Diversity") 
wcsdP7

### summarize 

wcsdIntPlot <- grid.arrange(wcsdP1, wcsdP2, wcsdP3, wcsdP4, wcsdP5, wcsdP6, ncol = 3)
ggsave(plot = wcsdIntPlot, "builds/plots/september/interactionPlots/WCSDInteractions.png", dpi = 600, width = 12, height = 5.5)

##### Canopy\nHeight\nHetero-\ngeneity ######

hstatsCHSD <- hstats(bestCHSD, X = dt.sub, v = c('MAT', 'MAP', 'elevation_sd_1000',
                                                 'CW_mean_species_body_mass', 'herbi_fun_div_distq1',
                                                 'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 
                                                 'browser_biomass_kgkm2', 'mixed_feeder_biomass_kgkm2',
                                                 'herbi_biomass_kgkm2', 'fire_events_since_2001', 'prop_burned_area'))
h2_pairwise(hstatsCHSD)

### plot interactions of 4 most important variables 
#MAP, MAT, Elev SD, Herbivore fun div 

## MAT:MAP
chsdP1 <- plotInteractionTile(mod = bestCHSD, var1 = "MAT", var2 = "MAP", data = dt.sub) +
  labs(fill = "Canopy\nHeight\nHetero-\ngeneity", x = "Mean Annual Temperature (°C)",  y = "Mean Annual Precipitation (mm)") 
chsdP1

## MAT:Elev
chsdP2 <- plotInteractionTile(mod = bestCHSD, var1 = "MAT", var2 = "elevation_sd_1000", data = dt.sub) +
  labs(fill = "Canopy\nHeight\nHetero-\ngeneity", x = "Mean Annual Temperature (°C)" , y = "Elevation SD") 
chsdP2

## MAT:hfd
chsdP3 <- plotInteractionTile(mod = bestCHSD, var1 = "MAT", var2 = "herbi_fun_div_distq1", data = dt.sub) +
  labs(fill = "Canopy\nHeight\nHetero-\ngeneity", x = "Mean Annual Temperature (°C)", y = "Herbivore Functional Diversity") 
chsdP3

## MAP:elev
chsdP4 <- plotInteractionTile(mod = bestCHSD, var1 = "MAP", var2 = "elevation_sd_1000", data = dt.sub) +
  labs(fill = "Canopy\nHeight\nHetero-\ngeneity", x = "Mean Annual Precipitation (mm)", y = "Elevation SD") 
chsdP4

## MAP:hfd
chsdP5 <- plotInteractionTile(mod = bestCHSD, var1 = "MAP", var2 = "herbi_fun_div_distq1", data = dt.sub) +
  labs(fill = "Canopy\nHeight\nHetero-\ngeneity", x = "Mean Annual Precipitation (mm)", y = "Herbivore Functional Diversity") 
chsdP5

## elev:hfd
chsdP6 <- plotInteractionTile(mod = bestCHSD, var1 = "elevation_sd_1000", var2 = "herbi_fun_div_distq1", data = dt.sub) +
  labs(fill = "Canopy\nHeight\nHetero-\ngeneity", x = "Elevation SD", y = "Herbivore Functional Diversity") 
chsdP6

## HBM:HDF
chsdP7 <- plotInteractionTile(mod = bestCHSD, var1 = "herbi_biomass_kgkm2", var2 = "herbi_fun_div_distq1", data = dt.sub) +
  labs(fill = "Canopy\nHeight\nHetero-\ngeneity", x = "Herbivore Biomass (kg/km2)", y = "Herbivore Functional Diversity") 
chsdP7

### summarize 

chsdIntPlot <- grid.arrange(chsdP1, chsdP2, chsdP3, chsdP4, chsdP5, chsdP6, ncol = 3)
ggsave(plot = chsdIntPlot, "builds/plots/september/interactionPlots/CHSDInteractions.png", dpi = 600, width = 12, height = 5.5)


## body mass / diversity interactions 
bmDivPlot <- grid.arrange(wccP7, wcsdP7, chsdP7, ncol = 3)
ggsave(plot = bmDivPlot, "builds/plots/september/interactionPlots/BodyMassDiversityInteractions.png", dpi = 600, width = 12, height = 2.75)


