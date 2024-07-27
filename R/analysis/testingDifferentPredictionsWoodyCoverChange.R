### play around with best model

source("R/functions/get.heterogeneity.R")
source("R/functions/pdplot.R")
source("R/functions/pdPlotEnsemble.R")



library(MuMIn)
library(glmmTMB)
library(terra)
library(tidyverse)
library(data.table)
library(sf)
library(tidylog)
library(GGally)
library(scales)
library(segmented)
library(gridExtra)
library("qgam")
library(parallel)
library(gstat)
library(grid)
library(gbm)
library(caret)
library(tictoc)
library(MetBrewer)
library("caretEnsemble")




dt <- fread("data/ReserveDataSouthAfricaFinal.csv")  %>% 
  mutate(
    Biome = as.factor(Biome), 
    elephant_yn = ifelse(elephant_biomass_ha > 0, "elephants", "no_elephants")) 

names(dt)
### create model data ---------------------------------

##### SUBSET #####

#guide.subset <- NULL
##############################################################################            
################################## CREATE MODEL DATA ########################         
##############################################################################    

dt.gbm <- dt %>% dplyr::select('woody_cover_trend_venter2019', 'MAT', 'MAP', 'n_deposition', 'CW_mean_species_body_mass', 'herbi_fun_div_distq1', 'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 'browser_biomass_kgkm2', 'mixed_feeder_biomass_kgkm2', 'herbi_biomass_kgkm2', 'fire_events_since_2001', 'prop_burned_area')

dt.names <- data.table(
  term = c("MAT", "MAP", "elevation_sd_1000",
           "CW_mean_species_body_mass", "herbi_fun_div_distq1", "n_herbi_sp_reserve", 
           "grazer_biomass_kgkm2", "browser_biomass_kgkm2", "mixed_feeder_biomass_kgkm2", 
           "herbi_biomass_kgkm2", "fire_events_since_2001", "prop_burned_area", "n_deposition", "elephant_biomass_kgkm2"),
  clean_term = c("MAT (°C)", "MAP (mm)", "Elevation SD (m)",
                 "Mean body mass (kg; cwm)", "Herbivore functional diversity", 
                 "Herbivore species richness", "Grazer biomass (kg/km2)", 
                 "Browser biomass (kg/km2)", "Mixed feeder biomass (kg/km2)",
                 "Herbivore biomass (kg/km2)", "Fire frequency", "Proportion burned area", "N deposition ((kg/km2)/year)", "Elephant biomass (kg/km2)"))



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






##############################################################################            
################################## LOOOOOOOOOOOOP ############################            
##############################################################################    


  
  listFit <- caretList(woody_cover_trend_venter2019 ~ .,
                       data = dt.gbm, 
                       trControl = fitControl,
                       verbose = FALSE,
                       tuneList=list(
                         gbm=caretModelSpec(method="gbm", tuneGrid=tuneGridGbm),
                         rf=caretModelSpec(method="rf", tuneGrid=tuneGridRf),
                         xgbTree=caretModelSpec(method="xgbTree", tuneGrid = tuneGridXgbTree, silent = 0) ))
  
  
  greedyEnsemble <- caretEnsemble(
    listFit,
    metric="RMSE",
    trControl = trainControl(method = "repeatedcv", number = 5, repeats = 5))
  
  
  ### try simple with low medium high 
  

  ###get value ranges 
  fundiv <- dt.gbm %>% 
    filter(!herbi_fun_div_distq1 < quantile(herbi_fun_div_distq1, 0.025) & !herbi_fun_div_distq1 > quantile(herbi_fun_div_distq1, 0.975)) %>% 
    dplyr::select(herbi_fun_div_distq1) %>% pull()
  
  range(fundiv)
  
  biomass <- dt.gbm %>% 
    filter(!herbi_biomass_kgkm2 < quantile(herbi_biomass_kgkm2, 0.025) & !herbi_biomass_kgkm2 > quantile(herbi_biomass_kgkm2, 0.975)) %>% 
    dplyr::select(herbi_biomass_kgkm2) %>% pull()
  range(biomass)
  
  ndep <- dt.gbm %>% 
    filter(!n_deposition < quantile(n_deposition, 0.025) & !n_deposition > quantile(n_deposition, 0.975)) %>% 
    dplyr::select(n_deposition) %>% pull()
  range(ndep)
  
  
  range(herbis)
  dt.new <- data.table(
    herbi_biomass_kgkm2 = round(seq(min(biomass), max(biomass), length.out = 5), 2), 
    herbi_fun_div_distq1 = round(seq(min(fundiv), max(fundiv), length.out = 5), 2), 
    n_deposition = round(seq(min(ndep), max(ndep), length.out = 10), 2))
  
  biomassPreds <- data.table()
  funDivPreds <- data.table()
  nDepPreds <- data.table()
  
  
  for(i in 1:nrow(dt.new)){
    
    
    ## Herbivore biomass
    newdata1 <- dt.gbm %>% 
      mutate(herbi_biomass_kgkm2 = dt.new[i,]$herbi_biomass_kgkm2)
    
    y1 <- predict(greedyEnsemble, newdata = newdata1) %>% as.data.frame() 
    colnames(y1) <- paste0("biomass", i)
    biomassPreds <- cbind(biomassPreds, y1)
    
    ## Herbivore func div
    newdata2 <- dt.gbm %>% 
      mutate(herbi_fun_div_distq1 = dt.new[i,]$herbi_fun_div_distq1)
    
    y2 <- predict(greedyEnsemble, newdata = newdata2) %>% as.data.frame() 
    colnames(y2) <- paste0("funDiv", i)
    
    funDivPreds <- cbind(funDivPreds, y2)
    
    ## n deposition 
    newdata3 <- dt.gbm %>% 
      mutate(n_deposition = dt.new[i,]$n_deposition)
    
    y3 <- predict(greedyEnsemble, newdata = newdata3) %>% as.data.frame() 
    colnames(y3) <- paste0("nDep", i)
    
    nDepPreds <- cbind(nDepPreds, y3)
    
    
  }

  
  
  
  dt.pred <- dt.gbm %>% cbind(biomassPreds, funDivPreds, nDepPreds) 

  ogPredNDep <- partial(greedyEnsemble, "n_deposition", train = newdata)
  ogPredNDep <- ogPredNDep
  

t1 <-   ggplot() +
    geom_point(data = dt.pred, aes(x = n_deposition, y = biomass1, color = "very low biomass"), se = F) +
    geom_smooth(data = dt.pred, aes(x = n_deposition, y = biomass1, color = "very low biomass"), se = F) +
    geom_point(data = dt.pred, aes(x = n_deposition, y = biomass10, color = "very high biomass"), se = F) +
    geom_smooth(data = dt.pred, aes(x = n_deposition, y = biomass10, color = "very high biomass"), se = F) +
    scale_color_manual(values = c("very low biomass" = "darkblue", "very high biomass" = "orange")) + 
    theme_classic() +
    theme(legend.position = "bottom") +
    labs(y = "Woody cover change", x = "N deposition", color = "", title = "Varying Herbivore Biomass")
t1

t2 <-   ggplot() +
  geom_point(data = dt.pred, aes(x = n_deposition, y = funDiv1, color = "very low diversity"), se = F) +
  geom_smooth(data = dt.pred, aes(x = n_deposition, y = funDiv1, color = "very low diversity"), se = F) +
  geom_point(data = dt.pred, aes(x = n_deposition, y = funDiv10, color = "very high diversity"), se = F) +
  geom_smooth(data = dt.pred, aes(x = n_deposition, y = funDiv10, color = "very high diversity"), se = F) +
  scale_color_manual(values = c("very low diversity" = "darkblue", "very high diversity" = "orange")) + 
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(y = "Woody cover change", x = "N deposition", color = "", title = "Varying Functional Diversity")
t2


median(dt.pred$funDiv10) - median(dt.pred$funDiv1)

hvsl <- grid.arrange(t1, t2, ncol = 2)
ggsave(plot = hvsl, "builds/plots/july/high_vs_low_herbivores_along_ndep.png", dpi = 600, height = 4, width = 8)



  biomassVarNDep <- data.table()
  funDivVarNDep <- data.table()
  nDepVarBiomass <- data.table()
  nDepVarFunDiv <- data.table()
  
  for(i in 1:nrow(dt.new)){
    
    
    ## Herbivore biomass varies varies
    newdata1 <- dt.gbm %>% 
      mutate(herbi_biomass_kgkm2 = dt.new[i,]$herbi_biomass_kgkm2)
    
    y1 <- partial(greedyEnsemble, "n_deposition",  train = newdata1)
    y1 <- as.data.table(y1) %>% mutate(biomass = unique(dt.new[i,]$herbi_biomass_kgkm2))
    biomassVarNDep <- rbind(biomassVarNDep, y1)

        ## Herbivore func div varies 
    newdata2 <- dt.gbm %>% 
      mutate(herbi_fun_div_distq1 = dt.new[i,]$herbi_fun_div_distq1)
    
    y2 <- partial(greedyEnsemble, "n_deposition",  train = newdata2)
    y2 <- as.data.table(y2) %>% mutate(fundiv = unique(dt.new[i,]$herbi_fun_div_distq1))
    funDivVarNDep <- rbind(funDivVarNDep, y2)
    
    
    ## N dep Herbivore biomass
    newdata3 <- dt.gbm %>% 
      mutate(n_deposition = dt.new[i,]$n_deposition)
    
    y3 <- partial(greedyEnsemble, "herbi_biomass_kgkm2",  train = newdata3)
    y3 <- as.data.table(y3) %>% mutate(nDep = unique(dt.new[i,]$n_deposition))
    nDepVarBiomass <- rbind(nDepVarBiomass, y3)
    
    ## N dep Herbivore func div 
    newdata4 <- dt.gbm %>% 
      mutate(n_deposition = dt.new[i,]$n_deposition)
    
    y4 <- partial(greedyEnsemble, "herbi_fun_div_distq1",  train = newdata4)
    y4 <- as.data.table(y4) %>% mutate(nDep = unique(dt.new[i,]$n_deposition))
    nDepVarFunDiv <- rbind(nDepVarFunDiv, y4)
    
    print(i)

  }

  
  ogPredNDep <- partial(greedyEnsemble, "n_deposition",  train = dt.gbm)
  ogPredFunDiv <- partial(greedyEnsemble, "herbi_fun_div_distq1",  train = dt.gbm)
  ogPredBiomass <- partial(greedyEnsemble, "herbi_biomass_kgkm2",  train = dt.gbm)

p1 <-   ggplot() +
  geom_line(data = biomassVarNDep, aes(x = n_deposition, y = yhat, group = biomass, color = biomass), linewidth = 1.1) +
  scale_color_viridis_c() +
  labs(x = "N deposition (kg/km2/year)", y = "Woody cover change", color = "Herbivore Biomass (kg/km2)") +
  theme_classic() +
  theme(legend.position = "bottom")
p1  
  

p2 <-   ggplot() +
  geom_line(data = funDivVarNDep, aes(x = n_deposition, y = yhat, group = fundiv, color = fundiv), linewidth = 1.1) +
  scale_color_viridis_c() +
  labs(x = "N deposition (kg/km2/year)", y = "Woody cover change", color = "Herbivore functional diversity") +
  theme_classic() +
  theme(legend.position = "bottom")
p2  

p3 <-   ggplot() +
  geom_line(data = nDepVarFunDiv, aes(x = herbi_fun_div_distq1, y = yhat, group = nDep, color = nDep), linewidth = 1.1) +
  scale_color_viridis_c() +
  labs(x = "Herbivore functional diversity", y = "Woody cover change", color = "N deposition") +
  theme_classic() +
  theme(legend.position = "bottom")
p3  

p4 <-   ggplot() +
  geom_line(data = nDepVarBiomass, aes(x = herbi_biomass_kgkm2, y = yhat, group = nDep, color = nDep), linewidth = 1.1) +
  scale_color_viridis_c() +
  labs(x = "Herbivore biomass", y = "Woody cover change", color = "N deposition") +
  theme_classic() +
  theme(legend.position = "bottom")
p4  

pdp.diff <- grid.arrange(p1, p2, p3, p4)
ggsave(plot = pdp.diff, "builds/plots/july/partial_dependece_varying_conditions.png", dpi = 600, height = 8, width = 8)
