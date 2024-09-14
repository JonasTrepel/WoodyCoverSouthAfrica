

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





sa.pas <- fread("data/south_african_pas_w_covariates.csv") %>% dplyr::select(WDPA_PID) %>% pull()

dt <- fread("data/sub_saharan_african_pas_w_covariates.csv") %>% 
  dplyr::select(WDPA_PID, DESIG_ENG, prop_burned_area, woody_trend_venter2019, MAT, MAP, fire_events_since_2001, n_deposition,
                spatial_predictor1, spatial_predictor2, spatial_predictor3, spatial_predictor4, spatial_predictor5) %>% 
  filter(complete.cases(.)) %>% 
  mutate(south_africa = ifelse(WDPA_PID %in% c(sa.pas), "yes", "no"))
table(dt$south_africa)


### create model data ---------------------------------

##### SUBSET #####

#guide.subset <- "response %in% c('tree_cover_sd_100') & tier %in% c('grassland', 'fynbos', 'nama_karoo')"
guide.subset <- NULL
##############################################################################            
################################## CREATE MODEL GUIDE ########################         
##############################################################################    


subsets <- c("!is.na(WDPA_PID)", "south_africa == 'no'")

tier_labels<- c("Protected areas across Sub-Saharan Africa", "Sub-Saharan African protected areas outside South Africa")

tiers <- c("main", "outside_sa")

responses <- c("woody_trend_venter2019")

dt.tier.raw <- data.table(
  subset = subsets, 
  tier = tiers, 
  tier_label = tier_labels, 
  n = 0)

tier.resp <- CJ(response = responses, tier = tiers) %>% 
  mutate(terms = case_when(
    response == 'woody_trend_venter2019' ~ "woody_trend_venter2019', 'MAT', 'MAP', 'n_deposition', 'fire_events_since_2001', 'prop_burned_area', 'spatial_predictor1', 'spatial_predictor2', 'spatial_predictor3', 'spatial_predictor4', 'spatial_predictor5'"))
dt.tier <- dt.tier.raw %>% left_join(tier.resp)  %>% 
  mutate(response_tier = paste0(response,"_",tier))

for(i in 1:nrow(dt.tier)){
  subset <- dt.tier[i, ]$subset
  dt.sub <- dt %>% dplyr::filter(eval(parse(text = subset)))
  
  nr <- nrow(dt.sub)
  dt.tier[i, ]$n <- nr
  
}
unique(dt.tier$n)


if(!is.null(guide.subset)){
  dt.tier <- dt.tier %>% filter(eval(parse(text = guide.subset)))
}


dt.tier <- dt.tier[!dt.tier$n < 35]

##############################################################################            
################################## DEFINE STUFF ############################            
##############################################################################    

# color palettes 
palette <- c("MAT (°C)" = "#213B48", "MAP (mm)" = "#2B4655", "N deposition ([kg/km2]/year)" = "#334F5D", "Elevation SD (m)" = "#3B5966",
             "Mean body mass (kg; cwm)" = "#44636F",
             "Herbivore functional diversity" = "#537179",
             "Herbivore species richness" = "#637F83", "Grazer biomass (kg/km2)" = "#738E8E",
             "Browser biomass (kg/km2)" = "#6F8679", "Mixed feeder biomass (kg/km2)" = "#6C7F66",
             "Herbivore biomass (kg/km2)" = "#697852", "Elephant biomass (kg/km2)" = "#7E7C55",
             "Fire frequency" = "#938058",
             "Proportion burned area" = "#A9845B")

c(met.brewer(name = "Egypt"))
palette.methods <- c("gbm" = "#dd5129", "rf" = "#0f7ba2", "xgbTree" = "#43b284", "ensemble" = "#fab255")

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






statsStorage <- data.table()
##############################################################################            
################################## LOOOOOOOOOOOOP ############################            
##############################################################################    

tic()

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
  
  
  greedyEnsemble <- caretEnsemble(
    listFit,
    metric="RMSE",
    trControl = trainControl(method = "repeatedcv", number = 5, repeats = 5))
  
  
  
  ### define optimal parameters
  
  ### GBM 
  tmp.gbm <- listFit$gbm$results %>% slice_min(RMSE) %>%
    mutate(tier = tier, n = nrow(dt.gbm), method = "gbm")
  
  tuneGridGbmOpt <- tmp.gbm %>% dplyr::select(shrinkage, interaction.depth, n.minobsinnode, n.trees)
  
  
  ### RF 
  
  tmp.rf <- listFit$rf$results %>% slice_min(RMSE) %>%
    mutate(tier = tier, n = nrow(dt.gbm), method = "rf")
  
  tuneGridRfOpt <- tmp.rf %>% dplyr::select(mtry)
  
  ### xGBTREE
  
  tmp.rf <- listFit$xgbTree$results %>% slice_min(RMSE) %>%
    mutate(tier = tier, n = nrow(dt.gbm), method = "xgbTree")
  
  tuneGridXgbTreeOpt <- tmp.rf %>% dplyr::select(eta, max_depth, gamma, colsample_bytree, min_child_weight, subsample, nrounds)
  
  
  ##############################################################################            
  ################################## Bootstrap #################################            
  ##############################################################################  
  
  
  statsBt <- data.table()
  varImpBt <- data.table()
  predsBt <- data.table()
  
  for(number in 1:100){
    
    Index <- createDataPartition(dt.gbm[[response]], p=0.8, list = F) 
    
    Train <- dt.gbm[Index,]
    Test <- dt.gbm[!Index,]
    
    
    listFitBt <- caretList(formula,
                           data = Train, 
                           trControl = fitControl,
                           verbose = FALSE,
                           tuneList=list(
                             gbm=caretModelSpec(method="gbm", tuneGrid=tuneGridGbmOpt),
                             rf=caretModelSpec(method="rf", tuneGrid=tuneGridRfOpt),
                             xgbTree=caretModelSpec(method="xgbTree", tuneGrid = tuneGridXgbTreeOpt) ))
    
    #get different models ----
    
    #ensemble 
    
    greedyEnsembleBt <- caretEnsemble(listFitBt,
                                      metric="RMSE",
                                      trControl = trainControl(method = "repeatedcv", number = 5, repeats = 5))
    
    prEnsemble <- caret::postResample(pred = predict(greedyEnsembleBt, newdata = Test),
                                      obs = Test[[response]])
    
    #Gbm
    rfGbm <- listFitBt$gbm
    
    prGbm <- caret::postResample(pred = predict(rfGbm, newdata = Test),
                                 obs = Test[[response]])
    
    #Rf
    rfBt <- listFitBt$rf
    
    prRf <- caret::postResample(pred = predict(rfBt, newdata = Test),
                                obs = Test[[response]])
    
    #xgbTree
    xgbTreeBt <- listFitBt$xgbTree
    
    prXgbTree <- caret::postResample(pred = predict(xgbTreeBt, newdata = Test),
                                     obs = Test[[response]])
    
    
    ### get stats ----
    
    # Create separate data tables for each method
    if(is.null(greedyEnsembleBt)){prEnsemble <- NA}
    ensembleStatsTmp <- data.table(
      rmse_bt = prEnsemble[1],
      r_sq_bt = prEnsemble[2],
      mae_bt = prEnsemble[3],
      method = "ensemble",
      iteration = paste0("it_", number)
    )
    
    gbmStatsTmp <- data.table(
      rmse_bt = prGbm[1],
      r_sq_bt = prGbm[2],
      mae_bt = prGbm[3],
      method = "gbm",
      iteration = paste0("it_", number)
    )
    
    rfStatsTmp <- data.table(
      rmse_bt = prRf[1],
      r_sq_bt = prRf[2],
      mae_bt = prRf[3],
      method = "rf",
      iteration = paste0("it_", number)
    )
    
    xgbtreeStatsTmp <- data.table(
      rmse_bt = prXgbTree[1],
      r_sq_bt = prXgbTree[2],
      mae_bt = prXgbTree[3],
      method = "xgbTree",
      iteration = paste0("it_", number)
    )
    
    
    tmpStats <- rbind(xgbtreeStatsTmp, rfStatsTmp, gbmStatsTmp, ensembleStatsTmp)
    
    
    
    statsBt <- rbind(statsBt, tmpStats)
    
    #### get termiable importances
    
    varImpTmp <- as.data.frame(varImp(greedyEnsembleBt)) %>% rownames_to_column(var = "term")
    
    
    varImpBt <- rbind(varImpBt, varImpTmp)
    
    #### get partial dependence plot predictions 
    TrainP <- Train %>% dplyr::select(-all_of(response))
    
    library(pdp)
    
    ### GBM 
    for(j in 1:ncol(TrainP)){
      predTmpGbm <- partial(listFitBt$gbm, j, train = TrainP)
      term_name <- colnames(predTmpGbm)[1]
      colnames(predTmpGbm) <- c("x", "y")
      predTmpGbm <- predTmpGbm %>% mutate(term = paste0(term_name),
                                          iteration = paste0("it_", number), 
                                          method = "gbm")
      if(j==1){
        margGbm <- predTmpGbm}else{
          margGbm <- rbind(margGbm, predTmpGbm)}
    }
    
    newData <- Train %>% dplyr::select(-all_of(response)) 
    
    
    ### RF
    for(k in 1:ncol(TrainP)){
      predTmpRf <- partial(listFitBt$rf, k, train = TrainP)
      term_name <- colnames(predTmpRf)[1]
      colnames(predTmpRf) <- c("x", "y")
      predTmpRf <- predTmpRf %>% mutate(term = paste0(term_name),
                                        iteration = paste0("it_", number),
                                        method = "rf")
      if(k==1){
        margRf <- predTmpRf}else{
          margRf <- rbind(margRf, predTmpRf)}
    }
    
    ### XgbTree 
    for(l in 1:ncol(TrainP)){
      predTmpXgbTree <- partial(listFitBt$xgbTree, l, train = TrainP)
      term_name <- colnames(predTmpXgbTree)[1]
      colnames(predTmpXgbTree) <- c("x", "y")
      predTmpXgbTree <- predTmpXgbTree %>% mutate(term = paste0(term_name),
                                                  iteration = paste0("it_", number), 
                                                  method = "xgbTree")
      if(l==1){
        margXgbTree <- predTmpXgbTree}else{
          margXgbTree <- rbind(margXgbTree, predTmpXgbTree)}
    }
    
    
    ## Ensemble 
    
    for(o in 1:ncol(TrainP)){
      predTmpEnsemble <- partial(greedyEnsembleBt, o, train = TrainP)
      term_name <- colnames(predTmpEnsemble)[1]
      colnames(predTmpEnsemble) <- c("x", "y")
      predTmpEnsemble <- predTmpEnsemble %>% mutate(term = paste0(term_name),
                                                    iteration = paste0("it_", number), 
                                                    method = "ensemble")
      if(o==1){
        margEnsemble <- predTmpEnsemble}else{
          margEnsemble <- rbind(margEnsemble, predTmpEnsemble)}
    }
    
    
    tmpPreds <- rbind(margGbm, margRf, margXgbTree, margEnsemble) 
    
    tmpPreds <- tmpPreds[!grepl("spatial_pred", tmpPreds$term), ]
    
    predsBt <- rbind(tmpPreds, predsBt)
    
    print(paste0(number,"/",100))
    
    
  }
  
  varImpBt
  predsBt
  statsBt
  
  ##############################################################################            
  ################################## SUMMARIZE #################################            
  ##############################################################################  
  
  varImpMeans <- varImpBt %>% 
    pivot_longer(cols = c(overall, gbm, rf, xgbTree), names_to = "method", values_to = "rel_imp") %>%
    group_by(method, term) %>% 
    summarize(
      mean_rel_imp = mean(rel_imp), 
      sd_rel_imp = sd(rel_imp)
    ) %>% 
    mutate(method = ifelse(method == "overall", "ensemble", method)) %>% left_join(dt.names)
  
  varImpMeans <- varImpMeans[!grepl("spatial_predictor", varImpMeans$term),]
  
  statMeans <- statsBt %>% 
    group_by(method) %>% 
    summarize(
      mean_r_sq = mean(r_sq_bt),
      sd_r_sq = sd(r_sq_bt),
      mean_rmse = mean(rmse_bt),
      sd_rmse = sd(rmse_bt),
      mean_mae = mean(mae_bt),
      sd_mae = sd(mae_bt),
    ) %>% mutate(
      response = response,
      tier = tier, 
      response_tier = response_tier
    )
  
  statsStorage <- rbind(statsStorage, statMeans)
  
  
  predMeans <- predsBt %>% 
    group_by(method, term, x) %>% 
    summarize(mean_y = median(y),
              sd_y = sd(y)
    )  %>% left_join(dt.names)
  
  predMeans <- predMeans[!grepl("spatial_predictor", predMeans$term),]
  
  
  best_method <- statMeans %>% slice_min(mean_rmse) %>% dplyr::select(method) %>% pull() 
  
  
  ##############################################################################            
  ############################ COMPARISON PLOT #################################            
  ############################################################################## 
  
  ## get terms in order
  ordered_terms <- varImpMeans[varImpMeans$method %in% c(best_method), ]
  ordered_terms <- ordered_terms %>%
    arrange((mean_rel_imp)) %>%
    pull(clean_term)
  
  
  varImpMeans$clean_term <- factor(varImpMeans$clean_term, levels = ordered_terms)
  
  
  ## get names in order: 
  varImpMeans[is.na(varImpMeans$clean_term), ]
  
  clean.label <- case_when(
    .default = response, 
    response == "tree_cover_mean" ~ "Current woody cover (%)", 
    response == "woody_cover_trend_venter2019" ~ "Woody cover change (%/year)",
    response == "woody_trend_venter2019" ~ "Woody cover change (%/year)",
    response == "tree_cover_sd_100" ~ "Woody cover heterogeneity", 
    response == "canopy_height_sd_100" ~ "Canopy height heterogeneity")
  
  p.var.comp <- ggplot(data = varImpMeans) +
    geom_col(aes(y = clean_term, x = mean_rel_imp, fill = method, group = method),
             position = position_dodge(width = 0.8), alpha = 0.9) +
    geom_errorbar(aes(y = clean_term, 
                      xmin = mean_rel_imp - sd_rel_imp, 
                      xmax = mean_rel_imp + sd_rel_imp,
                      group = method),
                  position = position_dodge(width = 0.8), width = 0.25) +
    scale_fill_manual(values = palette.methods) +
    labs(y = "", x = "Mean Relative Importance") +
    theme_classic() + 
    theme(axis.text = element_text(size = 11))
  
  p.var.comp
  
  ### pred plot comp 
  ordered_terms_pred <- varImpMeans[varImpMeans$method %in% c(best_method), ]
  
  ordered_terms_pred <- ordered_terms_pred %>%
    arrange(desc(mean_rel_imp)) %>%
    pull(clean_term)
  
  predMeans$clean_term <- factor(predMeans$clean_term, levels = ordered_terms_pred)
  
  it <- unique(predsBt$iteration)
  it <- sample(it, 5)
  predMeansPlot <- predsBt[predsBt$iteration %in% c(it), ] %>% left_join(dt.names)
  
  
  
  
  ######## Make it more complicated and plot the best model #####
  dt.gbm.P <- dt.gbm %>% dplyr::select(-all_of(response))
  
  ### GBM 
  for(j in 1:ncol(dt.gbm.P)){
    predTmpGbm <- partial(listFit$gbm, j, train = dt.gbm.P)
    term_name <- colnames(predTmpGbm)[1]
    colnames(predTmpGbm) <- c("x", "y")
    predTmpGbm <- predTmpGbm %>% mutate(term = paste0(term_name),
                                        iteration = paste0("it_", number), 
                                        method = "gbm")
    if(j==1){
      margGbm <- predTmpGbm}else{
        margGbm <- rbind(margGbm, predTmpGbm)}
  }
  
  
  ### RF
  for(k in 1:ncol(dt.gbm.P)){
    predTmpRf <- partial(listFit$rf, k, train = dt.gbm.P)
    term_name <- colnames(predTmpRf)[1]
    colnames(predTmpRf) <- c("x", "y")
    predTmpRf <- predTmpRf %>% mutate(term = paste0(term_name),
                                      iteration = paste0("it_", number),
                                      method = "rf")
    if(k==1){
      margRf <- predTmpRf}else{
        margRf <- rbind(margRf, predTmpRf)}
  }
  
  ### XgbTree 
  for(l in 1:ncol(dt.gbm.P)){
    predTmpXgbTree <- partial(listFit$xgbTree, l, train = dt.gbm.P)
    term_name <- colnames(predTmpXgbTree)[1]
    colnames(predTmpXgbTree) <- c("x", "y")
    predTmpXgbTree <- predTmpXgbTree %>% mutate(term = paste0(term_name),
                                                iteration = paste0("it_", number), 
                                                method = "xgbTree")
    if(l==1){
      margXgbTree <- predTmpXgbTree}else{
        margXgbTree <- rbind(margXgbTree, predTmpXgbTree)}
  }
  
  
  ## Ensemble 
  
  for(o in 1:ncol(TrainP)){
    predTmpEnsemble <- partial(greedyEnsemble, o, train = dt.gbm.P)
    term_name <- colnames(predTmpEnsemble)[1]
    colnames(predTmpEnsemble) <- c("x", "y")
    predTmpEnsemble <- predTmpEnsemble %>% mutate(term = paste0(term_name),
                                                  iteration = paste0("it_", number), 
                                                  method = "ensemble")
    if(o==1){
      margEnsemble <- predTmpEnsemble}else{
        margEnsemble <- rbind(margEnsemble, predTmpEnsemble)}
  }
  
  
  predMeansPlot <- rbind(margGbm, margRf, margXgbTree, margEnsemble) %>% 
    left_join(varImpMeans) %>% 
    left_join(dt.names)
  
  predMeansPlot$clean_term <- factor(predMeansPlot$clean_term, levels = ordered_terms_pred)
  predMeansPlot <- predMeansPlot[!grepl("spatial_predictor", predMeansPlot$term),]
  
  ### PREPARE PLOT STUFF ####
  
  dt.points <- dt.gbm %>% pivot_longer(
    cols = c(-{{response}}), 
    values_to = "x", names_to = "term") %>% 
    mutate(y = median(predMeansPlot$y, na.rm = T)) %>% 
    left_join(varImpMeans[varImpMeans$method %in% c(best_method), ]) 
  
  dt.points <- dt.points[!grepl("spatial_predictor", dt.points$term),]
  
  
  
  dt.mean.rug <- dt.points %>% dplyr::select(term, x, y) %>% left_join(dt.names)
  
  
  rects <- dt.mean.rug %>%
    group_by(term) %>%
    mutate(
      lower_quantile_x = quantile(x, 0.05),
      upper_quantile_x = quantile(x, 0.95),
    ) %>%
    ungroup() %>% 
    group_by(term) %>%
    summarize(
      ymin = -Inf,
      ymax = Inf,
      xmin1 = -Inf,
      xmax1 = first(lower_quantile_x),
      xmin2 = first(upper_quantile_x),
      xmax2 = Inf
    ) %>%
    ungroup() %>% left_join(dt.names)
  
  
  
  p.pd.comp <- ggplot()+
    geom_line(data = predMeansPlot[predMeansPlot$method == "gbm", ], aes(x=x, y=y, group = iteration, color = as.factor(method)), linewidth = 1.1, alpha = 0.9) +
    geom_line(data = predMeansPlot[predMeansPlot$method == "rf", ], aes(x=x, y=y, group = iteration, color = as.factor(method)), linewidth = 1.1, alpha = 0.9) +
    geom_line(data = predMeansPlot[predMeansPlot$method == "xgbTree", ], aes(x=x, y=y, group = iteration, color = as.factor(method)), linewidth = 1.1, alpha = 0.9) +
    geom_line(data = predMeansPlot[predMeansPlot$method == "ensemble", ], aes(x=x, y=y, group = iteration, color = as.factor(method)), linewidth = 1.1, alpha = 0.9) +
    facet_wrap(~factor(clean_term),
               scales="free_x", ncol = 4) +
    scale_x_continuous(breaks = extended_breaks(n = 3)) +
    scale_color_manual(values = palette.methods) +
    geom_rect(data = rects, aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax), 
              fill = "white", alpha = 0.8, inherit.aes = FALSE) +
    geom_rect(data = rects, aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax), 
              fill = "white", alpha = 0.8, inherit.aes = FALSE) +
    geom_rug(data = dt.mean.rug, aes(x = x, y = y), sides="b", length = unit(0.03, "npc"), outside = TRUE) +
    coord_cartesian(clip = "off") +
    theme_bw() +
    labs(y = paste0(clean.label), x = "") +
    theme(legend.position = "none",
          panel.grid = element_line(color = "white"), 
          axis.text.y = element_text(size = 12), 
          axis.text.x = element_text(size = 10), 
          axis.title = element_text(size = 12), 
          axis.ticks = element_blank(), 
          panel.spacing = unit(0.5, "lines"))
  p.pd.comp
  
  statMeans <- statMeans %>% as.data.table() %>% mutate_if(is.numeric, round, digits=2)
  stats.label.rmse <- paste0("RMSE: gbm = ", statMeans[method == "gbm"]$mean_rmse, "±", statMeans[method == "gbm"]$sd_rmse, "; rf = ", 
                             statMeans[method == "rf"]$mean_rmse, "±", statMeans[method == "rf"]$sd_rmse, "; xgbTree = ",
                             statMeans[method == "xgbTree"]$mean_rmse, "±", statMeans[method == "xgbTree"]$sd_rmse, "; ensemble = ",
                             statMeans[method == "ensemble"]$mean_rmse, "±", statMeans[method == "ensemble"]$sd_rmse)
  
  stats.label.rsq <- paste0("R-squared: gbm = ", statMeans[method == "gbm"]$mean_r_sq, "±", statMeans[method == "gbm"]$sd_rmse, "; rf = ", 
                            statMeans[method == "rf"]$mean_r_sq, "±", statMeans[method == "rf"]$sd_r_sq, "; xgbTree = ",
                            statMeans[method == "xgbTree"]$mean_r_sq, "±", statMeans[method == "xgbTree"]$sd_r_sq, "; ensemble = ",
                            statMeans[method == "ensemble"]$mean_r_sq, "±", statMeans[method == "ensemble"]$sd_r_sq)
  
  
  p.comp <- grid.arrange(p.pd.comp, p.var.comp, ncol = 2, widths = c(1.3, 1), 
                         top = textGrob(paste0("Method comparison: ", tier_label),gp=gpar(fontsize=14)), 
                         bottom = textGrob(paste0("Best method: ", best_method, "\n", stats.label.rmse, "\n", stats.label.rsq),gp=gpar(fontsize=10, fontface = "italic")))
  
  filename.comp <- paste0("builds/plots/september/subSaharanPas/comparison_plot_", response_tier, ".png")
  ggsave(plot = p.comp, filename = filename.comp, dpi = 600, height = 5.1, width = 13)
  
  
  ##############################################################################            
  ################################ BEST METHOD #################################            
  ##############################################################################  
  
  varImpMeansPlot <- varImpMeans[varImpMeans$method %in% c(best_method), ]
  
  p.var <- ggplot(data = varImpMeansPlot) +
    geom_col(aes(y = clean_term, x = mean_rel_imp, fill = clean_term),
             position = position_dodge(width = 0.8), alpha = 0.9) +
    geom_errorbar(aes(y = clean_term, 
                      xmin = mean_rel_imp - sd_rel_imp, 
                      xmax = mean_rel_imp + sd_rel_imp,
                      group = method),
                  position = position_dodge(width = 0.8), width = 0.25) +
    scale_fill_manual(values = palette) +
    labs(y = "", x = "Mean Relative Importance") +
    theme_classic() +
    theme(legend.position = "none",
          axis.text = element_text(size = 12))
  
  p.var
  
  
  
  dt.gbm.P <- dt.gbm %>% dplyr::select(-all_of(response))
  
  
  if(best_method != "ensemble"){
    for(p in 1:ncol(dt.gbm.P)){
      predTmp <- partial(listFit[[best_method]], p, train = dt.gbm.P)
      term_name <- colnames(predTmp)[1]
      colnames(predTmp) <- c("x", "y")
      predTmp <- predTmp %>% mutate(term = paste0(term_name),
                                    method = best_method)
      if(p==1){
        marg <- predTmp}else{
          marg <- rbind(marg, predTmp)}
    }
  }else{
    for(p in 1:ncol(dt.gbm.P)){
      predTmp <- partial(greedyEnsemble, p, train = dt.gbm.P)
      term_name <- colnames(predTmp)[1]
      colnames(predTmp) <- c("x", "y")
      predTmp <- predTmp %>% mutate(term = paste0(term_name),
                                    method = best_method)
      if(p==1){
        marg <- predTmp}else{
          marg <- rbind(marg, predTmp)}
      
    } 
    
  }
  
  marg.plot <- marg %>% 
    mutate(
      term = gsub("`", "", term)) %>% 
    left_join(varImpMeans)
  
  marg.plot <- marg.plot[marg.plot$method %in% c(best_method), ] 
  
  marg.plot <- marg.plot[!grepl("spatial_predictor", marg.plot$term),]
  
  
  dt.points <- dt.gbm %>% pivot_longer(
    cols = c(-{{response}}), 
    values_to = "x", names_to = "term") %>% 
    mutate(y = median(marg.plot$y, na.rm = T)) %>% 
    left_join(varImpMeans[varImpMeans$method %in% c(best_method), ]) 
  
  dt.points <- dt.points[!grepl("spatial_predictor", dt.points$term),]
  
  dt.mean.rug <- dt.points %>% dplyr::select(term, x, y) %>% left_join(dt.names)
  
  rects <- dt.mean.rug %>%
    group_by(term) %>%
    mutate(
      lower_quantile_x = quantile(x, 0.05),
      upper_quantile_x = quantile(x, 0.95),
    ) %>%
    ungroup() %>% 
    group_by(term) %>%
    summarize(
      ymin = -Inf,
      ymax = Inf,
      xmin1 = -Inf,
      xmax1 = first(lower_quantile_x),
      xmin2 = first(upper_quantile_x),
      xmax2 = Inf
    ) %>%
    ungroup() %>% left_join(dt.names)
  
  predsBtPlot <- predsBt[predsBt$method %in% c(best_method), ] %>% left_join(dt.names)
  
  cleanTerm <- "Mean body mass (kg; cwm)"   
  for(cleanTerm in unique(rects$clean_term)){
    
    upperLim <- rects[rects$clean_term == cleanTerm, ]$xmin2
    lowerLim <- rects[rects$clean_term == cleanTerm, ]$xmax1
    
    margPlotSub <- marg.plot[marg.plot$clean_term == cleanTerm,] %>% filter(x > lowerLim & x < upperLim)
    marg.plot <- marg.plot %>% 
      filter(!clean_term == cleanTerm) %>% 
      rbind(margPlotSub)
    
    
    margPlotSub <- marg.plot[marg.plot$clean_term == cleanTerm,] %>% filter(x > lowerLim & x < upperLim)
    marg.plot <- marg.plot %>% 
      filter(!clean_term == cleanTerm) %>% 
      rbind(margPlotSub)
    
    predsBtPlotSub <- predsBtPlot[predsBtPlot$clean_term == cleanTerm,] %>% filter(x > lowerLim & x < upperLim)
    predsBtPlot <- predsBtPlot %>% 
      filter(!clean_term == cleanTerm) %>% 
      rbind(predsBtPlotSub)
    
    dtRugSub <- dt.mean.rug[dt.mean.rug$clean_term == cleanTerm,] %>% filter(x > lowerLim & x < upperLim)
    dt.mean.rug <- dt.mean.rug %>% 
      filter(!clean_term == cleanTerm) %>% 
      rbind(dtRugSub)
    
    
  }
  
  marg.plot$clean_term <- factor(marg.plot$clean_term, levels = ordered_terms_pred)
  
  clean.label <- case_when(
    .default = response, 
    response == "tree_cover_mean" ~ "Current woody cover (%)", 
    response == "woody_cover_trend_venter2019" ~ "Woody cover change (%/year)",
    response == "tree_cover_sd_100" ~ "Woody cover heterogeneity", 
    response == "woody_trend_venter2019" ~ "Woody cover change (%/year)",
    response == "canopy_height_sd_100" ~ "Canopy height heterogeneity")
  
  p.pd.final <- ggplot()+
    geom_line(data = marg.plot, aes(x=x, y=y, color = clean_term), linewidth = 1.1) +
    geom_line(data = predsBtPlot, aes(x=x, y=y, group = iteration, color = clean_term), alpha = 0.15, linewidth = 0.5, color = "grey") +
    geom_line(data = marg.plot, aes(x=x, y=y, color = clean_term), linewidth = 1.1) +
    facet_wrap(~factor(clean_term),
               scales="free_x", ncol = 4) +
    scale_x_continuous(breaks = extended_breaks(n = 3)) +
    scale_color_manual(values = palette) +
    # geom_rect(data = rects, aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax), 
    #           fill = "white", alpha = 0.6, inherit.aes = FALSE) +
    # geom_rect(data = rects, aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax), 
    #           fill = "white", alpha = 0.6, inherit.aes = FALSE) +
    geom_rug(data = dt.mean.rug, aes(x = x, y = y), sides="b", length = unit(0.03, "npc"), outside = TRUE) +
    coord_cartesian(clip = "off") +
    ylim(min(marg.plot$y, na.rm =T), max(marg.plot$y, na.rm = T)) +
    theme_classic() +
    labs(y = paste0(clean.label), x = "") +
    theme(legend.position = "none",
          panel.grid = element_line(color = "white"), 
          axis.text.y = element_text(size = 12), 
          axis.text.x = element_text(size = 10), 
          axis.title = element_text(size = 12), 
          axis.ticks = element_blank(), 
          panel.spacing = unit(0.6, "lines"), 
          strip.background = element_rect(color = "white", fill= "grey95") 
    )
  p.pd.final
  
  statMeansBest <- statMeans[statMeans$method %in% c(best_method), ]
  
  p.comb <- grid.arrange(p.pd.final, p.var, ncol = 2, widths = c(1.5, 1), 
                         top = textGrob(paste0(clean.label, "\n",
                                               tier_label, " (n = ", dt.tier[i, ]$n, "; R-sq = ", round(statMeansBest$mean_r_sq, 2),
                                               "±", round(statMeansBest$sd_r_sq, 2),  "; RMSE = ",
                                               round(statMeansBest$mean_rmse, 2), "±", round(statMeansBest$sd_rmse, 2),")") ,gp=gpar(fontsize=14)), 
                         bottom = textGrob(paste0("Best method: ", best_method),gp=gpar(fontsize=10, fontface = "italic")))
  
  
  filename <- paste0("builds/plots/september/subSaharanPas/comb_plot_", response_tier, ".png")

    ggsave(plot = p.comb, filename = filename, dpi = 600, height = 5.1, width = 13)

}


toc()
print("done")




