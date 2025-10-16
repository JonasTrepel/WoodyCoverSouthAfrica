library(tidyverse)
library(data.table)
library(sf)
library(tidylog)
library(GGally)
library(scales)
library(gridExtra)
library(gstat)
library(grid)
library(gbm)
library("caretEnsemble")
library(caret)
library(tictoc)
library(MetBrewer)
library(furrr)
library(future)


n_workers <- 8

dt <- fread("data/clean_data/final_reserve_data.csv") %>% 
  filter(complete.cases(across(
    c(woody_cover_change, venter_woody_cover_trend, woody_cover_sd_ha_coef, woody_cover_sd_km_coef, 
      mat_coef, prec_coef, n_deposition, 
      CW_mean_species_body_mass, herbi_fun_div_distq1, n_herbi_sp_reserve,
      grazer_biomass_ha, browser_biomass_ha, 
      herbi_biomass_ha, fire_frequency, burned_area_coef)
  )))


### create model data ---------------------------------

##### SUBSET #####

guide_subset <- "tier %in% c('main', 'high_biomass')"

#guide_subset <- NULL
##############################################################################            
################################## CREATE MODEL GUIDE ########################         
##############################################################################    



subsets <- c("!is.na(unique_id)", 
             "herbi_biomass_kgkm2 >= 7500", 
             "establishment_year <= 2009"," area_ha >= 2400", 
             "Biome == 'Savanna'", "Biome == 'Albany Thicket'",
             "Biome == 'Grassland'", "Biome == 'Fynbos'", 
             "Biome == 'Nama-Karoo'")

tier_labels<- c("Full model", "Reserves with high herbivore biomass (≥7500 kg/km²)", 
                "Reserves established before 2010", "Reserves larger than 24 km2", 
                "Savanna", "Albany thicket", "Grassland", "Fynbos", "Nama Karoo")

tiers <- c("main","high_biomass", "old", "large", 
           "savanna", "albany_thicket", "grassland", "fynbos", "nama_karoo")

responses <- c("woody_cover_change", "venter_woody_cover_trend", "woody_cover_sd_ha_coef", "woody_cover_sd_km_coef")

dt_tier_raw <- data.table(
  subset = subsets, 
  tier = tiers, 
  tier_label = tier_labels, 
  n = 0)

tier_resp <- CJ(response = responses, tier = tiers) %>% 
  mutate(terms = case_when(
    response == "woody_cover_change" ~ "'mat_coef', 'prec_coef', 'mean_prec', 'n_deposition', 'CW_mean_species_body_mass', 'herbi_fun_div_distq1', 'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 'browser_biomass_kgkm2', 'herbi_biomass_kgkm2', 'fire_frequency', 'burned_area_coef'",
    response == "venter_woody_cover_trend" ~ "'mat_coef', 'prec_coef', 'mean_prec', 'n_deposition', 'CW_mean_species_body_mass', 'herbi_fun_div_distq1', 'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 'browser_biomass_kgkm2', 'herbi_biomass_kgkm2', 'fire_frequency', 'burned_area_coef'",
    response == "woody_cover_sd_ha_coef" ~ "'mat_coef', 'prec_coef', 'mean_prec', 'n_deposition', 'CW_mean_species_body_mass', 'herbi_fun_div_distq1', 'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 'browser_biomass_kgkm2', 'herbi_biomass_kgkm2', 'fire_frequency', 'burned_area_coef'",
    response == "woody_cover_sd_km_coef" ~ "'mat_coef', 'prec_coef', 'mean_prec', 'n_deposition', 'CW_mean_species_body_mass', 'herbi_fun_div_distq1', 'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 'browser_biomass_kgkm2', 'herbi_biomass_kgkm2', 'fire_frequency', 'burned_area_coef'"
  )) %>% mutate(
    response_tier = paste0(response, "_", tier))

dt_tier <- dt_tier_raw %>% left_join(tier_resp)

for(i in 1:nrow(dt_tier)){
  subset <- dt_tier[i, ]$subset
  dt_sub <- dt %>% dplyr::filter(eval(parse(text = subset)))
  
  nr <- nrow(dt_sub)
  dt_tier[i, ]$n <- nr
  
}
unique(dt_tier$n)


if(!is.null(guide_subset)){
  dt_tier <- dt_tier %>% filter(eval(parse(text = guide_subset)))
}


dt_tier <- dt_tier[!dt_tier$n < 120]


##############################################################################            
################################## DEFINE STUFF ############################            
##############################################################################    


c(met.brewer(name = "Egypt"))
c(met.brewer(name = "Archambault", n = 10))

palette_groups <- c("Herbivory" = "#4E2A67", "Global Change" = "#C77D83", "Fire" =  "#AB3329", "Rainfall" = "#88A0DC")

palette_methods <- c("gbm" = "#dd5129", "rf" = "#0f7ba2", "xgbTree" = "#43b284", "ensemble" = "#fab255")

dt_names <- data.table(
  term = c("chelsa_mat", "mean_prec",
           "mat_coef", "prec_coef", 
           "CW_mean_species_body_mass", "herbi_fun_div_distq1", "n_herbi_sp_reserve", 
           "grazer_biomass_kgkm2", "browser_biomass_kgkm2", "mixed_feeder_biomass_kgkm2", 
           "herbi_biomass_kgkm2", "fire_frequency", "burned_area_coef", "n_deposition", "elephant_biomass_kgkm2"),
  clean_term = c("MAT (°C)", "MAP (mm)",
                 "Temperature Change", "Precipitation Change",
                 "Mean body mass (kg; cwm)", "Herbivore functional diversity", 
                 "Herbivore species richness", "Grazer biomass (kg/km²)", 
                 "Browser biomass (kg/km²)", "Mixed feeder biomass (kg/km²)",
                 "Herbivore biomass (kg/km²)", "Fire frequency", "Burned area trend", 
                 "N deposition ([kg/km²]/year)", "Elephant biomass (kg/km²)")) %>% 
  mutate(
    response_group = case_when(
      term %in% c("CW_mean_species_body_mass", "herbi_fun_div_distq1", "n_herbi_sp_reserve", 
                  "grazer_biomass_kgkm2", "browser_biomass_kgkm2", "mixed_feeder_biomass_kgkm2", 
                  "herbi_biomass_kgkm2", "elephant_biomass_kgkm2") ~ "Herbivory", 
      term %in% c("fire_frequency", "burned_area_coef") ~ "Fire", 
      term %in% c("n_deposition", "mat_coef", "prec_coef") ~ "Global Change", 
      term %in% c("mean_prec") ~ "Rainfall"
    )
  )


### create Tune Grids 

### GBM 
tune_grid_gbm <- expand.grid(
  shrinkage = c( .001, .005, 0.01),
  interaction.depth = c(1,2,3),
  n.minobsinnode = c(5, 10, 15), 
  n.trees = seq(1000, 10000, 250)
)

### Random forest 
tune_grid_rf <- expand.grid(
  mtry = c(1:10))

### XGBoost

tune_grid_xgbtree <- expand.grid(
  nrounds = seq(50, 200, 50), 
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,
  max_depth = c(1, 3, 5, 7),
  min_child_weight = c(5, 10, 15), 
  colsample_bytree = 0.9, 
  subsample = 0.8
)

### Fit Control 

fit_control <- trainControl(## 10 fold cross validation
  method = "repeatedcv",
  number = 5, #number of splits 
  repeats = 5, #repeat 10 times
  savePredictions = "final", #keep final model predictions, would otherwise be dumped
  returnResamp = "final")


var_imp_storage <- data.table()
preds_storage <- data.table()
marg_storage <- data.table()
dt_rug_storage <- data.table()
stats_storage <- data.table()

##############################################################################            
################################## LOOOOOOOOOOOOP ############################            
##############################################################################    
options(future.globals.maxSize = 10 * 1024^3)  # 10 GB
plan(multisession, workers = n_workers)

tic()

ml_res_list <- future_map(1:nrow(dt_tier),
            .progress = TRUE,
            .options = furrr_options(seed = TRUE),
            function(i) {
              #for(i in 1:nrow(dt_tier)){
              
              tier <- dt_tier[i, ]$tier
              subset <- dt_tier[i, ]$subset
              tier_label <- dt_tier[i, ]$tier_label
              response <- dt_tier[i, ]$response
              response_tier <- dt_tier[i, ]$response_tier
              
              terms <- dt_tier[i, ]$terms
              
              terms_vector <- strsplit(gsub("'", "", terms), ", ")[[1]]
              
              
              dt_sub <- dt %>% dplyr::filter(eval(parse(text = subset))) #%>% sample_n(100)
              
              print(paste0("starting ", response,"; tier: ", tier, " (", i, "/", nrow(dt_tier), ")"))
              
              dt_gbm <- dt_sub %>% 
                dplyr::select(all_of(response), all_of(terms_vector)) %>% dplyr::filter(complete.cases(.)) # %>% sample_n(30)
              
              ##############################################################################            
              ################################## RUN ALL MODELS ############################            
              ##############################################################################            
              
              formula <- as.formula(paste(response, "~ ."))
              
              list_fit <- caretList(formula,
                                    data = dt_gbm, 
                                    # methodList = c("gbm", "rf", "xgbTree"),
                                    trControl = fit_control,
                                    verbose = FALSE,
                                    tuneList=list(
                                      gbm=caretModelSpec(method="gbm", tuneGrid=tune_grid_gbm),
                                      rf=caretModelSpec(method="rf", tuneGrid=tune_grid_rf),
                                      xgbTree=caretModelSpec(method="xgbTree", tuneGrid = tune_grid_xgbtree) ))
              
              
              greedy_ensemble <- caretEnsemble(
                list_fit,
                metric="RMSE",
                trControl = trainControl(method = "repeatedcv", number = 5, repeats = 5))
              
              
              
              ### define optimal parameters
              
              ### GBM 
              tmp_gbm <- list_fit$gbm$results %>% slice_min(RMSE) %>%
                mutate(tier = tier, n = nrow(dt_gbm), method = "gbm")
              
              tune_grid_gbm_opt <- tmp_gbm %>% dplyr::select(shrinkage, interaction.depth, n.minobsinnode, n.trees)
              
              
              ### RF 
              
              tmp_rf <- list_fit$rf$results %>% slice_min(RMSE) %>%
                mutate(tier = tier, n = nrow(dt_gbm), method = "rf")
              
              tune_grid_rf_opt <- tmp_rf %>% dplyr::select(mtry)
              
              ### xGBTREE
              
              tmp_xgb <- list_fit$xgbTree$results %>% slice_min(RMSE) %>%
                mutate(tier = tier, n = nrow(dt_gbm), method = "xgbTree")
              
              tune_grid_xgbtree_opt <- tmp_xgb %>% dplyr::select(eta, max_depth, gamma, colsample_bytree, min_child_weight, subsample, nrounds)
              
              
              ##############################################################################            
              ################################## Bootstrap #################################            
              ##############################################################################  
              
              
              stats_bt <- data.table()
              var_imp_bt <- data.table()
              preds_bt <- data.table()
              
              for(number in 1:100){
                
                index <- createDataPartition(dt_gbm[[response]], p=0.8, list = F) 
                
                train <- dt_gbm[index,]
                test <- dt_gbm[!index,]
                
                
                list_fit_bt <- caretList(formula,
                                         data = train, 
                                         trControl = fit_control,
                                         verbose = FALSE,
                                         tuneList=list(
                                           gbm=caretModelSpec(method="gbm", tuneGrid=tune_grid_gbm_opt),
                                           rf=caretModelSpec(method="rf", tuneGrid=tune_grid_rf_opt),
                                           xgbTree=caretModelSpec(method="xgbTree", tuneGrid = tune_grid_xgbtree_opt) ))
                
                #get different models ----
                
                #ensemble 
                
                greedy_ensemble_bt <- caretEnsemble(list_fit_bt,
                                                    metric="RMSE",
                                                    trControl = trainControl(method = "repeatedcv", number = 5, repeats = 5))
                
                pr_ensemble <- caret::postResample(pred = predict(greedy_ensemble_bt, newdata = test),
                                                   obs = test[[response]])
                
                #Gbm
                gbm_bt <- list_fit_bt$gbm
                
                pr_gbm <- caret::postResample(pred = predict(gbm_bt, newdata = test),
                                              obs = test[[response]])
                
                #Rf
                rf_bt <- list_fit_bt$rf
                
                pr_rf <- caret::postResample(pred = predict(rf_bt, newdata = test),
                                             obs = test[[response]])
                
                #xgbTree
                xgbtree_bt <- list_fit_bt$xgbTree
                
                pr_xgbtree <- caret::postResample(pred = predict(xgbtree_bt, newdata = test),
                                                  obs = test[[response]])
                
                
                ### get stats ----
                
                # Create separate data tables for each method
                if(is.null(greedy_ensemble_bt)){pr_ensemble <- NA}
                ensemble_stats_tmp <- data.table(
                  rmse_bt = pr_ensemble[1],
                  r_sq_bt = pr_ensemble[2],
                  mae_bt = pr_ensemble[3],
                  method = "ensemble",
                  iteration = paste0("it_", number)
                )
                
                gbm_stats_tmp <- data.table(
                  rmse_bt = pr_gbm[1],
                  r_sq_bt = pr_gbm[2],
                  mae_bt = pr_gbm[3],
                  method = "gbm",
                  iteration = paste0("it_", number)
                )
                
                rf_stats_tmp <- data.table(
                  rmse_bt = pr_rf[1],
                  r_sq_bt = pr_rf[2],
                  mae_bt = pr_rf[3],
                  method = "rf",
                  iteration = paste0("it_", number)
                )
                
                xgbtree_stats_tmp <- data.table(
                  rmse_bt = pr_xgbtree[1],
                  r_sq_bt = pr_xgbtree[2],
                  mae_bt = pr_xgbtree[3],
                  method = "xgbTree",
                  iteration = paste0("it_", number)
                )
                
                
                tmp_stats <- rbind(xgbtree_stats_tmp, rf_stats_tmp, gbm_stats_tmp, ensemble_stats_tmp)
                
                
                
                stats_bt <- rbind(stats_bt, tmp_stats)
                
                #### get variable importances ------
                ensemble_weights_bt <- as.data.frame(varImp(greedy_ensemble_bt)) %>%
                  rownames_to_column(var = "term")
                
                old_names <- names(ensemble_weights_bt)
                ensemble_weights_bt <- setnames(ensemble_weights_bt, old = old_names, 
                                                new = c("term", "weight"))
                
                
                gbm_imp_bt_raw <- varImp(list_fit_bt$gbm, scale = TRUE)
                gbm_imp_bt <- gbm_imp_bt_raw$importance %>%
                  as.data.frame() %>% 
                  rownames_to_column(var = "term") %>% 
                  rename(gbm = Overall) %>% 
                  mutate(method = "gbm")
                
                rf_imp_bt_raw <- varImp(list_fit_bt$rf, scale = TRUE)
                rf_imp_bt <-rf_imp_bt_raw$importance %>%
                  as.data.frame() %>% 
                  rownames_to_column(var = "term") %>% 
                  rename(rf = Overall) %>% 
                  mutate(method = "rf")
                
                
                xgbtree_imp_bt_raw <- varImp(list_fit_bt$xgbTree, scale = TRUE)
                xgbtree_imp_bt <-xgbtree_imp_bt_raw$importance %>%
                  as.data.frame() %>% 
                  rownames_to_column(var = "term") %>% 
                  rename(xgbTree = Overall) %>% 
                  mutate(method = "xgbTree")
                
                
                var_imp_tmp <- gbm_imp_bt[, c("term", "gbm")] %>% 
                  left_join(rf_imp_bt[, c("term", "rf")]) %>% 
                  left_join(xgbtree_imp_bt[, c("term", "xgbTree")]) %>% 
                  mutate(ensemble = gbm*ensemble_weights_bt[ensemble_weights_bt$term == "gbm",]$weight +
                           rf*ensemble_weights_bt[ensemble_weights_bt$term == "rf",]$weight +
                           xgbTree*ensemble_weights_bt[ensemble_weights_bt$term == "xgbTree",]$weight, 
                         iteration = paste0("it_", number))
                
                var_imp_bt <- rbind(var_imp_bt, var_imp_tmp)
                
                #### get partial dependence plot predictions ------
                train_p <- train %>% dplyr::select(-all_of(response))
                
                library(pdp)
                
                ### GBM 
                for(j in 1:ncol(train_p)){
                  pred_tmp_gbm <- partial(list_fit_bt$gbm, j, train = train_p)
                  term_name <- colnames(pred_tmp_gbm)[1]
                  colnames(pred_tmp_gbm) <- c("x", "y")
                  pred_tmp_gbm <- pred_tmp_gbm %>% mutate(term = paste0(term_name),
                                                          iteration = paste0("it_", number), 
                                                          method = "gbm")
                  if(j==1){
                    marg_gbm <- pred_tmp_gbm}else{
                      marg_gbm <- rbind(marg_gbm, pred_tmp_gbm)}
                }
                
                #newData <- train %>% dplyr::select(-all_of(response)) 
                
                
                ### RF
                for(k in 1:ncol(train_p)){
                  pred_tmp_rf <- partial(list_fit_bt$rf, k, train = train_p)
                  term_name <- colnames(pred_tmp_rf)[1]
                  colnames(pred_tmp_rf) <- c("x", "y")
                  pred_tmp_rf <- pred_tmp_rf %>% mutate(term = paste0(term_name),
                                                        iteration = paste0("it_", number),
                                                        method = "rf")
                  if(k==1){
                    marg_rf <- pred_tmp_rf}else{
                      marg_rf <- rbind(marg_rf, pred_tmp_rf)}
                }
                
                ### XgbTree 
                for(l in 1:ncol(train_p)){
                  pred_tmp_xgbtree <- partial(list_fit_bt$xgbTree, l, train = train_p)
                  term_name <- colnames(pred_tmp_xgbtree)[1]
                  colnames(pred_tmp_xgbtree) <- c("x", "y")
                  pred_tmp_xgbtree <- pred_tmp_xgbtree %>% mutate(term = paste0(term_name),
                                                                  iteration = paste0("it_", number), 
                                                                  method = "xgbTree")
                  if(l==1){
                    marg_xgb_tree <- pred_tmp_xgbtree}else{
                      marg_xgb_tree <- rbind(marg_xgb_tree, pred_tmp_xgbtree)}
                }
                
                
                ## Ensemble 
                
                for(o in 1:ncol(train_p)){
                  pred_tmp_ensemble <- partial(greedy_ensemble_bt, o, train = train_p)
                  term_name <- colnames(pred_tmp_ensemble)[1]
                  colnames(pred_tmp_ensemble) <- c("x", "y")
                  pred_tmp_ensemble <- pred_tmp_ensemble %>% mutate(term = paste0(term_name),
                                                                    iteration = paste0("it_", number), 
                                                                    method = "ensemble")
                  if(o==1){
                    marg_ensemble <- pred_tmp_ensemble}else{
                      marg_ensemble <- rbind(marg_ensemble, pred_tmp_ensemble)}
                }
                
                
                tmp_preds <- rbind(marg_gbm, marg_rf, marg_xgb_tree, marg_ensemble) 
                
                tmp_preds <- tmp_preds[!grepl("spatial_pred", tmp_preds$term), ]
                
                preds_bt <- rbind(tmp_preds, preds_bt)
                
                print(paste0(number,"/",100))
                
                
              }
              
              var_imp_bt
              preds_bt
              stats_bt
              
              ##############################################################################            
              ################################## SUMMARIZE #################################            
              ##############################################################################  
              
              var_imp_means <- var_imp_bt %>% 
                pivot_longer(cols = c(ensemble, gbm, rf, xgbTree), names_to = "method", values_to = "rel_imp") %>%
                group_by(method, term) %>% 
                summarize(
                  mean_rel_imp = mean(rel_imp), 
                  sd_rel_imp = sd(rel_imp)
                ) %>% left_join(dt_names)
              
              var_imp_means <- var_imp_means[!grepl("spatial_predictor", var_imp_means$term),]
              
              stat_means <- stats_bt %>% 
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
              
              stats_storage <- rbind(stats_storage, stat_means)
              
              
              pred_means <- preds_bt %>% 
                group_by(method, term, x) %>% 
                summarize(mean_y = median(y),
                          sd_y = sd(y)
                )  %>% left_join(dt_names)
              
              pred_means <- pred_means[!grepl("spatial_predictor", pred_means$term),]
              
              
              best_method <- stat_means %>% slice_min(mean_rmse) %>% dplyr::select(method) %>% pull() 
              
              
              ##############################################################################            
              ############################ COMPARISON PLOT #################################            
              ############################################################################## 
              
              ## get terms in order
              ordered_terms <- var_imp_means[var_imp_means$method %in% c(best_method), ]
              ordered_terms <- ordered_terms %>%
                arrange((mean_rel_imp)) %>%
                pull(clean_term)
              
              
              var_imp_means$clean_term <- factor(var_imp_means$clean_term, levels = ordered_terms)
              
              
              ## get names in order: 
              var_imp_means[!is.na(var_imp_means$clean_term), ]
              
              clean_label <- case_when(
                .default = response, 
                response == "woody_cover_change" ~ "Woody cover change (%/year)",
                response == "venter_woody_cover_trend" ~ "Venter woody cover change (%/year)",
                response == "woody_cover_sd_ha_coef" ~ "Woody cover heterogeneity change (ha-scale)", 
                response == "woody_cover_sd_km_coef" ~ "Woody cover heterogeneity change (km²-scale)")
              
              p_var_comp <- ggplot(data = var_imp_means) +
                geom_col(aes(y = clean_term, x = mean_rel_imp, fill = method, group = method),
                         position = position_dodge(width = 0.8), alpha = 0.9) +
                geom_errorbar(aes(y = clean_term, 
                                  xmin = mean_rel_imp - sd_rel_imp, 
                                  xmax = mean_rel_imp + sd_rel_imp,
                                  group = method),
                              position = position_dodge(width = 0.8), width = 0.25) +
                scale_fill_manual(values = palette_methods) +
                labs(y = "", x = "Mean Relative Importance") +
                theme_classic() + 
                theme(axis.text = element_text(size = 11))
              
              p_var_comp
              
              ### pred plot comp 
              ordered_terms_pred <- var_imp_means[var_imp_means$method %in% c(best_method), ]
              
              ordered_terms_pred <- ordered_terms_pred %>%
                arrange(desc(mean_rel_imp)) %>%
                pull(clean_term)
              
              pred_means$clean_term <- factor(pred_means$clean_term, levels = ordered_terms_pred)
              
              it <- unique(preds_bt$iteration)
              it <- sample(it, 5)
              pred_means_plot <- preds_bt[preds_bt$iteration %in% c(it), ] %>% left_join(dt_names)
              
              
              
              
              ######## Make it more complicated and plot the best model #####
              dt_gbm_p <- dt_gbm %>% dplyr::select(-all_of(response))
              
              ### GBM 
              for(j in 1:ncol(dt_gbm_p)){
                pred_tmp_gbm <- partial(list_fit$gbm, j, train = dt_gbm_p)
                term_name <- colnames(pred_tmp_gbm)[1]
                colnames(pred_tmp_gbm) <- c("x", "y")
                pred_tmp_gbm <- pred_tmp_gbm %>% mutate(term = paste0(term_name),
                                                        iteration = paste0("it_", number), 
                                                        method = "gbm")
                if(j==1){
                  marg_gbm <- pred_tmp_gbm}else{
                    marg_gbm <- rbind(marg_gbm, pred_tmp_gbm)}
              }
              
              
              ### RF
              for(k in 1:ncol(dt_gbm_p)){
                pred_tmp_rf <- partial(list_fit$rf, k, train = dt_gbm_p)
                term_name <- colnames(pred_tmp_rf)[1]
                colnames(pred_tmp_rf) <- c("x", "y")
                pred_tmp_rf <- pred_tmp_rf %>% mutate(term = paste0(term_name),
                                                      iteration = paste0("it_", number),
                                                      method = "rf")
                if(k==1){
                  marg_rf <- pred_tmp_rf}else{
                    marg_rf <- rbind(marg_rf, pred_tmp_rf)}
              }
              
              ### XgbTree 
              for(l in 1:ncol(dt_gbm_p)){
                pred_tmp_xgbtree <- partial(list_fit$xgbTree, l, train = dt_gbm_p)
                term_name <- colnames(pred_tmp_xgbtree)[1]
                colnames(pred_tmp_xgbtree) <- c("x", "y")
                pred_tmp_xgbtree <- pred_tmp_xgbtree %>% mutate(term = paste0(term_name),
                                                                iteration = paste0("it_", number), 
                                                                method = "xgbTree")
                if(l==1){
                  marg_xgb_tree <- pred_tmp_xgbtree}else{
                    marg_xgb_tree <- rbind(marg_xgb_tree, pred_tmp_xgbtree)}
              }
              
              
              ## Ensemble 
              
              for(o in 1:ncol(train_p)){
                pred_tmp_ensemble <- partial(greedy_ensemble, o, train = dt_gbm_p)
                term_name <- colnames(pred_tmp_ensemble)[1]
                colnames(pred_tmp_ensemble) <- c("x", "y")
                pred_tmp_ensemble <- pred_tmp_ensemble %>% mutate(term = paste0(term_name),
                                                                  iteration = paste0("it_", number), 
                                                                  method = "ensemble")
                if(o==1){
                  marg_ensemble <- pred_tmp_ensemble}else{
                    marg_ensemble <- rbind(marg_ensemble, pred_tmp_ensemble)}
              }
              
              
              pred_means_plot <- rbind(marg_gbm, marg_rf, marg_xgb_tree, marg_ensemble) %>% 
                left_join(var_imp_means) %>% 
                left_join(dt_names)
              
              pred_means_plot$clean_term <- factor(pred_means_plot$clean_term, levels = ordered_terms_pred)
              pred_means_plot <- pred_means_plot[!grepl("spatial_predictor", pred_means_plot$term),]
              
              ### PREPARE PLOT STUFF ####
              
              dt_points <- dt_gbm %>% pivot_longer(
                cols = c(-{{response}}), 
                values_to = "x", names_to = "term") %>% 
                mutate(y = median(pred_means_plot$y, na.rm = T)) %>% 
                left_join(var_imp_means[var_imp_means$method %in% c(best_method), ]) 
              
              dt_points <- dt_points[!grepl("spatial_predictor", dt_points$term),]
              
              
              dt_mean_rug <- dt_points %>% dplyr::select(term, x, y) %>% left_join(dt_names)
              
              
              rects <- dt_mean_rug %>%
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
                ungroup() %>% left_join(dt_names)
              
              
              
              p_pd_comp <- ggplot()+
                geom_line(data = pred_means_plot[pred_means_plot$method == "gbm", ], aes(x=x, y=y, group = iteration, color = as.factor(method)), linewidth = 1.1, alpha = 0.9) +
                geom_line(data = pred_means_plot[pred_means_plot$method == "rf", ], aes(x=x, y=y, group = iteration, color = as.factor(method)), linewidth = 1.1, alpha = 0.9) +
                geom_line(data = pred_means_plot[pred_means_plot$method == "xgbTree", ], aes(x=x, y=y, group = iteration, color = as.factor(method)), linewidth = 1.1, alpha = 0.9) +
                geom_line(data = pred_means_plot[pred_means_plot$method == "ensemble", ], aes(x=x, y=y, group = iteration, color = as.factor(method)), linewidth = 1.1, alpha = 0.9) +
                facet_wrap(~factor(clean_term),
                           scales="free_x", ncol = 4) +
                scale_x_continuous(breaks = extended_breaks(n = 3)) +
                scale_color_manual(values = palette_methods) +
                geom_rect(data = rects, aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax), 
                          fill = "white", alpha = 0.8, inherit.aes = FALSE) +
                geom_rect(data = rects, aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax), 
                          fill = "white", alpha = 0.8, inherit.aes = FALSE) +
                geom_rug(data = dt_mean_rug, aes(x = x, y = y), sides="b", length = unit(0.03, "npc"), outside = TRUE) +
                coord_cartesian(clip = "off") +
                theme_bw() +
                labs(y = paste0(clean_label), x = "") +
                theme(legend.position = "none",
                      panel.grid = element_line(color = "white"), 
                      axis.text.y = element_text(size = 12), 
                      axis.text.x = element_text(size = 10), 
                      axis.title = element_text(size = 12), 
                      axis.ticks = element_blank(), 
                      panel.spacing = unit(0.5, "lines"))
              p_pd_comp
              
              stat_means <- stat_means %>% as.data.table() %>% mutate_if(is.numeric, round, digits=2)
              stats_label_rmse <- paste0("RMSE: gbm = ", stat_means[method == "gbm"]$mean_rmse, "±", stat_means[method == "gbm"]$sd_rmse, "; rf = ", 
                                         stat_means[method == "rf"]$mean_rmse, "±", stat_means[method == "rf"]$sd_rmse, "; xgbTree = ",
                                         stat_means[method == "xgbTree"]$mean_rmse, "±", stat_means[method == "xgbTree"]$sd_rmse, "; ensemble = ",
                                         stat_means[method == "ensemble"]$mean_rmse, "±", stat_means[method == "ensemble"]$sd_rmse)
              
              stats_label_rsq <- paste0("R-squared: gbm = ", stat_means[method == "gbm"]$mean_r_sq, "±", stat_means[method == "gbm"]$sd_rmse, "; rf = ", 
                                        stat_means[method == "rf"]$mean_r_sq, "±", stat_means[method == "rf"]$sd_r_sq, "; xgbTree = ",
                                        stat_means[method == "xgbTree"]$mean_r_sq, "±", stat_means[method == "xgbTree"]$sd_r_sq, "; ensemble = ",
                                        stat_means[method == "ensemble"]$mean_r_sq, "±", stat_means[method == "ensemble"]$sd_r_sq)
              
              p_comp <- grid.arrange(p_pd_comp, p_var_comp, ncol = 2, widths = c(1.3, 1), 
                                     top = textGrob(paste0("Method comparison: ", tier_label),gp=gpar(fontsize=14)), 
                                     bottom = textGrob(paste0("Best method: ", best_method, "\n", stats_label_rmse, "\n", stats_label_rsq),gp=gpar(fontsize=10, fontface = "italic")))
              
              filename_comp <- paste0("builds/plots/revision/reserve_ml_results/comparison_plot_", response_tier, ".png")
              #ggsave(plot = p_comp, filename = filename_comp, dpi = 600, height = 6.75, width = 13)
              
              
              ##############################################################################            
              ################################ BEST METHOD #################################            
              ##############################################################################  
              
              var_imp_means_plot <- var_imp_means[var_imp_means$method %in% c(best_method), ]
              
              p_var <- ggplot(data = var_imp_means_plot) +
                geom_col(aes(y = clean_term, x = mean_rel_imp, fill = response_group),
                         position = position_dodge(width = 0.8), alpha = 0.9) +
                geom_errorbar(aes(y = clean_term, 
                                  xmin = mean_rel_imp - sd_rel_imp, 
                                  xmax = mean_rel_imp + sd_rel_imp,
                                  group = method),
                              position = position_dodge(width = 0.8), width = 0.25) +
                scale_fill_manual(values = palette_groups) +
                labs(y = "", x = "Mean Relative Importance", fill = "") +
                theme_classic() +
                theme(legend.position = c(.75, .15),
                      axis.text = element_text(size = 12))
              
              p_var
              
              dt_gbm_p <- dt_gbm %>% dplyr::select(-all_of(response))
              
              
              if(best_method != "ensemble"){
                for(p in 1:ncol(dt_gbm_p)){
                  pred_tmp <- partial(list_fit[[best_method]], p, train = dt_gbm_p)
                  term_name <- colnames(pred_tmp)[1]
                  colnames(pred_tmp) <- c("x", "y")
                  pred_tmp <- pred_tmp %>% mutate(term = paste0(term_name),
                                                  method = best_method)
                  if(p==1){
                    marg <- pred_tmp}else{
                      marg <- rbind(marg, pred_tmp)}
                }
              }else{
                for(p in 1:ncol(dt_gbm_p)){
                  pred_tmp <- partial(greedy_ensemble, p, train = dt_gbm_p)
                  term_name <- colnames(pred_tmp)[1]
                  colnames(pred_tmp) <- c("x", "y")
                  pred_tmp <- pred_tmp %>% mutate(term = paste0(term_name),
                                                  method = best_method)
                  if(p==1){
                    marg <- pred_tmp}else{
                      marg <- rbind(marg, pred_tmp)}
                  
                } 
                
              }
              
              
              
              marg_plot <- marg %>% 
                mutate(
                  term = gsub("`", "", term)) %>% 
                left_join(var_imp_means)
              
              marg_plot <- marg_plot[marg_plot$method %in% c(best_method), ] 
              
              marg_plot <- marg_plot[!grepl("spatial_predictor", marg_plot$term),]
              
              
              dt_points <- dt_gbm %>% pivot_longer(
                cols = c(-{{response}}), 
                values_to = "x", names_to = "term") %>% 
                mutate(y = median(marg_plot$y, na.rm = T)) %>% 
                left_join(var_imp_means[var_imp_means$method %in% c(best_method), ]) 
              
              dt_points <- dt_points[!grepl("spatial_predictor", dt_points$term),]
              
              
              
              dt_mean_rug <- dt_points %>% dplyr::select(term, x, y) %>% left_join(dt_names)
              
              
              rects <- dt_mean_rug %>%
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
                ungroup() %>% left_join(dt_names)
              
              preds_bt_plot <- preds_bt[preds_bt$method %in% c(best_method), ] %>% left_join(dt_names)
              
              #c_t <- "Mean body mass (kg; cwm)"   
              for(c_t in unique(rects$clean_term)){
                
                upper_lim <- rects[rects$clean_term == c_t, ]$xmin2
                lower_lim <- rects[rects$clean_term == c_t, ]$xmax1
                
                
                marg_plot_sub <- marg_plot[marg_plot$clean_term == c_t,] %>% filter(x > lower_lim & x < upper_lim)
                marg_plot <- marg_plot %>% 
                  filter(clean_term != c_t) %>% 
                  rbind(marg_plot_sub)
                
                preds_bt_plot_sub <- preds_bt_plot[preds_bt_plot$clean_term == c_t,] %>% filter(x > lower_lim & x < upper_lim)
                preds_bt_plot <- preds_bt_plot %>% 
                  filter(!clean_term == c_t) %>% 
                  rbind(preds_bt_plot_sub)
                
                dt_rug_plot_sub <- dt_mean_rug[dt_mean_rug$clean_term == c_t,] %>% filter(x > lower_lim & x < upper_lim)
                dt_mean_rug <- dt_mean_rug %>% 
                  filter(!clean_term == c_t) %>% 
                  rbind(dt_rug_plot_sub)
                
                
              }
              
              marg_plot$clean_term <- factor(marg_plot$clean_term, levels = ordered_terms_pred)
              
              clean_label <- case_when(
                .default = response, 
                response == "woody_cover_change" ~ "Woody cover change (%/year)",
                response == "venter_woody_cover_trend" ~ "Venter woody cover change (%/year)",
                response == "woody_cover_sd_ha_coef" ~ "Woody cover heterogeneity change (ha-scale)", 
                response == "woody_cover_sd_km_coef" ~ "Woody cover heterogeneity change (km²-scale)")
              
              p_pd_final <- ggplot()+
                geom_line(data = marg_plot, aes(x=x, y=y, color = response_group), linewidth = 1.1) +
                geom_line(data = preds_bt_plot, aes(x=x, y=y, group = iteration, color = response_group), alpha = 0.15, linewidth = 0.5, color = "grey") +
                geom_line(data = marg_plot, aes(x=x, y=y, color = response_group), linewidth = 1.1) +
                facet_wrap(~factor(clean_term),
                           scales="free_x", ncol = 4) +
                scale_x_continuous(breaks = extended_breaks(n = 3)) +
                scale_color_manual(values = palette_groups) +
                # geom_rect(data = rects, aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax), 
                #           fill = "white", alpha = 0.6, inherit.aes = FALSE) +
                # geom_rect(data = rects, aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax), 
                #           fill = "white", alpha = 0.6, inherit.aes = FALSE) +
                geom_rug(data = dt_mean_rug, aes(x = x, y = y), sides="b", length = unit(0.03, "npc"), outside = TRUE) +
                coord_cartesian(clip = "off") +
                ylim(min(marg_plot$y, na.rm =T), max(marg_plot$y, na.rm = T)) +
                theme_classic() +
                labs(y = paste0(clean_label), x = "") +
                theme(legend.position = "none",
                      panel.grid = element_line(color = "white"), 
                      axis.text.y = element_text(size = 12), 
                      axis.text.x = element_text(size = 10), 
                      axis.title = element_text(size = 12), 
                      axis.ticks = element_blank(), 
                      panel.spacing = unit(0.6, "lines"), 
                      strip.background = element_rect(color = "white", fill= "grey95") 
                )
              p_pd_final
              
              stat_means_best <- stat_means[stat_means$method %in% c(best_method), ]
              
              p_comb <- grid.arrange(p_pd_final, p_var, ncol = 2, widths = c(1.5, 1), 
                                     top = textGrob(paste0(clean_label, "\n",
                                                           tier_label, " (n = ", dt_tier[i, ]$n, "; R-sq = ", round(stat_means_best$mean_r_sq, 2),
                                                           "±", round(stat_means_best$sd_r_sq, 2),  "; RMSE = ",
                                                           round(stat_means_best$mean_rmse, 2), "±", round(stat_means_best$sd_rmse, 2),")") ,gp=gpar(fontsize=14)), 
                                     bottom = textGrob(paste0("Best method: ", best_method),gp=gpar(fontsize=10, fontface = "italic")))
              
              
              filename <- paste0("builds/plots/revision/reserve_ml_results/comb_plot_", response_tier, ".png")
              
              #ggsave(plot = p_comb, filename = filename, dpi = 600, height = 6.75, width = 13)
              
              
              #### write out the reguired data:
              
              #variable importance ----
              var_imp_means_plot <- var_imp_means_plot %>% 
                mutate(tier = tier,
                       tier_label = tier_label, 
                       response = response, 
                       response_tier = response_tier)
              
              #predictions 
              preds_bt_plot <- preds_bt_plot %>% 
                mutate(tier = tier,
                       tier_label = tier_label, 
                       response = response, 
                       response_tier = response_tier)
              
              preds_storage <- rbind(preds_storage, preds_bt_plot)
              
              #marg plot 
              
              marg_plot <- marg_plot %>% 
                mutate(tier = tier,
                       tier_label = tier_label, 
                       response = response, 
                       response_tier = response_tier)
              
              marg_storage <- rbind(marg_storage, marg_plot)
              

              dt_mean_rug <- dt_mean_rug %>% 
                mutate(tier = tier,
                       tier_label = tier_label, 
                       response = response, 
                       response_tier = response_tier)
              
              dt_rug_storage <- rbind(dt_rug_storage, dt_mean_rug)
              

              res_list <- list(var_imp_means_plot = var_imp_means_plot, 
                               preds_bt_plot = preds_bt_plot, 
                              marg_plot = marg_plot, 
                              dt_mean_rug = dt_mean_rug)
              
              return(res_list)
              
            }
)
plan(sequential)
toc()
print(paste0("done ", Sys.time()))

names(ml_res_list[[1]])

dt_var_imp_means <- ml_res_list %>% 
  map("var_imp_means_plot") %>%  
  rbindlist()

dt_preds_bt <- ml_res_list %>% 
  map("preds_bt_plot") %>%  
  rbindlist()

dt_marg_plot <- ml_res_list %>% 
  map("marg_plot") %>%  
  rbindlist()

dt_mean_rug <- ml_res_list %>% 
  map("dt_mean_rug") %>%  
  rbindlist()


res_list <- list(dt_var_imp_means = dt_var_imp_means, 
                 dt_preds_bt = dt_preds_bt, 
                 dt_marg_plot = dt_marg_plot, 
                 dt_mean_rug = dt_mean_rug)

res_list$dt_var_imp_means
saveRDS(res_list, "builds/model_results/plot_list.Rds")
