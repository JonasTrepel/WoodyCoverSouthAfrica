
### GBM woody cover SD

source("R/functions/get.heterogeneity.R")
source("R/functions/pdplot.R")


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

dt <- fread("data/ReserveDataSouthAfricaFinal.csv") 


### create model data ---------------------------------

complete.reserves <- dt %>% dplyr::select(c(reserve_name, browser_biomass_kgkm2, n_herbi_sp_reserve, grazer_biomass_kgkm2, herbi_biomass_kgkm2, 
                                            mixed_feeder_biomass_kgkm2, n_herbi_sp_reserve, CW_mean_species_body_mass, MAP, MAT,
                                            days_since_last_fire, fire_events_since_2001, prop_burned_area, 
                                            woody_cover_mean_venter2019, canopy_height_mean, n_deposition, woody_cover_mean_venter2019)) %>% 
  filter(complete.cases(.)) %>% 
  dplyr::select("reserve_name")


# log variables that need to get pushed towards a normal distribution 
dt.mod <- dt %>% 
  mutate(
    Biome = as.factor(Biome), 
    elephant_yn = ifelse(elephant_biomass_kgkm2 > 0, "elephants", "no_elephants")) %>% 
  filter(reserve_name %in% c(complete.reserves$reserve_name)) 



quantile(dt.mod$elevation_sd_1000)
###################################################################################################
# LOOOOOOOOOOP ----------------
###################################################################################################

names(dt.mod)               
subsets <- c("!is.na(woody_cover_mean_venter2019)", "elephant_yn == 'elephants'", "fire_events_since_2001 > 0", 
             "herbi_biomass_kgkm2 >= 100", "herbi_biomass_kgkm2 < 100", 
             "establishment_year <= 2009"," area_ha >= 2400", 
             "Biome == 'Savanna'", "Biome == 'Albany Thicket'",
             "Biome == 'Grassland'", "Biome == 'Fynbos'", 
             "Biome == 'Nama-Karoo'")

tier_labels<- c("Full model", "Reserves with elephants", "Reserves with fire", "Reserves with high herbivore biomass (≥10,000 kg/km2)", "Low biomass", "Reserves established before 2010", "Reserves larger than 24 km2", 
                "Savanna", "Albany thicket", "Grassland", "Fynbos", "Nama Karoo")

tiers <- c("main", "elephants", "fire", "high_biomass", "low_biomass", "old", "large", 
           "savanna", "albany_thicket", "grassland", "fynbos", "nama_karoo")
dt.tier <- data.table(
  subset = subsets, 
  tier = tiers, 
  tier_label = tier_labels, 
  n = 0)


## create storage objects for plots 

tune.res <- data.table(
  shrinkage = NA, 
  interaction.depth = NA, 
  n.minobsinnode = NA, 
  n.trees = NA, 
  RMSE = NA, 
  Rsquared = NA, 
  MAE = NA, 
  RMSESD = NA, 
  RsquaredSD = NA, 
  MAESD = NA, 
  tier = NA, 
  n = NA, 
  filename = NA, 
  pr_r_squared = NA, 
  pr_r_squared_sd = NA,
  pr_rmse = NA, 
  pr_rmse_sd = NA
) %>% filter(!is.na(tier))


for(i in 1:nrow(dt.tier)){
  subset <- dt.tier[i, ]$subset
  dt.sub <- dt.mod %>% filter(eval(parse(text = subset)))
  
  nr <- nrow(dt.sub)
  dt.tier[i, ]$n <- nr
  
}

## define color values 
palette.c <- MetBrewer::met.brewer("Pillement", n=14)
unique(palette.c)

palette <- c("MAT (°C)" = "#213B48", "MAP (mm)" = "#2B4655", "N deposition ((kg/km2)/year)" = "#334F5D", "Elevation SD (m)" = "#3B5966",
             "Mean body mass (kg; cwm)" = "#44636F",
             "Herbivore functional diversity" = "#537179",
             "Herbivore species richness" = "#637F83", "Grazer biomass (kg/km2)" = "#738E8E",
             "Browser biomass (kg/km2)" = "#6F8679", "Mixed feeder biomass (kg/km2)" = "#6C7F66",
             "Herbivore biomass (kg/km2)" = "#697852", "Elephant biomass (kg/km2)" = "#7E7C55",
             "Fire frequency" = "#938058",
             "Proportion burned area" = "#A9845B")



############### create cluster ####################
library(doSNOW)
library(foreach)
library(tictoc)

# Create and register a cluster
clust <- makeCluster(3)
registerDoSNOW(clust)

## progress bar 
iterations <- nrow(dt.tier)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

tic()
foreach.results <- foreach(i = 1:nrow(dt.tier),
                           .packages = c('dplyr', 'qgam', 'mgcv', 'broom', 'ggplot2', 'tidyr', 'data.table', 'Metrics', 'tidyverse', 'MuMIn',
                                         'gbm', 'caret', 'tictoc', 'MetBrewer', 'gridExtra', 'grid', 'scales'),
                           .options.snow = opts,
                           .inorder = FALSE,
                           .combine = rbind) %dopar% {
                             
                             tier <- dt.tier[i, ]$tier
                             subset <- dt.tier[i, ]$subset
                             tier_label <- dt.tier[i, ]$tier_label
                             
                             
                             dt.sub <- dt.mod %>% filter(eval(parse(text = subset)))
                             
                             print(paste0("starting tier: ", tier, " (", i, "/", nrow(dt.tier), ")"))
                             
                             
                             #create data for modeling
                             if(!tier == "elephants"){
                               dt.gbm <- dt.sub %>% 
                                 dplyr::select(tree_cover_sd_100, 
                                               MAT, MAP, elevation_sd_1000, 
                                               CW_mean_species_body_mass, 
                                               herbi_fun_div_distq1, n_herbi_sp_reserve,
                                               grazer_biomass_kgkm2, browser_biomass_kgkm2, mixed_feeder_biomass_kgkm2, herbi_biomass_kgkm2,
                                               fire_events_since_2001, prop_burned_area, spatial_predictor1)
                               setnames(dt.gbm, c("tree_cover_sd_100", 
                                                  "MAT", "MAP", "elevation_sd_1000",
                                                  "CW_mean_species_body_mass", 
                                                  "herbi_fun_div_distq1", "n_herbi_sp_reserve", 
                                                  "grazer_biomass_kgkm2", "browser_biomass_kgkm2", "mixed_feeder_biomass_kgkm2", "herbi_biomass_kgkm2",
                                                  "fire_events_since_2001", "prop_burned_area"), 
                                        c("Woody cover heterogeneity", "MAT (°C)", "MAP (mm)", "Elevation SD (m)",  "Mean body mass (kg; cwm)",
                                          "Herbivore functional diversity", "Herbivore species richness", 
                                          "Grazer biomass (kg/km2)", "Browser biomass (kg/km2)", "Mixed feeder biomass (kg/km2)",
                                          "Herbivore biomass (kg/km2)",  "Fire frequency", "Proportion burned area"))
                               
                             }else{
                               dt.gbm <- dt.sub %>% 
                                 dplyr::select(tree_cover_sd_100, 
                                               MAT, MAP, 
                                               CW_mean_species_body_mass, 
                                               herbi_fun_div_distq1, n_herbi_sp_reserve, elephant_biomass_kgkm2, herbi_biomass_kgkm2,
                                               fire_events_since_2001, spatial_predictor1)
                               
                               setnames(dt.gbm, c("tree_cover_sd_100", 
                                                  "MAT", "MAP", 
                                                  "CW_mean_species_body_mass", 
                                                  "herbi_fun_div_distq1", "n_herbi_sp_reserve", 
                                                  "elephant_biomass_kgkm2", "herbi_biomass_kgkm2",
                                                  "fire_events_since_2001"), 
                                        c("Woody cover heterogeneity", "MAT (°C)", "MAP (mm)",  "Mean body mass (kg; cwm)",
                                          "Herbivore functional diversity", "Herbivore species richness", 
                                          "Elephant biomass (kg/km2)", "Herbivore biomass (kg/km2)",  "Fire frequency"))
                             }
                             
                             
                             
                             # create hyperparameter grid
                             hyper.grid <- expand.grid(
                               shrinkage = c( .001, .005, 0.01),
                               interaction.depth = c(1,2,3),
                               n.minobsinnode = c(5, 10, 15), 
                               n.trees = seq(1000, 10000, 250)
                             )
                             
                             # total number of combinations
                             nrow(hyper.grid)
                             
                             ### REMEMBER TO SET BACK TO 10 FOR FINAL RUN
                             ## define training parameters 
                             fitControl <- trainControl(## 10 fold cross validation
                               method = "repeatedcv",
                               number = 5, #number of splits 
                               repeats = 5, #repeat 10 times
                               savePredictions = "final", #keep final model predictions, would otherwise be dumped
                               returnResamp = "final")
                             
                             
                             set.seed(161)
                             
                             ### run the models -----------------
                             print(paste0("start running models for tier: ", tier))
                             
                             tic()
                             gbmFit <- train(`Woody cover heterogeneity` ~ .,
                                             data = dt.gbm, 
                                             method = "gbm",
                                             trControl = fitControl,
                                             verbose = FALSE,
                                             tuneGrid = hyper.grid)
                             toc() 
                             #gbmFit
                             
                             #palette <- met.brewer("VanGogh3", n = 20)
                             
                             head(gbmFit$results[order(gbmFit$results$RMSE),]) #sort that the best model (lowest root mean squared error) is on top
                             
                             ### save final model 
                             final.mod <- gbmFit$finalModel
                             filename.final.mod <- paste0("builds/models/july/gbms/tree_cover_sd_100/", tier, ".rds")
                            # saveRDS(final.mod, file = filename.final.mod)
                             
                             
                             tmp <- gbmFit$results %>% slice_min(RMSE) %>%
                               mutate(tier = tier, n = nrow(dt.gbm), filename = filename.final.mod)
                             
                             print(paste0("start bootstrapping for tier: ", tier))
                             ### use the same hyperparameters as for the full model but split the data in 80 % training and 20 % test
                             ### new r-squared should be more solid. 
                             
                             #### SET 1000 for final run 
                             r.sq <- data.table(r_squared = NA, rmse_bootstrap = NA) %>% filter(!is.na(r_squared))
                             bt.rel <- data.table()
                             marg.plot.bt <- data.table()
                             for(number in 1:100){
                               
                               Index <- createDataPartition(dt.gbm$`Woody cover heterogeneity`, p =0.8, list = F)
                               Train <- dt.gbm[Index,]
                               Test <- dt.gbm[!Index,]
                               
                               
                               fit <- gbm(`Woody cover heterogeneity` ~ ., 
                                          data = Train, 
                                          shrinkage = tmp$shrinkage, 
                                          interaction.depth = tmp$interaction.depth, 
                                          distribution = "gaussian",
                                          n.trees = tmp$n.trees,
                                          n.minobsinnode = tmp$n.minobsinnode
                               )
                               
                               
                               pr <- caret::postResample(pred = predict(fit, newdata = Test),
                                                         obs = Test$`Woody cover heterogeneity`)
                               
                               
                               tmp.r.sq <- data.table(r_squared = NA, rmse_bootstrap = NA)
                               tmp.r.sq$r_squared <- unname(pr[2])
                               tmp.r.sq$rmse_bootstrap <- unname(pr[1])
                               
                               r.sq <- rbind(tmp.r.sq, r.sq)
                               
                               ### get relative variable importance 
                               
                               tmp.rel <- as.data.table(summary(fit)) %>% #takes summary from the best model 
                                 mutate(var = gsub("`", "", var)) %>% pivot_wider(names_from = var, values_from = rel.inf)
                               
                               bt.rel <- rbind(bt.rel, tmp.rel)
                               
                               ### get predictions 
                               
                               for(j in 1:(ncol(dt.gbm)-1)){
                                 temp <- plot.gbm(fit, j, return.grid = TRUE, continuous.resolution = 100)
                                 if(j==1){
                                   marg <- temp}else{
                                     marg <- cbind(marg, temp)}
                               }
                               # add meaningful names for marginal effect plots df
                               names(marg)[seq(2, ncol(marg), 2)] <- paste0(names(marg)[seq(1, (ncol(marg)-1), 2)], "_y")
                               
                               marg.t = melt(marg) # 
                               y = grep("_y", marg.t$variable)
                               marg.plot.bt.tmp = data.frame(var = marg.t[-y,]$variable,
                                                             x = marg.t[-y,]$value,
                                                             y = marg.t[y,]$value, 
                                                             iteration = paste0("it_", number)) %>% 
                                 mutate(
                                   var = gsub("`", "", var))
                               
                               marg.plot.bt <- rbind(marg.plot.bt, marg.plot.bt.tmp) %>% filter(!grepl("spatial_pred", var))
                               
                               print(paste0(number,"/",100))
                             }
                             
                             
                             tmp <- tmp %>% mutate(
                               pr_r_squared = mean(r.sq$r_squared), 
                               pr_r_squared_sd = sd(r.sq$r_squared),
                               pr_rmse = mean(r.sq$rmse_bootstrap), 
                               pr_rmse_sd = sd(r.sq$rmse_bootstrap)
                             )
                             
                             tune.res <- rbind(tune.res, tmp)
                             
                             
                             dt.rel.bt.raw <- bt.rel %>% pivot_longer(cols = c(all_of(names(bt.rel))), names_to = "var", values_to = "rel.inf") 
                             
                             dt.rel.bt <- dt.rel.bt.raw %>% group_by(var) %>% summarize(mean_rel_inf = mean(rel.inf), 
                                                                                        sd_rel_inf = sd(rel.inf)) %>% 
                               filter(!grepl("spatial_predictor", var))
                             
                             
                             ## psi plot 
                             
                             psi <- ggplot(gbmFit) +
                               theme_bw() +
                               scale_color_manual(values = c("#9cc184", "#669d62", "#1f5b25", "#1e3d14")) +
                               labs(x = "Number of Boosting Iterations", color = "Interaction depth", shape = "Interaction depth") +
                               scale_x_continuous(sec.axis = dup_axis(name = "Minimum number of points per node")) +
                               scale_y_continuous(sec.axis = dup_axis(name = "Learning rate (shrinkage)")) +
                               theme(axis.line.x.top=element_line(color="white"),
                                     axis.text.x.top = element_text(color="white"),
                                     axis.ticks.x.top = element_line(color="white"),
                                     axis.line.y.right =element_line(color="white"),
                                     axis.text.y.right = element_text(color="white"),
                                     axis.ticks.y.right = element_line(color="white"),
                                     axis.title.y = element_text(color="black")) +
                               guides(shape = "none")
                             psi
                             
                             print(psi)
                             
                             filename.psi <- paste0("builds/plots/july/gbm_res_sp/tree_cover_sd_100/psi_plot_", tier, ".png")
                             ggsave(plot = psi, filename = filename.psi, dpi = 600, height = 10, width = 10)
                             
                             
                             ### get relative variable importance plots 
                             
                             dt.rel <- as.data.table(summary(gbmFit)) %>% #takes summary from the best model 
                               mutate(var = gsub("`", "", var)) %>% 
                               filter(!grepl("spatial_predictor", var)) %>%
                               left_join(dt.rel.bt) %>% 
                               mutate(var = fct_reorder(var, -mean_rel_inf)) 
                             
                             
                             n.colors = (ncol(dt.gbm)-1)
                             
                             #palette <- met.brewer("Pillement", n=n.colors)
                             
                             
                             p.var <- ggplot() +
                               geom_col(data = dt.rel.bt, aes(y = reorder(var, mean_rel_inf), x = mean_rel_inf, fill = var), alpha = 0.9) +
                               geom_errorbar(data = dt.rel.bt, aes(y = var, xmin = mean_rel_inf-sd_rel_inf,
                                                                   xmax = mean_rel_inf+sd_rel_inf)) +
                               # geom_col(data = dt.rel[var != "Spatial predictor"], aes(y = reorder(var, rel.inf), x = rel.inf, fill = var), alpha = 0.2) +
                               
                               scale_fill_manual(values = palette) +
                               labs(x = "Relative variable importance", y = "", fill = "Variable") +
                               theme_classic() +
                               theme(legend.position = "none",
                                     axis.text = element_text(size = 12))
                             
                             p.var
                             
                             print(p.var)
                             
                             
                             #### Marginal effect plots ----
                             for(j in 1:(ncol(dt.gbm)-1)){
                               temp <- plot.gbm(gbmFit$finalModel, j, return.grid = TRUE, continuous.resolution = 100)
                               if(j==1){
                                 marg <- temp}else{
                                   marg <- cbind(marg, temp)}
                             }
                             # add meaningful names for marginal effect plots df
                             names(marg)[seq(2, ncol(marg), 2)] <- paste0(names(marg)[seq(1, (ncol(marg)-1), 2)], "_y")
                             
                             marg.t = melt(marg) # 
                             y = grep("_y", marg.t$variable)
                             marg.plot = data.frame(var = marg.t[-y,]$variable,
                                                    x = marg.t[-y,]$value,
                                                    y = marg.t[y,]$value) %>% 
                               mutate(
                                 var = gsub("`", "", var)) %>% 
                               filter(!grepl("spatial_predictor", var)) %>%
                               left_join(dt.rel) %>% 
                               mutate(
                                 var = fct_reorder(var, -mean_rel_inf)
                               ) 
                             
                             
                             if(!tier == "elephants"){
                               dt.points <- dt.gbm %>% pivot_longer(
                                 cols = c(`MAT (°C)`, `MAP (mm)`, `Elevation SD (m)`, `Mean body mass (kg; cwm)`, `Proportion burned area`,
                                          `Herbivore functional diversity`, `Herbivore species richness`, 
                                          `Grazer biomass (kg/km2)`, `Browser biomass (kg/km2)`, `Mixed feeder biomass (kg/km2)`,
                                          `Herbivore biomass (kg/km2)`,  `Fire frequency`), 
                                 values_to = "x", names_to = "var") %>% 
                                 mutate(y = median(marg.plot$y, na.rm = T)) %>% 
                                 left_join(dt.rel) %>% 
                                 filter(!grepl("spatial_predictor", var)) %>%
                                 mutate(
                                   var = gsub("`", "", var), 
                                   var = fct_reorder(var, -mean_rel_inf)
                                 ) 
                             }else{
                               dt.points <- dt.gbm %>% pivot_longer(
                                 cols = c(`MAT (°C)`, `MAP (mm)`, `Mean body mass (kg; cwm)`,
                                          `Herbivore functional diversity`, `Herbivore species richness`, 
                                          `Elephant biomass (kg/km2)`, `Herbivore biomass (kg/km2)`,  `Fire frequency`), 
                                 values_to = "x", names_to = "var") %>% 
                                 mutate(y = median(marg.plot$y, na.rm = T)) %>% 
                                 left_join(dt.rel) %>% 
                                 filter(!grepl("spatial_predictor", var)) %>%
                                 mutate(
                                   var = gsub("`", "", var), 
                                   var = fct_reorder(var, -mean_rel_inf)
                                 ) 
                             }
                             
                             
                             dt.mean.rug <- dt.points %>% dplyr::select(var, x, y)
                             
                             rects <- dt.mean.rug %>%
                               group_by(var) %>%
                               mutate(
                                 lower_quantile_x = quantile(x, 0.05),
                                 upper_quantile_x = quantile(x, 0.95),
                               ) %>%
                               ungroup() %>% 
                               group_by(var) %>%
                               summarize(
                                 ymin = -Inf,
                                 ymax = Inf,
                                 xmin1 = -Inf,
                                 xmax1 = first(lower_quantile_x),
                                 xmin2 = first(upper_quantile_x),
                                 xmax2 = Inf
                               ) %>%
                               ungroup()
                             
                             p.pd.final <- ggplot()+
                               geom_line(data = marg.plot, aes(x=x, y=y, color = var), linewidth = 1.1) +
                               geom_line(data = marg.plot.bt, aes(x=x, y=y, group = iteration), alpha = 0.15, linewidth = 0.5, color = "grey") +
                               geom_line(data = marg.plot, aes(x=x, y=y, color = var), linewidth = 1.1) +
                               facet_wrap(~factor(var),
                                          scales="free_x", ncol = 4) +
                               scale_x_continuous(breaks = extended_breaks(n = 3)) +
                               scale_color_manual(values = palette) +
                               geom_rect(data = rects, aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax), 
                                         fill = "white", alpha = 0.6, inherit.aes = FALSE) +
                               geom_rect(data = rects, aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax), 
                                         fill = "white", alpha = 0.6, inherit.aes = FALSE) +
                               geom_rug(data = dt.mean.rug, aes(x = x, y = y), sides="b", length = unit(0.03, "npc"), outside = TRUE) +
                               coord_cartesian(clip = "off") +
                               theme_bw() +
                               labs(y = "Woody cover heterogeneity", x = "") +
                               theme(legend.position = "none",
                                     panel.grid = element_line(color = "white"), 
                                     axis.text.y = element_text(size = 12), 
                                     axis.text.x = element_text(size = 10), 
                                     axis.title = element_text(size = 12), 
                                     axis.ticks = element_blank(), 
                                     panel.spacing = unit(0.5, "lines"))
                             p.pd.final
                             
                             print(p.pd.final)
                             
                             median(marg.plot$y, na.rm = T)
                             
                             dt.tier[i, ]$n
                             
                             p.comb <- grid.arrange(p.pd.final, p.var, ncol = 2, widths = c(1.5, 1), 
                                                    top = textGrob(paste0(tier_label, " (n = ", dt.tier[i, ]$n, "; R-sq = ", round(tmp$pr_r_squared, 2), "±", round(tmp$pr_r_squared_sd, 2),  "; RMSE = ", round(tmp$pr_rmse, 2), "±", round(tmp$pr_rmse_sd, 2),")") ,gp=gpar(fontsize=14)))
                             
                             ##save comb plot 
                             filename <- paste0("builds/plots/july/gbm_res_sp/tree_cover_sd_100/comb_plot_", tier, ".png")
                             ggsave(plot = p.comb, filename = filename, dpi = 600, height = 6.75, width = 13)
                             
                             
                             return(tune.res)
                           }

stopCluster(clust)
print("loop done")

fwrite(foreach.results, "builds/model_results/model_meta_tree_cover_sd_100.csv")
stop(pb)

#### IF WE END UP USING THIS, WE PROBABLY HAVE TO SPLIT THE DATA.....
### i.e., fit the model on 70 % of the data, then use it to predict the remaining 30 and compare to the actual ones... 





