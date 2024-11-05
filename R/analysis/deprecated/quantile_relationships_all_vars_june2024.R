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
library(broom)
library(qgam)


## load data 

dt <- fread("data/ReserveDataSouthAfricaFinal.csv")

### create model data ---------------------------------

complete.reserves <- dt %>% dplyr::select(c(reserve_name, browser_biomass_ha, n_herbi_sp_reserve, grazer_biomass_ha, herbi_biomass_ha, 
                                            mixed_feeder_biomass_ha, herbi_fun_ent, CW_mean_species_body_mass, MAP, MAT,
                                            days_since_last_fire, fire_events_since_2001, prop_burned_area, 
                                            tree_cover_sd_100, canopy_height_mean, n_deposition, tree_cover_sd_100, 
                                            tree_cover_mean, woody_cover_trend_venter2019)) %>% 
  filter(complete.cases(.)) %>% 
  dplyr::select("reserve_name")


# log variables that need to get pushed towards a normal distribution 
dt.mod.raw <- dt %>% 
  mutate(
    MAT_scaled = as.numeric(scale(MAT)),
    MAP_scaled = as.numeric(scale(MAP)),
    elevation_mean_scaled = as.numeric(scale(elevation_mean)), 
    elevation_sd_1000_scaled = as.numeric(scale(elevation_sd_1000)), 
    log_fire_events_since_2001_scaled = as.numeric(scale(log(fire_events_since_2001 + 0.000001))), 
    log_prop_burned_area_scaled = as.numeric(scale(log(prop_burned_area + 0.000001))), 
    prop_burned_area_scaled = as.numeric(scale(prop_burned_area)), 
    
    tree_cover_mean_scaled = as.numeric(scale(tree_cover_mean)),
    log_tree_cover_sd_100_scaled = as.numeric(scale(log(tree_cover_sd_100))), 
    tree_cover_sd_100_scaled = as.numeric(scale(tree_cover_sd_100)), 
    
    log_canopy_height_mean_scaled = as.numeric(scale(log(canopy_height_mean))), 
    n_deposition_scaled = as.numeric(scale(log(n_deposition))), 
    
    
    log_herbi_biomass_ha_scaled = as.numeric(scale(log(herbi_biomass_ha + 0.000001))), 
    log_grazer_biomass_ha_scaled = as.numeric(scale(log(grazer_biomass_ha + 0.000001))), 
    log_browser_biomass_ha_scaled = as.numeric(scale(log(browser_biomass_ha + 0.000001))), 
    log_mixed_feeder_biomass_ha_scaled = as.numeric(scale(log(mixed_feeder_biomass_ha + 0.000001))), 
    log_CW_mean_species_body_mass_scaled = as.numeric(scale(log(CW_mean_species_body_mass))), 
    log_CW_max_species_body_mass_scaled = as.numeric(scale(log(CW_max_species_body_mass))), 
    
    log_elephant_biomass_ha_scaled = as.numeric(scale(log(elephant_biomass_ha +0.000001))), 
    
    fire_events_since_2001_scaled = as.numeric(scale(fire_events_since_2001)), 
    
    herbi_fun_red_scaled = as.numeric(scale(herbi_fun_red)), 
    n_herbi_sp_reserve_scaled = as.numeric(scale(n_herbi_sp_reserve)), 
    
    herbi_fun_ent_scaled = as.numeric(scale(herbi_fun_ent)),
    herbi_fun_div_distq1_scaled = as.numeric(scale(herbi_fun_div_distq1)), 
    herbi_biomass_ha_scaled = as.numeric(scale(herbi_biomass_ha)), 
    grazer_biomass_ha_scaled = as.numeric(scale(grazer_biomass_ha)), 
    browser_biomass_ha_scaled = as.numeric(scale(browser_biomass_ha)), 
    mixed_feeder_biomass_ha_scaled = as.numeric(scale(mixed_feeder_biomass_ha)), 
    CW_mean_species_body_mass_scaled = as.numeric(scale(CW_mean_species_body_mass)), 
    CW_max_species_body_mass_scaled = as.numeric(scale(CW_max_species_body_mass)), 
    
    elephant_biomass_ha_scaled = as.numeric(scale(elephant_biomass_ha)), 
    
    
    Biome = as.factor(Biome), 
    elephant_yn = ifelse(elephant_biomass_ha > 0, "elephants", "no_elephants")) %>% 
  filter(reserve_name %in% c(complete.reserves$reserve_name)) 


####################################################################################################
############################# Prepare and combine responses ########################################
####################################################################################################



##distance to maximum tree cover -----------------------

m.elim.tc <- qgam(tree_cover_mean ~ s(MAP_scaled, MAT_scaled, k = 5) + s(MAP_scaled, k = 5) + s(MAT_scaled, k = 5) + s(elevation_mean_scaled, k =5), data = dt.mod.raw, qu = 0.95)
summary(m.elim.tc); AIC(m.elim.tc) #4521.731

dt.mod.raw$pred_max_tree_cover <- predict(m.elim.tc)

##distance to maximum tree cover SD -----------------------

m.elim.ch <- qgam(tree_cover_sd_100 ~ s(elevation_sd_1000_scaled, k = 5) + s(MAP_scaled, MAT_scaled, k = 5) + s(MAP_scaled, k = 5), data = dt.mod.raw, qu = 0.95)
summary(m.elim.ch); AIC(m.elim.ch)

dt.mod.raw$pred_max_tree_cover_sd_100 <- predict(m.elim.ch)

dt.mod <- dt.mod.raw %>%
  mutate(distance_to_max_tc = pred_max_tree_cover - tree_cover_mean,
         distance_to_max_tc_scaled = as.numeric(scale(distance_to_max_tc)),
         distance_to_max_tree_cover_sd_100 = pred_max_tree_cover_sd_100 - tree_cover_sd_100,
         distance_to_max_tree_cover_sd_100_scaled = as.numeric(scale(distance_to_max_tree_cover_sd_100)) )


summary(dt.mod$prop_burned_area)


####################################################################################################
### build model guide --------------------
####################################################################################################

vars <- c("s(CW_mean_species_body_mass_scaled, k = 5)", "s(MAT_scaled, k = 5)",
          "s(MAP_scaled, k = 5)", "s(n_herbi_sp_reserve_scaled, k = 5)", "s(herbi_fun_div_distq1_scaled, k = 5)",
          "s(browser_biomass_ha_scaled, k = 5)", "s(grazer_biomass_ha_scaled, k = 5)", "s(mixed_feeder_biomass_ha_scaled, k = 5)",
          "s(herbi_biomass_ha_scaled, k = 5)", "s(log_fire_events_since_2001_scaled, k = 5)", "s(n_deposition_scaled, k = 5)",
          "s(prop_burned_area_scaled, k = 5)", "s(log_elephant_biomass_ha_scaled, k = 5)", 
          "s(elevation_sd_1000_scaled, k = 5)")
vars


# Function to generate all combinations of the variables
generate.combinations <- function(vars, max.n = NA) {
  
  if(is.na(max.n)){max.n <- length(vars)}
  
  all.combs <- c()  # Initialize an empty vector to store combinations
  
  # Loop through different sizes of combinations 
  for (i in 1:max.n) {
    comb <- combn(vars, i, simplify = FALSE)  # Generate combinations of size i
    all.combs <- c(all.combs, comb)  # Append to all_combinations
  }
  
  return(all.combs)
}

# Generate all combinations
combinations <- generate.combinations(vars)

c.comb <- sapply(combinations, function(x) paste(x, collapse = " + "))


### responses 
names(dt)

responses <- c("tree_cover_mean", 
              # "distance_to_max_tc", 
               "woody_cover_trend_venter2019", 
             #  "distance_to_max_tree_cover_sd_100",
               "tree_cover_sd_100", 
             "canopy_height_sd_100")

model_type <- c("regular", "q95", "q05")


### tiers and subsets --------

#### define data tiers -------------------
subsets <- c("!is.na(reserve_name)", "elephant_yn == 'elephants'", "fire_events_since_2001 > 0", 
             "herbi_biomass_ha >= 100", "herbi_biomass_ha < 100", 
             "establishment_year <= 2009"," area_ha >= 2400", 
             "Biome == 'Savanna'", "Biome == 'Albany Thicket'",
             "Biome == 'Grassland'", "Biome == 'Fynbos'", 
             "Biome == 'Nama-Karoo'"
)

tier_labels<- c("Full model", "Elephants", "Fire", "High biomass", "Low biomass", "Old reserves", "Large reserves", 
                "Savanna", "Albany thicket", "Grassland", "Fynbos", "Nama Karoo")

tiers <- c("main", "elephants", "fire", "high_biomass", "low_biomass", "old", "large", 
           "savanna", "albany_thicket", "grassland", "fynbos", "nama_karoo")
dt.tier <- data.table(
  subset = subsets, 
  tier = tiers, 
  tier_label = tier_labels, 
  n = 0)

for(i in 1:nrow(dt.tier)){
  subset <- dt.tier[i, ]$subset
  dt.sub <- dt.mod %>% filter(eval(parse(text = subset)))
  
  nr <- nrow(dt.sub)
  dt.tier[i, ]$n <- nr
  
}

## build guides
guide.raw <- CJ(vars = c.comb, 
                response = responses, 
                model_type = model_type, 
                tier = tiers) %>% 
  mutate(formula_id = paste0("formula_", 1:nrow(.)), 
         n_var =  sapply(vars, function(x) length(unlist(strsplit(x, " \\+ ")))), 
         formula = paste0(response, " ~ ", vars), 
         mod_spec = paste0(response, "_", model_type)) %>% 
  left_join(dt.tier)


########## check correlations --------------------------------

vars.clean <- gsub("s\\(", "", vars)
vars.clean <- gsub(", k = 5\\)", "", vars.clean)
vars.clean <- gsub("log_", "", vars.clean)

pair.combs <- combn(vars.clean, 2, simplify = FALSE)  

dt.corr <- data.table(
  var1 = character(), 
  var2 = character(), 
  corr = numeric(), 
  exclude_if = character()
)

exclusions <- c()

for(i in 1:length(pair.combs)){
  
  dt.tmp <- data.table(
    var1 = pair.combs[[i]][1], 
    var2 = pair.combs[[i]][2], 
    corr = NA,
    exclude_if = NA
  )
  
  var1 <- dt.mod %>% dplyr::select(all_of(dt.tmp$var1)) %>% pull()
  var2 <- dt.mod %>% dplyr::select(all_of(dt.tmp$var2)) %>% pull()
  
  cor.ob <- cor.test(var1, var2)
  
  corr <- unname(cor.ob$estimate)
  
  dt.tmp$corr <- corr
  
  exclusions.tmp <- c()
  
  if(abs(corr) >= 0.6){
    
    dt.tmp$exclude_if <- paste0("grepl('", dt.tmp$var1, "', formula) & grepl('", dt.tmp$var2,"', formula)")
    
    exclusions.tmp <- guide.raw %>% filter(eval(parse(text = dt.tmp$exclude_if))) %>% dplyr::select(formula_id) %>% pull
    
  }
  
  dt.corr <- rbind(dt.corr, dt.tmp)
  
  exclusions <- c(exclusions, exclusions.tmp)
  
  exclusions <- unique(exclusions)
  
  print(i)
}

guide.raw2 <- guide.raw %>% filter(!formula_id %in% c(exclusions)) #%>% filter(n_var < 5) %>% unique() #%>% filter(n_var == 1) #%>% filter(response %in% c("site_plant_fun_red", "graminoids_per_site"))

#### define further exclusion criteria -------


guide <- guide.raw2 %>% 
  mutate(exclude = case_when(
    .default = "no", 
    grepl("elephant_biomass", formula) & tier != "elephants" ~ "exclude", 
    n_var > n/10 ~ "exclude", 
    grepl("MAP", formula) & response == "distance_to_max_tc" ~ "exclude", 
    grepl("MAT", formula) & response == "distance_to_max_tc" ~ "exclude", 
    grepl("n_deposition", formula) & response == "distance_to_max_tc" ~ "exclude", 
    grepl("s\\(tree_cover_mean", formula) & response == "distance_to_max_tc" ~ "exclude", 
    grepl("MAP", formula) & response == "distance_to_max_tree_cover_sd_100" ~ "exclude", 
    grepl("MAT", formula) & response == "distance_to_max_tree_cover_sd_100" ~ "exclude", 
    grepl("n_deposition", formula) & response == "distance_to_max_tree_cover_sd_100" ~ "exclude", 
    grepl("s\\(tree_cover_mean", formula) & response == "distance_to_max_tree_cover_sd_100" ~ "exclude", 
    
    grepl("n_deposition", formula) & response == "tree_cover_mean" ~ "exclude", 
    grepl("s\\(tree_cover_mean", formula) & response == "tree_cover_mean" ~ "exclude",
    grepl("grazer_biomass", formula) & response == "tree_cover_mean" ~ "exclude", 
    grepl("s\\(mixed_feeder_biomass", formula) & response == "tree_cover_mean" ~ "exclude",
    grepl("s\\(browser_biomass", formula) & response == "tree_cover_mean" ~ "exclude",
    
    grepl("elevation_sd", formula) & response %in% c("tree_cover_mean", "distance_to_max_tree_cover_sd_100", "distance_to_max_tc", "woody_cover_trend_venter2019") ~ "exclude", 
    
    
    grepl("n_deposition", formula) & response == "tree_cover_sd_100" ~ "exclude", 
    grepl("s\\(tree_cover_mean", formula) & response == "tree_cover_sd_100" ~ "exclude",
    grepl("n_deposition", formula) & response == "canopy_height_sd_100" ~ "exclude", 
    grepl("s\\(tree_cover_mean", formula) & response == "canopy_height_sd_100" ~ "exclude",
    grepl("s\\(tree_cover_mean", formula) ~ "exclude"),
    mod_spec_tier = paste0(mod_spec, "_", tier)) %>% 
  filter(exclude == "no") #%>% filter(n_var < 2)


n_distinct(guide[,.(mod_spec, formula)])
n_distinct(guide[,.(response, formula)])
n_distinct(guide[,.(formula)])


saveRDS(guide, "data/qgam_model_guide.Rds")
guide <- readRDS("data/qgam_model_guide.Rds")

#### run models --------

res <- data.frame(
  r_squared = NA, 
  aicc = NA, 
  aic = NA,
  bic = NA,
  formula = NA, 
  response = NA, 
  mod_spec_tier = NA,
  deviance_expl = NA) %>% filter(!is.na(formula))

plot.paths <- data.frame(
  filename = NA, 
  response = NA, 
  mod_spec = NA
) %>% filter(!is.na(response))

pdps <- list()

k = 0

## change to no, if too lazy to specify nice names below 
use.clean.names <- "yes"


#mod_spec_tier <- "tree_cover_sd_100_q05_albany_thicket"

#guide.backup <- copy(guide)

#guide <- guide %>% sample_n(50)
#guide <- copy(guide.backup)

guide <- guide %>% 
  filter(tier %in% c("main", "elephants", "fire", "fynbos", "grassland", "high_biomass", "large", "low_biomass", "albany_thicket", "nama_karoo", "old", "savanna") & 
           response %in% c("tree_cover_mean", "tree_cover_sd_100", "woody_cover_trend_venter2019", "canopy_height_sd_100") & 
           model_type %in% c("q95", "q05")) %>% filter(tier == "main") %>% 
  mutate(formula = ifelse(!tier == "main", gsub("k = 5", "k = 3",  formula), formula)) %>% filter(tier == "main")

############### create cluster ####################
library(doSNOW)
library(foreach)
library(tictoc)
library(parallel)
# Number of cores to use
num_cores <- detectCores() - 2

# Create and register a cluster
clust <- makeCluster(num_cores)
registerDoSNOW(clust)

## progress bar 
iterations <- n_distinct(guide$mod_spec_tier)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


rbind.lists <- function(x, y) {combined.list <- list(res = rbind(x$res, y$res), plot.paths = rbind(x$plot.paths, y$plot.paths))
return(combined.list)}

#mod_spec_tier <- "tree_cover_sd_100_q95_main"


tic()
foreach.results <- foreach(mod_spec_tier = unique(guide$mod_spec_tier),
                           .packages = c('dplyr', 'qgam', 'mgcv', 'broom', 'ggplot2', 'tidyr', 'data.table', 'Metrics', 'tidyverse', 'MuMIn' ,"MetBrewer"),
                           .options.snow = opts,
                           .inorder = FALSE,
                           .combine = rbind.lists) %dopar% {
                             #for(mod_spec_tier in unique(guide$mod_spec_tier)){
                             print(mod_spec_tier)
                             
                             mod.spec.tier <- mod_spec_tier
                             
                             guide <- guide %>% data.table() %>% as_tibble() %>% as.data.table() 
                             
                             subset <- unique(guide[mod_spec_tier == mod.spec.tier, ]$subset)
                             
                             filter.resp <- unique(guide[mod_spec_tier == mod.spec.tier, ]$response)
                             
                             guide.sub <- guide %>% filter(mod_spec_tier %in% c(mod.spec.tier))
                             
                             dt.sub <- dt.mod %>% filter(eval(parse(text = subset)))
                             
                             for(i in 1:nrow(guide.sub)){
                               
                               formula <- as.formula(guide.sub[i,]$formula)
                               
                               
                               ### 
                               if(grepl("q95", mod_spec_tier)){
                                 m <- tryCatch(
                                   {qgam(formula, data = dt.sub, qu = 0.95)},
                                   error = function(e) {cat("Model", i, "failed: ", e$message, "\n") 
                                     return(NULL) })
                               }
                               
                               if(grepl("regular", mod_spec_tier)){
                                 m <- tryCatch(
                                   {gam(formula, data = dt.sub, method = "REML", select = TRUE)},
                                   error = function(e) {cat("Model", i, "failed: ", e$message, "\n") 
                                     return(NULL) })
                               }
                               
                               if(grepl("q05", mod_spec_tier)){
                                 m <- tryCatch(
                                   {qgam(formula, data = dt.sub, qu = 0.05)},
                                   error = function(e) {cat("Model", i, "failed: ", e$message, "\n") 
                                     return(NULL) })
                               }
                               
                               if(is.null(m)){next}
                               
                               m.sum <- summary(m)
                               
                               m.sum$dev.expl
                               m.sum$r.sq
                               
                               tmp <- data.frame(r_squared = NA, 
                                                 aicc = NA, 
                                                 aic = NA,
                                                 bic = NA,
                                                 formula = NA, 
                                                 response = NA, 
                                                 deviance_expl = NA)
                               
                               
                               tmp <- tmp %>% 
                                 mutate(
                                   r_squared = m.sum$r.sq, 
                                   aicc = AICc(m), 
                                   aic = AIC(m),
                                   bic = BIC(m),
                                   formula = guide.sub[i,]$formula, 
                                   response = guide.sub[i,]$response, 
                                   mod_spec_tier = guide.sub[i,]$mod_spec_tier,
                                   deviance_expl = m.sum$dev.expl
                                 )
                               
                               
                               res <- rbind(res, tmp)
                               
                               print(paste0(i, "/", nrow(guide.sub), " model spec: ", mod_spec_tier))
                             }
                             
                             
                             bm.spec <- res %>% filter(mod_spec_tier == mod.spec.tier) %>% slice_min(aicc) %>% unique()
                             
                             if(grepl("q95", mod_spec_tier)){
                               m <- qgam(as.formula(bm.spec$formula), data = dt.sub, qu = 0.95)
                             }
                             
                             if(grepl("regular", mod_spec_tier)){
                               m <- gam(as.formula(bm.spec$formula), data = dt.sub, select = TRUE, method = "REML")
                             }
                             
                             if(grepl("q05", mod_spec_tier)){
                               m <- qgam(as.formula(bm.spec$formula), data = dt.mod, qu = 0.05)
                             }
                             
                             tidy.m <- broom::tidy(m, parametric = FALSE) %>% 
                               mutate(term = gsub("s\\(", "", term), 
                                      term = gsub("\\)", "", term))
                             
                             var.names <- tidy.m %>% dplyr::select(term) %>% filter(!grepl("ntercept", term)) 
                             
                             ### loop through vars and get pred <
                             
                             for(j in 1:nrow(var.names)){
                               
                               var <- var.names[j,] %>% pull()
                               
                               marg.tmp <- pdplot(model = m, response = filter.resp, 
                                                  var = var,
                                                  data = dt.sub, newdata = dt.sub %>% dplyr::select(-c(all_of(filter.resp))), 
                                                  dt = TRUE)
                               
                               
                               var.clean <- gsub("_scaled", "", var)
                               var.clean <- gsub("log_", "", var.clean)
                               
                               marg.tmp <- marg.tmp %>% mutate(term = var,
                                                               clean_var = gsub("log_", "", term), 
                                                               clean_var = gsub("_scaled", "", clean_var)) %>% rename(var_value = paste0(var.clean))
                               if(j==1){
                                 marg <- marg.tmp}else{
                                   marg <- rbind(marg, marg.tmp)}
                             }
                             
                             dt.plot <- marg %>% left_join(tidy.m) %>% mutate(sig = ifelse(p.value < 0.05, "significant", "non significant"))
                             
                             var.names.c <- var.names %>% mutate(term = gsub("_scaled", "", term),
                                                                 term <- gsub("log_", "", term)) %>% pull()
                             
                             dt.mod.long <- dt.sub %>% dplyr::select(all_of(unique(dt.plot$clean_var)), all_of(filter.resp), Biome) %>% 
                               pivot_longer(cols = c(all_of(var.names.c)), names_to = "clean_var", values_to = "var_value")
                             
                             if(use.clean.names == "yes"){
                               
                               dt.mod.long <- dt.mod.long %>% mutate(
                                 clean_var = case_when(
                                   clean_var == 'CW_max_species_body_mass' ~ 'Max body mass (kg; cwm)', 
                                   clean_var == 'CW_mean_species_body_mass' ~ 'Mean body mass (kg; cwm)', 
                                   clean_var == 'MAT' ~ 'MAT', 
                                   clean_var == 'MAP' ~ 'MAP', 
                                   clean_var == 'n_herbi_sp_reserve' ~ 'Herbivore species richness', 
                                   clean_var == 'herbi_fun_red' ~ 'Herbivore functional redundancy', 
                                   clean_var == 'herbi_fun_div_distq1' ~ 'Herbivore functional diversity', 
                                   clean_var == 'herbi_fun_ent' ~ 'Herbivore functional groups', 
                                   clean_var == 'browser_biomass_ha' ~ 'Browser biomass (kg/ha)', 
                                   clean_var == 'grazer_biomass_ha' ~ 'Grazer biomass (kg/ha)', 
                                   clean_var == 'mixed_feeder_biomass_ha' ~ 'Mixed feeder biomass (kg/ha)', 
                                   clean_var == 'herbi_biomass_ha' ~ 'Herbivore biomass (kg/ha)', 
                                   clean_var == 'elephant_biomass_ha' ~ 'Elephant biomass (kg/ha)', 
                                   clean_var == 'fire_events_since_2001' ~ 'Fire frequency', 
                                   clean_var == 'prop_burned_area' ~ 'Proportion burned area', 
                                   clean_var == 'n_deposition' ~ 'N deposition', 
                                   clean_var == 'tree_cover_mean' ~ 'Average tree cover',
                                   clean_var == 'elevation_sd_1000' ~ 'Elevation SD',
                                   TRUE ~ clean_var)
                               )
                               
                               dt.plot <- dt.plot %>% mutate(
                                 clean_var = case_when(
                                   clean_var == 'CW_max_species_body_mass' ~ 'Max body mass (kg; cwm)', 
                                   clean_var == 'CW_mean_species_body_mass' ~ 'Mean body mass (kg; cwm)', 
                                   clean_var == 'MAT' ~ 'MAT', 
                                   clean_var == 'MAP' ~ 'MAP', 
                                   clean_var == 'herbi_fun_red' ~ 'Herbivore functional redundancy', 
                                   clean_var == 'n_herbi_sp_reserve' ~ 'Herbivore species richness', 
                                   clean_var == 'herbi_fun_div_distq1' ~ 'Herbivore functional diversity', 
                                   clean_var == 'herbi_fun_ent' ~ 'Herbivore functional groups', 
                                   clean_var == 'browser_biomass_ha' ~ 'Browser biomass (kg/ha)', 
                                   clean_var == 'grazer_biomass_ha' ~ 'Grazer biomass (kg/ha)', 
                                   clean_var == 'mixed_feeder_biomass_ha' ~ 'Mixed feeder biomass (kg/ha)', 
                                   clean_var == 'herbi_biomass_ha' ~ 'Herbivore biomass (kg/ha)', 
                                   clean_var == 'elephant_biomass_ha' ~ 'Elephant biomass (kg/ha)', 
                                   clean_var == 'fire_events_since_2001' ~ 'Fire frequency', 
                                   clean_var == 'prop_burned_area' ~ 'Proportion burned area', 
                                   clean_var == 'n_deposition' ~ 'N deposition', 
                                   clean_var == 'tree_cover_mean' ~ 'Average tree cover',
                                   clean_var == 'elevation_sd_1000' ~ 'Elevation SD',
                                   TRUE ~ clean_var)
                               )
                               
                               ylabel <- case_when(
                                 filter.resp == "tree_cover_mean" ~ "Tree cover", 
                                 filter.resp == "distance_to_max_tc" ~ "Dist. to pot. tree cover", 
                                 filter.resp == "woody_cover_trend_venter2019" ~ "Woody cover trend", 
                                 filter.resp == "tree_cover_sd_100" ~ "Tree cover SD",
                                 filter.resp == "canopy_height_sd_100" ~ "Canopy height SD",
                                 
                                 filter.resp == "distance_to_max_tree_cover_sd_100" ~ "Dist. to pot. tree cover SD"
                               )
                               
                               
                             }else{ylabel = filter.resp}
                             
                             pal = c("Albany Thicket" = "#433E85FF", "Azonal Vegetation" = "#38598CFF", "Savanna" = "#C2DF23FF", 
                                     "Forests" = "#25858EFF", "Grassland" = "#2BB07FFF", 
                                     "Nama-Karoo" = "#85D54AFF", "Indian Ocean Coastal Belt" = "#51C56AFF", 
                                     "Desert" = "#2D708EFF", "Succulent Karoo" = "#FDE725FF", 
                                     "Fynbos" = "#1E9B8AFF")
                             
                             
                             build.pdp <- function(dt_plot, dt_mod_long, filter_resp, clean_var, y_label, pal) {
                               pdp <- ggplot() +
                                 geom_point(data = dt_mod_long, aes(x = var_value, y = .data[[filter_resp]], color = Biome)) +
                                 geom_ribbon(data = dt_plot, aes(x = var_value, ymin = ci.lb, ymax = ci.ub, alpha = sig)) +
                                 geom_line(data = dt_plot, aes(x = var_value, y = fit, linewidth = sig, linetype = sig), color = "black") +
                                 scale_linewidth_manual(values = c("significant" = 1.4, "non significant" = 0.6)) +
                                 scale_linetype_manual(values = c("significant" = "solid", "non significant" = "dashed")) +
                                 scale_alpha_manual(values = c("significant" = 0.5, "non significant" = 0.3, "NA" = 0.1)) +
                                 facet_wrap(~ clean_var, scales = "free_x", ncol = 4) +
                                 scale_color_manual(values = pal) +
                                 labs(y = y_label, x = "") +
                                 guides(linewidth = "none", alpha = "none", linetype = "none", color = guide_legend(override.aes = list(size = 4))) +
                                 theme(legend.background = element_blank(), legend.position = "none", legend.key.size = unit(0.5, "cm")) +
                                 theme_bw()
                               
                               return(pdp)
                             }
                             
                             pdp <- build.pdp(dt_plot = dt.plot, dt_mod_long = dt.mod.long, filter_resp = filter.resp, y_label = ylabel, pal = pal)
                             print(pdp)
                             
                             
                             out <- ggplot_build(pdp)
                             n.panels <- length(levels(out$data[[1]]$PANEL))
                             
                             
                             plot.width = ifelse(n.panels >= 4, 12, n.panels*3 +1 )
                             plot.height <- ceiling(n.panels/4)*3
                             
                             
                             
                             filename <- paste0("builds/plots/july/qgam_res/plot_", mod.spec.tier, ".png")
                             ggsave(plot = pdp, filename = filename, dpi = 600, height = plot.height, width = plot.width)
                             filename.rds <- paste0("builds/plots/july/qgam_res/plot_", mod.spec.tier, ".Rds")
                             
                           #  saveRDS(pdp, filename.rds)
                             
                             tmp.p <- data.table(filename = filename.rds, response = filter.resp, mod_spec_tier = mod.spec.tier)
                             plot.paths <- rbind(plot.paths, tmp.p)
                             
                             res.list <- list(res = res, plot.paths = plot.paths)
                             return(res.list)
                           }
stopCluster(clust)
print("loop done")
toc()

res <- foreach.results$res %>% unique()
plot.paths <- foreach.results$plot.paths %>% unique()
saveRDS(res, "builds/plots/july/qgam_res/model_results.Rds")

fwrite(plot.paths, "builds/plots/july/qgam_res/plot_paths.csv")

stop(pb)

