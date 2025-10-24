#### Make interaction plots ####
library("hstats")
library(caret)
library(caretEnsemble)
library(data.table)
library(tidyverse)
library(gridExtra)
library(scico)
library(patchwork)

source("R/functions/plot_interaction_tile.R")

dt <- fread("data/clean_data/final_reserve_data.csv") %>% 
  filter(complete.cases(across(
    c(woody_cover_change, venter_woody_cover_trend, woody_cover_sd_ha_coef, woody_cover_sd_km_coef, 
      mat_change, prec_change, n_deposition, 
      CW_mean_species_body_mass, herbi_fun_div_distq1, n_herbi_sp_reserve,
      grazer_biomass_ha, browser_biomass_ha, 
      herbi_biomass_ha, fire_frequency, burned_area_coef)
  ))) 

dt_high_biomass <- dt %>% filter(herbi_biomass_kgkm2 > 7500)

dt_var_combs <- data.frame(
  var1 = c(rep("herbi_biomass_kgkm2", 4)),
  var2 = c("n_deposition", 'mat_change', 'prec_change', 'fire_frequency'), 
  var1_name = c(rep("Herbivore biomass (kg/km²)", 4)), 
  var2_name = c("N deposition", "MAT Change", "Precipitation Change", "Fire Frequency")
)

p_empty <- ggplot() + theme_void()
############################ Woody Cover Change #################################

### main -----
m_wcc_main <- readRDS("builds/models/reserve_models/woody_cover_change_main.Rds")

# hstats_wcc_main <- hstats(m_wcc_main, X = dt, v = c('mat_change', 'prec_change', 'mean_prec', 'n_deposition',
#                                           'CW_mean_species_body_mass', 'herbi_fun_div_distq1',
#                                           'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 
#                                           'browser_biomass_kgkm2', 'herbi_biomass_kgkm2', 
#                                           'fire_frequency', 'burned_area_coef'))
# h2_pairwise(hstats_wcc_main)

### plot interactions of 4 most important variables 
list_p_wcc_main <- list()
for(i in 1:nrow(dt_var_combs)){
  
  p <- plot_interaction_tile(m = m_wcc_main, var1 = dt_var_combs[i,]$var1, var2 = dt_var_combs[i,]$var2, 
                             var1_name = dt_var_combs[i,]$var1_name, var2_name = dt_var_combs[i,]$var2_name,
                             data = dt) +
    scale_fill_scico(palette = "bamako", direction = 1, end = 0.9) +
    labs(fill = "Tree\nCover\nChange\n(%/year)") 
  
  list_p_wcc_main[[i]] <- p
}

(p_wcc_main <- patchwork::wrap_plots(list_p_wcc_main, ncol = 4)) +
  plot_annotation(title = "Full Model", theme = theme(plot.title = element_text(hjust = 0.5)))

### high_biomass -----
m_wcc_high_biomass <- readRDS("builds/models/reserve_models/woody_cover_change_high_biomass.Rds")

# hstats_wcc_high_biomass <- hstats(m_wcc_high_biomass, X = dt_high_biomass, v = c('mat_change', 'prec_change', 'mean_prec', 'n_deposition',
#                                                     'CW_mean_species_body_mass', 'herbi_fun_div_distq1',
#                                                     'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 
#                                                     'browser_biomass_kgkm2', 'herbi_biomass_kgkm2', 
#                                                     'fire_frequency', 'burned_area_coef'))
# h2_pairwise(hstats_wcc_high_biomass)

### plot interactions of 4 most important variables 
list_p_wcc_high_biomass <- list()
for(i in 1:nrow(dt_var_combs)){
  
  p <- plot_interaction_tile(m = m_wcc_high_biomass, var1 = dt_var_combs[i,]$var1, var2 = dt_var_combs[i,]$var2, 
                             var1_name = dt_var_combs[i,]$var1_name, var2_name = dt_var_combs[i,]$var2_name,
                             data = dt_high_biomass) +
    scale_fill_scico(palette = "bamako", direction = 1, end = 0.9) +
    labs(fill = "Tree\nCover\nChange\n(%/year)") 
  
  list_p_wcc_high_biomass[[i]] <- p
}

(p_wcc_high_biomass <- (patchwork::wrap_plots(list_p_wcc_high_biomass, ncol = 4) +
  plot_annotation(title = "Reserves With High Biomass (>7500 kg/km²)", theme = theme(plot.title = element_text(hjust = 0.5)))))

p_wcc_main <- grid.arrange(list_p_wcc_main[[1]], list_p_wcc_main[[2]], 
                           list_p_wcc_main[[3]], list_p_wcc_main[[4]], 
                           ncol = 4, top = "Full Model")

p_wcc_high_biomass <- grid.arrange(list_p_wcc_high_biomass[[1]], list_p_wcc_high_biomass[[2]], 
                                   list_p_wcc_high_biomass[[3]], list_p_wcc_high_biomass[[4]], 
                                   ncol = 4, top = "Reserves With High Biomass (>7500 kg/km²)")


p_wcc_comb <- grid.arrange(p_wcc_main, p_empty, p_wcc_high_biomass, heights = c(1, 0.1, 1), ncol = 1)
ggsave(plot = p_wcc_comb, "builds/plots/revision/wcc_interaction_plots.png", 
       dpi = 600, height = 7, width = 16)

############################ Venter's Woody Cover Trend #################################

### main -----
m_vwct_main <- readRDS("builds/models/reserve_models/venter_woody_cover_trend_main.Rds")
#m_vwct_main <- m_vwct_main$models$xgbTree #this one was the most important 

# hstats_vwct_main <- hstats(m_vwct_main, X = dt, v = c('mat_change', 'prec_change', 'mean_prec', 'n_deposition',
#                                                     'CW_mean_species_body_mass', 'herbi_fun_div_distq1',
#                                                     'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 
#                                                     'browser_biomass_kgkm2', 'herbi_biomass_kgkm2', 
#                                                     'fire_frequency', 'burned_area_coef'))
# h2_pairwise(hstats_vwct_main)

### plot interactions of 4 most important variables 
list_p_vwct_main <- list()
for(i in 1:nrow(dt_var_combs)){
  
  p <- plot_interaction_tile(m = m_vwct_main, var1 = dt_var_combs[i,]$var1, var2 = dt_var_combs[i,]$var2, 
                             var1_name = dt_var_combs[i,]$var1_name, var2_name = dt_var_combs[i,]$var2_name,
                             data = dt) +
    scale_fill_scico(palette = "batlow", direction = 1, end = 0.85) +
    labs(fill = "Venter's\nWoody\nCover\nChange\n(%/year)") 
  
  list_p_vwct_main[[i]] <- p
}

(p_vwct_main <- patchwork::wrap_plots(list_p_vwct_main, ncol = 4)) +
  plot_annotation(title = "Full Model", theme = theme(plot.title = element_text(hjust = 0.5)))

### high_biomass -----
m_vwct_high_biomass <- readRDS("builds/models/reserve_models/venter_woody_cover_trend_high_biomass.Rds")

# hstats_vwct_high_biomass <- hstats(m_vwct_high_biomass, X = dt_high_biomass, v = c('mat_change', 'prec_change', 'mean_prec', 'n_deposition',
#                                                                                  'CW_mean_species_body_mass', 'herbi_fun_div_distq1',
#                                                                                  'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 
#                                                                                  'browser_biomass_kgkm2', 'herbi_biomass_kgkm2', 
#                                                                                  'fire_frequency', 'burned_area_coef'))
# h2_pairwise(hstats_vwct_high_biomass)

### plot interactions of 4 most important variables 
list_p_vwct_high_biomass <- list()
for(i in 1:nrow(dt_var_combs)){
  
  p <- plot_interaction_tile(m = m_vwct_high_biomass, var1 = dt_var_combs[i,]$var1, var2 = dt_var_combs[i,]$var2, 
                             var1_name = dt_var_combs[i,]$var1_name, var2_name = dt_var_combs[i,]$var2_name,
                             data = dt_high_biomass) +
    scale_fill_scico(palette = "batlow", direction = 1, end = 0.85) +
    labs(fill = "Venter's\nWoody\nCover\nChange\n(%/year)") 
  
  list_p_vwct_high_biomass[[i]] <- p
}

(p_vwct_high_biomass <- (patchwork::wrap_plots(list_p_vwct_high_biomass, ncol = 4) +
                          plot_annotation(title = "Reserves With High Biomass (>7500 kg/km²)", theme = theme(plot.title = element_text(hjust = 0.5)))))

p_vwct_main <- grid.arrange(list_p_vwct_main[[1]], list_p_vwct_main[[2]], 
                           list_p_vwct_main[[3]], list_p_vwct_main[[4]], 
                           ncol = 4, top = "Full Model")

p_vwct_high_biomass <- grid.arrange(list_p_vwct_high_biomass[[1]], list_p_vwct_high_biomass[[2]], 
                                   list_p_vwct_high_biomass[[3]], list_p_vwct_high_biomass[[4]], 
                                   ncol = 4, top = "Reserves With High Biomass (>7500 kg/km²)")


p_vwct_comb <- grid.arrange(p_vwct_main, p_empty, p_vwct_high_biomass, heights = c(1, 0.1, 1), ncol = 1)
ggsave(plot = p_vwct_comb, "builds/plots/revision/vwct_interaction_plots.png", 
       dpi = 600, height = 7, width = 16)


############################ Woody Cover Heterogeneity Trend ha scale #################################

### main -----
m_wcsdha_main <- readRDS("builds/models/reserve_models/woody_cover_sd_ha_coef_main.Rds")
#m_wcsdha_main <- m_wcsdha_main$models$xgbTree #this one was the most important 

# hstats_wcsdha_main <- hstats(m_wcsdha_main, X = dt, v = c('mat_change', 'prec_change', 'mean_prec', 'n_deposition',
#                                                       'CW_mean_species_body_mass', 'herbi_fun_div_distq1',
#                                                       'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 
#                                                       'browser_biomass_kgkm2', 'herbi_biomass_kgkm2', 
#                                                       'fire_frequency', 'burned_area_coef'))
# h2_pairwise(hstats_wcsdha_main)

### plot interactions of 4 most important variables 
list_p_wcsdha_main <- list()
for(i in 1:nrow(dt_var_combs)){
  
  p <- plot_interaction_tile(m = m_wcsdha_main, var1 = dt_var_combs[i,]$var1, var2 = dt_var_combs[i,]$var2, 
                             var1_name = dt_var_combs[i,]$var1_name, var2_name = dt_var_combs[i,]$var2_name,
                             data = dt) +
    scale_fill_scico(palette = "lajolla", direction = 1, end = 0.9) +
    labs(fill = "Tree\nCover\nHetero-\ngeneity\nChange") 
  
  list_p_wcsdha_main[[i]] <- p
}

(p_wcsdha_main <- patchwork::wrap_plots(list_p_wcsdha_main, ncol = 4)) +
  plot_annotation(title = "Full Model", theme = theme(plot.title = element_text(hjust = 0.5)))

### high_biomass -----
m_wcsdha_high_biomass <- readRDS("builds/models/reserve_models/venter_woody_cover_trend_high_biomass.Rds")

# hstats_wcsdha_high_biomass <- hstats(m_wcsdha_high_biomass, X = dt_high_biomass, v = c('mat_change', 'prec_change', 'mean_prec', 'n_deposition',
#                                                                                    'CW_mean_species_body_mass', 'herbi_fun_div_distq1',
#                                                                                    'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 
#                                                                                    'browser_biomass_kgkm2', 'herbi_biomass_kgkm2', 
#                                                                                    'fire_frequency', 'burned_area_coef'))
# h2_pairwise(hstats_wcsdha_high_biomass)

### plot interactions of 4 most important variables 
list_p_wcsdha_high_biomass <- list()
for(i in 1:nrow(dt_var_combs)){
  
  p <- plot_interaction_tile(m = m_wcsdha_high_biomass, var1 = dt_var_combs[i,]$var1, var2 = dt_var_combs[i,]$var2, 
                             var1_name = dt_var_combs[i,]$var1_name, var2_name = dt_var_combs[i,]$var2_name,
                             data = dt_high_biomass) +
    scale_fill_scico(palette = "lajolla", direction = 1, end = 0.9) +
    labs(fill = "Tree\nCover\nHetero-\ngeneity\nChange") 
  
  list_p_wcsdha_high_biomass[[i]] <- p
}

(p_wcsdha_high_biomass <- (patchwork::wrap_plots(list_p_wcsdha_high_biomass, ncol = 4) +
                           plot_annotation(title = "Reserves With High Biomass (>7500 kg/km²)", theme = theme(plot.title = element_text(hjust = 0.5)))))

p_wcsdha_main <- grid.arrange(list_p_wcsdha_main[[1]], list_p_wcsdha_main[[2]], 
                            list_p_wcsdha_main[[3]], list_p_wcsdha_main[[4]], 
                            ncol = 4, top = "Full Model")

p_wcsdha_high_biomass <- grid.arrange(list_p_wcsdha_high_biomass[[1]], list_p_wcsdha_high_biomass[[2]], 
                                    list_p_wcsdha_high_biomass[[3]], list_p_wcsdha_high_biomass[[4]], 
                                    ncol = 4, top = "Reserves With High Biomass (>7500 kg/km²)")


p_wcsdha_comb <- grid.arrange(p_wcsdha_main, p_empty, p_wcsdha_high_biomass, heights = c(1, 0.1, 1), ncol = 1)
ggsave(plot = p_wcsdha_comb, "builds/plots/revision/wcsdha_interaction_plots.png", 
       dpi = 600, height = 7, width = 16)


############################ Woody Cover Heterogeneity Trend km scale #################################

### main -----
m_wcsdkm_main <- readRDS("builds/models/reserve_models/woody_cover_sd_km_coef_main.Rds")
#m_wcsdkm_main <- m_wcsdkm_main$models$xgbTree #this one was the most important 

# hstats_wcsdkm_main <- hstats(m_wcsdkm_main, X = dt, v = c('mat_change', 'prec_change', 'mean_prec', 'n_deposition',
#                                                           'CW_mean_species_body_mass', 'herbi_fun_div_distq1',
#                                                           'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 
#                                                           'browser_biomass_kgkm2', 'herbi_biomass_kgkm2', 
#                                                           'fire_frequency', 'burned_area_coef'))
# h2_pairwise(hstats_wcsdkm_main)

### plot interactions of 4 most important variables 
list_p_wcsdkm_main <- list()
for(i in 1:nrow(dt_var_combs)){
  
  p <- plot_interaction_tile(m = m_wcsdkm_main, var1 = dt_var_combs[i,]$var1, var2 = dt_var_combs[i,]$var2, 
                             var1_name = dt_var_combs[i,]$var1_name, var2_name = dt_var_combs[i,]$var2_name,
                             data = dt) +
    scale_fill_scico(palette = "lajolla", direction = 1, end = 0.9) +
    labs(fill = "Tree\nCover\nHetero-\ngeneity\nChange") 
  
  list_p_wcsdkm_main[[i]] <- p
}

(p_wcsdkm_main <- patchwork::wrap_plots(list_p_wcsdkm_main, ncol = 4)) +
  plot_annotation(title = "Full Model", theme = theme(plot.title = element_text(hjust = 0.5)))

### high_biomass -----
m_wcsdkm_high_biomass <- readRDS("builds/models/reserve_models/venter_woody_cover_trend_high_biomass.Rds")

# hstats_wcsdkm_high_biomass <- hstats(m_wcsdkm_high_biomass, X = dt_high_biomass, v = c('mat_change', 'prec_change', 'mean_prec', 'n_deposition',
#                                                                                        'CW_mean_species_body_mass', 'herbi_fun_div_distq1',
#                                                                                        'n_herbi_sp_reserve', 'grazer_biomass_kgkm2', 
#                                                                                        'browser_biomass_kgkm2', 'herbi_biomass_kgkm2', 
#                                                                                        'fire_frequency', 'burned_area_coef'))
# h2_pairwise(hstats_wcsdkm_high_biomass)

### plot interactions of 4 most important variables 
list_p_wcsdkm_high_biomass <- list()
for(i in 1:nrow(dt_var_combs)){
  
  p <- plot_interaction_tile(m = m_wcsdkm_high_biomass, var1 = dt_var_combs[i,]$var1, var2 = dt_var_combs[i,]$var2, 
                             var1_name = dt_var_combs[i,]$var1_name, var2_name = dt_var_combs[i,]$var2_name,
                             data = dt_high_biomass) +
    scale_fill_scico(palette = "lajolla", direction = 1, end = 0.9) +
    labs(fill = "Tree\nCover\nHetero-\ngeneity\nChange") 
  
  list_p_wcsdkm_high_biomass[[i]] <- p
}

(p_wcsdkm_high_biomass <- (patchwork::wrap_plots(list_p_wcsdkm_high_biomass, ncol = 4) +
                             plot_annotation(title = "Reserves With High Biomass (>7500 kg/km²)", theme = theme(plot.title = element_text(hjust = 0.5)))))

p_wcsdkm_main <- grid.arrange(list_p_wcsdkm_main[[1]], list_p_wcsdkm_main[[2]], 
                              list_p_wcsdkm_main[[3]], list_p_wcsdkm_main[[4]], 
                              ncol = 4, top = "Full Model")

p_wcsdkm_high_biomass <- grid.arrange(list_p_wcsdkm_high_biomass[[1]], list_p_wcsdkm_high_biomass[[2]], 
                                      list_p_wcsdkm_high_biomass[[3]], list_p_wcsdkm_high_biomass[[4]], 
                                      ncol = 4, top = "Reserves With High Biomass (>7500 kg/km²)")


p_wcsdkm_comb <- grid.arrange(p_wcsdkm_main, p_empty, p_wcsdkm_high_biomass, heights = c(1, 0.1, 1), ncol = 1)
ggsave(plot = p_wcsdkm_comb, "builds/plots/revision/wcsdkm_interaction_plots.png", 
       dpi = 600, height = 7, width = 16)

