## data plots 
library(tidyverse)
library(data.table)

dt <- fread("data/clean_data/final_reserve_data.csv") %>% 
  filter(complete.cases(across(
    c(woody_cover_change, venter_woody_cover_trend, woody_cover_sd_ha_coef, woody_cover_sd_km_coef, 
      mat_change, prec_change, n_deposition, 
      CW_mean_species_body_mass, herbi_fun_div_distq1, n_herbi_sp_reserve,
      grazer_biomass_ha, browser_biomass_ha, 
      herbi_biomass_ha, fire_frequency, burned_area_coef)
  ))) 



dt_long <- dt %>% 
  rename(`Mean body mass (CWM)` = CW_mean_species_body_mass, 
         `Browser biomass` = browser_biomass_ha, 
         `Grazer biomass` = grazer_biomass_ha, 
         `Mixed feeder biomass` = mixed_feeder_biomass_ha, 
         `Herbivore biomass` = herbi_biomass_ha, 
         `N herbivore species` = n_herbi_sp_reserve, 
         `Herbivore Functional Div.` = herbi_fun_div_distq1, 
         `Burned area trend` = burned_area_coef,
         `Temperature trend` = mat_change, 
         `Precipitation trend` = prec_change, 
         `Fire frequency` = fire_frequency, 
         `N deposition` = n_deposition, 
         `Woody cover change` = woody_cover_change, 
         `Venter woody cover change` = venter_woody_cover_trend, 
         `Woody cover SD 100m trend` = woody_cover_sd_ha_coef, 
         `Woody cover SD 1000m trend` = woody_cover_sd_km_coef) %>% 
  pivot_longer(
  cols = c(`Mean body mass (CWM)`, `Browser biomass`, `Grazer biomass`, 
           `Herbivore biomass`,`N herbivore species`, `Herbivore Functional Div.`,
           `Burned area trend`, `Fire frequency`, `Temperature trend`, `Precipitation trend`, `N deposition`), 
  names_to = "predictor_name", 
  values_to = "predictor_value"
)

p_wcc <- dt_long %>% 
  ggplot() +
  geom_point(aes(x = predictor_value, y = `Woody cover change`), size = 0.5, alpha = 0.5) +
  facet_wrap(~predictor_name, scales = "free") + 
  labs(x = "", y = "Woody cover change (%/year)") +
  theme(panel.background = element_rect(fill = "linen"))
p_wcc

ggsave(plot = p_wcc, "builds/plots/revision/supplement/raw_data_plot_wcc.png", dpi = 600,
       height = 6, width = 8)


p_vwct <- dt_long %>% 
  ggplot() +
  geom_point(aes(x = predictor_value, y = `Venter woody cover change`), size = 0.5, alpha = 0.5) +
  facet_wrap(~predictor_name, scales = "free") + 
  labs(x = "", y = "Venter's woody cover change (%/year)") +
  theme(panel.background = element_rect(fill = "linen"))
p_vwct

ggsave(plot = p_vwct, "builds/plots/revision/supplement/raw_data_plot_vwct.png", dpi = 600,
       height = 6, width = 8)

p_wcsdkm <- dt_long %>% 
  ggplot() +
  geom_point(aes(x = predictor_value, y = `Woody cover SD 1000m trend`), size = 0.5, alpha = 0.5) +
  facet_wrap(~predictor_name, scales = "free") + 
  labs(x = "", y = "Woody cover heterogeneity trend (km2-scale)") +
  theme(panel.background = element_rect(fill = "linen"))
p_wcsdkm

ggsave(plot = p_wcsdkm, "builds/plots/revision/supplement/raw_data_plot_wcsdkm.png", dpi = 600,
       height = 6, width = 8)

p_wcsdha <- dt_long %>% 
  ggplot() +
  geom_point(aes(x = predictor_value, y = `Woody cover SD 100m trend`), size = 0.5, alpha = 0.5) +
  facet_wrap(~predictor_name, scales = "free") + 
  labs(x = "", y = "Woody cover heterogeneity trend (ha-scale)") +
  theme(panel.background = element_rect(fill = "linen"))
p_wcsdha

ggsave(plot = p_wcsdha, "builds/plots/revision/supplement/raw_data_plot_wcsdha.png", dpi = 600,
       height = 6, width = 8)


### scatterplot rainfall N depo


(p <- dt %>% ggplot() +
  geom_point(aes(x = mean_prec, y = n_deposition), alpha = 0.75, size = 0.75) +
  labs(x = "MAP (mm)", y = "N deposition (kg/km2/year)") +
  theme_classic())
cor.test(dt$mean_prec, dt$n_deposition)
ggsave(plot = p, "builds/plots/september/map_vs_ndepo.png", dpi = 600, height = 4, width = 4) 
