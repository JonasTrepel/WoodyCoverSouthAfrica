## data plots 
library(tidyverse)
library(data.table)

dt <- fread("data/ReserveDataSouthAfricaFinal.csv")

cor.test(dt$MAP, dt$MAT)


dt_cwc <- dt %>% 
  rename(`Mean body mass (CWM; kg)` = CW_mean_species_body_mass, 
         `Elephant biomass (kg/km2)` = elephant_biomass_kgkm2, 
         `Browser biomass (kg/km2)` = browser_biomass_kgkm2, 
         `Grazer biomass (kg/km2)` = grazer_biomass_kgkm2, 
         `Mixed feeder\nbiomass (kg/km2)` = mixed_feeder_biomass_ha, 
         `Herbivore\nbiomass (kg/km2)` = herbi_biomass_kgkm2, 
         `N herbivore species` = n_herbi_sp_reserve, 
         `Functional diversity` = herbi_fun_div_distq1, 
         `Proportion burned area` = prop_burned_area,
         `Elevation SD` = elevation_sd_1000, 
         `MAT (°C)` = MAT, 
         `MAP (mm)` = MAP, 
         `Fire frequency` = fire_events_since_2001, 
         `N deposition` = n_deposition, 
         `Woody cover change` = woody_cover_trend_venter2019, 
         `Woody cover` = tree_cover_mean, 
         `Woody cover SD 100m` = tree_cover_sd_100) %>% 
  pivot_longer(
  cols = c(`Herbivore\nbiomass (kg/km2)`, `MAT (°C)`,  `MAP (mm)`,
           `Fire frequency`, `Mean body mass (CWM; kg)`, `Proportion burned area`), 
  names_to = "predictor_name", 
  values_to = "predictor_value"
)

p1 <- dt_cwc %>% 
  ggplot() +
  geom_point(aes(x = predictor_value, y = `Woody cover`)) +
  facet_wrap(~predictor_name, scales = "free") + 
  labs(x = "", y = "Woody Cover (%)")
p1
ggsave(plot = p1, "builds/plots/september/raw_data_plot_cwc.png", dpi = 600, height = 5)


dt_wcc <- dt %>% 
  rename(`Mean body mass (CWM)` = CW_mean_species_body_mass, 
         `Elephant biomass (kg/km2)` = elephant_biomass_kgkm2, 
         `Browser biomass (kg/km2)` = browser_biomass_kgkm2, 
         `Grazer biomass (kg/km2)` = grazer_biomass_kgkm2, 
         `Mixed feeder\nbiomass (kg/km2)` = mixed_feeder_biomass_ha, 
         `Herbivore\nbiomass (kg/km2)` = herbi_biomass_kgkm2, 
         `N herbivore species` = n_herbi_sp_reserve, 
         `Herbivore\nFunctional diversity` = herbi_fun_div_distq1, 
         `Proportion burned area` = prop_burned_area,
         `Elevation SD` = elevation_sd_1000, 
         `MAT (°C)` = MAT, 
         `MAP (mm)` = MAP, 
         `Fire frequency` = fire_events_since_2001, 
         `N deposition` = n_deposition, 
         `Woody cover change` = woody_cover_trend_venter2019, 
         `Woody cover` = tree_cover_mean, 
         `Woody cover SD 100m` = tree_cover_sd_100) %>% 
  pivot_longer(
    cols = c(`Herbivore\nbiomass (kg/km2)`, `MAT (°C)`,  `MAP (mm)`,
             `Fire frequency`, `Mean body mass (CWM)`, `Proportion burned area`, 
             `N deposition`, `Herbivore\nFunctional diversity`, `N herbivore species`, 
             `Browser biomass (kg/km2)`, `Grazer biomass (kg/km2)`, `Mixed feeder\nbiomass (kg/km2)`
             ), 
    names_to = "predictor_name", 
    values_to = "predictor_value"
  )

p2 <- dt_wcc %>% 
  ggplot() +
  geom_point(aes(x = predictor_value, y = `Woody cover change`)) +
  facet_wrap(~predictor_name, scales = "free") + 
  labs(x = "", y = "Woody Cover Change (%/year)")
p2

ggsave(plot = p2, "builds/plots/september/raw_data_plot_wcc.png", dpi = 600)


dt_wcsd <- dt %>% 
  rename(`Mean body mass (CWM)` = CW_mean_species_body_mass, 
         `Elephant biomass (kg/km2)` = elephant_biomass_kgkm2, 
         `Browser biomass (kg/km2)` = browser_biomass_kgkm2, 
         `Grazer biomass (kg/km2)` = grazer_biomass_kgkm2, 
         `Mixed feeder\nbiomass (kg/km2)` = mixed_feeder_biomass_ha, 
         `Herbivore\nbiomass (kg/km2)` = herbi_biomass_kgkm2, 
         `N herbivore species` = n_herbi_sp_reserve, 
         `Herbivore\nFunctional diversity` = herbi_fun_div_distq1, 
         `Proportion burned area` = prop_burned_area,
         `Elevation SD` = elevation_sd_1000, 
         `MAT (°C)` = MAT, 
         `MAP (mm)` = MAP, 
         `Fire frequency` = fire_events_since_2001, 
         `N deposition` = n_deposition, 
         `Woody cover change` = woody_cover_trend_venter2019, 
         `Woody cover` = tree_cover_mean, 
         `Woody cover SD 100m` = tree_cover_sd_100) %>% 
  pivot_longer(
    cols = c(`Herbivore\nbiomass (kg/km2)`, `MAT (°C)`,  `MAP (mm)`,
             `Fire frequency`, `Mean body mass (CWM)`, `Proportion burned area`, 
             `Elevation SD`, `Herbivore\nFunctional diversity`, `N herbivore species`, 
             `Browser biomass (kg/km2)`, `Grazer biomass (kg/km2)`, `Mixed feeder\nbiomass (kg/km2)`
    ), 
    names_to = "predictor_name", 
    values_to = "predictor_value"
  )

p3 <- dt_wcsd %>% 
  ggplot() +
  geom_point(aes(x = predictor_value, y = `Woody cover SD 100m`)) +
  facet_wrap(~predictor_name, scales = "free") + 
  labs(x = "", y = "Woody Cover Heterogeneity")
p3


ggsave(plot = p3, "builds/plots/september/raw_data_plot_wcsd.png", dpi = 600)

### scatterplot rainfall N depo


p <- dt %>% ggplot() +
  geom_point(aes(x = MAP, y = n_deposition), alpha = 0.75) +
  labs(x = "MAP (mm)", y = "N deposition (kg/km2/year)") +
  theme_classic()

ggsave(plot = p, "builds/plots/september/map_vs_ndepo.png", dpi = 600, height = 4, width = 4) 
