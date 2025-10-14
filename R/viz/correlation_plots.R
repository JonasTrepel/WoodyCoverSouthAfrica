### correlation plots etc. 
library(tidyverse)
library(data.table)
library(gridExtra)


dt <- fread("data/clean_data/final_reserve_data.csv") %>% 
  filter(complete.cases(across(
    c(woody_cover_change, venter_woody_cover_trend, woody_cover_sd_ha_coef, woody_cover_sd_km_coef, 
      mat_coef, prec_coef, n_deposition, 
      CW_mean_species_body_mass, herbi_fun_div_distq1, n_herbi_sp_reserve,
      grazer_biomass_ha, browser_biomass_ha, 
      herbi_biomass_ha, fire_frequency, burned_area_coef)
  )))
quantile(dt$herbi_biomass_kgkm2, na.rm = T)



nrow(dt[herbi_biomass_kgkm2 > 7500, ])

nrow(dt[herbi_biomass_kgkm2 > 10000 & Biome == "Savanna", ])/nrow(dt[Biome == "Savanna", ])


#### 

names(dt)
dt_long <- dt %>% 
  filter(!is.na(woody_cover_change)) %>% 
  mutate(woody_trend_yn = ifelse(woody_cover_change > 0, "increasing", "decreasing")) %>% 
  rename(`Mean body mass (CWM)` = CW_mean_species_body_mass, 
         `Browser biomass` = browser_biomass_ha, 
         `Grazer biomass` = grazer_biomass_ha, 
         `Mixed feeder biomass` = mixed_feeder_biomass_ha, 
         `Herbivore biomass` = herbi_biomass_ha, 
         `N herbivore species` = n_herbi_sp_reserve, 
         `Functional diversity` = herbi_fun_div_distq1, 
         `Burned area trend` = burned_area_coef,
         `MAT trend` = mat_coef, 
         `MAP` = mean_prec, 
         `MAP trend` = prec_coef, 
         `Fire frequency` = fire_frequency, 
         `N deposition` = n_deposition, 
         `Woody cover change` = woody_cover_change, 
         `Venter woody cover change` = venter_woody_cover_trend, 
         `Woody cover SD 100m trend` = woody_cover_sd_ha_coef, 
         `Woody cover SD 1000m trend` = woody_cover_sd_km_coef
  ) %>% 
  pivot_longer(cols = c(`Mean body mass (CWM)`, `Browser biomass`, `Grazer biomass`, `Mixed feeder biomass`,
                        `Herbivore biomass`,`N herbivore species`, `Functional diversity`,
                        `Burned area trend`, `Fire frequency`, MAP, `MAT trend`, `MAP trend`, `N deposition`), 
               names_to = "var", values_to = "values") %>%
  mutate(values = ifelse(is.infinite(values), NA, values)) %>% filter(!is.na(values))

library(ggridges)

p_ridge <- ggplot() +
  geom_density_ridges_gradient(data = dt_long, aes(x = values, y = woody_trend_yn, fill = woody_trend_yn), color = "white") +
  scale_fill_viridis_d(option = "H") +
  facet_wrap(~ var, scales = "free_x") +
  labs(y = "Woody cover change", x = "") +
  theme_bw() +
  theme(legend.position = "none")
p_ridge

ggsave(plot = p_ridge, "builds/plots/revision/supplement/woody_cover_trend_ridges.png", dpi = 600)



### biomes vs environmental factors --------------------

dt$Biome <-  reorder(dt$Biome, dt$mean_prec, FUN = mean)

bp1 <- ggplot(data = dt, aes(x = Biome, y = mean_prec)) + 
  geom_jitter(aes(color = Biome), size = 2, alpha = 0.7) +
  geom_boxplot(aes(color = Biome), size = 1, alpha = 0.7) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "", y = "MAP") +
  #geom_smooth(method = "lm") +
  guides(size = "none", color = guide_legend(override.aes = list(size = 1.5))) +
  theme(legend.background = element_blank(), legend.position = "none", axis.text.x = element_text(angle = 25, hjust = 1))
bp1

dt$Biome <-  reorder(dt$Biome, dt$mean_mat, FUN = mean)

bp2 <- ggplot(data = dt, aes(x = Biome, y = mean_mat)) + 
  geom_jitter(aes(color = Biome), size = 2, alpha = 0.7) +
  geom_boxplot(aes(color = Biome), size = 1, alpha = 0.7) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "", y = "MAT") +
  #geom_smooth(method = "lm") +
  guides(size = "none", color = guide_legend(override.aes = list(size = 1.5))) +
  theme(legend.background = element_blank(), legend.position = "none", axis.text.x = element_text(angle = 25, hjust = 1))
bp2

dt$Biome <-  reorder(dt$Biome, dt$elevation, FUN = mean)

bp3 <- ggplot(data = dt, aes(x = Biome, y = elevation)) + 
  geom_jitter(aes(color = Biome), size = 2, alpha = 0.7) +
  geom_boxplot(aes(color = Biome), size = 1, alpha = 0.7) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "", y = "Elevation") +
  #geom_smooth(method = "lm") +
  guides(size = "none", color = guide_legend(override.aes = list(size = 1.5))) +
  theme(legend.background = element_blank(), legend.position = "none", axis.text.x = element_text(angle = 25, hjust = 1))
bp3


bp <- grid.arrange(bp1, bp2, bp3, ncol = 3)
ggsave(plot = bp, "builds/plots/revision/supplement/environmental_biome.png", dpi = 600, height = 4, width = 8)


### variable correlation --------------------------------------
dt$herb
names(dt)
dt.corr <- dt %>% 
  dplyr::select(c(CW_mean_species_body_mass, browser_biomass_ha,
                  grazer_biomass_ha, #mixed_feeder_biomass_ha,
                  herbi_biomass_ha, burned_area_coef,
                  n_herbi_sp_reserve, herbi_fun_div_distq1, 
                  mat_coef, prec_coef, mean_mat, mean_prec, 
                  fire_frequency, n_deposition, woody_cover_change,
                  venter_woody_cover_trend, woody_cover_sd_ha_coef, woody_cover_sd_km_coef
                  )) %>% 
  rename(`Mean body mass (CWM)` = CW_mean_species_body_mass, 
         `Browser biomass` = browser_biomass_ha, 
         `Grazer biomass` = grazer_biomass_ha, 
       #  `Mixed feeder biomass` = mixed_feeder_biomass_ha, 
         `Herbivore biomass` = herbi_biomass_ha, 
         `N herbivore species` = n_herbi_sp_reserve, 
         `Herbivore Functional diversity` = herbi_fun_div_distq1, 
         `Burned area trend` = burned_area_coef,
         `MAT trend` = mat_coef, 
         `MAP trend` = prec_coef, 
         `MAT (°C)` = mean_mat, 
         `MAP (mm)` = mean_prec, 
         `Fire frequency` = fire_frequency, 
         `N deposition` = n_deposition, 
         `Woody cover change` = woody_cover_change, 
         `Venter woody cover change` = venter_woody_cover_trend, 
         `Woody cover SD 100m trend` = woody_cover_sd_ha_coef, 
         `Woody cover SD 1000m trend` = woody_cover_sd_km_coef
  ) %>% filter(complete.cases(.))


corr <- round(cor(dt.corr), 1)
head(corr[, 1:6])
# argument lab = TRUE
library(ggcorrplot)
cp <- ggcorrplot(corr, hc.order = TRUE, type = "lower", colors = c("#6D9EC1", "white", "#E46726"), lab = TRUE)
cp
ggsave(plot = cp, "builds/plots/revision/supplement/variable_corrplot.png", dpi = 600, width = 11, height = 11)
           


#### Biome - N deposition ---------------------

dt[, mean_n_dep := mean(n_deposition, na.rm = T), by = Biome]
dt.plot <- dt %>% mutate(Biome = fct_reorder(Biome, mean_n_dep))

n.dep.biome <- ggplot() +
  geom_boxplot(data = dt.plot, aes(x = Biome, y = n_deposition), outliers = FALSE, fill = "white", linewidth = 1.1) +
  geom_jitter(data = dt.plot, aes(x = Biome, y = n_deposition), width = 0.2, alpha = 0.3) +
  theme_classic() +
  labs(y = "N deposition") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
n.dep.biome
ggsave(plot = n.dep.biome, "builds/plots/revision/supplement/n_dep_per_biome.png", dpi = 600, height = 5, width = 10)

  