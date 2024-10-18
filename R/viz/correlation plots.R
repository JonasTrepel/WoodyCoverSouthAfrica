### correlation plots etc. 
library(tidyverse)
library(data.table)
library(gridExtra)

dt <- fread("data/ReserveDataSouthAfricaFinal.csv")

table(dt[herbi_biomass_kgkm2 > 10000, ]$Biome)
nrow(dt[herbi_biomass_kgkm2 > 10000, ])
64/94

nrow(dt[herbi_biomass_kgkm2 > 10000 & Biome == "Savanna", ])/nrow(dt[Biome == "Savanna", ])


#### 

dt[woody_cover_trend_venter2019 == 0, ]
names(dt)
dt.long <- dt %>% 
  mutate(woody_trend_yn = ifelse(woody_cover_trend_venter2019 > 0, "increasing", "decreasing")) %>% 
  rename(`Mean body mass (CWM)` = CW_mean_species_body_mass, 
         `Elephant biomass` = elephant_biomass_ha, 
         `Browser biomass` = browser_biomass_ha, 
         `Grazer biomass` = grazer_biomass_ha, 
         `Mixed feeder biomass` = mixed_feeder_biomass_ha, 
         `Herbivore biomass` = herbi_biomass_ha, 
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
         `Woody cover SD 100m` = tree_cover_sd_100, 
         `Canopy height SD 100m` = canopy_height_sd_100, 
         
  ) %>% 
  pivot_longer(cols = c(`Mean body mass (CWM)`,
                        `Elephant biomass`, `Browser biomass`, `Grazer biomass`, `Mixed feeder biomass`,
                        `Herbivore biomass`,`N herbivore species`, `Functional diversity`,
                        `Proportion burned area`, `Fire frequency`, `MAT (°C)`, `MAP (mm)`, `N deposition`), 
               names_to = "var", values_to = "values") %>%
  mutate(values = ifelse(is.infinite(values), NA, values)) %>% filter(!is.na(values))

library(ggridges)

p.ridge <- ggplot() +
  geom_density_ridges_gradient(data = dt.long, aes(x = values, y = woody_trend_yn, fill = woody_trend_yn), color = "white") +
  scale_fill_viridis_d(option = "H") +
  facet_wrap(~ var, scales = "free_x") +
  labs(y = "Woody cover change", x = "") +
  theme_bw() +
  theme(legend.position = "none")
p.ridge

ggsave(plot = p.ridge, "builds/plots/july/woody_cover_trend_ridges.png", dpi = 600)



### biomes vs environmental factors --------------------

dt$Biome <-  reorder(dt$Biome, dt$MAP, FUN = mean)

bp1 <- ggplot(data = dt, aes(x = Biome, y = MAP)) + 
  geom_jitter(aes(color = Biome), size = 2, alpha = 0.7) +
  geom_boxplot(aes(color = Biome), size = 1, alpha = 0.7) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "", y = "MAP") +
  #geom_smooth(method = "lm") +
  guides(size = "none", color = guide_legend(override.aes = list(size = 1.5))) +
  theme(legend.background = element_blank(), legend.position = "none", axis.text.x = element_text(angle = 25, hjust = 1))
bp1

dt$Biome <-  reorder(dt$Biome, dt$MAT, FUN = mean)

bp2 <- ggplot(data = dt, aes(x = Biome, y = MAT)) + 
  geom_jitter(aes(color = Biome), size = 2, alpha = 0.7) +
  geom_boxplot(aes(color = Biome), size = 1, alpha = 0.7) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "", y = "MAT") +
  #geom_smooth(method = "lm") +
  guides(size = "none", color = guide_legend(override.aes = list(size = 1.5))) +
  theme(legend.background = element_blank(), legend.position = "none", axis.text.x = element_text(angle = 25, hjust = 1))
bp2

dt$Biome <-  reorder(dt$Biome, dt$elevation_mean, FUN = mean)

bp3 <- ggplot(data = dt, aes(x = Biome, y = elevation_mean)) + 
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
ggsave(plot = bp, "builds/plots/july/environmental_biome.png", dpi = 600, height = 4, width = 12)


### variable correlation --------------------------------------
dt$herb
names(dt)
dt.corr <- dt %>% 
  dplyr::select(c(CW_mean_species_body_mass, elephant_biomass_ha,
                  browser_biomass_ha, grazer_biomass_ha, mixed_feeder_biomass_ha, herbi_biomass_ha, prop_burned_area,
                  n_herbi_sp_reserve, herbi_fun_div_distq1, elevation_sd_1000,
                  MAT, MAP, fire_events_since_2001, n_deposition, woody_cover_trend_venter2019, 
                  tree_cover_mean, tree_cover_sd_100, canopy_height_sd_100
                  )) %>% 
  rename(`Mean body mass (CWM)` = CW_mean_species_body_mass, 
         `Elephant biomass` = elephant_biomass_ha, 
         `Browser biomass` = browser_biomass_ha, 
         `Grazer biomass` = grazer_biomass_ha, 
         `Mixed feeder biomass` = mixed_feeder_biomass_ha, 
         `Herbivore biomass` = herbi_biomass_ha, 
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
         `Woody cover SD 100m` = tree_cover_sd_100, 
         `Canopy height SD 100m` = canopy_height_sd_100
  ) %>% filter(complete.cases(.))


corr <- round(cor(dt.corr), 1)
head(corr[, 1:6])
# argument lab = TRUE
library(ggcorrplot)
cp <- ggcorrplot(corr, hc.order = TRUE, type = "lower", colors = c("#6D9EC1", "white", "#E46726"), lab = TRUE)
ggsave(plot = cp, "builds/plots/september/variable_corrplot.png", dpi = 600, width = 11, height = 11)
           

### correlation between woody cover and canopy height SD #####

cor.test(dt$tree_cover_sd_100, dt$canopy_height_sd_100)

rel.tc.ch <- ggplot() +
  geom_point(data = dt, aes(x = tree_cover_sd_100, y = canopy_height_sd_100)) +
  theme_classic() +
  annotate("text", x = 3, y = 1.5, label = "cor = 0.79; p = <0.0001") +
  labs(x = "Tree cover SD", y = "Canopy height SD")

rel.tc.ch
ggsave(plot = rel.tc.ch, "builds/plots/july/sd_ch_wc_rel.png", dpi = 600, height = 7, width = 7)

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
ggsave(plot = n.dep.biome, "builds/plots/july/n_dep_per_biome.png", dpi = 600, height = 5, width = 10)

#### Elephants vs no elephants 
dtEle <- dt %>% 
  mutate(Elephants = ifelse(elephant_biomass_kgkm2 > 0, "Elephants", "No Elephants"))
names(dt$cat)

ggplot() +
  geom_boxplot(data = dtEle, aes(x = Elephants, y= herbi_biomass_kgkm2)) +
  geom_jitter(data = dtEle, aes(x = Elephants, y= herbi_biomass_kgkm2)) +
  labs(y = "Herbivore Biomass (kg/km2)", x = "") +
  theme_classic()
  