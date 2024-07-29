#### JULY 2024



rm(list = ls())

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

dt <- fread("data/ReserveDataSouthAfricaFinal.csv") %>% 
  mutate(elephants = ifelse(elephant_biomass_kgkm2 > 0, "Elephants", "No elephants"))



## elephants 
dt.ele <- dt %>% filter(elephant_biomass_kgkm2 > 0)
table(dt.ele$Biome)

#### Savanna 

min(dt.ele[Biome == "Savanna"]$MAP)
max(dt.ele[Biome == "Savanna"]$MAP)
max(dt[Biome == "Savanna"]$MAP)
min(dt[Biome == "Savanna"]$MAP)

min(dt.ele[Biome == "Savanna"]$MAT)
max(dt.ele[Biome == "Savanna"]$MAT)
max(dt[Biome == "Savanna"]$MAT)
min(dt[Biome == "Savanna"]$MAT)

mean(dt.ele[Biome == "Savanna"]$elephant_biomass_kgkm2)
median(dt.ele[Biome == "Savanna"]$elephant_biomass_kgkm2)
quantile(dt.ele[Biome == "Savanna"]$elephant_biomass_kgkm2, c(0.25, 0.75))
sd(dt.ele[Biome == "Savanna"]$elephant_biomass_kgkm2)

mean(dt.ele[Biome == "Albany Thicket"]$elephant_biomass_kgkm2)
median(dt.ele[Biome == "Albany Thicket"]$elephant_biomass_kgkm2)
quantile(dt.ele[Biome == "Albany Thicket"]$elephant_biomass_kgkm2, c(0.25, 0.75))
sd(dt.ele[Biome == "Albany Thicket"]$elephant_biomass_kgkm2)


dt.ele.sav <- dt %>% 
  filter(MAP >= min(dt.ele[Biome == "Savanna"]$MAP) 
         & MAP <= max(dt.ele[Biome == "Savanna"]$MAP) 
         & Biome == "Savanna" 
         & MAT <= max(dt[Biome == "Savanna"]$MAT)
         & MAT >= min(dt[Biome == "Savanna"]$MAT))


#### wilcoxon test because not always normally distributed  

nrow(dt.ele.sav[elephants == "Elephants"])
nrow(dt.ele.sav[!elephants == "Elephants"])


wilcox.test(dt.ele.sav[elephants == "Elephants"]$tree_cover_mean, dt.ele.sav[!elephants == "Elephants"]$tree_cover_mean )

p.ele.wc.sav <- ggplot() +
  geom_boxplot(data = dt.ele.sav, aes(x = elephants, y= tree_cover_mean)) +
  geom_jitter(data = dt.ele.sav, aes(x = elephants, y= tree_cover_mean, color = elephant_biomass_kgkm2), width = 0.2, size = 2.5, alpha = 0.6) +
  scale_color_viridis_c() +
  theme_classic() +
  labs(y = "Current woody cover", x = "", title = "p = 0.94") +
  theme(axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(face = "italic"))
p.ele.wc.sav


wilcox.test(dt.ele.sav[elephants == "Elephants"]$woody_cover_trend_venter2019, dt.ele.sav[!elephants == "Elephants"]$woody_cover_trend_venter2019 )

p.ele.wt.sav <- ggplot() +
  geom_boxplot(data = dt.ele.sav, aes(x = elephants, y= woody_cover_trend_venter2019)) +
  geom_jitter(data = dt.ele.sav, aes(x = elephants, y= woody_cover_trend_venter2019, color = elephant_biomass_kgkm2), width = 0.2, size = 2.5, alpha = 0.6) +
  scale_color_viridis_c() +
  theme_classic() +
  labs(y = "Woody cover trend", x = "", title = "p = 0.25") +
  theme(axis.text.x = element_text(size = 12), 
        axis.title = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(face = "italic"))
p.ele.wt.sav
  
wilcox.test(dt.ele.sav[elephants == "Elephants"]$tree_cover_sd_100, dt.ele.sav[!elephants == "Elephants"]$tree_cover_sd_100 )

p.ele.wcsd.sav <- ggplot() +
  geom_boxplot(data = dt.ele.sav, aes(x = elephants, y= tree_cover_sd_100)) +
  geom_jitter(data = dt.ele.sav, aes(x = elephants, y= tree_cover_sd_100, color = elephant_biomass_kgkm2), width = 0.2, size = 2.5, alpha = 0.6) +
  scale_color_viridis_c() +
  theme_classic() +
  labs(y = "Tree cover SD", x = "", title = "p = 0.2") +
  theme(axis.text.x = element_text(size = 12), 
        axis.title = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(face = "italic"))
p.ele.wcsd.sav

wilcox.test(dt.ele.sav[elephants == "Elephants"]$canopy_height_sd_100, dt.ele.sav[!elephants == "Elephants"]$canopy_height_sd_100 )

p.ele.chsd.sav <- ggplot() +
  geom_boxplot(data = dt.ele.sav, aes(x = elephants, y= canopy_height_sd_100)) +
  geom_jitter(data = dt.ele.sav, aes(x = elephants, y= canopy_height_sd_100, color = elephant_biomass_kgkm2), width = 0.2, size = 2.5, alpha = 0.6) +
  scale_color_viridis_c() +
  theme_classic() +
  labs(y = "Canopy height SD", x = "", title = "p = 0.79") +
  theme(axis.text.x = element_text(size = 12), 
        axis.title = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(face = "italic"))
p.ele.chsd.sav

p.ele.sav <- grid.arrange(p.ele.wc.sav, p.ele.wt.sav, p.ele.wcsd.sav, p.ele.chsd.sav, ncol = 4, 
                          top = textGrob("Savanna (n = 22 with elephants; n = 169 without elephants)",gp=gpar(fontsize=14,font=3)))

### albany thicket 
min(dt.ele[Biome == "Albany Thicket"]$MAP)
max(dt.ele[Biome == "Albany Thicket"]$MAP)
max(dt[Biome == "Albany Thicket"]$MAP)
min(dt[Biome == "Albany Thicket"]$MAP)

min(dt.ele[Biome == "Albany Thicket"]$MAT)
max(dt.ele[Biome == "Albany Thicket"]$MAT)
max(dt[Biome == "Albany Thicket"]$MAT)
min(dt[Biome == "Albany Thicket"]$MAT)

dt.ele.at <- dt %>% 
  filter(MAP >= min(dt.ele[Biome == "Albany Thicket"]$MAP) 
         & MAP <= max(dt.ele[Biome == "Albany Thicket"]$MAP) 
         & Biome == "Albany Thicket" 
         & MAT <= max(dt[Biome == "Albany Thicket"]$MAT)
         & MAT >= min(dt[Biome == "Albany Thicket"]$MAT))


#### t tests 

shapiro.test(dt.ele.at[elephants == "Elephants"]$tree_cover_mean) # p = 0.13
shapiro.test(dt.ele.at[!elephants == "Elephants"]$tree_cover_mean)


nrow(dt.ele.at[elephants == "Elephants"])
nrow(dt.ele.at[!elephants == "Elephants"])


wilcox.test(dt.ele.at[elephants == "Elephants"]$tree_cover_mean, dt.ele.at[!elephants == "Elephants"]$tree_cover_mean )
library(scico)
p.ele.wc.at <- ggplot() +
  geom_boxplot(data = dt.ele.at, aes(x = elephants, y= tree_cover_mean)) +
  geom_jitter(data = dt.ele.at, aes(x = elephants, y= tree_cover_mean, color = elephant_biomass_kgkm2), width = 0.2, size = 2.5, alpha = 0.6) +
  scale_color_scico(palette = "batlow", end = 0.7) +
  theme_classic() +
  labs(y = "Current woody cover", x = "", title = "p = 0.12") +
  theme(axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(face = "italic"))
p.ele.wc.at


wilcox.test(dt.ele.at[elephants == "Elephants"]$woody_cover_trend_venter2019, dt.ele.at[!elephants == "Elephants"]$woody_cover_trend_venter2019 )

p.ele.wt.at <- ggplot() +
  geom_boxplot(data = dt.ele.at, aes(x = elephants, y= woody_cover_trend_venter2019)) +
  geom_jitter(data = dt.ele.at, aes(x = elephants, y= woody_cover_trend_venter2019, color = elephant_biomass_kgkm2), width = 0.2, size = 2.5, alpha = 0.6) +
  scale_color_scico(palette = "batlow", end = 0.7) +
  theme_classic() +
  labs(y = "Woody cover trend", x = "", title = "p = 0.07") +
  theme(axis.text.x = element_text(size = 12), 
        axis.title = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(face = "italic"))
p.ele.wt.at

wilcox.test(dt.ele.at[elephants == "Elephants"]$tree_cover_sd_100, dt.ele.at[!elephants == "Elephants"]$tree_cover_sd_100 )

p.ele.wcsd.at <- ggplot() +
  geom_boxplot(data = dt.ele.at, aes(x = elephants, y= tree_cover_sd_100)) +
  geom_jitter(data = dt.ele.at, aes(x = elephants, y= tree_cover_sd_100, color = elephant_biomass_kgkm2), width = 0.2, size = 2.5, alpha = 0.6) +
  scale_color_scico(palette = "batlow", end = 0.7) +
  theme_classic() +
  labs(y = "Tree cover SD", x = "", title = "p = 0.93") +
  theme(axis.text.x = element_text(size = 12), 
        axis.title = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(face = "italic"))
p.ele.wcsd.at

wt.chsd.at <- wilcox.test(dt.ele.at[elephants == "Elephants"]$canopy_height_sd_100, dt.ele.at[!elephants == "Elephants"]$canopy_height_sd_100 )
round(wt.chsd.at$p.value, 2)
round(wt.chsd.at$statistic, 2)


p.ele.chsd.at <- ggplot() +
  geom_boxplot(data = dt.ele.at, aes(x = elephants, y= canopy_height_sd_100)) +
  geom_jitter(data = dt.ele.at, aes(x = elephants, y= canopy_height_sd_100, color = elephant_biomass_kgkm2), width = 0.2, size = 2.5, alpha = 0.6) +
  scale_color_scico(palette = "batlow", end = 0.7) +
  theme_classic() +
  labs(y = "Canopy height SD", x = "", title = paste0("p = ", round(wt.chsd.at$p.value, 2))) +
  theme(axis.text.x = element_text(size = 12), 
        axis.title = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(face = "italic"))
p.ele.chsd.at

p.ele.at <- grid.arrange(p.ele.wc.at, p.ele.wt.at, p.ele.wcsd.at, p.ele.chsd.at, ncol = 4, 
                          top = textGrob("Albany Thicket (n = 12 with elephants; n = 60 without elephants)",gp=gpar(fontsize=14,font=3)))

lp.at <- ggplot() +
  geom_jitter(data = dt.ele.at, aes(x = elephants, y= canopy_height_sd_100, color = elephant_biomass_kgkm2), width = 0.2, size = 2.5, alpha = 0.6) +
  scale_color_scico(palette = "batlow", end = 0.7) +
  theme_classic() +
  labs(color = "Elephant biomass (kg/km2)\n[Albany thicket]") +
  theme(axis.text.x = element_text(size = 12), 
        axis.title = element_text(size = 12),
        legend.position = "bottom",
        plot.title = element_text(face = "italic"))
lp.at
library(lemon)
leg.at <- g_legend(lp.at)
grid.draw(leg.at)  

lp.sav <- ggplot() +
  geom_jitter(data = dt.ele.sav, aes(x = elephants, y= canopy_height_sd_100, color = elephant_biomass_kgkm2), width = 0.2, size = 2.5, alpha = 0.6) +
  scale_color_viridis_c(breaks = c(2500, 5000, 7500)) +
  theme_classic() +
  labs(color = "Elephant biomass (kg/km2)\n[Savanna]") +
  theme(axis.text.x = element_text(size = 12), 
        axis.title = element_text(size = 12),
        legend.position = "bottom",
        plot.title = element_text(face = "italic"))
lp.sav
library(lemon)
leg.sav <- g_legend(lp.sav)
grid.draw(leg.sav)  

legs <- grid.arrange(leg.sav, leg.at, ncol = 2)

p.ele <- grid.arrange(p.ele.sav, p.ele.at, legs, heights = c(1, 1, 0.2))
ggsave(plot = p.ele, "builds/plots/july/elephant_sensitivity.png", dpi = 600, height = 7.25, width = 12)

