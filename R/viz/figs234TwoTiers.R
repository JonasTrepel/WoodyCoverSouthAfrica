## make figure parts for Figs 2-4. 

library(data.table)
library(tidyverse)
library(scales)
library(gridExtra)



resList <- readRDS("builds/model_results/plotList.Rds")

dtRugRaw <- resList$dtRugStorage
dtMargRaw <- resList$margStorage
dtBTPredsRaw <- resList$predsStorage 
  
dtVarImp <- resList$varImpStorage

### get max values 

varTierLims <- dtMargRaw %>% 
  filter(tier == "main") %>% 
  group_by(response, term) %>% 
  summarize(maxX = max(x, na.rm = T), 
            minX = min(x, na.rm = T))

#### 
dtRug <- dtRugRaw %>% 
  left_join(varTierLims) %>% 
  mutate(x = ifelse(x < minX | x > maxX, NA, x)) %>% 
  filter(!is.na(x))

dtMarg <- dtMargRaw %>% 
  left_join(varTierLims) %>% 
  mutate(x = ifelse(x < minX | x > maxX, NA, x)) %>% 
  filter(!is.na(x))

dtBTPreds <- dtBTPredsRaw %>% 
  left_join(varTierLims) %>% 
  mutate(x = ifelse(x < minX | x > maxX, NA, x)) %>% 
  filter(!is.na(x))

palette.groups <- c("Herbivory" = "#5C5698", "Global Change" = "#88A0DC", "Environmental" = "#905877", "Fire" =  "#DB7B71")

########### Tree Cover Mean ###############

##### Variable importance 
## get terms in order
oTCWC <- dtVarImp[tier == "main" & response == "tree_cover_mean", ] %>%
  arrange((mean_rel_imp)) %>%
  pull(clean_term)

dtVarCWC <- dtVarImp[tier == "main" & response == "tree_cover_mean", ]
dtVarCWC$clean_term <- factor(dtVarCWC$clean_term, levels = oTCWC)


pVarCWC <- ggplot(data = dtVarCWC) +
  geom_col(aes(y = clean_term, x = mean_rel_imp, fill = responseGroup),
           position = position_dodge(width = 0.8), alpha = 0.9) +
  geom_errorbar(aes(y = clean_term, 
                    xmin = mean_rel_imp - sd_rel_imp, 
                    xmax = mean_rel_imp + sd_rel_imp,
                    group = method),
                position = position_dodge(width = 0.8), width = 0.25) +
  scale_fill_manual(values = palette.groups) +
  labs(y = "", x = "Mean Relative Importance", fill = "") +
  theme_classic() +
  theme(legend.position = c(.75, .15),
        axis.text = element_text(size = 12))
pVarCWC


##### PDP 

# subsets 
dtMargCWC <- dtMarg %>% 
  filter(response == "tree_cover_mean")
dtMargCWC$clean_term <- factor(dtMargCWC$clean_term, levels = rev(oTCWC))


dtRugCWC <- dtRug %>% 
  filter(response == "tree_cover_mean" & tier == "main")
  
dtBTPredsCWC <- dtBTPreds  %>% 
  filter(response == "tree_cover_mean")

pdpCWC <- ggplot()+
  geom_line(data = dtMargCWC[tier == "main"], aes(x=x, y=y, color = responseGroup), linewidth = 1.1) +
  geom_line(data = dtBTPredsCWC[tier == "high_biomass"], aes(x=x, y=y, group = iteration, color = responseGroup), alpha = 0.15, linewidth = 0.5, color = "grey90") +
  geom_line(data = dtMargCWC[tier == "high_biomass"], aes(x=x, y=y, color = responseGroup), linewidth = 1, linetype = "dashed", alpha = .75) +
  geom_line(data = dtBTPredsCWC[tier == "main"], aes(x=x, y=y, group = iteration, color = responseGroup), alpha = 0.15, linewidth = 0.5, color = "grey") +
  geom_line(data = dtMargCWC[tier == "main"], aes(x=x, y=y, color = responseGroup), linewidth = 1.1) +
  facet_wrap(~factor(clean_term),
             scales="free_x", ncol = 4) +
  scale_x_continuous(breaks = extended_breaks(n = 3)) +
  scale_color_manual(values = palette.groups) +
  geom_rug(data = dtRugCWC, aes(x = x, y = y), sides="b", length = unit(0.03, "npc"), outside = TRUE) +
  coord_cartesian(clip = "off") +
  ylim(min(dtMargCWC[tier == "main"]$y, na.rm =T), max(dtMargCWC[tier == "main"]$y, na.rm = T)) +
  theme_classic() +
  labs(y = "Current woody cover (%)", x = "") +
  theme(legend.position = "none",
        strip.text.x = element_text(face = "bold", size = 8), 
        panel.grid = element_line(color = "white"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 10), 
        axis.title = element_text(size = 12), 
        axis.ticks = element_blank(), 
        panel.spacing = unit(0.6, "lines"), 
        strip.background = element_rect(color = "white", fill= "grey95"), 
  )
pdpCWC

pCombCWC <- grid.arrange(pdpCWC, pVarCWC, ncol = 2, widths = c(1.5, 1))
ggsave(plot = pCombCWC, filename = "builds/plots/september/TwoTierPlots/twoTiersCWC.png", dpi = 600, height = 5, width = 13)


###### Woody Cover Change ########

##### Variable importance 
## get terms in order
oTWCC <- dtVarImp[tier == "main" & response == "woody_cover_trend_venter2019", ] %>%
  arrange((mean_rel_imp)) %>%
  pull(clean_term)

dtVarWCC <- dtVarImp[tier == "main" & response == "woody_cover_trend_venter2019", ]
dtVarWCC$clean_term <- factor(dtVarWCC$clean_term, levels = oTWCC)


pVarWCC <- ggplot(data = dtVarWCC) +
  geom_col(aes(y = clean_term, x = mean_rel_imp, fill = responseGroup),
           position = position_dodge(width = 0.8), alpha = 0.9) +
  geom_errorbar(aes(y = clean_term, 
                    xmin = mean_rel_imp - sd_rel_imp, 
                    xmax = mean_rel_imp + sd_rel_imp,
                    group = method),
                position = position_dodge(width = 0.8), width = 0.25) +
  scale_fill_manual(values = palette.groups) +
  labs(y = "", x = "Mean Relative Importance", fill = "") +
  theme_classic() +
  theme(legend.position = c(.75, .15),
        axis.text = element_text(size = 12))
pVarWCC


##### PDP 

# subsets 
dtMargWCC <- dtMarg %>% 
  filter(response == "woody_cover_trend_venter2019")
dtMargWCC$clean_term <- factor(dtMargWCC$clean_term, levels = rev(oTWCC))


dtRugWCC <- dtRug %>% 
  filter(response == "woody_cover_trend_venter2019" & tier == "main")

dtBTPredsWCC <- dtBTPreds  %>% 
  filter(response == "woody_cover_trend_venter2019")

pdpWCC <- ggplot()+
  geom_line(data = dtMargWCC[tier == "main"], aes(x=x, y=y, color = responseGroup), linewidth = 1.1) +
  geom_line(data = dtBTPredsWCC[tier == "high_biomass"], aes(x=x, y=y, group = iteration, color = responseGroup), alpha = 0.15, linewidth = 0.5, color = "grey90") +
  geom_line(data = dtMargWCC[tier == "high_biomass"], aes(x=x, y=y, color = responseGroup), linewidth = 1, linetype = "dashed", alpha = .75) +
  geom_line(data = dtBTPredsWCC[tier == "main"], aes(x=x, y=y, group = iteration, color = responseGroup), alpha = 0.15, linewidth = 0.5, color = "grey") +
  geom_line(data = dtMargWCC[tier == "main"], aes(x=x, y=y, color = responseGroup), linewidth = 1.1) +
  facet_wrap(~factor(clean_term),
             scales="free_x", ncol = 4) +
  scale_x_continuous(breaks = extended_breaks(n = 3)) +
  scale_color_manual(values = palette.groups) +
  geom_rug(data = dtRugWCC, aes(x = x, y = y), sides="b", length = unit(0.03, "npc"), outside = TRUE) +
  coord_cartesian(clip = "off") +
  ylim(min(dtMargWCC[tier == "main"]$y, na.rm =T), max(dtMargWCC[tier == "main"]$y, na.rm = T)) +
  theme_classic() +
  labs(y = "Woody cover change (%/year)", x = "") +
  theme(legend.position = "none",
        strip.text.x = element_text(face = "bold", size = 8), 
        panel.grid = element_line(color = "white"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 10), 
        axis.title = element_text(size = 12), 
        axis.ticks = element_blank(), 
        panel.spacing = unit(0.6, "lines"), 
        strip.background = element_rect(color = "white", fill= "grey95") 
  )
pdpWCC

pCombWCC <- grid.arrange(pdpWCC, pVarWCC, ncol = 2, widths = c(1.5, 1))
ggsave(plot = pCombWCC, filename = "builds/plots/september/TwoTierPlots/twoTiersWCC.png", dpi = 600, height = 6.75, width = 13)


###### Woody Cover Heterogeneity #######

##### Variable importance 
## get terms in order
oTWCSD <- dtVarImp[tier == "main" & response == "tree_cover_sd_100", ] %>%
  arrange((mean_rel_imp)) %>%
  pull(clean_term)

dtVarWCSD <- dtVarImp[tier == "main" & response == "tree_cover_sd_100", ]
dtVarWCSD$clean_term <- factor(dtVarWCSD$clean_term, levels = oTWCSD)


pVarWCSD <- ggplot(data = dtVarWCSD) +
  geom_col(aes(y = clean_term, x = mean_rel_imp, fill = responseGroup),
           position = position_dodge(width = 0.8), alpha = 0.9) +
  geom_errorbar(aes(y = clean_term, 
                    xmin = mean_rel_imp - sd_rel_imp, 
                    xmax = mean_rel_imp + sd_rel_imp,
                    group = method),
                position = position_dodge(width = 0.8), width = 0.25) +
  scale_fill_manual(values = palette.groups) +
  labs(y = "", x = "Mean Relative Importance", fill = "") +
  theme_classic() +
  theme(legend.position = c(.75, .15),
        axis.text = element_text(size = 12))
pVarWCSD


##### PDP 

# subsets 
dtMargWCSD <- dtMarg %>% 
  filter(response == "tree_cover_sd_100")
dtMargWCSD$clean_term <- factor(dtMargWCSD$clean_term, levels = rev(oTWCSD))


dtRugWCSD <- dtRug %>% 
  filter(response == "tree_cover_sd_100" & tier == "main")

dtBTPredsWCSD <- dtBTPreds  %>% 
  filter(response == "tree_cover_sd_100")

pdpWCSD <- ggplot()+
  geom_line(data = dtMargWCSD[tier == "main"], aes(x=x, y=y, color = responseGroup), linewidth = 1.1) +
  geom_line(data = dtBTPredsWCSD[tier == "high_biomass"], aes(x=x, y=y, group = iteration, color = responseGroup), alpha = 0.15, linewidth = 0.5, color = "grey90") +
  geom_line(data = dtMargWCSD[tier == "high_biomass"], aes(x=x, y=y, color = responseGroup), linewidth = 1, linetype = "dashed", alpha = .75) +
  geom_line(data = dtBTPredsWCSD[tier == "main"], aes(x=x, y=y, group = iteration, color = responseGroup), alpha = 0.15, linewidth = 0.5, color = "grey") +
  geom_line(data = dtMargWCSD[tier == "main"], aes(x=x, y=y, color = responseGroup), linewidth = 1.1) +
  facet_wrap(~factor(clean_term),
             scales="free_x", ncol = 4) +
  scale_x_continuous(breaks = extended_breaks(n = 3)) +
  scale_color_manual(values = palette.groups) +
  geom_rug(data = dtRugWCSD, aes(x = x, y = y), sides="b", length = unit(0.03, "npc"), outside = TRUE) +
  coord_cartesian(clip = "off") +
  ylim(min(dtMargWCSD[tier == "main"]$y, na.rm =T), max(dtMargWCSD[tier == "main"]$y, na.rm = T)) +
  theme_classic() +
  labs(y = "Woody cover heterogeneity", x = "") +
  theme(legend.position = "none",
        strip.text.x = element_text(face = "bold", size = 8), 
        panel.grid = element_line(color = "white"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 10), 
        axis.title = element_text(size = 12), 
        axis.ticks = element_blank(), 
        panel.spacing = unit(0.6, "lines"), 
        strip.background = element_rect(color = "white", fill= "grey95") 
  )
pdpWCSD

pCombWCSD <- grid.arrange(pdpWCSD, pVarWCSD, ncol = 2, widths = c(1.5, 1))
ggsave(plot = pCombWCSD, filename = "builds/plots/september/TwoTierPlots/twoTiersWCSD.png", dpi = 600, height = 6.75, width = 13)

