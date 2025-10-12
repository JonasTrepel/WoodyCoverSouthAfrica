## make figure parts for Figs 2-4. 

library(data.table)
library(tidyverse)
library(scales)
library(gridExtra)
library("ggpattern")
library(patchwork) 


res_list <- readRDS("builds/model_results/plot_list.Rds")
names(res_list)

dt_rug_raw <- res_list$dt_mean_rug
dt_marg_raw <- res_list$dt_marg_plot
dt_preds_bt_raw <- res_list$dt_preds_bt 
dt_var_imp <- res_list$dt_var_imp_means %>%
  filter(tier %in% c("main", "high_biomass")) %>% 
  mutate(clean_tier = case_when(
    tier == "main" ~"Full Model", 
    tier == "high_biomass" ~ "High Biomass Subset"), 
    clean_tier = factor(clean_tier, levels = c("High Biomass Subset", "Full Model"))
    )

### get max values 

var_tier_lims <- dt_marg_raw %>% 
  filter(tier == "main") %>% 
  group_by(response, term) %>% 
  summarize(max_x = max(x, na.rm = T), 
            min_x = min(x, na.rm = T))

#### 
dt_rug <- dt_rug_raw %>% 
  left_join(var_tier_lims) %>% 
  mutate(x = ifelse(x < min_x | x > max_x, NA, x)) %>% 
  filter(!is.na(x))

dt_marg <- dt_marg_raw %>% 
  left_join(var_tier_lims) %>% 
  mutate(x = ifelse(x < min_x | x > max_x, NA, x)) %>% 
  filter(!is.na(x))

dt_bt_preds <- dt_preds_bt_raw %>% 
  left_join(var_tier_lims) %>% 
  mutate(x = ifelse(x < min_x | x > max_x, NA, x)) %>% 
  filter(!is.na(x))

palette_groups <- c("Herbivory" = "#88a0dc", "Global Change" = "#7c4b73", "Fire" =  "#ed968c")

########### Woody Cover Change ###############

##### _variable importance 
## get terms in order
ord_wcc <- dt_var_imp[tier == "main" & response == "woody_cover_change", ] %>%
  arrange((mean_rel_imp)) %>%
  pull(clean_term)

dt_var_wcc <- dt_var_imp[response == "woody_cover_change", ]
dt_var_wcc$clean_term <- factor(dt_var_wcc$clean_term, levels = ord_wcc)


p_var_wcc <- ggplot(data = dt_var_wcc) +
  geom_col_pattern(aes(y = clean_term, x = mean_rel_imp, fill = response_group, color = response_group, 
                       group = clean_tier, pattern = clean_tier),
           position = position_dodge(width = .9), alpha = 0.9, 
           width = 0.8,
           pattern_fill = 'grey', pattern_color = 'transparent') +
  geom_errorbar(aes(y = clean_term, 
                    xmin = mean_rel_imp - sd_rel_imp, 
                    xmax = mean_rel_imp + sd_rel_imp,
                    group = tier),
                position = position_dodge(width = 1), width = 0.25) +
  scale_fill_manual(values = palette_groups) +
  scale_color_manual(values = palette_groups) +
  scale_pattern_manual(values = c("stripe", "none")) + 
  guides(
    fill = guide_legend(override.aes = list(pattern = "none")),
    pattern = guide_legend(override.aes = list(fill = "grey50", color = "grey25"))
  ) +
  labs(y = "", x = "Variable Importance", pattern = "Tier", fill = "Response Group", color = "Response Group") +
  theme_classic() +
  theme(legend.position = c(.75, .25),
        axis.text = element_text(size = 12))
p_var_wcc


##### PDP 

# subsets 
dt_marg_wcc <- dt_marg %>% 
  filter(response == "woody_cover_change")
dt_marg_wcc$clean_term <- factor(dt_marg_wcc$clean_term, levels = rev(ord_wcc))


dt_rug_wcc <- dt_rug %>% 
  filter(response == "woody_cover_change" & tier == "main")

dt_bt_preds_wcc <- dt_bt_preds  %>% 
  filter(response == "woody_cover_change")

pdp_wcc <- ggplot()+
  geom_line(data = dt_marg_wcc[tier == "main"], aes(x=x, y=y, color = response_group), linewidth = 1.1) +
  geom_line(data = dt_bt_preds_wcc[tier == "high_biomass"], aes(x=x, y=y, group = iteration, color = response_group), alpha = 0.15, linewidth = 0.5, color = "grey90") +
  geom_line(data = dt_marg_wcc[tier == "high_biomass"], aes(x=x, y=y, color = response_group), linewidth = 1, linetype = "dashed", alpha = .75) +
  geom_line(data = dt_bt_preds_wcc[tier == "main"], aes(x=x, y=y, group = iteration, color = response_group), alpha = 0.15, linewidth = 0.5, color = "grey") +
  geom_line(data = dt_marg_wcc[tier == "main"], aes(x=x, y=y, color = response_group), linewidth = 1.1) +
  facet_wrap(~factor(clean_term),
             scales="free_x", ncol = 4) +
  scale_x_continuous(breaks = extended_breaks(n = 3)) +
  scale_color_manual(values = palette_groups) +
  geom_rug(data = dt_rug_wcc, aes(x = x, y = y), sides="b", length = unit(0.03, "npc"), outside = TRUE) +
  coord_cartesian(clip = "off") +
  ylim(min(dt_marg_wcc[tier == "main"]$y, na.rm =T), max(dt_marg_wcc[tier == "main"]$y, na.rm = T)) +
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
        strip.background = element_rect(color = "white", fill= "grey95"), 
  )
pdp_wcc

p_comb_wcc <- (pdp_wcc| p_var_wcc) + plot_layout(widths = c(2.5, 1))
ggsave(plot = p_comb_wcc, filename = "builds/plots/revision/two_tiers_wcc.png", dpi = 600, height = 6.75, width = 13)


########### Venter Woody Cover Change ###############

##### _variable importance 
## get terms in order
ord_vwct <- dt_var_imp[tier == "main" & response == "venter_woody_cover_trend", ] %>%
  arrange((mean_rel_imp)) %>%
  pull(clean_term)

dt_var_vwct <- dt_var_imp[response == "venter_woody_cover_trend", ]
dt_var_vwct$clean_term <- factor(dt_var_vwct$clean_term, levels = ord_vwct)


p_var_vwct <- ggplot(data = dt_var_vwct) +
  geom_col_pattern(aes(y = clean_term, x = mean_rel_imp, fill = response_group, color = response_group, 
                       group = clean_tier, pattern = clean_tier),
                   position = position_dodge(width = .9), alpha = 0.9, 
                   width = 0.8,
                   pattern_fill = 'grey', pattern_color = 'transparent') +
  geom_errorbar(aes(y = clean_term, 
                    xmin = mean_rel_imp - sd_rel_imp, 
                    xmax = mean_rel_imp + sd_rel_imp,
                    group = tier),
                position = position_dodge(width = 1), width = 0.25) +
  scale_fill_manual(values = palette_groups) +
  scale_color_manual(values = palette_groups) +
  scale_pattern_manual(values = c("stripe", "none")) + 
  guides(
    fill = guide_legend(override.aes = list(pattern = "none")),
   # pattern = guide_legend(override.aes = list(fill = "grey75", color = "grey25"))
  ) +
  labs(y = "", x = "Variable Importance", pattern = "Tier", fill = "Response Group", color = "Response Group") +
  theme_classic() +
  theme(legend.position = c(.75, .25),
        axis.text = element_text(size = 12))
p_var_vwct


##### PDP 

# subsets 
dt_marg_vwct <- dt_marg %>% 
  filter(response == "venter_woody_cover_trend")
dt_marg_vwct$clean_term <- factor(dt_marg_vwct$clean_term, levels = rev(ord_vwct))


dt_rug_vwct <- dt_rug %>% 
  filter(response == "venter_woody_cover_trend" & tier == "main")

dt_bt_preds_vwct <- dt_bt_preds  %>% 
  filter(response == "venter_woody_cover_trend")

pdp_vwct <- ggplot()+
  geom_line(data = dt_marg_vwct[tier == "main"], aes(x=x, y=y, color = response_group), linewidth = 1.1) +
  geom_line(data = dt_bt_preds_vwct[tier == "high_biomass"], aes(x=x, y=y, group = iteration, color = response_group), alpha = 0.15, linewidth = 0.5, color = "grey90") +
  geom_line(data = dt_marg_vwct[tier == "high_biomass"], aes(x=x, y=y, color = response_group), linewidth = 1, linetype = "dashed", alpha = .75) +
  geom_line(data = dt_bt_preds_vwct[tier == "main"], aes(x=x, y=y, group = iteration, color = response_group), alpha = 0.15, linewidth = 0.5, color = "grey") +
  geom_line(data = dt_marg_vwct[tier == "main"], aes(x=x, y=y, color = response_group), linewidth = 1.1) +
  facet_wrap(~factor(clean_term),
             scales="free_x", ncol = 4) +
  scale_x_continuous(breaks = extended_breaks(n = 3)) +
  scale_color_manual(values = palette_groups) +
  geom_rug(data = dt_rug_vwct, aes(x = x, y = y), sides="b", length = unit(0.03, "npc"), outside = TRUE) +
  coord_cartesian(clip = "off") +
  ylim(min(dt_marg_vwct[tier == "main"]$y, na.rm =T), max(dt_marg_vwct[tier == "main"]$y, na.rm = T)) +
  theme_classic() +
  labs(y = "Venter's woody cover change (%/year)", x = "") +
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
pdp_vwct

p_comb_vwct <- (pdp_vwct | p_var_vwct) + plot_layout(widths = c(2.5, 1))
ggsave(plot = p_comb_vwct, filename = "builds/plots/revision/two_tiers_vwct.png", dpi = 600, height = 6.75, width = 13)

########### Woody Cover SD ha ###############

##### variable importance 
## get terms in order
ord_wcsdha <- dt_var_imp[tier == "main" & response == "woody_cover_sd_ha_coef", ] %>%
  arrange((mean_rel_imp)) %>%
  pull(clean_term)

dt_var_wcsdha <- dt_var_imp[response == "woody_cover_sd_ha_coef", ]
dt_var_wcsdha$clean_term <- factor(dt_var_wcsdha$clean_term, levels = ord_wcsdha)


p_var_wcsdha <- ggplot(data = dt_var_wcsdha) +
  geom_col_pattern(aes(y = clean_term, x = mean_rel_imp, fill = response_group, color = response_group, 
                       group = clean_tier, pattern = clean_tier),
                   position = position_dodge(width = .9), alpha = 0.9, 
                   width = 0.8,
                   pattern_fill = 'grey', pattern_color = 'transparent') +
  geom_errorbar(aes(y = clean_term, 
                    xmin = mean_rel_imp - sd_rel_imp, 
                    xmax = mean_rel_imp + sd_rel_imp,
                    group = tier),
                position = position_dodge(width = 1), width = 0.25) +
  scale_fill_manual(values = palette_groups) +
  scale_color_manual(values = palette_groups) +
  scale_pattern_manual(values = c("stripe", "none")) + 
  guides(
    fill = guide_legend(override.aes = list(pattern = "none")),
    pattern = guide_legend(override.aes = list(fill = "grey50", color = "grey25"))
  ) +
  labs(y = "", x = "Variable Importance", pattern = "Tier", fill = "Response Group", color = "Response Group") +
  theme_classic() +
  theme(legend.position = c(.75, .25),
        axis.text = element_text(size = 12))
p_var_wcsdha


##### PDP 

# subsets 
dt_marg_wcsdha <- dt_marg %>% 
  filter(response == "woody_cover_sd_ha_coef")
dt_marg_wcsdha$clean_term <- factor(dt_marg_wcsdha$clean_term, levels = rev(ord_wcsdha))


dt_rug_wcsdha <- dt_rug %>% 
  filter(response == "woody_cover_sd_ha_coef" & tier == "main")

dt_bt_preds_wcsdha <- dt_bt_preds  %>% 
  filter(response == "woody_cover_sd_ha_coef")

pdp_wcsdha <- ggplot()+
  geom_line(data = dt_marg_wcsdha[tier == "main"], aes(x=x, y=y, color = response_group), linewidth = 1.1) +
  geom_line(data = dt_bt_preds_wcsdha[tier == "high_biomass"], aes(x=x, y=y, group = iteration, color = response_group), alpha = 0.15, linewidth = 0.5, color = "grey90") +
  geom_line(data = dt_marg_wcsdha[tier == "high_biomass"], aes(x=x, y=y, color = response_group), linewidth = 1, linetype = "dashed", alpha = .75) +
  geom_line(data = dt_bt_preds_wcsdha[tier == "main"], aes(x=x, y=y, group = iteration, color = response_group), alpha = 0.15, linewidth = 0.5, color = "grey") +
  geom_line(data = dt_marg_wcsdha[tier == "main"], aes(x=x, y=y, color = response_group), linewidth = 1.1) +
  facet_wrap(~factor(clean_term),
             scales="free_x", ncol = 4) +
  scale_x_continuous(breaks = extended_breaks(n = 3)) +
  scale_color_manual(values = palette_groups) +
  geom_rug(data = dt_rug_wcsdha, aes(x = x, y = y), sides="b", length = unit(0.03, "npc"), outside = TRUE) +
  coord_cartesian(clip = "off") +
  ylim(min(dt_marg_wcsdha[tier == "main"]$y, na.rm =T), max(dt_marg_wcsdha[tier == "main"]$y, na.rm = T)) +
  theme_classic() +
  labs(y = "Woody cover heterogeneity change", x = "") +
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
pdp_wcsdha

p_comb_wcsdha <- (pdp_wcsdha | p_var_wcsdha) + plot_layout(widths = c(2.5, 1))
ggsave(plot = p_comb_wcsdha, filename = "builds/plots/revision/two_tiers_wcsdha.png", dpi = 600, height = 6.75, width = 13)



########### Woody Cover SD km ###############

##### variable importance 
## get terms in order
ord_wcsdkm <- dt_var_imp[tier == "main" & response == "woody_cover_sd_km_coef", ] %>%
  arrange((mean_rel_imp)) %>%
  pull(clean_term)

dt_var_wcsdkm <- dt_var_imp[response == "woody_cover_sd_km_coef", ]
dt_var_wcsdkm$clean_term <- factor(dt_var_wcsdkm$clean_term, levels = ord_wcsdkm)


p_var_wcsdkm <- ggplot(data = dt_var_wcsdkm) +
  geom_col_pattern(aes(y = clean_term, x = mean_rel_imp, fill = response_group, color = response_group, 
                       group = clean_tier, pattern = clean_tier),
                   position = position_dodge(width = .9), alpha = 0.9, 
                   width = 0.8,
                   pattern_fill = 'grey', pattern_color = 'transparent') +
  geom_errorbar(aes(y = clean_term, 
                    xmin = mean_rel_imp - sd_rel_imp, 
                    xmax = mean_rel_imp + sd_rel_imp,
                    group = tier),
                position = position_dodge(width = 1), width = 0.25) +
  scale_fill_manual(values = palette_groups) +
  scale_color_manual(values = palette_groups) +
  scale_pattern_manual(values = c("stripe", "none")) + 
  guides(
    fill = guide_legend(override.aes = list(pattern = "none")),
    pattern = guide_legend(override.aes = list(fill = "grey50", color = "grey25"))
  ) +
  labs(y = "", x = "Variable Importance", pattern = "Tier", fill = "Response Group", color = "Response Group") +
  theme_classic() +
  theme(legend.position = c(.75, .25),
        axis.text = element_text(size = 12))
p_var_wcsdkm


##### PDP 

# subsets 
dt_marg_wcsdkm <- dt_marg %>% 
  filter(response == "woody_cover_sd_km_coef")
dt_marg_wcsdkm$clean_term <- factor(dt_marg_wcsdkm$clean_term, levels = rev(ord_wcsdkm))


dt_rug_wcsdkm <- dt_rug %>% 
  filter(response == "woody_cover_sd_km_coef" & tier == "main")

dt_bt_preds_wcsdkm <- dt_bt_preds  %>% 
  filter(response == "woody_cover_sd_km_coef")

pdp_wcsdkm <- ggplot()+
  geom_line(data = dt_marg_wcsdkm[tier == "main"], aes(x=x, y=y, color = response_group), linewidth = 1.1) +
  geom_line(data = dt_bt_preds_wcsdkm[tier == "high_biomass"], aes(x=x, y=y, group = iteration, color = response_group), alpha = 0.15, linewidth = 0.5, color = "grey90") +
  geom_line(data = dt_marg_wcsdkm[tier == "high_biomass"], aes(x=x, y=y, color = response_group), linewidth = 1, linetype = "dashed", alpha = .75) +
  geom_line(data = dt_bt_preds_wcsdkm[tier == "main"], aes(x=x, y=y, group = iteration, color = response_group), alpha = 0.15, linewidth = 0.5, color = "grey") +
  geom_line(data = dt_marg_wcsdkm[tier == "main"], aes(x=x, y=y, color = response_group), linewidth = 1.1) +
  facet_wrap(~factor(clean_term),
             scales="free_x", ncol = 4) +
  scale_x_continuous(breaks = extended_breaks(n = 3)) +
  scale_color_manual(values = palette_groups) +
  geom_rug(data = dt_rug_wcsdkm, aes(x = x, y = y), sides="b", length = unit(0.03, "npc"), outside = TRUE) +
  coord_cartesian(clip = "off") +
  ylim(min(dt_marg_wcsdkm[tier == "main"]$y, na.rm =T), max(dt_marg_wcsdkm[tier == "main"]$y, na.rm = T)) +
  theme_classic() +
  labs(y = "Woody cover heterogeneity change", x = "") +
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
pdp_wcsdkm

p_comb_wcsdkm <- (pdp_wcsdkm | p_var_wcsdkm) + plot_layout(widths = c(2.5, 1))
ggsave(plot = p_comb_wcsdkm, filename = "builds/plots/revision/two_tiers_wcsdkm.png", dpi = 600, height = 6.75, width = 13)

