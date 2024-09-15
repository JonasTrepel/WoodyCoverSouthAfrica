## trend 

pChange <- ggplot()+
  geom_line(data = marg.plot[marg.plot$clean_term %in% c("Herbivore functional diversity", "N deposition ([kg/km2]/year)"), ], aes(x=x, y=y, color = clean_term), linewidth = 1.1) +
  geom_line(data = predsBtPlot[predsBtPlot$clean_term %in% c("Herbivore functional diversity", "N deposition ([kg/km2]/year)"), ], aes(x=x, y=y, group = iteration, color = clean_term), alpha = 0.15, linewidth = 0.5, color = "grey") +
  geom_line(data = marg.plot[marg.plot$clean_term %in% c("Herbivore functional diversity", "N deposition ([kg/km2]/year)"), ], aes(x=x, y=y, color = clean_term), linewidth = 1.1) +
  facet_wrap(~factor(clean_term),
             scales="free_x", ncol = 2) +
  scale_x_continuous(breaks = extended_breaks(n = 3)) +
  scale_color_manual(values = palette) +
  geom_rug(data = dt.mean.rug[dt.mean.rug$clean_term %in% c("Herbivore functional diversity", "N deposition ([kg/km2]/year)"), ], aes(x = x, y = y), sides="b", length = unit(0.03, "npc"), outside = TRUE) +
  coord_cartesian(clip = "off") +
  ylim((min(marg.plot[marg.plot$clean_term %in% c("Herbivore functional diversity", "N deposition ([kg/km2]/year)"), ]$y, na.rm =T)),
       (max(marg.plot[marg.plot$clean_term %in% c("Herbivore functional diversity", "N deposition ([kg/km2]/year)"), ]$y, na.rm = T))) +
  theme_classic() +
  labs(y = paste0(clean.label), x = "") +
  theme(legend.position = "none",
        panel.grid = element_line(color = "white"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 10), 
        axis.title = element_text(size = 12), 
        axis.ticks = element_blank(), 
        panel.spacing = unit(0.6, "lines"), 
        strip.background = element_rect(color = "white", fill= "grey95") 
  )
pChange

####ggsave(plot= pChange, "builds/presentationPlots/wcChangeHighBiomass.png", dpi = 600, height = 3, width = 6)
       


## woody cover heterogeneity 

pHet <- ggplot()+
  geom_line(data = marg.plot[marg.plot$clean_term %in% c("Mean body mass (kg; cwm)", "Browser biomass (kg/km2)",
                                               "Herbivore functional diversity", "Grazer biomass (kg/km2)"), ], aes(x=x, y=y, color = clean_term), linewidth = 1.1) +
  geom_line(data = predsBtPlot[predsBtPlot$clean_term %in% c("Mean body mass (kg; cwm)", "Browser biomass (kg/km2)",
                                                          "Herbivore functional diversity", "Grazer biomass (kg/km2)"), ], aes(x=x, y=y, group = iteration, color = clean_term), alpha = 0.15, linewidth = 0.5, color = "grey") +
  geom_line(data = marg.plot[marg.plot$clean_term %in% c("Mean body mass (kg; cwm)", "Browser biomass (kg/km2)",
                                                         "Herbivore functional diversity", "Grazer biomass (kg/km2)"), ], aes(x=x, y=y, color = clean_term), linewidth = 1.1) +
  facet_wrap(~factor(clean_term),
             scales="free_x", ncol = 4) +
  scale_x_continuous(breaks = extended_breaks(n = 3)) +
  scale_color_manual(values = palette) +
  geom_rug(data = dt.mean.rug[dt.mean.rug$clean_term %in% c("Mean body mass (kg; cwm)", "Browser biomass (kg/km2)",
                                                           "Herbivore functional diversity", "Grazer biomass (kg/km2)"), ], aes(x = x, y = y), sides="b", length = unit(0.03, "npc"), outside = TRUE) +
  coord_cartesian(clip = "off") +
  ylim((min(marg.plot[marg.plot$clean_term %in% c("Mean body mass (kg; cwm)", "Browser biomass (kg/km2)",
                                                  "Herbivore functional diversity", "Grazer biomass (kg/km2)"), ]$y, na.rm =T) -1.5),
       (max(marg.plot[marg.plot$clean_term %in% c("Mean body mass (kg; cwm)", "Browser biomass (kg/km2)",
                                                 "Herbivore functional diversity", "Grazer biomass (kg/km2)"), ]$y, na.rm = T)+1.5)) +
  theme_classic() +
  labs(y = paste0(clean.label), x = "") +
  theme(legend.position = "none",
        panel.grid = element_line(color = "white"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 10), 
        axis.title = element_text(size = 12), 
        axis.ticks = element_blank(), 
        panel.spacing = unit(0.6, "lines"), 
        strip.background = element_rect(color = "white", fill= "grey95") 
  )
pHet

#ggsave(plot= pHet, "builds/presentationPlots/wcsdHighBiomassDisturbance.png", dpi = 600, height = 6, width = 6)

## Canopy height heterogeneity 


pChHet <- ggplot()+
  geom_line(data = marg.plot[marg.plot$clean_term %in% c("Mean body mass (kg; cwm)", "Browser biomass (kg/km2)",
                                                         "Herbivore functional diversity", "Grazer biomass (kg/km2)"), ], aes(x=x, y=y, color = clean_term), linewidth = 1.1) +
  geom_line(data = predsBtPlot[predsBtPlot$clean_term %in% c("Mean body mass (kg; cwm)", "Browser biomass (kg/km2)",
                                                             "Herbivore functional diversity", "Grazer biomass (kg/km2)"), ], aes(x=x, y=y, group = iteration, color = clean_term), alpha = 0.15, linewidth = 0.5, color = "grey") +
  geom_line(data = marg.plot[marg.plot$clean_term %in% c("Mean body mass (kg; cwm)", "Browser biomass (kg/km2)",
                                                         "Herbivore functional diversity", "Grazer biomass (kg/km2)"), ], aes(x=x, y=y, color = clean_term), linewidth = 1.1) +
  facet_wrap(~factor(clean_term),
             scales="free_x", ncol = 4) +
  scale_x_continuous(breaks = extended_breaks(n = 3)) +
  scale_color_manual(values = palette) +
  geom_rug(data = dt.mean.rug[dt.mean.rug$clean_term %in% c("Mean body mass (kg; cwm)", "Browser biomass (kg/km2)",
                                                            "Herbivore functional diversity", "Grazer biomass (kg/km2)"), ], aes(x = x, y = y), sides="b", length = unit(0.03, "npc"), outside = TRUE) +
  coord_cartesian(clip = "off") +
  ylim((min(marg.plot[marg.plot$clean_term %in% c("Mean body mass (kg; cwm)", "Browser biomass (kg/km2)",
                                                  "Herbivore functional diversity", "Grazer biomass (kg/km2)"), ]$y, na.rm =T) -.05),
       (max(marg.plot[marg.plot$clean_term %in% c("Mean body mass (kg; cwm)", "Browser biomass (kg/km2)",
                                                  "Herbivore functional diversity", "Grazer biomass (kg/km2)"), ]$y, na.rm = T)+.05)) +
  theme_classic() +
  labs(y = paste0(clean.label), x = "") +
  theme(legend.position = "none",
        panel.grid = element_line(color = "white"), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 10), 
        axis.title = element_text(size = 12), 
        axis.ticks = element_blank(), 
        panel.spacing = unit(0.6, "lines"), 
        strip.background = element_rect(color = "white", fill= "grey95") 
  )
pChHet
ggsave(plot= pChHet, "builds/presentationPlots/chsdHighBiomassDisturbance.png", dpi = 600, height = 3, width = 12)

