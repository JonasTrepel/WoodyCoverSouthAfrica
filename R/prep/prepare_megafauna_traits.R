
rm(list = ls())
options(scipen = 999)
### updated June 8, 2024

# Prepare megafauna traits and groups----------

library(data.table)
library(tidyverse)

## Herbivore fgs-------
#https://megapast2future.github.io/PHYLACINE_1.2/
phyl <- fread("../../../../../resources/traits/Using_PHYLACINE/data/PHYLACINE_1.2.1/Data/Traits/Trait_data.csv")
phyl[grepl("Camelus", Binomial.1.2),]$Binomial.1.2

#https://github.com/MegaPast2Future/HerbiTraits

herbi <- fread("../../../../../resources/traits/Using_PHYLACINE/data/HerbiTraits_1.2/HerbiTraits_1.2.csv") %>% 
  mutate(Binomial.1.2 = gsub(" ", "_", Binomial)) %>% 
  rename(Mass.g.herbi = Mass.g)
herbi[grepl("Bos", Binomial.1.2),]$Binomial.1.2


herbi.phyl <- phyl %>% 
  left_join(herbi, by = "Binomial.1.2") %>% 
  mutate(IUCN.Status.1.2 = factor(IUCN.Status.1.2, levels=c("EP", "EX", "EW", "CR", "EN", "VU", "NT", "LC", "DD")), 
         mass_kg = Mass.g/1000, 
         mass_bins_raw = case_when(
           mass_kg >= 10 & mass_kg < 45 ~ "s1",
           mass_kg >= 45 & mass_kg < 100 ~ "s2",
           mass_kg >= 100 & mass_kg < 1000 ~ "s3",
           mass_kg >= 1000 ~ "s4"
         ), 
         fermentation_simple = case_when(
           grepl("Foregut", Fermentation.Type)  ~ "Foregut", 
           grepl("Hindgut", Fermentation.Type)  ~ "Hindgut",
           grepl("Simple Gut", Fermentation.Type)  ~ "Simple"
         ), 
         trophic_level_herbivore =  case_when(
           mass_kg >= 10 & mass_kg < 45 ~ "small herbivore",
           mass_kg >= 45 & mass_kg < 100 ~ "medium herbivore",
           mass_kg >= 100 & mass_kg < 1000 ~ "large herbivore",
           mass_kg >= 1000 ~ "megaherbivore",
           Guild.w.Omnivory == "Omnivore" ~ "omnivore"
         ), 
         source_herbi = ifelse(!is.na(Guild.w.Omnivory), "HerbiTRAITS", "Phylacine"), 
         herbivore_fg_own = paste0(mass_bins_raw, Guild.w.Omnivory, fermentation_simple),
         herbivore_fg_own = ifelse(grepl("NA", herbivore_fg_own),"not_complete", herbivore_fg_own)
         ) %>%  filter(mass_kg > 10 & Terrestrial == 1 & !IUCN.Status.1.2 %in% c("EX", "EP") | Binomial.1.2 %in% c("Bos_primigenius", "Camelus_dromedarius"))
  

phyl[Diet.Vertebrate >0.5,]$Binomial.1.2
sum(is.na(herbi.phyl$Guild.only.Herbivory))
n_distinct(herbi.phyl$herbivore_fg_own)
n_distinct(herbi.phyl$trophic_level_herbivore)
table(herbi.phyl$source_herbi)
### split into body mass bins ------------------------

# calculate number of bins according to sturges rule 
names(herbi.phyl)

# sample size here using only terrestrial , extant herbivores > 10kg, 
n <- n_distinct(herbi.phyl[mass_kg > 10 & Terrestrial == 1 & !IUCN.Status.1.2 %in% c("EX", "EP") & source_herbi == "HerbiTRAITS", Binomial.1.2])
#number of breakpoints using Sturges rule
k <- ceiling(log2(n) + 1)
km1 <- k-1

herbi.phyl2 <- herbi.phyl %>% 
  mutate(herbi_body_mass_bins = as.numeric(cut_number(mass_kg, n = km1)), 
         herbi_body_mass_bins = ifelse(mass_kg > 1000, 10, herbi_body_mass_bins)) %>% ## add additional category for megaherbivores 
  dplyr::select(Binomial.1.2, Order.1.2, Family.1.2, Genus.1.2, Terrestrial, mass_kg,
                Guild.w.Omnivory, Guild.only.Herbivory, fermentation_simple,IUCN.Status.1.2,
                trophic_level_herbivore, source_herbi, herbivore_fg_own, herbi_body_mass_bins, Common.Name)
table(herbi.phyl2$herbi_body_mass_bins)
herbi.phyl2[grepl("Bos", Binomial.1.2),]$Binomial.1.2

## predators --------------
## not published yet unfortunately 
carni <- fread("../../../../../resources/traits/CarniTRAITS/CarniTRAITS_v1.2_prelim.csv", header = TRUE)
carni[, Pounce_pursuit := `Pounce-pursuit`]

names(carni)
table(carni$Terrestrial)
carni[, Pounce_pursuit := `Pounce-pursuit`]
## Cooperatio: group hunting = 1, no group = 0
## mammal column gives diet information. We're only interested in diet mammal >= 1
preds <- carni %>% 
  dplyr::select(Binomial.1.2, Mass.g,
                   Diet.Plant, Diet.Vertebrate, Diet.Invertebrate, Mammal,
                   Scavenging_Notable, Cooperative_Hunting,
                   Ambush, Pursuit, Pounce_pursuit, Opportunistic,
                   Bone_Manipulator)  %>% 
  rename(mass_g_preds = Mass.g) %>% 
  mutate(mass_kg = mass_g_preds/1000,
         social = ifelse(Cooperative_Hunting == 1,
                          TRUE, FALSE)) %>% 
  filter(mass_kg > 10) ## predators over 10 kg
names(preds)


### Estimate predator prey ranges -------------------------------------------

preySize <- function(mass_kg,
                     social){
  
  # From Van Valkenburgh et al. 2016 PNAS (supplement, Table S5)
  
  smallest <- ifelse(social == FALSE,
                     15.74 * log(mass_kg) - 33.749,
                     0.6869 * mass_kg + 2.404)
  
  largest_accessible <- ifelse(social == FALSE,
                               2.245 * mass_kg - 19.49,
                               204.78 * log(mass_kg) - 279.59)
  
  maximum <- ifelse(social == FALSE,
                    504.74 * log(mass_kg) - 1166.6,
                    22.781 * mass_kg - 92.089)
  
  out <- data.table("smallest" = smallest, "largest_accessible" = largest_accessible, "maximum" = maximum)
  return(out)
  
}


preySize(preds[1, ]$mass_kg, preds[1, ]$social)

sizes <- list()
for(i in 1:nrow(preds)){
  sizes[[i]] <- preySize(preds[i, ]$mass_kg,
                         preds[i, ]$social)
}
preysizes <- do.call(rbind.data.frame, sizes)
preysizes


## base prey range bins on the largest accessible prey which seems to be most representative for the usual upper limit in prey size 

# calculate bin number  
npb <- n_distinct(preds$Binomial.1.2)

kp <- ceiling(log2(npb) + 1)

preds2 <- preds %>% 
  cbind(preysizes) %>% 
  mutate(prey_range_bins = as.numeric(cut_interval(largest_accessible, n = kp)),
         group_hunting = ifelse(social == TRUE, "cooperative hunter", "solitary"), 
         hunting_style = case_when(
           Pounce_pursuit == 1 ~ "Pounce pursuit", 
           Pursuit == 1 ~ "Pursuit", 
           Ambush == 1 ~ "Ambush"),
         source_carni = "CarniTRAITS", 
         mass_kg_carni = mass_kg,
         mass_kg = NULL, 
         trophic_level_carni = case_when(
           largest_accessible >= 1000 ~ "mega prey predator", 
           largest_accessible < 1000 & largest_accessible >= 100 ~ "large prey predator", 
           largest_accessible < 100 & largest_accessible >= 45 ~ "medium prey predator",
           largest_accessible < 45 ~ "small prey predator"
         ),
         carnivore_fg_own = paste0(trophic_level_carni, group_hunting, hunting_style), 
         carnivore_fg_own = ifelse(grepl("NA", carnivore_fg_own), "not_complete", carnivore_fg_own), 
         Binomial.1.2 = gsub(" ", "_", Binomial.1.2))

table(preds2$prey_range_bins)
table(preds2$trophic_level_carni)

table(preds2$largest_accessible)


mam.traits <- herbi.phyl2 %>% left_join(preds2, by = "Binomial.1.2") %>% 
  mutate(trophic_level = ifelse(!is.na(source_carni), trophic_level_carni, trophic_level_herbivore)) %>% 
  dplyr::select(Binomial.1.2, Order.1.2, Family.1.2, Genus.1.2, IUCN.Status.1.2, Terrestrial, Common.Name, Guild.w.Omnivory, Guild.only.Herbivory, fermentation_simple, 
                trophic_level_herbivore, source_herbi, herbivore_fg_own, herbi_body_mass_bins,
                mass_kg, mass_kg_carni, maximum, largest_accessible, prey_range_bins, 
                group_hunting, hunting_style, carnivore_fg_own, source_carni, trophic_level_carni, trophic_level) %>% 
  rename(largest_accessible_prey_size = largest_accessible, 
         maximum_prey_size = maximum, 
         order = Order.1.2, 
         family = Family.1.2, 
         genus = Genus.1.2, 
         iucn_status = IUCN.Status.1.2, 
         common_name = Common.Name, 
         guild_with_omnivore = Guild.w.Omnivory, 
         guild_only_herbivory = Guild.only.Herbivory) %>% 
  mutate(species = gsub("_", " ", Binomial.1.2), 
        browser = ifelse(guild_with_omnivore == "Browser" | guild_only_herbivory == "Browser", 1, 0), 
        grazer = ifelse(guild_with_omnivore == "Grazer" | guild_only_herbivory == "Grazer", 1, 0),
        mixed_feeder = ifelse(guild_with_omnivore == "Mixed Feeder" | guild_only_herbivory == "Mixed Feeder", 1, 0),
        omnivore = ifelse(guild_with_omnivore == "Omnivore", 1, 0), 
        feeding_type = as.factor(ifelse(is.na(guild_with_omnivore), "Predator", guild_with_omnivore)), 
        fermentation_type = as.factor(ifelse(is.na(fermentation_simple), "Shortgut", fermentation_simple)),
        mr_mass_kg = mass_kg*0.75) #10.1126/science.276.5309.12

table(mam.traits$browser)

table(mam.traits$trophic_level)
table(mam.traits$feeding_type)
mam.traits[feeding_type == "Predator", species]

# herbivore functional types, hempson et al 2015: https://doi.org/10.1126/science.aac7978

mam.traits[species %in% c("Ammodorcas clarkei", "Cephalophus adersi", 
                          "Cephalophus callipygus", "Cephalophus dorsalis",
                          "Cephalophus harveyi", "Cephalophus jentinki", 
                          "Cephalophus leucogaster", "Cephalophus natalensis",
                          "Cephalophus niger", "Cephalophus nigrifrons", 
                          "Cephalophus ogilbyi", "Cephalophus rufilatus",
                          "Cephalophus silvicultor", "Cephalophus spadix",
                          "Cephalophus weynsi", "Cephalophus zebra", 
                          "Dorcatragus megalotis", "Hyemoschus aquaticus", 
                          "Litocranius walleri", "Madoqua guentheri",
                          "Madoqua kirkii", "Madoqua piacentinii", 
                          "Madoqua saltiana", "Neotragus batesi", 
                          "Neotragus pygmaeus", "Nesotragus moschatus",
                          "Oreotragus oreotragus", "Pelea capreolus", 
                          "Philantomba maxwellii", "Philantomba monticola",
                          "Raphicerus campestris", "Raphicerus melanotis", 
                          "Raphicerus sharpei", "Sylvicapra grimmia",
                          "Tragelaphus scriptus"),
           hempson_herbivore_group := "Small nonsocial browsers"]

mam.traits[species %in% c("Ammotragus lervia", "Antidorcas marsupialis", 
                          "Beatragus hunteri", "Damaliscus lunatus",
                          "Eudorcas albonotata", "Eudorcas rufifrons",
                          "Eudorcas thomsonii", "Gazella dorcas", 
                          "Gazella spekei", "Nanger dama",
                          "Nanger granti", "Nanger soemmerringii",
                          "Oryx beisa", "Oryx gazella", 
                          "Ourebia ourebi", "Redunca redunca",
                          "Tragelaphus imberbis", "Tragelaphus oryx"),
           hempson_herbivore_group := "Medium-sized social mixed diets"]

mam.traits[species %in% c("Capra walie", "Giraffa camelopardalis",
                          "Okapia johnstoni", "Tragelaphus angasii",
                          "Tragelaphus buxtoni", "Tragelaphus derbianus",
                          "Tragelaphus eurycerus", "Tragelaphus strepsiceros"),
           hempson_herbivore_group := "Large browsers"]

mam.traits[species %in% c("Aepyceros melampus", "Alcephalus buselaphus", 
                          "Capra nubiana", "Connochaetes gnou", 
                          "Connochaetes taurinus", "Damaliscus pygargus",
                          "Hippotragus equinus", "Hippotragus niger", 
                          "Kobus ellipsiprymnus", "Kobus kob", 
                          "Kobus leche", "Kobus megaceros",
                          "Kobus vardonii", "Redunca arundinum", 
                          "Redunca fulvorufula", "Syncerus caffer", 
                          "Tragelaphus spekii"),
           hempson_herbivore_group := "Water-dependent grazers"]

mam.traits[species %in% c("Ceratotherium simum", "Choeropsis liberiensis", 
                          "Diceros bicornis", "Equus africanus", 
                          "Equus grevyi", "Equus quagga",
                          "Equus zebra", "Hippopotamus amphibius", 
                          "Hylochoerus meinertzhageni", "Loxodonta africana", 
                          "Phacochoerus aethiopicus", "Phacochoerus africanus",
                          "Potamochoerus larvatus", "Potamochoerus porcus"),
           hempson_herbivore_group := "Nonruminants"]

table(mam.traits$hempson_herbivore_group)

mam.traits[source_herbi == "HerbiTRAITS", trophic_level_hempson := hempson_herbivore_group]
mam.traits[source_carni == "CarniTRAITS", trophic_level_hempson := trophic_level]

table(mam.traits$trophic_level_hempson)
n_distinct(mam.traits$hempson_herbivore_group, na.rm = TRUE)
mam.traits[!hempson_herbivore_group %in% c("Water-dependent grazers",
                                           "Small nonsocial browsers",
                                           "Medium-sized social mixed diets",
                                           "Large browsers",
                                           "Nonruminants"), hempson_herbivore_group := NA]


mam.traits[hempson_herbivore_group == "", hempson_herbivore_group := NA]

names(mam.traits)



traits.final <- mam.traits %>% dplyr::select(-c(Terrestrial))

fwrite(traits.final, "data/mammal_traits.csv")

