library(tidyverse)
library(ggExtra)
library(taxize)
library(rotl)
library(ape)
library(rredlist)

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(letsR)
library(geoshpere)
library(wesanderson)
library(ggtree)
library(ggtreeExtra)
library(treeio)
library(scales)
library(ggbeeswarm)
library(ggridges)
library(ggnewscale)
library(gghalves)
library(ggpol)
library(plotly)
library(extrafont)

hindex <- read.csv(file = "outputs/hindex.csv", header = T)
combinedf <- read.csv(file = "outputs/combinedf.csv", header = T)

combinedf <- combinedf %>%
  rename(genus_species = species)

hindex$genus_species <- str_replace_all(hindex$genus_species, "_", " ")

includeh <- list(hindex, combinedf) %>% 
  reduce(left_join, by = "genus_species")

#combine includeh + domestication_h

includeh <- includeh %>%
  mutate(domestication = NA)

table(indices_df$genus_species %in% includeh$genus_species) #50 unique ones

includeh <- rbind(includeh, indices_df)

#fill in orders and families

includeh[6847, "id"] <- 539171
includeh[6849, "id"] <- 897692
includeh[6890, "id"] <- 906301

#using ott_ids; this is much faster
for (i in 1:length(includeh$id)) {
  if (is.na(includeh$order[i])) {
    print(i)
    tax_df <- reshape::melt(tax_lineage(taxonomy_taxon_info(ott_ids = includeh$id[i], include_lineage = TRUE)))
    tax_filter <- tax_df %>%
      filter(rank == "order")
    order <- tax_filter$unique_name
    includeh$order[i] <- as.character(order)
  }
}

for (i in 1:length(includeh$id)) {
  if (is.na(includeh$family[i])) {
    print(i)
    tax_df <- reshape::melt(tax_lineage(taxonomy_taxon_info(ott_ids = includeh$id[i], include_lineage = TRUE)))
    tax_filter <- tax_df %>%
      filter(rank == "family")
    family <- tax_filter$unique_name
    includeh$family[i] <- as.character(family)
  }
}

#check orders and families

unique(includeh$order)
sum(is.na(includeh$order)) #16

unique(includeh$family)
sum(is.na(includeh$family)) #7

#clean orders and families

includeh$order <- word(includeh$order, 1)

for (i in 1:length(includeh$order)) {
  if (includeh$family[i] == "Chrysochloridae" & is.na(includeh$order[i])) {
    includeh$order[i] <- "Afrosoricida"
  }
}

for (i in 1:length(includeh$order)) {
  if (includeh$family[i] == "Tenrecidae" & is.na(includeh$order[i])) {
    includeh$order[i] <- "Afrosoricida"
  }
}

for (i in 1:length(includeh$family)) {
  if (is.na(includeh$family[i])) {
    includeh$family[i] <- "Cetotheriidae"
  }
}

for (i in 1:length(includeh$order)) {
  if (includeh$order[i] == "Proboscidea (order in Deuterostomia)") {
    includeh$order[i] <- "Proboscidea"
  }
}

#fix non-mammalian orders

for (i in 1:nrow(includeh)) {
  if (includeh$genus_species[i] == "Lepus formosus") {
    includeh$family[i] <- "Leporidae"
    includeh$order[i] <- "Lagomorpha"
  } else if (includeh$genus_species[i] == "Mus colletti") {
    includeh$family[i] <- "Muridae"
    includeh$order[i] <- "Rodentia"
  } else if (includeh$genus_species[i] == "Mus paulina") {
    includeh$family[i] <- "Muridae"
    includeh$order[i] <- "Rodentia"
  } else if (includeh$genus_species[i] == "Mus tomentosus") {
    includeh$family[i] <- "Muridae"
    includeh$order[i] <- "Rodentia"
  } else if (includeh$genus_species[i] == "Sus lybicus") {
    includeh$family[i] <- "Suidae"
    includeh$order[i] <- "Artiodactyla"
  } else if (includeh$genus_species[i] == "Sus palustris") {
    includeh$family[i] <- "Suidae"
    includeh$order[i] <- "Artiodactyla"
  }
}

includeh[6731, "family"] <- "Muridae"
includeh[6731, "order"] <- "Rodentia"
includeh[6731, "clade"] <- "Euarchontoglires"

#remove extinct mammals

includeh <- includeh[!(includeh$order == "Ascaridida"),] #worm
includeh <- includeh[!(includeh$genus == "Homo"),] #humans
includeh <- includeh[!(includeh$genus == "Mammuthus"),] #woolly mammoth

status <- data.frame(tnrs_match_names(names = includeh$genus_species)$flags)
status <- status %>%
  rename(status = tnrs_match_names.names...includeh.genus_species..flags)
includeh <- cbind(includeh, status)

includeh <- includeh[!(includeh$status == "extinct"),] #status from rotl
includeh <- includeh[!(includeh$status == "extinct_inherited"),]

includeh <- includeh[!(includeh$genus_species == "Mylodon darwinii"),] #ground sloth
includeh <- includeh[!(includeh$genus_species == "Orrorin tugenensis"),] #early human
includeh <- includeh[!(includeh$genus_species == "Brandtocetus chongulek"),] #miocene whale

#cleaning iucn status

for (i in 1:nrow(includeh)) {
  if (!is.na(includeh$redlistCategory1[i]) & includeh$redlistCategory1[i] == "Not Applicable") {
    includeh$redlistCategory1[i] <- includeh$redlistCategory2[i]
    includeh$redlistCategory2[i] <- NA
  }
}

for (i in 1:nrow(includeh)) {
  if (is.na(includeh$redlistCategory1[i]) & !is.na(includeh$redlistCategory2[i])) {
    includeh$redlistCategory1[i] <- includeh$redlistCategory2[i]
    includeh$redlistCategory2[i] <- NA
  }
}

for (i in 1:nrow(includeh)) {
  if (is.na(includeh$redlistCategory2[i]) & !is.na(includeh$redlistCategory3[i])) {
    includeh$redlistCategory2[i] <- includeh$redlistCategory3[i]
    includeh$redlistCategory3[i] <- NA
  }
}

includeh$redlistCategory2 <- na_if(includeh$redlistCategory2, "Not Applicable")

for (i in 1:nrow(includeh)) {
  if(!is.na(includeh$redlistCategory3[i])) {
    includeh$redlistCategory2[i] <- includeh$redlistCategory3[i]
    includeh$redlistCategory3[i] <- NA
  }
}

for (i in 1:nrow(includeh)) {
  if(!is.na(includeh$redlistCategory2[i])) {
    includeh$redlistCategory1[i] <- includeh$redlistCategory2[i]
    includeh$redlistCategory2[i] <- NA
  }
}

includeh[443, 21] <- "Vulnerable"
includeh <- includeh[-c(22, 23)]
includeh <- includeh %>%
  rename(redlistCategory = redlistCategory1)

#fill in red list status with rredlist

iucn <- rl_history(name = "Dorcopsis veterum", key = "4eacf586ea255313b1646429c0f5b566cfa6f789cfb634f9704a8050a6123933")#$result[1,3]
length(iucn$result)
iucn <- rl_history(name = "Dugong dugon", key = "4eacf586ea255313b1646429c0f5b566cfa6f789cfb634f9704a8050a6123933")

for (i in 5249:6788) {
  if (is.na(includeh$redlistCategory[i])) {
    print(paste(i, includeh$genus_species[i], "missing IUCN status."))
    iucn_query <- rl_history(name = includeh$genus_species[i], key = "4eacf586ea255313b1646429c0f5b566cfa6f789cfb634f9704a8050a6123933")
    iucn_df <- data.frame(iucn_query$result)
    includeh$redlistCategory <- as.character(includeh$redlistCategory)
    if (nrow(iucn_df > 0)) {
      print(paste("filling in missing data."))
      includeh$redlistCategory[i] <- iucn_df[1,"category"]
    }
  }
}


#LOG TRANSFORM

includeh <- includeh %>%
  mutate(logh = log10(h),
         logmass = log10(BodyMass.Value))

includeh <- includeh %>%
  mutate(logh1 = log10(h+1), .after = logh)

#fill in domestication

for (i in 1:length(includeh$genus_species)) {
  if (includeh$genus_species[i] %in% domesticated$species) {
    includeh$domestication[i] <- "Domesticated"
  } else if (includeh$genus_species[i] %in% partially_domesticated$species) {
    includeh$domestication[i] <- "Partially-domesticated"
  }
}

includeh$domestication[is.na(includeh$domestication)] <- "Wild"

#delete duplicated

unique(includeh$genus_species) #6788
includeh <- includeh %>% #something went wrong here, 110 duplicates
  distinct(genus_species, domestication, .keep_all = TRUE) 
includeh <- includeh[!rev(duplicated(rev(includeh$genus_species))),] #remove first occurrence of duplicate
table(includeh$domestication)
table(indices_df$domestication) #13 domesticated, 146 partially-domesticated

#condense grouping

includeh <- includeh %>%
  mutate(clade = NA)

for (i in 1:length(includeh$order)) {
  if (includeh$order[i] == "Pilosa"|
      includeh$order[i] == "Cingulata") {
    includeh$clade[i] <- "Xenarthra"
  } else if (includeh$order[i] == "Macroscelidea"|
             includeh$order[i] == "Afrosoricida"|
             includeh$order[i] == "Proboscidea"|
             includeh$order[i] == "Sirenia"|
             includeh$order[i] == "Hyracoidea") {
    includeh$clade[i] <- "Afrotheria"
  } else if (includeh$order[i] == "Chiroptera"|
             includeh$order[i] == "Perissodactyla"|
             includeh$order[i] == "Artiodactyla"|
             includeh$order[i] == "Cetacea"|
             includeh$order[i] == "Pholidota"|
             includeh$order[i] == "Carnivora"|
             includeh$order[i] == "Eulipotyphla"|
             includeh$order[i] == "Soricomorpha"|
             includeh$order[i] == "Erinaceomorpha") {
    includeh$clade[i] <- "Laurasiatheria"
  } else if (includeh$order[i] == "Primates"|
             includeh$order[i] == "Scandentia"|
             includeh$order[i] == "Lagomorpha"|
             includeh$order[i] == "Rodentia"|
             includeh$order[i] == "Dermoptera") {
    includeh$clade[i] <- "Euarchontoglires"
  } else if (includeh$order[i] == "Diprotodontia"|
             includeh$order[i] == "Dasyuromorphia"|
             includeh$order[i] == "Microbiotheria"|
             includeh$order[i] == "Peramelemorphia"|
             includeh$order[i] == "Notoryctemorphia"|
             includeh$order[i] == "Monotremata"|
             includeh$order[i] == "Paucituberculata"|
             includeh$order[i] == "Didelphimorphia") {
    includeh$clade[i] <- "Marsupials & monotremes"
  }
}

#PLOTS

#sorting

med_mass <- includeh %>%
  group_by(order) %>%
  summarise_at(vars(BodyMass.Value), median, na.rm = T) %>%
  ungroup()
med_mass <- med_mass %>%
  arrange(by_group = BodyMass.Value)
includeh$order <- factor(includeh$order, levels = med_mass$order)

includeh$clade <- factor(includeh$clade, levels = c("Afrotheria", "Xenarthra", "Euarchontoglires", "Laurasiatheria", "Marsupials & monotremes"))

unique(includeh$redlistCategory)
includeh$redlistCategory <- factor(includeh$redlistCategory, levels = c("Least Concern", "Near Threaten", "Vulnerable",
                                                                        "Endangered", "Critically Endangered",
                                                                        "Regionally Extinct", "Extinct in the Wild", "Extinct",
                                                                        "Data Deficient"))

#h by order
ggplot(includeh, aes(x = logh1,
                     y = order)) +
  geom_boxplot() +
  geom_jitter(aes(colour = redlistCategory),
              size = 2,
              alpha = 0.5) +
  labs(x = "h-index",
       y = "Order",
       colour = "IUCN Red List Category") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = c(0.88, 0.862)) +
  scale_x_continuous() +
  scale_colour_manual(values = c("#0d1e7d", "#194cb3", "#6b40e1", "#aa55ea",
                                 "#ea559d", "#cd2d54", "#951433"),
                      na.value = c("#a5a5a5")) +
  scale_y_discrete(limits = rev)

#h-index
ggplot(includeh, aes(x = reorder(genus_species, logh1),
                     y = logh1)) +
  geom_point(aes(colour = order),
             alpha = 0.5) +
  labs(x = "Species",
       y = "h-index",
       colour = "Order") +
  theme(legend.position = "bottom",
        axis.text.x = element_blank()) 

ggplot(includeh, aes(x = logh1,
                     fill = order)) +
  geom_bar() 

ggplot(includeh, aes(x = h,
                     fill = order)) +
  geom_bar() 

#mass; fix the body mass values
ggplot(includeh, aes(x = logmass,
                     y = logh1,
                     colour = clade)) +
  geom_point(size = 2,
             alpha = 0.4) +
  labs(x = "Body mass (kg; need to correct the unit)",
       y = "h-index") +
  scale_x_log10() +
  coord_trans(y = "log1p") +
  scale_y_continuous(breaks = c(0, 1, 2),
                     labels = c(0, 9, 99)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1))) +
  new_scale_colour() +
  geom_quantile(aes(colour = clade),
                quantiles = 0.5,
                size = 2,
                alpha = 0.8,
                lineend = "round") +
  scale_colour_manual(values = c("#d4ac0d", "#ca6f1e", "#cb4335", "#7d3c98", "#2e86c1")) +
  guides(colour = FALSE) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key = element_rect(fill = "white"),
        legend.position = "top",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_line(colour = "grey80",
                                        linetype = "longdash"))

#iucn category
ggplot(includeh, aes(x = logh1,
                     y = redlistCategory)) +
  geom_quasirandom(aes(colour = clade),
                   groupOnX = FALSE,
                   size = 2,
                   alpha = 0.4) +
  geom_boxplot(fill = "grey80",
               size = 0.8,
               width = 0.4,
               alpha = 0.2) +
  labs(x = "h-index") +
  scale_x_continuous(breaks = c(0, 1, 2),
                     labels = c(0, 9, 99)) +
  scale_y_discrete(limits = rev,
                   labels = label_wrap(16)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1))) +
  theme(axis.title = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 14,
                                   colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key = element_rect(fill = "white"),
        legend.position = "top",
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey80"),
        panel.grid.minor.x = element_line(colour = "grey80",
                                          linetype = "longdash")) 

#human use
includeh_pivot <- includeh %>%
  pivot_longer(cols = starts_with("use"),
               names_to = "use_count",
               values_to = "human_use")
includeh_pivot$human_use <- str_to_title(toupper(includeh_pivot$human_use)) #first letter uppercase
includeh_pivot$human_use <- str_replace_all(includeh_pivot$human_use, "_", " ")

for (i in 1:nrow(includeh_pivot)) {
  if (!is.na(includeh_pivot$human_use[i]) & includeh_pivot$human_use[i] == "Food animal") {
    includeh_pivot$human_use[i] <- "Food (for animals)"
  } else if (!is.na(includeh_pivot$human_use[i]) & includeh_pivot$human_use[i] == "Food human") {
    includeh_pivot$human_use[i] <- "Food (for humans)"
  } else if (!is.na(includeh_pivot$human_use[i]) & includeh_pivot$human_use[i] == "Handicrafts jewellery") {
    includeh_pivot$human_use[i] <- "Handicrafts & jewellery"
  } else if (!is.na(includeh_pivot$human_use[i]) & includeh_pivot$human_use[i] == "Pets display animals horticulture") {
    includeh_pivot$human_use[i] <- "Pets, display animals & horticulture"
  } else if (!is.na(includeh_pivot$human_use[i]) & includeh_pivot$human_use[i] == "Sport hunting specimen collecting") {
    includeh_pivot$human_use[i] <- "Sport hunting & specimen collecting"
  } else if (!is.na(includeh_pivot$human_use[i]) & includeh_pivot$human_use[i] == "Wearing apparel accessories") {
    includeh_pivot$human_use[i] <- "Wearing apparel & accessories"
  } 
}

includeh_pivot <- includeh_pivot %>%
  mutate(human_use_group = NA)

for (i in 1:length(includeh_pivot$human_use)) {
  if (!is.na(includeh_pivot$human_use[i]) &
      (includeh_pivot$human_use[i] == "Food (for humans)"|
      includeh_pivot$human_use[i] == "Food (for animals)")) {
    includeh_pivot$human_use_group[i] <- "Food"
  } else if (!is.na(includeh_pivot$human_use[i]) &
             (includeh_pivot$human_use[i] == "Fuels"|
             includeh_pivot$human_use[i] == "Manufacturing chemicals"|
             includeh_pivot$human_use[i] == "Other chemicals"|
             includeh_pivot$human_use[i] == "Poisons")) {
    includeh_pivot$human_use_group[i] <- "Chemicals"
  } else if (!is.na(includeh_pivot$human_use[i]) &
             (includeh_pivot$human_use[i] == "Wearing apparel & accessories"|
             includeh_pivot$human_use[i] == "Handicrafts & jewellery")) {
    includeh_pivot$human_use_group[i] <- "Clothes & accessories"
  } else if (!is.na(includeh_pivot$human_use[i]) &
             (includeh_pivot$human_use[i] == "Sport hunting & specimen collecting"|
             includeh_pivot$human_use[i] == "Pets, display animals & horticulture")) {
    includeh_pivot$human_use_group[i] <- "Recreational activities"
  } else if (!is.na(includeh_pivot$human_use[i]) &
             (includeh_pivot$human_use[i] == "Construction or structural materials"|
             includeh_pivot$human_use[i] == "Fibre"|
             includeh_pivot$human_use[i] == "Establishing ex situ production")) {
    includeh_pivot$human_use_group[i] <- "Industrial goods"
  } else if (!is.na(includeh_pivot$human_use[i]) &
             includeh_pivot$human_use[i] == "Other household goods") {
    includeh_pivot$human_use_group[i] <- "Other consumer goods"
  } else if (!is.na(includeh_pivot$human_use[i]) &
             (includeh_pivot$human_use[i] == "Research"|
             includeh_pivot$human_use[i] == "Medicine")) {
    includeh_pivot$human_use_group[i] <- "Research & medicine"
  } else if (!is.na(includeh_pivot$human_use[i]) &
             (includeh_pivot$human_use[i] == "Other")) {
    includeh_pivot$human_use_group[i] <- "Others"
  } else if (is.na(includeh_pivot$human_use[i])|
             includeh_pivot$human_use[i] == "Unknown") {
    includeh_pivot$human_use_group[i] <- "Unknown"
  }
}

med_use <- includeh_pivot %>%
  group_by(human_use_group) %>%
  summarise_at(vars(logh1), median, na.rm = T) %>%
  ungroup()
med_use <- med_use %>%
  arrange(logh1)
med_use <- med_use[c(2, 4:9, 3, 1),]

includeh_pivot$human_use_group <- factor(includeh_pivot$human_use_group, levels = med_use$human_use_group)

ggplot(includeh_pivot, aes(x = logh1,
                           y = human_use_group)) +
  geom_quasirandom(aes(colour = clade),
                   groupOnX = FALSE,
                   size = 2,
                   alpha = 0.4) +
  geom_boxplot(fill = "grey80",
               size = 0.8,
               width = 0.4,
               alpha = 0.2) +
  labs(x = "h-index") +
  scale_x_continuous(breaks = c(0, 1, 2),
                     labels = c(0, 9, 99)) +
  scale_y_discrete(limits = rev,
                   labels = label_wrap(18)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1))) +
  theme(axis.title = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 14,
                                   colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key = element_rect(fill = "white"),
        legend.position = "top",
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey80"),
        panel.grid.minor.x = element_line(colour = "grey80",
                                          linetype = "longdash"))
  

#domestication
ggplot(includeh, aes(x = logh1,
                     y = reorder(domestication, logh1))) +
  geom_quasirandom(aes(colour = clade),
                   groupOnX = FALSE,
                   size = 2,
                   alpha = 0.4) +
  geom_boxplot(fill = "grey80",
               size = 0.8,
               width = 0.4,
               alpha = 0.2) +
  labs(x = "h-index",
       colour = "Clade") +
  scale_x_continuous(breaks = c(0, 1, 2),
                     labels = c(0, 9, 99)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1))) +
  theme(axis.title = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 14,
                                   colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key = element_rect(fill = "white"),
        legend.position = "top",
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey80"),
        panel.grid.minor.x = element_line(colour = "grey80",
                                        linetype = "longdash")) +
  scale_y_discrete(labels = function(x) sub("-","-\n", x, fixed = TRUE))

#latitude
med_lat <- ggplot(includeh, aes(x = median_lat,
                                y = logh1,
                                colour = clade)) +
  geom_point(size = 2,
             alpha = 0.4) +
  geom_smooth(colour = "black") +
  labs(x = "Latitude (median)",
       y = "h-index",
       colour = "Clade") +
  scale_y_continuous(breaks = c(0, 1, 2),
                     labels = c(0, 9, 99)) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key = element_rect(fill = "white"),
        legend.position = "top",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_line(colour = "grey80",
                                        linetype = "longdash")) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1)))

ggMarginal(med_lat,
           type = "histogram",
           margins = "x",
           bins = 100)

#map
sbs <- read.csv(file = "intermediate_data/gbif_processed.csv", header = T)

PAM <- lets.presab.points(cbind(sbs$decimalLongitude,sbs$decimalLatitude), sbs$species,
                          xmn = -180, xmx = 180, 
                          ymn = -90, ymx = 90,resol = 2)
summary(PAM)
plot(PAM)
#lets.midpoint.fixed() in file "R/geo_plotting"
mid <- lets.midpoint.fixed(PAM)
mid$x<-as.numeric(mid$x)
mid$y<-as.numeric(mid$y)
mid <- mid %>%
  rename(genus_species = Species)
includeh <- list(includeh, mid) %>%
  reduce(left_join, by = "genus_species")

world <- ne_countries(scale = "large", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  geom_point(data = includeh, aes(x = x,
                                  y = y,
                                  colour = logh1),
             size = 2,
             alpha = 0.4) +
  coord_sf(expand = FALSE) +
  labs(x = "Longitude",
       y = "Latitude",
       colour = "h-index") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey80",
                                        linetype = "dashed")) +
  scale_colour_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous")) 

#phylogenetic tree
tree <- tol_induced_subtree(ott_ids = includeh$id, label_format = "name")

includeh_join <- includeh %>%
  rename(label = genus_species)
includeh_join$label <- str_replace_all(includeh_join$label, " ", "_")

tree <- as_tibble(tree)
tree_join <- full_join(tree, includeh_join, by = "label")
tree_join <- as.treedata(tree_join)

ggtree(tree_join,
       layout = "circular") +
  geom_tippoint(aes(colour = clade)) +
  geom_fruit(geom = geom_bar,
             mapping = aes(x = h,
                           colour = clade),
             pwidth = 0.5,
             orientation = "y", 
             stat = "identity") +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db")) +
  guides(colour = guide_legend(override.aes = list(shape = 16,
                                                   size = 4))) + #shape of legend icons not changing need to find out why
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14)) 

#not related to h-index
ggplot(includeh, aes(y = order)) +
  geom_bar(aes(fill = redlistCategory),
           position = "fill") +
  labs(x = "Proportion",
       y = "Order",
       fill = "IUCN Red List Category") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#0d1e7d", "#194cb3", "#6b40e1", "#aa55ea",
                               "#ea559d", "#cd2d54", "#951433"),
                    na.value = c("#a5a5a5"))

ggplot(includeh, aes(x = log10(years_publishing),
                     y = logh1)) +
  geom_point(aes(size = log10(m+1)),
             alpha = 0.5)

#save and read

write.csv(includeh, file = "outputs/includeh.csv")
includeh <- read.csv(file = "outputs/includeh.csv")[-c(1)]

write.csv(indices_df, file = "intermediate_data/domestication_h.csv")
indices_df <- read.csv(file = "intermediate_data/domestication_h.csv", header = T)[-c(1)]

write.tree(tree, "intermediate_data/tree.")

#testing

test <- includeh[1761:1770,]

for (i in 1:length(test$genus_species)) {
  if (is.na(test$redlistCategory[i])) {
    iucn_query <- rl_history(name = test$genus_species[i], key = "4eacf586ea255313b1646429c0f5b566cfa6f789cfb634f9704a8050a6123933")
    iucn_df <- data.frame(iucn_query$result)
    test$redlistCategory <- as.character(test$redlistCategory)
    if (nrow(iucn_df > 0)) {
      test$redlistCategory[i] <- iucn_df[1,"category"]
    }
  }
}

