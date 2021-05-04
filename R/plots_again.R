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
library(sysfonts)

#loading df

combinedf_dup <- read.csv(file = "outputs/data/combinedf_dup.csv", header = T)[-c(1)]
indices_df <- read.csv(file = "outputs/data/hindex.csv", header = T)[-c(1)]
includeh <- read.csv(file = "outputs/data/includeh.csv")[-c(1)] 

write.csv(includeh, file = "outputs/data/includeh.csv")

#combining

indices_df$genus_species <- str_replace(indices_df$genus_species, "_", " ")
names(combinedf_dup)[names(combinedf_dup) == "species"] <- "genus_species"
includeh <- left_join(indices_df, combinedf_dup,
                      by = "genus_species")

#some cleaning
includeh$domestication[includeh$genus_species == "Bos taurus"] <- "Domesticated" #making the cow domesticated

#TRANSFORMATIONS

includeh <- includeh %>%
  mutate(logh = log10(h),
         logmass = log10(BodyMass.Value))

includeh <- includeh %>%
  mutate(logh1 = log10(h+1), .after = logh)

#sorting

includeh$clade <- factor(includeh$clade, levels = c("Afrotheria", "Xenarthra", "Euarchontoglires", "Laurasiatheria", "Marsupials & monotremes"))

unique(includeh$redlistCategory)
includeh$redlistCategory <- factor(includeh$redlistCategory, levels = c("Least Concern", "Near Threaten", "Vulnerable",
                                                                        "Endangered", "Critically Endangered",
                                                                        "Extinct in the Wild", "Extinct", "Data Deficient"))

#themes

source("R/themes.R")

#add font

font_add_google("Roboto")

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

#mass
ggplot(includeh, aes(x = logmass,
                     y = logh1,
                     colour = clade)) +
  geom_point(size = 2,
             alpha = 0.4) +
  labs(x = "Body mass (kg)",
       y = "h-index") +
  scale_x_log10() +
  scale_y_log10() +
  coord_trans(x = "log1p",
              y = "log1p") +
  scale_x_continuous(breaks = c(0.3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0),
                     labels = c(0.002, 0.01, 0.1, 1, 10, 100, "1,000", "10,000", "100,000")) +
  scale_y_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
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
  themebyjess_light_point()

#iucn category
ggplot(includeh, aes(x = logh1,
                     y = redlistCategory)) +
  geom_quasirandom(aes(colour = clade),
                   groupOnX = FALSE,
                   size = 2,
                   alpha = 0.4) +
  geom_boxplot(fill = "grey80",
               position = position_dodge(width = 0.5),
               size = 0.8,
               width = 0.4,
               alpha = 0.2) +
  labs(x = "h-index") +
  scale_x_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_y_discrete(limits = rev,
                   labels = label_wrap(16)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1))) +
  themebyjess_light_boxplot()

#EW: oryx dammah, elaphurus davidianus

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
  scale_x_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_y_discrete(limits = rev,
                   labels = label_wrap(18)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1))) +
  themebyjess_light_boxplot()


#domestication
ggplot(includeh, aes(x = logh1,
                     y = reorder(domestication, logh1))) +
  geom_quasirandom(aes(colour = clade),
                   groupOnX = FALSE,
                   size = 2,
                   alpha = 0.4) +
  geom_boxplot(fill = "grey80",
               face = "bold",
               size = 0.8,
               width = 0.4,
               alpha = 0.2) +
  labs(x = "h-index",
       colour = "Clade") +
  scale_x_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_y_discrete(labels = function(x) sub("-","-\n", x, fixed = TRUE)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1))) +
  themebyjess_light_boxplot()

#latitude
ggplot(includeh, aes(x = median_lat,
                     y = logh1,
                     colour = clade)) +
  geom_point(size = 2,
             alpha = 0.4) +
  geom_smooth(colour = "black") +
  labs(x = "Latitude (median)",
       y = "h-index",
       colour = "Clade") +
  scale_x_continuous(breaks = c(-40, 0, 40, 80),
                     labels = c("-40°", 0, "40°", "80°")) +
  scale_y_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1))) +
  themebyjess_light_point()

#map

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
  scale_colour_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous"),
                         labels = c(0, 2, 9, 31, 99, 316)) +
  themebyjess_light_map()



#phylogenetic tree

includeh_join <- includeh %>%
  rename(label = genus_species)
#includeh_join$label <- str_replace_all(includeh_join$label, " ", "_")

all_names <- tnrs_match_names(includeh$genus_species)
in_tree <- is_in_tree(ott_id(all_names))
tree <- tol_induced_subtree(ott_id(all_names)[in_tree])

taxa <- tnrs_match_names(includeh$genus_species, context = "Animals")
tr <- tol_induced_subtree(ott_ids = includeh$id)

tree <- as_tibble(tree)
tree_join <- full_join(tree, includeh_join, by = "label")
tree_join <- as.treedata(tree_join)

saveRDS(tree_join, "outputs/tree_join.rds")
tree_join <- readRDS("outputs/tree_join.rds")

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
        legend.text = element_text(family = "Roboto",
                                   size = 14)) 

tree_2 <- ggtree(tree_join,
                 layout = "circular") +
  geom_tippoint(aes(colour = logh1)) +
  scale_colour_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous"),
                         labels = c(0, 2, 9, 31, 99, 316)) +
  guides(colour = guide_legend(override.aes = list(shape = 16,
                                                   size = 4))) + #shape of legend icons not changing need to find out why
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(family = "Roboto",
                                   size = 14)) 
  
  
#google trends

ggplot(includeh, aes(x = log_sumgtrends,
                     y = logh1,
                     colour = clade)) +
  geom_point(size = 2,
             alpha = 0.5) +
  labs(x = "Google Trends index (sum)",
       y = "h-index") +
  scale_x_continuous(breaks = c(0, 2, 3, 4),
                     labels = c(0, 100, "1,000", "10,000")) +
  scale_y_continuous(breaks = c(0, 1, 2),
                     labels = c(0, 9, 99)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1))) +
  #guides(colour = FALSE) +
  themebyjess_light_point()

#supp
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
