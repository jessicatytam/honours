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

combinedf2 <- read.csv(file = "outputs/combinedf2.csv", header = T)[-c(1)]
write.csv(combinedf2, file = "outputs/combinedf2.csv")
includeh <- read.csv(file = "outputs/includeh.csv")[-c(1)] #only for checking here

#more cleaning

combinedf2$genus_species <- str_replace(combinedf2$genus_species, "_", " ")

for (i in 1:length(combinedf2$genus_species)) { #fill in red list status
  if (is.na(combinedf2$redlistCategory[i])) {
    print(paste(i, combinedf2$genus_species[i], "missing IUCN status."))
    iucn_query <- rl_history(name = combinedf2$genus_species[i], key = "4eacf586ea255313b1646429c0f5b566cfa6f789cfb634f9704a8050a6123933")
    iucn_df <- data.frame(iucn_query$result)
    combinedf2$redlistCategory <- as.character(combinedf2$redlistCategory)
    if (nrow(iucn_df > 0)) {
      print(paste("filling in missing data."))
      combinedf2$redlistCategory[i] <- iucn_df[1,"category"]
    }
  }
}

#sorting

med_mass <- combinedf2 %>%
  group_by(order) %>%
  summarise_at(vars(BodyMass.Value), median, na.rm = T) %>%
  ungroup()
med_mass <- med_mass %>%
  arrange(by_group = BodyMass.Value)
combinedf2$order <- factor(combinedf2$order, levels = med_mass$order)

combinedf2$clade <- factor(combinedf2$clade, levels = c("Afrotheria", "Xenarthra", "Euarchontoglires", "Laurasiatheria", "Marsupials & monotremes"))

unique(combinedf2$redlistCategory)
combinedf2$redlistCategory <- factor(combinedf2$redlistCategory, levels = c("Least Concern", "Near Threaten", "Vulnerable",
                                                                        "Endangered", "Critically Endangered",
                                                                        "Regionally Extinct", "Extinct in the Wild", "Extinct",
                                                                        "Data Deficient"))

#add font

font_add_google("Source Sans Pro")

#h by order
ggplot(combinedf2, aes(x = logh1,
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
ggplot(combinedf2, aes(x = reorder(genus_species, logh1),
                     y = logh1)) +
  geom_point(aes(colour = order),
             alpha = 0.5) +
  labs(x = "Species",
       y = "h-index",
       colour = "Order") +
  theme(legend.position = "bottom",
        axis.text.x = element_blank()) 

ggplot(combinedf2, aes(x = logh1,
                     fill = order)) +
  geom_bar() 

ggplot(combinedf2, aes(x = h,
                     fill = order)) +
  geom_bar() 

#mass
ggplot(combinedf2, aes(x = logmass,
                     y = logh1,
                     colour = clade)) +
  geom_point(size = 2,
             alpha = 0.4) +
  labs(x = "Body mass (g)",
       y = "h-index") +
  scale_x_log10() +
  scale_y_log10() +
  coord_trans(x = "log1p",
              y = "log1p") +
  scale_x_continuous(breaks = c(0.3, 1.0, 2.0, 3.0, 4.0, 5.0),
                     labels = c(2.0, 10.0, 100.0, "1,000", "10,000", "100,000")) +
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
ggplot(combinedf2, aes(x = logh1,
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
        legend.position = "top",v
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey80"),
        panel.grid.minor.x = element_line(colour = "grey80",
                                          linetype = "longdash")) 

#EW: oryx dammah, elaphurus davidianus

#human use
combinedf2_pivot <- combinedf2 %>%
  pivot_longer(cols = starts_with("use"),
               names_to = "use_count",
               values_to = "human_use")
combinedf2_pivot$human_use <- str_to_title(toupper(combinedf2_pivot$human_use)) #first letter uppercase
combinedf2_pivot$human_use <- str_replace_all(combinedf2_pivot$human_use, "_", " ")

for (i in 1:nrow(combinedf2_pivot)) {
  if (!is.na(combinedf2_pivot$human_use[i]) & combinedf2_pivot$human_use[i] == "Food animal") {
    combinedf2_pivot$human_use[i] <- "Food (for animals)"
  } else if (!is.na(combinedf2_pivot$human_use[i]) & combinedf2_pivot$human_use[i] == "Food human") {
    combinedf2_pivot$human_use[i] <- "Food (for humans)"
  } else if (!is.na(combinedf2_pivot$human_use[i]) & combinedf2_pivot$human_use[i] == "Handicrafts jewellery") {
    combinedf2_pivot$human_use[i] <- "Handicrafts & jewellery"
  } else if (!is.na(combinedf2_pivot$human_use[i]) & combinedf2_pivot$human_use[i] == "Pets display animals horticulture") {
    combinedf2_pivot$human_use[i] <- "Pets, display animals & horticulture"
  } else if (!is.na(combinedf2_pivot$human_use[i]) & combinedf2_pivot$human_use[i] == "Sport hunting specimen collecting") {
    combinedf2_pivot$human_use[i] <- "Sport hunting & specimen collecting"
  } else if (!is.na(combinedf2_pivot$human_use[i]) & combinedf2_pivot$human_use[i] == "Wearing apparel accessories") {
    combinedf2_pivot$human_use[i] <- "Wearing apparel & accessories"
  } 
}

combinedf2_pivot <- combinedf2_pivot %>%
  mutate(human_use_group = NA)

for (i in 1:length(combinedf2_pivot$human_use)) {
  if (!is.na(combinedf2_pivot$human_use[i]) &
      (combinedf2_pivot$human_use[i] == "Food (for humans)"|
       combinedf2_pivot$human_use[i] == "Food (for animals)")) {
    combinedf2_pivot$human_use_group[i] <- "Food"
  } else if (!is.na(combinedf2_pivot$human_use[i]) &
             (combinedf2_pivot$human_use[i] == "Fuels"|
              combinedf2_pivot$human_use[i] == "Manufacturing chemicals"|
              combinedf2_pivot$human_use[i] == "Other chemicals"|
              combinedf2_pivot$human_use[i] == "Poisons")) {
    combinedf2_pivot$human_use_group[i] <- "Chemicals"
  } else if (!is.na(combinedf2_pivot$human_use[i]) &
             (combinedf2_pivot$human_use[i] == "Wearing apparel & accessories"|
              combinedf2_pivot$human_use[i] == "Handicrafts & jewellery")) {
    combinedf2_pivot$human_use_group[i] <- "Clothes & accessories"
  } else if (!is.na(combinedf2_pivot$human_use[i]) &
             (combinedf2_pivot$human_use[i] == "Sport hunting & specimen collecting"|
              combinedf2_pivot$human_use[i] == "Pets, display animals & horticulture")) {
    combinedf2_pivot$human_use_group[i] <- "Recreational activities"
  } else if (!is.na(combinedf2_pivot$human_use[i]) &
             (combinedf2_pivot$human_use[i] == "Construction or structural materials"|
              combinedf2_pivot$human_use[i] == "Fibre"|
              combinedf2_pivot$human_use[i] == "Establishing ex situ production")) {
    combinedf2_pivot$human_use_group[i] <- "Industrial goods"
  } else if (!is.na(combinedf2_pivot$human_use[i]) &
             combinedf2_pivot$human_use[i] == "Other household goods") {
    combinedf2_pivot$human_use_group[i] <- "Other consumer goods"
  } else if (!is.na(combinedf2_pivot$human_use[i]) &
             (combinedf2_pivot$human_use[i] == "Research"|
              combinedf2_pivot$human_use[i] == "Medicine")) {
    combinedf2_pivot$human_use_group[i] <- "Research & medicine"
  } else if (!is.na(combinedf2_pivot$human_use[i]) &
             (combinedf2_pivot$human_use[i] == "Other")) {
    combinedf2_pivot$human_use_group[i] <- "Others"
  } else if (is.na(combinedf2_pivot$human_use[i])|
             combinedf2_pivot$human_use[i] == "Unknown") {
    combinedf2_pivot$human_use_group[i] <- "Unknown"
  }
}

med_use <- combinedf2_pivot %>%
  group_by(human_use_group) %>%
  summarise_at(vars(logh1), median, na.rm = T) %>%
  ungroup()
med_use <- med_use %>%
  arrange(logh1)
med_use <- med_use[c(2, 4:9, 3, 1),]

combinedf2_pivot$human_use_group <- factor(combinedf2_pivot$human_use_group, levels = med_use$human_use_group)

ggplot(combinedf2_pivot, aes(x = logh1,
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
ggplot(combinedf2, aes(x = logh1,
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
ggplot(combinedf2, aes(x = median_lat,
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

#map

world <- ne_countries(scale = "large", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  geom_point(data = combinedf2, aes(x = x,
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
#combinedf2 <- combinedf2 %>%
#  unique()
tree <- tol_induced_subtree(ott_ids = combinedf2$id, label_format = "name")

combinedf2_join <- combinedf2 %>%
  rename(label = genus_species)
combinedf2_join$label <- str_replace_all(combinedf2_join$label, " ", "_")

tree <- as_tibble(tree)
tree_join <- full_join(tree, combinedf2_join, by = "label")
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

#supp
ggplot(combinedf2, aes(y = order)) +
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

ggplot(combinedf2, aes(x = log10(years_publishing),
                     y = logh1)) +
  geom_point(aes(size = log10(m+1)),
             alpha = 0.5)
