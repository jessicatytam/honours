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
library(ggstream)
library(reshape2)
library(ggrepel)
library(ggpubr)

#loading df

combinedf_dup <- read.csv(file = "outputs/data/combinedf_dup.csv", header = T)[-c(1)]
indices_df <- read.csv(file = "outputs/data/hindex.csv", header = T)[-c(1)]
includeh <- read.csv(file = "outputs/data/includeh.csv")[-c(1)] 
tree100 <- tree100 <- readRDS("data/intermediate_data/tree100.nex")
scopus_results1 <- readRDS("data/intermediate_data/scopus_results1.RDS")
scopus_results2 <- readRDS("data/intermediate_data/scopus_results2.RDS")

write.csv(includeh, file = "outputs/data/includeh.csv")

#combining

indices_df$genus_species <- str_replace(indices_df$genus_species, "_", " ")
names(combinedf_dup)[names(combinedf_dup) == "species"] <- "genus_species"
includeh <- left_join(indices_df, combinedf_dup,
                      by = "genus_species")

#some cleaning
includeh$domestication[includeh$genus_species == "Bos taurus"] <- "Domesticated" #making the cow domesticated
includeh <- includeh[-c(2886),] #remove homo sapiens

#TRANSFORMATIONS

includeh <- includeh %>%
  mutate(logh = log10(h),
         logmass = log10(BodyMass.Value))

includeh <- includeh %>%
  mutate(logh1 = log10(h+1), .after = logh)

includeh <- includeh %>%
  mutate(log_sumgtrends = log10(sum_gtrends+1), .after = sum_gtrends)

#sorting

includeh$clade <- factor(includeh$clade, levels = c("Afrotheria", "Xenarthra", "Euarchontoglires", "Laurasiatheria", "Marsupials & monotremes"))
includeh$order <- factor(includeh$order, levels = med_mass$order)
unique(includeh$redlistCategory)
includeh$redlistCategory <- factor(includeh$redlistCategory, levels = c("Least Concern", "Near Threaten", "Vulnerable",
                                                                        "Endangered", "Critically Endangered",
                                                                        "Extinct in the Wild", "Extinct", "Data Deficient"))

#themes

source("R/themes.R")

#h distribution

h_dist <- ggplot(includeh, aes(x = h)) +
  labs(x = expression(bold(paste("species ", italic(h), "-index"))),
       y = "Frequency of species") +
  geom_bar() +
  themebyjess_light_point()

ggplot2::ggsave("outputs/h_dist.png", h_dist, width = 16, height = 9, units = "in", dpi = 300)

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
h100 <- ggplot(includeh %>% filter(h>99),
       aes(x = h,
           y = reorder(genus_species, h),
           fill = order)) +
  geom_segment(aes(x = 0,
                   xend = h,
                   y = reorder(genus_species, h),
                   yend = reorder(genus_species, h)),
               size = 1,
               colour = "grey70") +
  geom_point(size = 4.5,
             alpha = 0.8,
             shape = 21,
             stroke = 1.5) +
  labs(x = expression(bold(paste("species ", italic(h), "-index")))) +
  scale_fill_manual(values = c("#f1c40f", "#E98935", "#e74c3c", "#8e44ad", "#3498db", "#2ECC71"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1),
                                           nrow = 1)) +
  themebyjess_light_col()

ggplot2::ggsave("outputs/h100.png", h100, width = 16, height = 9, units = "in", dpi = 300)
ggplot2::ggsave("outputs/h100_text.png", h100, width = 16, height = 13, units = "in", dpi = 300)

allh <- ggplot(includeh,
       aes(x = h,
           y = reorder(genus_species, h))) +
  geom_point(size = 1,
             alpha = 0.2) +
  labs(x = "h-index") + 
  scale_y_discrete(expand = c(0.005, 0.005)) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(family = "Lato",
                                   size = 12,
                                   colour = "grey30"),
        axis.text.y = element_blank(),
        axis.line = element_line(size = 1.05,
                                 colour = "grey20"),
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Roboto",
                                   size = 20,
                                   colour = "black"),
        legend.key = element_rect(fill = "white"),
        legend.position = "top",
        legend.justification = "centre",
        plot.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black",
                                    fill = NA,
                                    size = 3),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.grid.minor.x = element_line(colour = "grey90",
                                          linetype = "longdash"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

ggplot2::ggsave("outputs/allh.png", allh, width = 9, height = 16, units = "in", dpi = 300)

ggplot(includeh %>%
         filter(h == 0),
       aes(y = reorder(order, desc(order)))) +
  geom_bar() +
  themebyjess_light_col()

orders_sum <- includeh %>% 
  group_by(order) %>% 
  summarise(n = n())
# Compute percentages
orders_sum$fraction = orders_sum$n / sum(orders_sum$n)
# Compute the cumulative percentages (top of each rectangle)
orders_sum$ymax = cumsum(orders_sum$fraction)
# Compute the bottom of each rectangle
orders_sum$ymin = c(0, head(orders_sum$ymax, n = -1))

doughnut <- ggplot(orders_sum,
       aes(xmax = 4,
           xmin = 3,
           ymax = ymax,
           ymin = ymin,
           fill = order)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  labs(title = "Species diversity") +
  scale_fill_manual(values = c("#FAEBB3",
                               "#F7E28D", "#F6DB6E", "#F4D34E", "#F3CC2F",
                               "#f1c40f", "#EFB519", "#EDA722", "#EB982C",
                               "#E98935", "#E97A37", "#E86B39", "#E85B3A",
                               "#e74c3c", "#D14A58", "#BB4875", "#A44691",
                               "#8e44ad", "#7859B9", "#616EC4", "#4B83D0",
                               "#3498db", "#33A5C1", "#31B2A6", "#30BF8C",
                               "#2ECC71", "#4AD384", "#65DA97", "#81E0AA",
                               "#8CE3B2")) +
  theme(plot.title = element_text(family = "Lato",
                                  face = "bold",
                                  size = 36,
                                  colour = "black",
                                  hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

ggplot2::ggsave("outputs/doughnut.png", doughnut, width = 8, height = 8, units = "in", dpi = 300)

#mass
mass_plot <- ggplot(includeh, aes(x = logmass,
                     y = logh1,
                     colour = clade)) +
  geom_point(size = 3,
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
                size = 2.5,
                alpha = 0.8,
                lineend = "round") +
  scale_colour_manual(values = c("#d4ac0d", "#ca6f1e", "#cb4335", "#7d3c98", "#2e86c1")) +
  guides(colour = FALSE) +
  themebyjess_light_point()

ggplot2::ggsave("outputs/logh_vs_logmass.png", mass_plot, width = 16, height = 9, units = "in", dpi = 300)

#iucn category
iucn_plot <- ggplot(includeh, aes(x = logh1,
                     y = redlistCategory)) +
  geom_quasirandom(aes(colour = clade),
                   groupOnX = FALSE,
                   size = 3,
                   alpha = 0.4) +
  geom_boxplot(fill = "grey80",
               position = position_dodge(width = 0.5),
               size = 0.8,
               width = 0.4,
               alpha = 0.2,
               outlier.shape = NA) +
  labs(x = expression(bold(paste("species ", italic(h), "-index")))) +
  scale_x_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_y_discrete(limits = rev,
                   labels = label_wrap(16)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1))) +
  themebyjess_light_boxplot()

ggplot2::ggsave("outputs/logh_vs_iucn.png", iucn_plot, width = 16, height = 9, units = "in", dpi = 300)

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

humanuse_plot <- ggplot(includeh_pivot, aes(x = logh1,
                           y = human_use_group)) +
  geom_quasirandom(aes(colour = clade),
                   groupOnX = FALSE,
                   size = 3,
                   alpha = 0.4) +
  geom_boxplot(fill = "grey80",
               size = 0.8,
               width = 0.4,
               alpha = 0.2,
               outlier.shape = NA) +
  labs(x = expression(bold(paste("species ", italic(h), "-index")))) +
  scale_x_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_y_discrete(limits = rev,
                   labels = label_wrap(18)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1))) +
  themebyjess_light_boxplot()

ggplot2::ggsave("outputs/logh_vs_humanuse.png", humanuse_plot, width = 16, height = 9, units = "in", dpi = 300)

#domestication
domestication_plot <- ggplot(includeh, aes(x = logh1,
                     y = reorder(domestication, logh1))) +
  geom_quasirandom(aes(colour = clade),
                   groupOnX = FALSE,
                   size = 3,
                   alpha = 0.4) +
  geom_boxplot(fill = "grey80",
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

ggplot2::ggsave("outputs/logh_vs_domestication.png", domestication_plot, width = 16, height = 9, units = "in", dpi = 300)

#latitude
lat_plot <- ggplot(includeh, aes(x = median_lat,
                     y = logh1,
                     colour = clade)) +
  geom_point(size = 3,
             alpha = 0.4) +
  geom_smooth(colour = "black",
              size = 1.2) +
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

ggplot2::ggsave("outputs/logh_vs_lat.png", lat_plot, width = 16, height = 9, units = "in", dpi = 300)

#map

world <- ne_countries(scale = "large", returnclass = "sf")

iucnmap_plot <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = includeh %>% 
               drop_na(iucn_bin), aes(x = x,
                                      y = y,
                                      colour = factor(iucn_bin),
                                      size = logh1*3,
                                      alpha = logh1*3)) + 
  coord_sf(expand = FALSE) +
  labs(x = "Longitude",
       y = "Latitude",
       colour = "IUCN Red List status",
       size = expression(bold(paste("species ", italic(h), "-index"))),
       alpha = expression(bold(paste("species ", italic(h), "-index")))) +
  scale_colour_manual(values = c("#6B97DB", "#876CD9", "#A340D7", "#B83480", "#CC2828"),
                      labels = c("Least Concern", "Vulnerable", "Endangered", "Critically Endangered", "Extinct in the Wild"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1))) +
  themebyjess_light_map()

ggplot2::ggsave("outputs/iucn_map2.png", iucnmap_plot, width = 16, height = 9, units = "in", dpi = 300)

iucnmap2_plot <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = includeh %>% 
               drop_na(iucn_bin), aes(x = x,
                                  y = y,
                                  colour = logh1,
                                  size = iucn_bin,
                                  alpha = iucn_bin)) + 
  coord_sf(expand = FALSE) +
  labs(x = "Longitude",
       y = "Latitude",
       colour = expression(bold(paste("species ", italic(h), "-index"))),
       size = "IUCN Red List status",
       alpha = "IUCN Red List status") +
  scale_colour_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous"),
                         labels = c(0, 2, 9, 31, 99, 316)) +
  scale_size(range = c(3, 7),
             labels = c("Least Concern", "Vulnerable", "Endangered", "Critically Endangered", "Extinct in the Wild")) +
  scale_alpha(range = c(0.2, 0.6),
              labels = c("Least Concern", "Vulnerable", "Endangered", "Critically Endangered", "Extinct in the Wild")) +
  themebyjess_light_map()

ggplot2::ggsave("outputs/iucn_map3.png", iucnmap2_plot, width = 16, height = 9, units = "in", dpi = 300)

map_plot <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = includeh, aes(x = x,
                                  y = y,
                                  colour = logh1),
             size = 3,
             alpha = 0.4) +
  coord_sf(expand = FALSE) +
  labs(title = "  (b)",
       x = "Longitude",
       y = "Latitude",
       colour = expression(bold(paste("species ", italic(h), "-index")))) +
  scale_colour_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous"),
                         labels = c(0, 2, 9, 31, 99, 316)) +
  themebyjess_light_map()

ggplot2::ggsave("outputs/logh_map.png", map_plot, width = 16, height = 9, units = "in", dpi = 300)


maps <- ggarrange(iucnmap_plot + rremove("xlab") + rremove("ylab"), map_plot + rremove("xlab") + rremove("ylab"),
                  nrow = 2,
                  heights = c(1, 1),
                  align = c("v"))

maps_an <- annotate_figure(maps,
                           left = text_grob("Latitude",
                                            rot = 90,
                                            family = "Lato",
                                            size = 22,
                                            face = "bold"),
                           bottom = text_grob("Longitude",
                                              family = "Lato",
                                              size = 22,
                                              face = "bold"))

ggplot2::ggsave("outputs/maps_plot.png", maps_an, width = 16, height = 13, units = "in", dpi = 300)


#phylogenetic tree

saveRDS(tree, "outputs/tree.rds")
tree <- readRDS("outputs/tree.rds")

table(includeh_join$label %in% tree$tip.label) #checking
includeh_join <- includeh_join %>%
  filter(label %in% tree$tip.label)
table(includeh_join$label %in% tree$tip.label) #checking

tree3 <- tol_induced_subtree(ott_ids = includeh_join$id, label_format = "name")

tree_join_3 <- treeio::full_join(tree3, includeh_join)

length(tree3$tip.label) #7148
length(includeh_join$label) #7148



includeh_join <- includeh %>%
  rename(label = genus_species)
includeh_join$label <- str_replace_all(includeh_join$label, " ", "_")

#all_names <- tnrs_match_names(includeh_join$label)
in_tree <- is_in_tree(ott_ids = includeh_join$id) #this takes forever
in_tree_tbl <- tibble(in_tree)
all_names_clean <- cbind(includeh_join, in_tree)
table(all_names_clean$in_tree) #checking

for (i in 1:length(all_names_clean$in_tree)) {
  if (isFALSE(all_names_clean$in_tree[i])) {
    all_names_clean <- all_names_clean[-i,]
  }
}

tree_again <- tol_induced_subtree(ott_ids = all_names_clean$id, label_format = "name")
tree_tips <- tree_again$tip.label

table(includeh_join$label %in% tree_again$tip.label) #checking
includeh_join <- includeh_join %>%
  filter(label %in% tree_again$tip.label)
table(includeh_join$label %in% tree_again$tip.label) #checking

length(tree_again$tip.label) #7445
length(includeh_join$label) #7

tree_join <- treeio::full_join(tree_again, includeh_join)

tree_plot <- ggtree(tree_join_3,
       layout = "circular") +
  geom_tippoint(aes(colour = clade)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1))) + #shape of legend icons not changing need to find out why
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(family = "Roboto",
                                   size = 20)) +
  new_scale_colour() +
  geom_fruit(geom = geom_bar,
             mapping = aes(x = h, 
                           colour = clade),
             pwidth = 0.4,
             orientation = "y", 
             stat = "identity") +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db")) +
  guides(colour = FALSE)

ggplot2::ggsave("outputs/h_tree.png", tree_plot, width = 16, height = 9, units = "in", dpi = 300)





plot_check <- ggtree(tree_join, layout = "circular") +
  geom_point(aes(y = h)) +
  geom_fruit(geom = geom_bar,
             mapping = aes(x = h, 
                           colour = label),
             pwidth = 0.4,
             orientation = "y", 
             stat = "identity") +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"))
ggplotly(plot_check)

ggtree(tree_again, layout = "circular") +
  geom_fruit(data = all_names_clean,
             geom = geom_bar,
             mapping = aes(x = h, 
                           colour = clade),
             pwidth = 0.4,
             orientation = "y", 
             stat = "identity")

## tree from vertlife 

for (i in 1:length(tree100$tree_9684$tip.label)) { #run this 3 times
  if (!tree100$tree_9684$tip.label[i] %in% includeh_join$label) {
    tree100$tree_9684 <- drop.tip(tree100$tree_9684, tree100$tree_9684$tip.label[i])
  }
} 

length(tree100$tree_9684$tip.label) #5518

table(includeh_join$label %in% tree100$tree_9684$tip.label) #checking
includeh_join <- includeh_join %>%
  filter(label %in% tree100$tree_9684$tip.label)
table(includeh_join$label %in% tree100$tree_9684$tip.label) #checking

length(includeh_join$label) #5497

tree100_combine <- treeio::full_join(tree100$tree_9684, includeh_join)

tree_plot2 <- ggtree(tree100_combine,
                    layout = "circular") +
  geom_tippoint(aes(colour = clade)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1))) + #shape of legend icons not changing need to find out why
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(family = "Roboto",
                                   size = 20)) +
  new_scale_colour() +
  geom_fruit(geom = geom_bar,
             mapping = aes(x = h, 
                           colour = clade),
             pwidth = 0.4,
             orientation = "y", 
             stat = "identity") +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db")) +
  guides(colour = FALSE)

ggplot2::ggsave("outputs/h_tree2.png", tree_plot, width = 16, height = 9, units = "in", dpi = 300)

#google trends

gtrends_plot <- ggplot(includeh, aes(x = log_sumgtrends,
                     y = logh1,
                     colour = clade)) +
  geom_point(size = 3,
             alpha = 0.4) +
  labs(x = "Google Trends index (sum)",
       y = "h-index") +
  scale_x_continuous(breaks = c(0, 2, 3, 4),
                     labels = c(0, 100, "1,000", "10,000")) +
  scale_y_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 4,
                                                               alpha = 1))) +
  themebyjess_light_point()

ggplot2::ggsave("outputs/logh_vs_logsumgtrends.png", gtrends_plot, width = 16, height = 9, units = "in", dpi = 300)

#trying ggstream

scopus_results1 <- scopus_results1[-2886] #remove homo sapiens

scopus1_melt <- melt(scopus_results1)
scopus2_melt <- melt(scopus_results2)

scopus1_df <- scopus1_melt %>%
  select(cover_date, L1) 
scopus2_df <- scopus2_melt %>%
  select(cover_date, L1) 

scopus2_df$L1 <- scopus2_df$L1+3999

scopus_df <- rbind(scopus1_df, scopus2_df)

scopus_df_year <- scopus_df %>%
  mutate(year = as.numeric(substr(scopus_df$cover_date, 1, 4)))

unique(scopus_df_year$L1) #7521

scopus_df_summ <- scopus_df_year %>%
  group_by(L1, year) %>%
  summarise(n())

scopus_df_summ <- scopus_df_summ %>%
  rename(sp = L1,
         count = `n()`)

for (i in 1:length(includeh$genus_species)) {
  includeh$sp[i] <- i
}

scopus_df_summ$sp <- as.character(scopus_df_summ$sp)

scopus_df_summ <- left_join(scopus_df_summ, includeh)

scopus_order <- scopus_df_summ %>%
  group_by(clade, order, year) %>%
  summarise(n())

scopus_order <- scopus_order %>% 
  rename(count = `n()`)

#some sorting

med_mass <- includeh %>%
  group_by(order) %>%
  summarise_at(vars(BodyMass.Value), median, na.rm = T) %>%
  ungroup()
med_mass <- med_mass %>%
  arrange(by_group = BodyMass.Value)

for (i in 1:length(med_mass$order)) {
  if (med_mass$order[i] == "Pilosa"|
      med_mass$order[i] == "Cingulata") {
    med_mass$clade[i] <- "Xenarthra"
  } else if (med_mass$order[i] == "Macroscelidea"|
             med_mass$order[i] == "Afrosoricida"|
             med_mass$order[i] == "Proboscidea"|
             med_mass$order[i] == "Sirenia"|
             med_mass$order[i] == "Hyracoidea"|
             med_mass$order[i] == "Tubulidentata") {
    med_mass$clade[i] <- "Afrotheria"
  } else if (med_mass$order[i] == "Chiroptera"|
             med_mass$order[i] == "Perissodactyla"|
             med_mass$order[i] == "Artiodactyla"|
             med_mass$order[i] == "Cetacea"|
             med_mass$order[i] == "Pholidota"|
             med_mass$order[i] == "Carnivora"|
             med_mass$order[i] == "Eulipotyphla"|
             med_mass$order[i] == "Soricomorpha"|
             med_mass$order[i] == "Erinaceomorpha") {
    med_mass$clade[i] <- "Laurasiatheria"
  } else if (med_mass$order[i] == "Primates"|
             med_mass$order[i] == "Scandentia"|
             med_mass$order[i] == "Lagomorpha"|
             med_mass$order[i] == "Rodentia"|
             med_mass$order[i] == "Dermoptera") {
    med_mass$clade[i] <- "Euarchontoglires"
  } else if (med_mass$order[i] == "Diprotodontia"|
             med_mass$order[i] == "Dasyuromorphia"|
             med_mass$order[i] == "Microbiotheria"|
             med_mass$order[i] == "Peramelemorphia"|
             med_mass$order[i] == "Notoryctemorphia"|
             med_mass$order[i] == "Monotremata"|
             med_mass$order[i] == "Paucituberculata"|
             med_mass$order[i] == "Didelphimorphia") {
    med_mass$clade[i] <- "Marsupials & monotremes"
  }
}

med_mass$clade <- factor(med_mass$clade, levels = c("Afrotheria", "Xenarthra", "Euarchontoglires", "Laurasiatheria", "Marsupials & monotremes"))

med_mass <- med_mass %>%
  arrange(clade, BodyMass.Value)

scopus_order$order <- factor(scopus_order$order, levels = med_mass$order)

orders <- scopus_order %>%
  filter(year > 2009)

scopus_order_1950 <- scopus_order %>%
  filter(year > 1949)

scopus_order_1940 <- scopus_order %>%
  filter(year > 1939)

#check percentages

scopus_order_1960 <- scopus_order %>% 
  filter(year == 1960)
scopus_order_1970 <- scopus_order %>% 
  filter(year == 1970)
scopus_order_1980 <- scopus_order %>% 
  filter(year == 1980)
scopus_order_1990 <- scopus_order %>% 
  filter(year == 1990)
scopus_order_2000 <- scopus_order %>% 
  filter(year == 2000)
scopus_order_2010 <- scopus_order %>% 
  filter(year == 2010)

sum_1940 <- aggregate(scopus_order_1940$count, by = list(Category = scopus_order_1940$order), FUN = sum)
sum_1960 <- aggregate(scopus_order_1960$count, by = list(Category = scopus_order_1960$order), FUN = sum)
sum_1960 <- sum_1960 %>% 
  rename("1960" = "x")
sum_1970 <- aggregate(scopus_order_1970$count, by = list(Category = scopus_order_1970$order), FUN = sum)
sum_1970 <- sum_1970 %>% 
  rename("1970" = "x")
sum_1980 <- aggregate(scopus_order_1980$count, by = list(Category = scopus_order_1980$order), FUN = sum)
sum_1980 <- sum_1980 %>% 
  rename("1980" = "x")
sum_1990 <- aggregate(scopus_order_1990$count, by = list(Category = scopus_order_1990$order), FUN = sum)
sum_1990 <- sum_1990 %>% 
  rename("1990" = "x")
sum_2000 <- aggregate(scopus_order_2000$count, by = list(Category = scopus_order_2000$order), FUN = sum)
sum_2000 <- sum_2000 %>% 
  rename("2000" = "x")
sum_2010 <- aggregate(scopus_order_2010$count, by = list(Category = scopus_order_2010$order), FUN = sum)
sum_2010 <- sum_2010 %>% 
  rename("2010" = "x")

sum_all <- join_all(list(sum_1960, sum_1970, sum_1980, sum_1990, sum_2000, sum_2010))
sum_all %>% 
  mutate("1960%" = 1960/sum(1960))

#plots

pub_ridge_plot <- ggplot(data = scopus_order_1940,
                         aes(x = year,
                             y = count,
                             fill = order)) +
  geom_stream(type = "ridge",
              n_grid = 10000) +
  geom_stream_label(type = "ridge",
                    aes(label = order)) +
  labs(title = "  (a)",
       x = "Year",
       y = "Number of publications") +
  scale_fill_manual(values = c("#FAEBB3",
                               "#F7E28D", "#F6DB6E", "#F4D34E", "#F3CC2F",
                               "#f1c40f", "#EFB519", "#EDA722", "#EB982C",
                               "#E98935", "#E97A37", "#E86B39", "#E85B3A",
                               "#e74c3c", "#D14A58", "#BB4875", "#A44691",
                               "#8e44ad", "#7859B9", "#616EC4", "#4B83D0",
                               "#3498db", "#33A5C1", "#31B2A6", "#30BF8C",
                               "#2ECC71", "#4ED487", "#6EDC9D", "#8DE3B2",
                               "#8CE3B2"),
                    guide = guide_legend(override.aes = list(size = 6,
                                                             alpha = 1),
                                         ncol = 1)) #+
themebyjess_light_stream()

ggplot2::ggsave("outputs/pub_ridge.png", pub_ridge_plot, width = 16, height = 9, units = "in", dpi = 300)

pub_mirror_plot <- scopus_order %>%
  filter(year > 1949) %>%
  ggplot(aes(x = year,
             y = count,
             fill = order)) +
  geom_stream(type = "mirror") +
  labs(x = "Year",
       y = "Number of publications") +
  scale_fill_manual(values = c("#FAEBB3",
                               "#F7E28D", "#F6DB6E", "#F4D34E", "#F3CC2F",
                               "#f1c40f", "#EFB519", "#EDA722", "#EB982C",
                               "#E98935", "#E97A37", "#E86B39", "#E85B3A",
                               "#e74c3c", "#D14A58", "#BB4875", "#A44691",
                               "#8e44ad", "#7859B9", "#616EC4", "#4B83D0",
                               "#3498db", "#33A5C1", "#31B2A6", "#30BF8C",
                               "#2ECC71", "#4ED487", "#6EDC9D", "#8DE3B2",
                               "#8CE3B2"),
                    guide = guide_legend(override.aes = list(size = 4,
                                                             alpha = 1),
                                         ncol = 1)) +
  themebyjess_light_stream()

ggplot2::ggsave("outputs/pub_mirror.png", pub_mirror_plot, width = 16, height = 9, units = "in", dpi = 300)

pub_proportion_plot <- scopus_order %>%
  filter(year > 1939) %>%
  ggplot(aes(x = year,
             y = count,
             fill = order)) +
  geom_stream(type = "proportion") +
  geom_stream_label(aes(label = order)) +
  labs(title = "  (b)",
       x = "Year",
       y = "Proportion of publications") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_manual(values = c("#FAEBB3",
                               "#F7E28D", "#F6DB6E", "#F4D34E", "#F3CC2F",
                               "#f1c40f", "#EFB519", "#EDA722", "#EB982C",
                               "#E98935", "#E97A37", "#E86B39", "#E85B3A",
                               "#e74c3c", "#D14A58", "#BB4875", "#A44691",
                               "#8e44ad", "#7859B9", "#616EC4", "#4B83D0",
                               "#3498db", "#33A5C1", "#31B2A6", "#30BF8C",
                               "#2ECC71", "#4AD384", "#65DA97", "#81E0AA",
                               "#8CE3B2"),
                    guide = guide_legend(override.aes = list(size = 4,
                                                             alpha = 1),
                                         ncol = 1)) +
  themebyjess_light_stream()

ggplotly(pub_proportion_plot)

ggplot2::ggsave("outputs/pub_proportion.png", pub_proportion_plot, width = 16, height = 9, units = "in", dpi = 300)

pub_plot_combine <- ggarrange(pub_ridge_plot, pub_proportion_plot,
                              common.legend = TRUE,
                              legend = "right",
                              ncol = 1)

ggplot2::ggsave("outputs/pub_plot_combine.png", pub_plot_combine, width = 16, height = 9, units = "in", dpi = 300)
ggplot2::ggsave("outputs/pub_plot_combine_text.png", pub_plot_combine, width = 16, height = 13, units = "in", dpi = 300)

#facet plot
newdata <- includeh %>%
  select(genus_species, clade, h, logh1, 
         logmass, median_lat, humanuse_bin, iucn_bin, domestication_bin, log_sumgtrends) 
newdata <- newdata %>% pivot_longer(cols = c(5:10),
                         names_to = "var",
                         values_to = "val")

newdata$var <- factor(newdata$var, levels = c("logmass", "median_lat", "log_sumgtrends", "iucn_bin", "humanuse_bin", "domestication_bin"))

replace_val <- function(data, res, x, output) {
  for (i in 1:nrow(data)) {
    if (data$var[i]==res & newdata$val[i]==x) {
      data$val[i] <- output
    }
  }
}

replace_val(newdata, "domestication_bin", 3, "Wild")

labs <- c("Body mass", "Latitude", "Google Trends", "IUCN Red List status", "Human use", "Domestication")
levels(newdata$var) <- labs

ggplot(newdata %>%
         drop_na(val), aes(y = logh1)) +
  geom_point(data = subset(newdata, var == "Body mass"), #mass
             aes(x = val,
                 colour = clade),
             size = 3,
             alpha = 0.4) +
  geom_point(data = subset(newdata, var == "Latitude"), #latitude
             aes(x = val,
                 colour = clade),
             size = 3,
             alpha = 0.4) +
  geom_smooth(data = subset(newdata, var == "Latitude"),
              aes(x = val,
                  colour = clade),
              colour = "black",
              size = 1.2) +
  geom_point(data = subset(newdata, var == "Google Trends"), #google trends
             aes(x = val,
                 colour = clade),
             size = 3,
             alpha = 0.4) +
  geom_quasirandom(data = subset(newdata, var == "IUCN Red List status"), #iucn
                   aes(x = val,
                       colour = clade),
                   size = 3,
                   alpha = 0.4) +
  geom_quasirandom(data = subset(newdata, var == "Human use"), #human use
                   aes(x = val,
                       colour = clade),
                   size = 3,
                   alpha = 0.4) +
  geom_quasirandom(data = subset(newdata, var == "Domestication"), #domestication
                   aes(x = val,
                       colour = clade),
                   size = 3,
                   alpha = 0.4) +
  labs(y = "h-index") +
  scale_y_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  facet_wrap(~var,
             scales = "free") +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 6,
                                                               alpha = 1))) +
  themebyjess_light_facet()

ggplot2::ggsave("outputs/facet_plot.png", facet_plot, width = 16, height = 9, units = "in", dpi = 300)

#grid arrange

mass_combine <- ggplot(includeh, aes(x = logmass,
                                  y = logh1,
                                  colour = clade)) +
  geom_point(size = 3,
             alpha = 0.2) +
  labs(x = "(a) Body mass (kg)") +
  ylim(c(0, 500)) +
  coord_trans(x = "log1p") +
  scale_x_continuous(breaks = c(0.3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0),
                     labels = c(0.002, 0.01, 0.1, 1, 10, 100, "1,000", "100,000")) +
  scale_y_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 5,
                                                               alpha = 1))) +
  new_scale_colour() +
  geom_quantile(aes(colour = clade),
                quantiles = 0.5,
                size = 2.5,
                alpha = 0.8,
                 lineend = "round") +
  scale_colour_manual(values = c("#d4ac0d", "#ca6f1e", "#cb4335", "#7d3c98", "#2e86c1")) +
  guides(colour = FALSE) +
  themebyjess_light_point()

lat_combine <- ggplot(includeh, aes(x = median_lat,
                                 y = logh1,
                                 colour = clade)) +
  geom_point(size = 3,
             alpha = 0.2) +
  geom_smooth(colour = "black",
              size = 1.2) +
  geom_rug(sides = "t",
           col = rgb(0.5, 0, 0,
                     alpha = 0.05)) +
  labs(x = "(b) Latitude") +
  ylim(c(0, 500)) +
  scale_x_continuous(breaks = c(-40, 0, 40, 80),
                     labels = c("-40°", 0, "40°", "80°")) +
  scale_y_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 5,
                                                               alpha = 1))) +
  themebyjess_light_point()

lat_combine_margin <- ggMarginal(lat_combine, 
           margins = "x",
           groupColour = TRUE,
           groupFill = TRUE)

ggMarginal(lat_combine, 
           margins = "x",
           type = "histogram",
           xparams = list(binwidth = 1))

gtrends_combine <- ggplot(includeh, aes(x = log_sumgtrends,
                                     y = logh1,
                                     colour = clade)) +
  geom_point(size = 3,
             alpha = 0.2) +
  labs(x = "(c) Google Trends index") +
  ylim(c(0, 500)) +
  scale_x_continuous(breaks = c(0, 2, 3, 4),
                     labels = c(0, 100, "1,000", "10,000")) +
  scale_y_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 5,
                                                               alpha = 1))) +
  themebyjess_light_point()

iucn_combine <- ggplot(includeh %>% 
                         drop_na(iucn_bin), aes(x = factor(iucn_bin),
                                     y = logh1)) +
  geom_quasirandom(aes(colour = clade),
                   size = 3,
                   alpha = 0.2) +
  geom_boxplot(fill = "grey80",
               size = 0.8,
               width = 0.4,
               alpha = 0.2,
               outlier.shape = NA) +
  labs(x = "(d) IUCN Red List status") +
  ylim(c(0, 500)) +
  scale_x_discrete(breaks = c(1, 2, 3, 4, 5),
                     labels = c("LC", "VU", "EN", "CE", "EW")) +
  scale_y_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                       labels = c(0, 2, 9, 31, 99, 316)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 5,
                                                               alpha = 1))) +
  themebyjess_light_quasirandom()

humanuse_combine <- ggplot(includeh, aes(x = factor(humanuse_bin),
                                     y = logh1)) +
  geom_quasirandom(aes(colour = clade),
                   size = 3,
                   alpha = 0.2) +
  geom_boxplot(fill = "grey80",
               size = 0.8,
               width = 0.4,
               alpha = 0.2,
               outlier.shape = NA) +
  labs(x = "(e) Human use") +
  ylim(c(0, 500)) +
  scale_x_discrete(breaks = c(0, 1),
                     labels = c("No documented use", "Use documented")) +
  scale_y_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 5,
                                                               alpha = 1))) +
  themebyjess_light_quasirandom()

domestication_combine <- ggplot(includeh, aes(x = factor(domestication_bin),
                                         y = logh1)) +
  geom_quasirandom(aes(colour = clade),
                   size = 3,
                   alpha = 0.2) +
  geom_boxplot(fill = "grey80",
               size = 0.8,
               width = 0.4,
               alpha = 0.2,
               outlier.shape = NA) +
  labs(x = "(f) Domestication") +
  ylim(c(0, 500)) +
  scale_x_discrete(breaks = c(1, 2, 3),
                     labels = c("Domesticated", "Partially-domesticated", "Wild")) +
  scale_y_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_colour_manual(values = c("#f1c40f", "#e67e22", "#e74c3c", "#8e44ad", "#3498db"),
                      guide = guide_legend(override.aes = list(size = 5,
                                                               alpha = 1))) +
  themebyjess_light_quasirandom()


grid_plot <- ggarrange(mass_combine + rremove("ylab"), lat_combine + rremove("ylab"), gtrends_combine + rremove("ylab"), 
          iucn_combine + rremove("ylab"), humanuse_combine + rremove("ylab"), domestication_combine + rremove("ylab"),
          common.legend = TRUE,
          nrow = 2, ncol = 3)

grid_plot_an <- annotate_figure(grid_plot, left = text_grob(expression(bold(paste("species ", italic(h), "-index"))),
                                            rot = 90,
                                            family = "Lato",
                                            size = 22))

ggplot2::ggsave("outputs/grid_plot.png", grid_plot_an, width = 20, height = 11, units = "in", dpi = 300)


