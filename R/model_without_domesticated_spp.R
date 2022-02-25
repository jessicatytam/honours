library(tidyverse)
library(ggExtra)
library(taxize)
library(rotl)
library(ape)
library(rredlist)
library(rvest)

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

#load data
includeh <- read.csv(file = "outputs/data/includeh.csv")[-c(1)]

includeh[includeh$domestication_bin==1,] #domesticated spp
sum(includeh$domestication_bin==1) #12
View(includeh[includeh$domestication_bin==2,]) #partially-domesticated spp)
sum(includeh$domestication_bin==2) #136
View(includeh[includeh$humanuse_bin==1,])

includeh <- includeh %>% 
  filter(domestication_bin==3)

#domestication 
##get the data

wiki_url <- read_html("https://en.wikipedia.org/wiki/List_of_domesticated_animals")

domesticated <- wiki_url %>% 
  html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>%
  html_table()

partially_domesticated <- wiki_url %>% 
  html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>%
  html_table()

##remove non-mammals

domesticated <- domesticated %>%
  filter(str_extract(domesticated$`Taxon group`, "^1") == 1)

partially_domesticated <- partially_domesticated %>%
  filter(str_extract(partially_domesticated$`Taxon group`, "^1") == 1)

##get binomial names 

domesticated <- domesticated %>% #extract names using the brackets
  mutate(species = str_extract(domesticated$`Species and subspecies`, "(?<=\\().+(?=\\))")) 

partially_domesticated <- partially_domesticated %>% #extract names using the brackets
  mutate(species = str_extract_all(partially_domesticated$`Species and subspecies`, "\\(([^\\)]+)")) 

##cleaning binomial names

partially_domesticated$species <- str_replace_all(partially_domesticated$species, "\\(", "") #removing mismatched brackets
partially_domesticated$species <- str_replace_all(partially_domesticated$species, "\\)", "")

for (i in 1:length(partially_domesticated$species)) { #removing "c"
  if (grepl('"', partially_domesticated$species[i])) {
    partially_domesticated$species[i] <- str_replace(partially_domesticated$species[i], "c", "")
  }
}

partially_domesticated$species <- str_replace_all(partially_domesticated$species, '"', "") #removing quotation marks

partially_domesticated <- partially_domesticated %>% #separate the names using ","
  separate(species, c("species1", "species2", "species3", "species4", "species5",
                      "species6", "species7", "species8", "species9", "species10"), sep = "[,]+")
domesticated <- domesticated %>% #separate the names using ","
  separate(species, c("species1", "species2", "species3"), sep = "[,]+")
domesticated <- domesticated %>% #separate the names using ","
  separate(species3, c("species3", "species4"), sep = " and ")

##fill in genus names

domesticated[,"species2"]==" A. algirus" #find row with "C. l. lupus"
domesticated[26,"species2"] <- "Atelerix algirus" #change to full name
domesticated[,"species4"]=="H. collaris" #find row with "C. l. lupus"
domesticated[26,"species4"] <- "Hemiechinus algirus" #change to full name
domesticated[26,"species2"] #check
domesticated[26,"species4"] #check

partially_domesticated[,"species1"] #find row with "C. l. lupus"
partially_domesticated[92,"species1"] <- "Canis lupus lupus" #change to full name
partially_domesticated[92,"species1"] #check

for (j in 11:ncol(partially_domesticated)) {
  for (i in 1:nrow(partially_domesticated)) {
    if (!is.na(partially_domesticated[i,j]) & (lengths(str_split(partially_domesticated[i,j], " ")) == 3)) {
      partially_domesticated[i,j] <- str_replace(partially_domesticated[i,j], ".+(?=[:space:])", word(partially_domesticated$species1[i], 1))
    } else if (!is.na(partially_domesticated[i,j]) & (lengths(str_split(partially_domesticated[i,j], " ")) == 4)) {
      partially_domesticated[i,j] <- str_replace(partially_domesticated[i,j], ".+(?=[:space:])", word(partially_domesticated$species1[i], 1, 2))
    } 
  }
}

# partially_domesticated[46, 13] <- "Osphranter rufus"

##check for and fix spelling mistakes

for (i in 1:length(domesticated$species)) {
  table(tnrs_match_names(names = domesticated$species[i])$approximate_match)
}

for (i in 1:length(partially_domesticated$species1)) {
  table(tnrs_match_names(names = partially_domesticated$species1[i])$approximate_match)
} 

for (i in 1:length(partially_domesticated$species10)) {
  if (!is.na(partially_domesticated$species10[i])) {
    table(tnrs_match_names(names = partially_domesticated$species10[i])$approximate_match)
  }
}

##pivot longer

partially_domesticated <- partially_domesticated %>%
  rename("spp and subspp" = "Species and subspecies")

partially_domesticated <- partially_domesticated %>%
  pivot_longer(cols = starts_with("species"),
               values_to = "species",
               values_drop_na = TRUE)

partially_domesticated <- partially_domesticated %>%
  select(!name)

domesticated <- domesticated %>%
  rename("spp and subspp" = "Species and subspecies")

domesticated <- domesticated %>%
  pivot_longer(cols = starts_with("species"),
               values_to = "species",
               values_drop_na = TRUE)

domesticated <- domesticated %>%
  select(!name)

##combine

partially_domesticated <- partially_domesticated %>%
  rename(Purposes = Purpose)

domestication <- rbind(domesticated, partially_domesticated)

##remove unwanted data

domestication <- domestication[c(10, 5, 7:8)] 
domestication <- domestication %>%
  arrange(species)

##get id for domestication

domestication <- domestication %>%
  mutate(id = tnrs_match_names(names = domestication$species)$ott_id, .after = species)

#check if spp are in the domestication list

includeh_dom <- data.frame()
for (i in 1:length(includeh$genus_species)) {
  if (length(grep(includeh$genus_species[i], domestication$species))!=0) {
    includeh_dom <- rbind(includeh_dom, includeh$genus_species[i])
    print(paste(i, includeh$genus_species[i]))
  }
}

includeh_wo_dom <- includeh[!includeh$genus_species %in% includeh_dom$X.Acinonyx.jubatus.,]
includeh_wo_dom$genus_species <- str_replace(includeh_wo_dom$genus_species, " ", "_")

# new imputation list

imp_list <- readRDS("data/intermediate_data/MCMCglmm/imp_list.rds")

table(imp_list[[1]]$animal %in% includeh_wo_dom$genus_species) #5343 
table(!imp_list[[1]]$animal %in% includeh_wo_dom$genus_species) #155

imp_list_new <- imp_list
for (j in 1:length(imp_list)) {
  for (i in 1:length(imp_list[[j]]$animal)) {
    if (!imp_list[[j]]$animal[i] %in% includeh_wo_dom$genus_species) {
      imp_list_new[[j]] <- imp_list_new[[j]][-i,]
    }
  }
}

saveRDS(imp_list_new, "data/intermediate_data/MCMCglmm/imp_list_new.rds")

#new tree list

random_trees <- readRDS("data/intermediate_data/random_trees.rds")
length(random_trees$tree_9522$tip.label) #5498

random_trees_new <- random_trees
for (j in 1:length(random_trees_new)) { 
  for (i in 1:length(random_trees_new[[j]]$tip.label)) {
    if (random_trees_new[[j]]$tip.label[i] %in% imp_list_new[[1]]$animal) {
      random_trees_new[[j]] <- keep.tip(random_trees_new[[j]], imp_list_new[[1]]$animal)
      print(paste("tree", j, "trimmed"))
    }
  } 
}

saveRDS(random_trees_new, "data/intermediate_data/random_trees_new.rds")


#plots

#sorting

includeh_wo_dom$genus_species <- str_replace(includeh_wo_dom$genus_species, "_", " ")
includeh_wo_dom$clade <- factor(includeh_wo_dom$clade,
                                levels = c("Afrotheria", "Xenarthra", "Euarchontoglires",
                                           "Laurasiatheria", "Marsupials & monotremes"))
# includeh$order <- factor(includeh$order, levels = med_mass$order)
# unique(includeh$redlistCategory)
includeh_wo_dom$redlistCategory <- factor(includeh_wo_dom$redlistCategory,
                                          levels = c("Least Concern", "Near Threaten", "Vulnerable",
                                                     "Endangered", "Critically Endangered",
                                                     "Extinct in the Wild", "Extinct", "Data Deficient"))

#themes

source("R/themes.R")

h100_wo_dom <- ggplot(includeh_wo_dom %>% filter(h>99),
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
  xlim(c(0, 500)) +
  labs(x = expression(bold(paste("species ", italic(h), "-index")))) +
  scale_fill_manual(values = c("#E98935", "#3498db"),
                    guide = guide_legend(override.aes = list(size = 4,
                                                             alpha = 1),
                                         nrow = 1)) +
  themebyjess_light_col()

ggplot2::ggsave("outputs/h100_wo_dom.png", h100_wo_dom, width = 16, height = 9, units = "in", dpi = 300)
ggplot2::ggsave("outputs/h100_wo_dom_text.png", h100_wo_dom, width = 16, height = 13, units = "in", dpi = 300)

allh_wo_dom <- ggplot(includeh_wo_dom,
               aes(x = h,
                   y = reorder(genus_species, h))) +
  geom_point(size = 1,
             alpha = 0.2) +
  labs(x = "h-index") + 
  xlim(c(0, 500)) +
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

ggplot2::ggsave("outputs/allh_wo_dom.png", allh_wo_dom, width = 9, height = 16, units = "in", dpi = 300)

#grid arrange

mass_combine <- ggplot(includeh, aes(x = logmass,
                                     y = logh1,
                                     colour = domestication)) +
  geom_point(size = 3,
             alpha = 0.2) +
  labs(x = "(a) Body mass (kg)") +
  ylim(c(0, 500)) +
  coord_trans(x = "log1p") +
  scale_x_continuous(breaks = c(0.3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0),
                     labels = c(0.002, 0.01, 0.1, 1, 10, 100, "1,000", "100,000")) +
  scale_y_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_colour_manual(values = c("#649fea", "#a9dfbf", "#f0b27a"),
                      guide = guide_legend(override.aes = list(size = 5,
                                                               alpha = 1))) +
  new_scale_colour() +
  geom_quantile(aes(colour = clade),
                quantiles = 0.5,
                size = 2.5,
                alpha = 0.8,
                lineend = "round") +
  scale_colour_manual(values = c("#649fea", "#a9dfbf", "#f0b27a")) +
  guides(colour = FALSE) +
  themebyjess_light_point()

lat_combine <- ggplot(includeh, aes(x = median_lat,
                                    y = logh1,
                                    colour = domestication)) +
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
  scale_colour_manual(values = c("#649fea", "#a9dfbf", "#f0b27a"),
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

humanuse_combine <- ggplot(includeh, aes(x = factor(humanuse_bin),
                                         y = logh1)) +
  geom_quasirandom(aes(colour = domestication),
                   size = 3,
                   alpha = 0.2) +
  geom_boxplot(fill = "grey80",
               size = 0.8,
               width = 0.4,
               alpha = 0.2,
               outlier.shape = NA) +
  labs(x = "(c) Human use") +
  ylim(c(0, 500)) +
  scale_x_discrete(breaks = c(0, 1),
                   labels = c("No documented use", "Use documented")) +
  scale_y_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_colour_manual(values = c("#649fea", "#a9dfbf", "#f0b27a"),
                      guide = guide_legend(override.aes = list(size = 5,
                                                               alpha = 1))) +
  themebyjess_light_quasirandom()

domestication_combine <- ggplot(includeh, aes(x = factor(domestication_bin),
                                              y = logh1)) +
  geom_quasirandom(aes(colour = domestication),
                   size = 3,
                   alpha = 0.2) +
  geom_boxplot(fill = "grey80",
               size = 0.8,
               width = 0.4,
               alpha = 0.2,
               outlier.shape = NA) +
  labs(x = "(d) Domestication") +
  ylim(c(0, 500)) +
  scale_x_discrete(breaks = c(1, 2, 3),
                   labels = c("Domesticated", "Partially-domesticated", "Wild")) +
  scale_y_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_colour_manual(values = c("#649fea", "#a9dfbf", "#f0b27a"),
                      guide = guide_legend(override.aes = list(size = 5,
                                                               alpha = 1))) +
  themebyjess_light_quasirandom()

iucn_combine <- ggplot(includeh %>% 
                         drop_na(iucn_bin), aes(x = factor(iucn_bin),
                                                y = logh1)) +
  geom_quasirandom(aes(colour = domestication),
                   size = 3,
                   alpha = 0.2) +
  geom_boxplot(fill = "grey80",
               size = 0.8,
               width = 0.4,
               alpha = 0.2,
               outlier.shape = NA) +
  labs(x = "(e) IUCN Red List status") +
  ylim(c(0, 500)) +
  scale_x_discrete(breaks = c(1, 2, 3, 4, 5),
                   labels = c("LC", "VU", "EN", "CE", "EW")) +
  scale_y_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_colour_manual(values = c("#649fea", "#a9dfbf", "#f0b27a"),
                      guide = guide_legend(override.aes = list(size = 5,
                                                               alpha = 1))) +
  themebyjess_light_quasirandom()

gtrends_combine <- ggplot(includeh, aes(x = log_sumgtrends,
                                        y = logh1,
                                        colour = domestication)) +
  geom_point(size = 3,
             alpha = 0.2) +
  labs(x = "(f) Google Trends index") +
  ylim(c(0, 500)) +
  scale_x_continuous(breaks = c(0, 2, 3, 4),
                     labels = c(0, 100, "1,000", "10,000")) +
  scale_y_continuous(breaks = c(0, 0.477, 1, 1.505, 2, 2.501),
                     labels = c(0, 2, 9, 31, 99, 316)) +
  scale_colour_manual(values = c("#649fea", "#a9dfbf", "#f0b27a"),
                      guide = guide_legend(override.aes = list(size = 5,
                                                               alpha = 1))) +
  themebyjess_light_point()

grid_plot <- ggarrange(mass_combine + rremove("ylab"), lat_combine + rremove("ylab"), humanuse_combine + rremove("ylab"),
                       domestication_combine + rremove("ylab"), iucn_combine + rremove("ylab"),
                       gtrends_combine + rremove("ylab"),
                       common.legend = TRUE,
                       nrow = 2, ncol = 3)

grid_plot_an <- annotate_figure(grid_plot, left = text_grob(expression(bold(paste("species ", italic(h), "-index"))),
                                                            rot = 90,
                                                            family = "Lato",
                                                            size = 22))

ggplot2::ggsave("outputs/grid_plot_domestication.png", grid_plot_an, width = 20, height = 11, units = "in", dpi = 300)



