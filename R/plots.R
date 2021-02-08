library(tidyverse)
library(ggExtra)
library(taxize)
library(rotl)
library(ape)

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


hindex <- read.csv(file = "outputs/hindex.csv", header = T)
combinedf <- read.csv(file = "outputs/combinedf.csv", header = T)

combinedf <- combinedf %>%
  rename(genus_species = species)

hindex$genus_species <- str_replace_all(hindex$genus_species, "_", " ")

includeh <- list(hindex, combinedf) %>% 
  reduce(left_join, by = "genus_species")

#fill in orders and families

#using ott_ids; this is much faster
for (i in 4631:length(includeh$id)) {
  if (is.na(includeh$order[i])) {
    print(i)
    tax_df <- melt(tax_lineage(taxonomy_taxon_info(ott_ids = includeh$id[i], include_lineage = TRUE)))
    tax_filter <- tax_df %>%
      filter(rank == "order")
    order <- tax_filter$unique_name
    includeh$order[i] <- as.character(order)
  }
}

for (i in 5156:length(includeh$id)) {
  if (is.na(includeh$family[i])) {
    print(i)
    tax_df <- melt(tax_lineage(taxonomy_taxon_info(ott_ids = includeh$id[i], include_lineage = TRUE)))
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
includeh <- includeh%>%
  rename(redlistCategory = redlistCategory1)


#LOG TRANSFORM

includeh <- includeh %>%
  mutate(logh = log10(h),
         logmass = log10(BodyMass.Value))

includeh <- includeh %>%
  mutate(logh1 = log10(h+1), .after = logh)

#quick plots

#sorting

med_mass <- includeh %>%
  group_by(order) %>%
  summarise_at(vars(BodyMass.Value), median, na.rm = T) %>%
  ungroup()
med_mass <- med_mass %>%
  arrange(by_group = BodyMass.Value)
includeh$order <- factor(includeh$order, levels = med_mass$order)

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
mass <- ggplot(includeh, aes(x = logmass,
                     y = logh1)) +
  geom_point(aes(colour = order),
             size = 2,
             alpha = 0.5) +
  geom_smooth() +
  labs(x = "Body mass",
       y = "h-index",
       colour = "Order") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  guides(colour = guide_legend(ncol = 1))
ggMarginal(mass,
           margins = "x",
           type = "histogram",
           bins = 150)

ggplot(includeh, aes(x = logmass,
                     y = log10(m+1))) +
  geom_point(aes(colour = order),
             alpha = 0.5) +
  geom_smooth() +
  labs(x = "Body mass",
       y = "m-index",
       colour = "Order") +
  theme(legend.position = "bottom")

#iucn category
ggplot(includeh, aes(x = logh1,
                     y = redlistCategory)) +
  geom_boxplot() +
  geom_jitter(aes(colour = order),
              size = 2,
              alpha = 0.5) +
  labs(x = "h-index",
       y = "IUCN Red List Category",
       colour = "Order") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = c(0.92, 0.6)) +
  guides(colour = guide_legend(ncol = 1)) +
  scale_y_discrete(limits = rev,
                   labels = label_wrap(16))

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

med_use <- includeh_pivot %>%
  group_by(human_use) %>%
  summarise_at(vars(logh1), median, na.rm = T) %>%
  ungroup()
med_use <- med_use %>%
  arrange(logh1)
med_use <- med_use[c(3:4, 6:19, 5, 2, 1),]

includeh_pivot$human_use <- factor(includeh_pivot$human_use, levels = med_use$human_use)

ggplot(includeh_pivot, aes(x = logh1,
                           y = human_use)) +
  geom_boxplot() +
  geom_jitter(aes(colour = order),
              size = 2,
              alpha = 0.5) +
  labs(x = "h-index",
       y = "Human use",
       colour = "Order") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = c(0.9, 0.62)) +
  guides(colour = guide_legend(ncol = 1)) +
  scale_y_discrete(limits = rev,
                   labels = label_wrap(18))
  

#latitude
med_lat <- ggplot(includeh, aes(x = median_lat,
                                y = logh1,
                                colour = order)) +
  geom_point(size = 2,
             alpha = 0.5) +
  labs(x = "Latitude (median)",
       y = "h-index",
       colour = "Order") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  guides(colour = guide_legend(ncol = 1))
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
             size = 3,
             alpha = 0.7) +
  coord_sf(expand = FALSE) +
  labs(x = "Longitude",
       y = "Latitude",
       colour = "h-index") +
  theme(panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  scale_colour_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous")) 

#phylogenetic tree
tree <- tol_induced_subtree(ott_ids = includeh$id, label_format = "name")

includeh_join <- includeh %>%
  rename(label = genus_species)
includeh_join$label <- str_replace_all(includeh_join$label, " ", "_")

tree <- as_tibble(tree)
tree_join <- full_join(tree, includeh_join, by = "label")
tree_join <- as.treedata(tree_join)

ggtree(tree_join, aes(colour = order,
                      fill = order),
       layout = "circular") +
  geom_fruit(geom = geom_bar,
             mapping = aes(x = h),
             pwidth = 0.5,
             orientation = "y", 
             stat = "identity") +
  labs(fill = "Order") +
  guides(colour = FALSE)

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
includeh <- read.csv(file = "outputs/includeh.csv", header = T)[-c(1)]

#testing

test <- includeh %>%
  arrange(logh1)
test <- test[c(6738:6719),]

ggplot(data = world) +
  geom_sf() +
  geom_point(data = test, aes(x = x,
                              y = y,
                              colour = logh1),
             size = 4) +
  coord_sf(expand = FALSE) +
  labs(x = "Longitude",
       y = "Latitude",
       colour = "h-index") +
  scale_colour_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous")) 
