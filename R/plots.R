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

includeh <- includeh[!(includeh$genus == "Homo"),] #humans
includeh <- includeh[!(includeh$genus == "Mammuthus"),] #woolly mammoth

#removing "not applicable" in iucn status

for (i in 1:nrow(includeh)) {
  if (includeh$redlistCategory1[i] == "Not Applicable") {
    includeh$redlistCategory1[i] <- includeh$redlistCategory2[i]
    includeh$redlistCategory2[i] <- NA
  }
}

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

unique(includeh$redlistCategory1)
includeh$redlistCategory1 <- factor(includeh$redlistCategory1, levels = c("Least Concern", "Near Threaten", "Vulnerable",
                                                                          "Endangered", "Critically Endangered",
                                                                          "Regionally Extinct", "Extinct in the Wild", "Extinct",
                                                                          "Data Deficient", "Not Applicable", NA))

#h by order
ggplot(includeh, aes(x = logh1,
                     y = order)) +
  geom_boxplot() +
  geom_jitter(aes(colour = redlistCategory1),
              alpha = 0.5)

#h-index
ggplot(includeh, aes(x = reorder(genus_species, logh1),
                     y = logh1)) +
  geom_point(aes(colour = order),
             alpha = 0.5) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90)) 

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
             alpha = 0.5) +
  geom_smooth() +
  theme(legend.position = "bottom")
ggMarginal(mass,
           type = "histogram",
           bins = 100)

#iucn category
ggplot(includeh, aes(x = redlistCategory1,
                     y = logh1)) +
  geom_boxplot() +
  geom_jitter(aes(colour = order),
              alpha = 0.5) 

#human use
includeh_pivot <- includeh %>%
  pivot_longer(cols = starts_with("use"),
               names_to = "use_count",
               values_to = "human_use")

ggplot(includeh_pivot, aes(x = logh1,
                           y = human_use)) +
  geom_boxplot() +
  geom_jitter(aes(colour = order),
              alpha = 0.5) 

#latitude
med_lat <- ggplot(includeh, aes(x = median_lat,
                                y = logh1,
                                colour = order)) +
  geom_point(alpha = 0.5) +
  geom_smooth()
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
             alpha = 0.6) +
  coord_sf(expand = FALSE) +
  labs(x = "Longitude",
       y = "Latitude",
       colour = "h-index") +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  scale_colour_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous")) 

#phylogenetic tree; need help
tree <- tol_induced_subtree(ott_ids = includeh$id, label_format = "name")
plot(tree, type = "fan", cex = 0.4, label.offset = 0.1, no.margin = T)

includeh_join <- includeh %>%
  rename(label = genus_species)
includeh_join$label <- str_replace_all(includeh_join$label, " ", "_")

tree <- as_tibble(tree)
tree_join <- full_join(tree, includeh_join, by = "label")
tree_join <- as.treedata(tree_join)

ggtree(tree_join, aes(colour = order),
       layout = "circular") +
  geom_fruit(geom = geom_bar,
             mapping = aes(x = h, fill = order),
             pwidth = 0.5,
             orientation = "y", 
             stat = "identity")



#save and read

write.csv(includeh, file = "outputs/includeh.csv")
includeh <- read.csv(file = "outputs/includeh.csv", header = T)[-c(1)]



#testing

test <- includeh[44:63,]

for (i in 1:length(test$id)) {
  if (is.na(test$family[i])) {
    tax_df <- melt(tax_lineage(taxonomy_taxon_info(ott_ids = test$id[i], include_lineage = TRUE)))
    tax_filter <- tax_df %>%
      filter(rank == "family")
    family <- tax_filter$unique_name
    test$family[i] <- as.character(family)
  }
}

tax_df_test <- melt(tax_lineage(taxonomy_taxon_info(ott_ids = 6145836, include_lineage = TRUE)))
tax_filter_test <- tax_df_test %>%
  filter(rank == "family")
tax_filter_test$unique_name

sum(is.na(includeh$redlistCategory1))
