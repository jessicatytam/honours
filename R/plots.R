library(tidyverse)
library(ggExtra)
library(taxize)
library(rotl)
library(reshape)

library(rnaturalearth)
library(rnaturalearthdata)

library(sp)
library(maptools)
library(rgeos)
library(raster)
library(RColorBrewer)

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

#Musserakis sulawesiensis a nematode?!?!

#LOG TRANSFORM

includeh <- includeh %>%
  mutate(logh = log10(h),
         logmass = log10(BodyMass.Value))

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
ggplot(includeh, aes(x = logh,
                     y = order)) +
  geom_boxplot() +
  geom_jitter(aes(colour = redlistCategory1),
              alpha = 0.5)

#h-index
ggplot(includeh, aes(x = reorder(genus_species, logh),
                     y = logh)) +
  geom_point(aes(colour = order),
             alpha = 0.5) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90)) 

ggplot(includeh, aes(x = logh,
                     fill = order)) +
  geom_bar() 

ggplot(includeh, aes(x = h,
                     fill = order)) +
  geom_bar() 

#mass; fix the body mass values
ggplot(includeh, aes(x = logmass,
                     y = logh)) +
  geom_point(aes(colour = order),
             alpha = 0.5) 

#iucn category
ggplot(includeh, aes(x = redlistCategory1,
                     y = logh)) +
  geom_boxplot() +
  geom_jitter(aes(colour = order),
              alpha = 0.5) 

#human use
includeh_pivot <- includeh %>%
  pivot_longer(cols = starts_with("use"),
               names_to = "use_count",
               values_to = "human_use")

ggplot(includeh_pivot, aes(x = logh,
                           y = human_use)) +
  geom_boxplot() +
  geom_jitter(aes(colour = order),
              alpha = 0.5) 

#latitude

med_lat <- ggplot(includeh, aes(x = median_lat,
                                y = logh,
                                colour = order)) +
  geom_point(alpha = 0.5)
ggMarginal(med_lat,
           type = "histogram",
           margins = "x",
           bins = 100)

#map
world <- ne_countries(scale = "large", returnclass = "sp")

ggplot(data = world) +
  geom_sf() +
  geom_point(data = includeh, aes(x = x,
                                  y = y,
                                  colour = h),
             alpha = 0.5) 

#phylogenetic tree; need help
taxa <- tnrs_match_names("Mammalia") #find iTOL record for Mammalia
res <- tol_subtree(ott_id = taxa$ott_id, label_format = "name") #extract subtree of mammals
str(res)

hindex$genus_species <- str_replace_all(hindex$genus_species, " ", "_")

newres <- map(res$tip.label, ~ keep(.x, hindex$genus_species))



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
