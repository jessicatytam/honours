library(tidyverse)
library(ggExtra)
library(ape)

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

#LOG TRANSFORM

includeh <- includeh %>%
  mutate(logh = log10(h),
         logmass = log10(BodyMass.Value))

#quick plots

#h-index
ggplot(includeh, aes(x = reorder(genus_species, logh),
                     y = logh)) +
  geom_point(aes(colour = order),
             alpha = 0.5) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90)) 

ggplot(includeh, aes(x = h,
                     fill = order)) +
  geom_bar() 

#mass; fix the body mass values
ggplot(includeh, aes(x = logmass,
                     y = logh)) +
  geom_point(aes(colour = order),
             alpha = 0.5) 

glm(logh ~ logmass, data = includeh, family = "poisson") #filter negative values

#iucn category
unique(includeh$redlistCategory1)
includeh$redlistCategorySort <- factor(includeh$redlistCategory1, levels = c("Least Concern", "Near Threaten", "Vulnerable",
                                                                             "Endangered", "Critically Endangered",
                                                                             "Regionally Extinct", "Extinct in the Wild", "Extinct",
                                                                             "Data Deficient", "Not Applicable", NA))

ggplot(includeh, aes(x = redlistCategorySort,
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
  geom_point()
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

#phylogenetic tree
taxa <- tnrs_match_names("Mammalia") #find iTOL record for Mammalia
res <- tol_subtree(ott_id = taxa$ott_id, label_format = "name") #extract subtree of mammals
str(res)

hindex$genus_species <- str_replace_all(hindex$genus_species, " ", "_")

newres <- map(res$tip.label, ~ keep(.x, hindex$genus_species))
