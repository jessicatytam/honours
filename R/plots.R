library(tidyverse)

hindex <- read.csv(file = "outputs/hindex.csv", header = T)
combinedf <- read.csv(file = "outputs/combinedf.csv", header = T)

combinedf <- combinedf %>%
  rename(genus_species = species)

hindex$genus_species <- str_replace_all(hindex$genus_species, "_", " ")

includeh <- list(combinedf, hindex) %>% 
  reduce(full_join, by = "genus_species")



#some cleaning and filtering

mass <- combinedf %>%
  filter(phylogeny == "Yes") %>%
  filter(!is.na(BodyMass.Value))

phylogeny <- filter(hindex$genus_species %in% mass$species)

#quick plots

#h-index
ggplot(hindex, aes(x = reorder(genus_species, h),
                   y = h)) +
  geom_point(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90))

#mass
ggplot(includeh, aes(x = BodyMass.Value,
                     y = h)) +
  geom_point(alpha = 0.5)

#iucn category
unique(includeh$redlistCategory1)
includeh$redlistCategorySort <- factor(includeh$redlistCategory1, levels = c("Least Concern", "Near Threaten", "Vulnerable",
                                                                             "Endangered", "Critically Endangered",
                                                                             "Regionally Extinct", "Extinct in the Wild", "Extinct",
                                                                             "Data Deficient", "Not Applicable", NA))



ggplot(includeh, aes(x = redlistCategorySort,
                     y = h)) +
  geom_boxplot() +
  geom_jitter(aes(colour = order),
              alpha = 0.5) #+
  theme(legend.position = "none")


