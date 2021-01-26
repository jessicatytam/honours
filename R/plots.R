library(tidyverse)

hindex <- read.csv(file = "outputs/hindex.csv", header = T)
combinedf <- read.csv(file = "outputs/combinedf.csv", header = T)

#some cleaning and filtering

mass <- combinedf %>%
  filter(phylogeny == "Yes") %>%
  filter(!is.na(BodyMass.Value))

phylogeny <- hindex$genus_species %in% mass$species = T

#plots

ggplot(hindex, aes(x = reorder(genus_species, h),
                   y = h)) +
  geom_point()

ggplot(aes(x = mass$BodyMass.Value,
           y = hindex$h)) +
  geom_point()
