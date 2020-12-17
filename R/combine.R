library(dplyr)
library(purrr)

#load datasets

mass <- read.csv(file = "intermediate_data/EltonTraits.csv", header = T)
phylogeny <- read.csv(file = "intermediate_data/species_list_from_phylo.csv", header = T)
location <- read.csv(file = "intermediate_data/gbif_lat_medians.csv", header = T)
humanuse <- read.csv(file = "intermediate_data/IUCN_use.csv", header = T)
redlist <- read.csv(file = "intermediate_data/IUCN_redlist.csv", header = T)

#renaming and cleaning for species matching

mass <- mass[-c(1)] %>%
  rename(species = Scientific,
         family = MSWFamilyLatin)

phylogeny <- phylogeny %>%
  rename(species = specieslist) %>%
  mutate(phylogeny = "Yes")

humanuse <- humanuse[-c(1)] %>%
  rename(species = usescientificName)

redlist <- redlist[-c(1)] %>%
  rename(species = scientificName)

#combining the datasets

combinedf <- list(mass, phylogeny, location, redlist, humanuse) %>% 
  reduce(full_join, by = "species") %>%
  arrange(species) %>% #8711
  unique() #8433 

combinedf <- combinedf[c(1:2,7,3:6,8:24)]

length(unique(combinedf$species)) #8308
count(unique(combinedf)) #8433

combinedf 