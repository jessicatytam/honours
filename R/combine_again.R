library(dplyr)
library(rotl)

#saving and loading
write.csv(combinedf, file = "outputs/combinedf.csv")
combinedf <- read.csv(file = "outputs/combinedf.csv", header = T)[-c(1)]

#datasets
mass <- read.csv(file = "intermediate_data/EltonTraits.csv", header = T)
phylogeny <- read.csv(file = "intermediate_data/species_list_from_phylo.csv", header = T)
location <- read.csv(file = "intermediate_data/gbif_lat_medians.csv", header = T)
humanuse <- read.csv(file = "intermediate_data/IUCN_use.csv", header = T)
redlist <- read.csv(file = "intermediate_data/IUCN_redlist.csv", header = T)

#synonym matching

mass <- mass[-c(1)] %>%
  rename(species = Scientific,
         family = MSWFamilyLatin)
for (i in 1:length(mass$species)) {
  mass$species[i] <- tnrs_match_names(mass$species[i])$unique_name
  print(paste("Done:", mass$species[i]))
} 

phylogeny <- phylogeny %>%
  rename(species = specieslist) %>%
  mutate(phylogeny = "Yes")
for (i in 1:length(phylogeny$species)) {
  phylogeny$species[i] <- tnrs_match_names(phylogeny$species[i])$unique_name
  print(paste("Done:", phylogeny$species[i]))
}

humanuse <- humanuse[-c(1)] %>%
  rename(species = usescientificName)
for (i in 1:length(humanuse$species)) {
  humanuse$species[i] <- tnrs_match_names(humanuse$species[i])$unique_name
  print(paste("Done:", humanuse$species[i]))
}

redlist <- redlist[-c(1)] %>%
  unique() #6093

redlist <- redlist %>%
  mutate(sppcount = sequence(rle(redlist$scientificName)$lengths)) #new column to count the occurrences of each species

redlist <- pivot_wider(redlist, names_from = sppcount, values_from = redlistCategory)
colnames(redlist) <- paste0("redlistCategory", colnames(redlist))
redlist <- redlist %>%
  rename(species = redlistCategoryscientificName)

for (i in 1:length(redlist$species)) {
  redlist$species[i] <- tnrs_match_names(redlist$species[i])$unique_name
  print(paste("Done:", redlist$species[i]))
}

#combine
combinedf <- list(mass, phylogeny, location, redlist, humanuse) %>% 
  reduce(full_join, by = "species") %>%
  arrange(species) %>% #8308
  unique() 

