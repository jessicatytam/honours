library(tidyverse)
library(rotl)

#saving and loading
write.csv(combinedf, file = "outputs/combinedf.csv")
combinedf <- read.csv(file = "outputs/combinedf.csv", header = T)[-c(1)]
domestication_h <- read.csv(file = "intermediate_data/domestication_h.csv", header = T)

#datasets
mass <- read.csv(file = "intermediate_data/EltonTraits.csv", header = T)
phylogeny <- read.csv(file = "intermediate_data/species_list_from_phylo.csv", header = T)
location <- read.csv(file = "intermediate_data/gbif_lat_medians.csv", header = T)
humanuse <- read.csv(file = "intermediate_data/IUCN_use.csv", header = T)
redlist <- read.csv(file = "intermediate_data/IUCN_redlist.csv", header = T)
domestication <- read.csv(file = "intermediate_data/domestication_h.csv", header = T)

#synonym matching

#mass

for (i in 1:length(mass$Scientific)) {
  mass$species[i] <- tnrs_match_names(mass$Scientific[i])$unique_name
  print(paste("Done:", mass$Scientific[i]))
} 

for (i in 1:length(mass$species)) {
  if (is.na(mass$species[i])) {
    mass$species[i] <- mass$Scientific[i]
  }
}

#location

location$genus_species <- location$species
  
for (i in 1:length(location$genus_species)) {
  location$species[i] <- tnrs_match_names(location$genus_species[i])$unique_name
  print(paste("Done:", location$genus_species[i]))
} 

for (i in 1:length(location$species)) {
  if (is.na(location$species[i])) {
    location$species[i] <- location$genus_species[i]
  }
}

#phylogeny

phylogeny <- phylogeny %>%
  mutate(phylogeny = "Yes")

#human use

for (i in 1:length(humanuse$usescientificName)) {
  humanuse$species[i] <- tnrs_match_names(humanuse$usescientificName[i])$unique_name
  print(paste("Done:", humanuse$usescientificName[i]))
}

for (i in 1:length(humanuse$species)) {
  if (is.na(humanuse$species[i])) {
    humanuse$species[i] <- humanuse$usescientificName[i]
  }
}

#iucn red list

redlist <- redlist[-c(1)] %>%
  unique() #6093

redlist <- redlist %>%
  mutate(sppcount = sequence(rle(redlist$scientificName)$lengths)) #new column to count the occurrences of each species

redlist <- pivot_wider(redlist, names_from = sppcount, values_from = redlistCategory)
colnames(redlist) <- paste0("redlistCategory", colnames(redlist))

for (i in 1:length(redlist$redlistCategoryscientificName)) {
  redlist$species[i] <- tnrs_match_names(redlist$redlistCategoryscientificName[i])$unique_name
  print(paste("Done:", redlist$redlistCategoryscientificName[i]))
}

for (i in 1:length(redlist$species)) {
  if (is.na(redlist$species[i])) {
    redlist$species[i] <- redlist$redlistCategoryscientificName[i]
  }
}

#domestication

domestication <- domestication[c("genus_species", "domestication")]
domestication <- domestication %>%
  rename(species = "genus_species")


#combine
combinedf2 <- list(mass, phylogeny, location, redlist, humanuse, domestication) %>% 
  reduce(full_join, by = "species") %>%
  arrange(species) %>% #7955
  unique() 

#remove unwanted variables
combinedf2 <- combinedf2[-c(1, 2, 7, 12, 13, 17, 18)]

#renaming
combinedf2 <- combinedf2 %>%
  rename(genus_species = species,
         family = MSWFamilyLatin)

#reordering
combinedf2 <- combinedf2[c(4, 5, 1, 8, 2:3, 6:7, 9:28)]
