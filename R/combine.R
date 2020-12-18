library(dplyr)
library(purrr)
library(tidyr)
library(taxize)

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
  unique() #6093

redlist <- redlist %>%
  mutate(sppcount = sequence(rle(redlist$scientificName)$lengths)) #new column to count the occurrences of each species

redlist <- pivot_wider(redlist, names_from = sppcount, values_from = redlistCategory)
colnames(redlist) <- paste0("redlistCategory", colnames(redlist))
redlist <- redlist %>%
  rename(species = redlistCategoryscientificName)

#combining the datasets

combinedf <- list(mass, phylogeny, location, redlist, humanuse) %>% 
  reduce(full_join, by = "species") %>%
  arrange(species) %>% #8308
  unique() 

combinedf <- combinedf[c(1:2,7,3:6,8:26)]

length(unique(combinedf$species)) #8308
count(unique(combinedf)) #8308

#add families and orders

for (i in 45:nrow(combinedf)) { 
  if (is.na(combinedf$family[i])) {
    family <- tax_name(combinedf$species[i], get = "family", db = "ncbi")
    combinedf$family[i] <- as.character(family[3])
  }
}

for (i in 1:nrow(combinedf)) { 
  if (is.na(combinedf$order[i])) {
    order <- tax_name(combinedf$species[i], get = "order", db = "ncbi")
    combinedf$order[i] <- as.character(order[3])
  }
}

#checking the number of NA values

sum(is.na(combinedf$family)) #1741 for ncbi
sum(is.na(combinedf$order)) #1669 for ncbi

#REMEMBER TO SAVE

write.csv(combinedf, file = "outputs/combined.csv")
