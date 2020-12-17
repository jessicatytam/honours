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

test <- tax_name("Abrawayaomys chebezi", get = "family")
test[3]

testdf <- combinedf[1:20,]

for (i in 1:20) { #i think this is working but there is something wrong with the data provider
  if (isTRUE(is.na(testdf$order[i]))) {
    order <- tax_name(testdf$species[i], get = "order")
    testdf$order[i] <- as.character(order[3])
  }
}

class(testdf$family)
class(testdf$order)
