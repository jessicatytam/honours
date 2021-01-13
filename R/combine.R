library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(taxize)
library(rinat)
library(rotl)

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

for (i in 1:nrow(combinedf)) { 
  if (is.na(combinedf$family[i])) {
    family <- tax_name(sci = combinedf$species[i], get = "family", db = "itis")
    combinedf$family[i] <- as.character(family[3])
  }
} #not working suddenly

for (i in 1:nrow(combinedf)) { 
  if (is.na(combinedf$order[i])) {
    order <- tax_name(sci = combinedf$species[i], get = "order", db = "itis")
    combinedf$order[i] <- as.character(order[3])
  }
} #not working suddenly

#checking the number of NA values

sum(is.na(combinedf$family)) #1741 for ncbi
sum(is.na(combinedf$order)) #1669 for ncbi

#replacing NA in phylogeny

combinedf <- combinedf %>%
  replace_na(list(phylogeny = "No"))

#remove species with 1 word in the string

sum(lengths(gregexpr("\\w+", combinedf$species))==1) #3 

for (i in 1:nrow(combinedf)) {
  if (lengths(gregexpr("\\w+", combinedf$species[i]))==1) {
    combinedf <- combinedf[-i,]
  }
} #8305 species

#update phylogeny list

phylo_list <- combinedf %>%
  filter(phylogeny=="Yes") %>%
  select(species) %>%
  write.csv("intermediate_data/species_list_from_phylo.csv")

#synonym matching

combinedf <- combinedf %>%
  mutate(ID = tnrs_match_names(species)$ott_id, .after = species)

length(unique(combinedf$ID)) #7746

tnrs_match_names("Macropus rufus")
synonyms(tnrs_match_names("Macropus rufus"))


#remove duplicates after synonyms are matched
synonymsdf <- combinedf %>%
  group_by(ID) %>%
  fill(c(3:27), .direction = "downup") %>%
  ungroup() #this didn't really do anything


#common name matching

get_inat_common_name <- function(scientificname){
  a_mac <- get_inat_obs(taxon_name = scientificname, maxresults = 1000)
  return(names(sort(table(a_mac$species_guess), decreasing = TRUE)[1]))
}

commondf <- data.frame()
for (i in 1:nrow(combinedf)) {
  common_name <- tryCatch(get_inat_common_name(combinedf$species[i]), error = function(e) NA)
  common_name <- data.frame(common_name)
  commondf <- rbind(commondf, common_name)
} #could not get all of the common names; "No encoding supplied: defaulting to UTF-8."
combinedf <- cbind(combinedf, commondf)
combinedf <- combinedf[c(1:2,28,3:27)]

#replace blanks with NA


#REMEMBER TO SAVE

write.csv(combinedf, file = "outputs/combinedf.csv")

combinedf <- read.csv(file = "outputs/combinedf.csv", header = T)[-c(1)]






#some testing

test <- combinedf[11:20,]

test <- test %>%
  group_by(ID) %>%
  fill(c(3:27), .direction = "downup") %>%
  ungroup()

