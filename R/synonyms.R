library(dplyr)
library(rotl)


phylo <- read.csv(file = "intermediate_data/species_list_from_phylo.csv")[-c(1)]
combinedf_dup <- read.csv(file = "outputs/data/combinedf_dup.csv")[-c(1)]

#get synonyms

synonyms <- data.frame()
for (i in 1:nrow(combinedf_dup)) {
  results <- synonyms(tnrs_match_names(combinedf_dup$species))[i]
  resultsdf <- data.frame(results)
  if (length(resultsdf) > 0) {
    colnames(resultsdf) = colnames(synonyms)
    synonyms <- bind_rows(synonyms, resultsdf)
  }
  print(paste("Done:", combinedf_dup$species[i]))
}

synonyms <- synonyms %>% rename(synonyms = ...1) 

#get species ID

#id <- data.frame()
#for(i in 1:nrow(synonyms)) {
#  getID <- tnrs_match_names(synonyms$synonyms[i])
#  getID <- data.frame(getID$ott_id)
#  id <- bind_rows(id, getID)
#  print(paste("Done:", synonyms$synonyms[i]))
#}

#synonyms <- cbind(synonyms, id)
#synonyms <- synonyms %>% rename(id = getID.ott_id)

for(i in 1:nrow(synonyms)) {
  synonyms$id[i] <- tnrs_match_names(synonyms$synonyms[i])$ott_id
  print(paste("Done:", synonyms$synonyms[i]))
}

#phyloid <- data.frame()
#for(i in 1:nrow(phylo)) {
#  getID <- tnrs_match_names(phylo$species[i])
#  getID <- data.frame(getID$ott_id)
#  phyloid <- bind_rows(phyloid, getID)
#  print(paste("Done:", phylo$species[i]))
#}

#phylo <- cbind(phylo, phyloid)phyloid <- data.frame()
#phylo <- phylo %>% rename(id = getID.ott_id)

phyloid <- combinedf_dup[c(1, 4)] 

#checking

length(unique(synonyms$id)) #3381
length(unique(phyloid$id)) #7522

#saving files

write.csv(synonyms, file = "data/intermediate_data/synonyms.csv")
write.csv(phyloid, file = "data/intermediate_data/species_list_from_phylo.csv")
