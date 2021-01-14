library(dplyr)
library(rotl)


phylo <- read.csv(file = "intermediate_data/species_list_from_phylo.csv")[-c(1)]

#get synonyms

synonyms <- data.frame()
for (i in 1:nrow(phylo)) {
  results <- synonyms(tnrs_match_names(phylo$species))[i]
  resultsdf <- data.frame(results)
  if (length(resultsdf) > 0) {
    colnames(resultsdf) = colnames(synonyms)
    synonyms <- bind_rows(synonyms, resultsdf)
  }
  print(paste("Done:", phylo$species[i]))
}

synonyms <- synonyms %>% rename(synonyms = ...1)

#get species ID

id <- data.frame()
for(i in 1:nrow(synonyms)) {
  getID <- tnrs_match_names(synonyms$synonyms[i])
  getID <- data.frame(getID$ott_id)
  id <- bind_rows(id, getID)
  print(paste("Done:", synonyms$synonyms[i]))
}

synonyms <- cbind(synonyms, id)
synonyms <- synonyms %>% rename(id = getID.ott_id)

phyloid <- data.frame()
for(i in 1:nrow(phylo)) {
  getID <- tnrs_match_names(phylo$species[i])
  getID <- data.frame(getID$ott_id)
  phyloid <- bind_rows(phyloid, getID)
  print(paste("Done:", phylo$species[i]))
}

phylo <- cbind(phylo, phyloid)
phylo <- phylo %>% rename(id = getID.ott_id)

#saving files

write.csv(synonyms, file = "intermediate_data/synonyms.csv")
write.csv(phylo, file = "intermediate_data/species_list_from_phylo.csv")

#testing

kangaroo <- data.frame(synonyms(tnrs_match_names("Macropus rufus")))
data.frame(synonyms(tnrs_match_names("Abrocoma vaccarum")))

