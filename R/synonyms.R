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

tnrs_match_names(synonyms$...1)


#saving files

write.csv(synonyms, file = "intermediate_data/synonyms.csv")


#testing

kangaroo <- data.frame(synonyms(tnrs_match_names("Macropus rufus")))
data.frame(synonyms(tnrs_match_names("Abrocoma vaccarum")))

test <- data.frame(phylo[1:10,])

synonyms <- data.frame()
for (i in 1:nrow(test)) {
  results <- synonyms(tnrs_match_names(test$phylo.1.10...))[i]
  resultsdf <- data.frame(results)
  #colnames(resultsdf) = colnames(synonyms)
  synonyms <- bind_rows(synonyms, resultsdf)
}

is.atomic(results)
