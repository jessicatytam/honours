library(tidyverse)
library(specieshindex)

syn <- read_csv("intermediate_data/synonyms.csv")
sp <- read_csv("intermediate_data/species_list_from_phylo.csv")

sp$species[16]
syn$synonyms[match(sp$id[16], syn$id)]

#checking that synomym search works
FetchSpTAK(genus = "Abrothrix", species = "sanborni", synonyms = "Akodon sanborni", APIkey = "442b9048417ef20cf680a0ae26ee4d86")

#add quotation marks around synonyms
syn$synonyms <- shQuote(syn$synonyms, "cmd")

#list to get citation records
scopus_out <- list() #initializing empty lists
for (i in 1:100) {  
  if (!sp$id[i] %in% syn$id) {
    scopus_out[[i]] <- FetchSpTAK(genus = str_split(sp$species[i], pattern = " ")[[1]][1],
                                  species = str_split(sp$species[i], pattern = " ")[[1]][2],
                                  APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  } else {
   syns <- syn$synonyms[match(sp$id[i], syn$id)]
   scopus_out[[i]] <- FetchSpTAK(genus = str_split(sp$species[i], pattern = " ")[[1]][1],
                                 species = str_split(sp$species[i], pattern = " ")[[1]][2],
                                 synonyms = syns,
                                 APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  }
}

saveRDS(scopus_out, "intermediate_data/temp_scopus_results.RDS")


#currently getting at error at index 14 and also later on
indices <- list()
for (i in 1:length(scopus_out)){
  indices[[i]] <- Allindices(scopus_out[[i]],
                             genus = str_split(sp$species[i], pattern = " ")[[1]][1],
                             species = str_split(sp$species[i], pattern = " ")[[1]][2])
}

indices_df <- bind_rows(indices)






#testing

test <- syn[1:10,]

for (i in 1:10) {
  test$synonyms <- shQuote(test$synonyms, "cmd")
}

test_scopus_out <- list() #initializing empty lists
for (i in 1:nrow(test)) {  
  if (!sp$id[i] %in% test$id){
    scopus_out[[i]] <- FetchSpTAK(genus = str_split(sp$species[i], pattern = " ")[[1]][1],
                                  species = str_split(sp$species[i], pattern = " ")[[1]][2],
                                  APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  }
  else {
    syns <- syn$synonyms[match(sp$id[i], syn$id)]
    scopus_out[[i]] <- FetchSpTAK(genus = str_split(sp$species[i], pattern = " ")[[1]][1],
                                  species = str_split(sp$species[i], pattern = " ")[[1]][2],
                                  synonyms = syns,
                                  APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  }
}

