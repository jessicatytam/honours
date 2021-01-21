library(tidyverse)
devtools::install_github("jessicatytam/specieshindex", force = TRUE, build_vignettes = FALSE)
library(specieshindex)

syn <- read_csv("intermediate_data/synonyms.csv")
sp1 <- read_csv("intermediate_data/species_list_from_phylo.csv")[1:3500,]
sp2 <- read_csv("intermediate_data/species_list_from_phylo.csv")[3501:6949,]

sp$species[16]
syn$synonyms[match(sp$id[16], syn$id)]

#checking that synomym search works
FetchSpTAK(genus = "Abrothrix", species = "sanborni", synonyms = "Akodon sanborni", APIkey = "442b9048417ef20cf680a0ae26ee4d86")

#add quotation marks around synonyms
syn$synonyms <- shQuote(syn$synonyms, "cmd")

#list to get citation records
scopus_out1 <- list() #initializing empty lists
for (i in 1:length(sp1$species)) {  
  if (!sp1$id[i] %in% syn$id) {
    scopus_out1[[i]] <- FetchSpTAK(genus = str_split(sp1$species[i], pattern = " ")[[1]][1],
                                   species = str_split(sp1$species[i], pattern = " ")[[1]][2],
                                   APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  } else {
   syns <- syn$synonyms[match(sp1$id[i], syn$id)]
   scopus_out1[[i]] <- FetchSpTAK(genus = str_split(sp1$species[i], pattern = " ")[[1]][1],
                                  species = str_split(sp1$species[i], pattern = " ")[[1]][2],
                                  synonyms = syns,
                                  APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  }
}

scopus_out2 <- list() #initializing empty lists
for (i in 1:length(sp2$species)) {  
  if (!sp2$id[i] %in% syn$id) {
    scopus_out2[[i]] <- FetchSpTAK(genus = str_split(sp2$species[i], pattern = " ")[[1]][1],
                                   species = str_split(sp2$species[i], pattern = " ")[[1]][2],
                                   APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  } else {
    syns <- syn$synonyms[match(sp2$id[i], syn$id)]
    scopus_out2[[i]] <- FetchSpTAK(genus = str_split(sp2$species[i], pattern = " ")[[1]][1],
                                   species = str_split(sp2$species[i], pattern = " ")[[1]][2],
                                   synonyms = syns,
                                   APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  }
}

saveRDS(scopus_out1, "intermediate_data/scopus_results1.RDS")
saveRDS(scopus_out2, "intermediate_data/scopus_results2.RDS")

#currently getting at error at index 14 and also later on
indices1 <- list()
for (i in 1:length(scopus_out1)){
  indices1[[i]] <- Allindices(scopus_out1[[i]],
                              genus = str_split(sp1$species[i], pattern = " ")[[1]][1],
                              species = str_split(sp1$species[i], pattern = " ")[[1]][2])
}

indices2 <- list()
for (i in 1:length(scopus_out2)){
  indices2[[i]] <- Allindices(scopus_out2[[i]],
                              genus = str_split(sp2$species[i], pattern = " ")[[1]][1],
                              species = str_split(sp2$species[i], pattern = " ")[[1]][2])
}

indices1_df <- bind_rows(indices1)
indices2_df <- bind_rows(indices2)
indices_df <- rbind(indices1_df, indices2_df)

write.csv(indices_df, file = "outputs/hindex")






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


CountSpTAK(genus = "Allactaga", species = "bullata", synonyms = "Allactaga (Orientallactaga) bullata", APIkey = "442b9048417ef20cf680a0ae26ee4d86") #not working
CountSpTAK(genus = "Allactaga", species = "bullata", APIkey = "442b9048417ef20cf680a0ae26ee4d86") #this works
