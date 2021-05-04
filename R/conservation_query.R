library(tidyverse)
devtools::install_github("jessicatytam/specieshindex", force = TRUE, build_vignettes = FALSE)
library(specieshindex)

syn <- read_csv("data/intermediate_data/synonyms.csv")
includeh <- read_csv("outputs/data/includeh.csv")
sp1 <- data.frame(includeh[1:3500,])
sp2 <- data.frame(includeh[3501:7522,])

#add quotation marks to synonyms

syn$synonyms <- shQuote(syn$synonyms, "cmd")

#list to get citation records for "conserv*"
scopus_out1 <- list() #initializing empty list 1
for (i in 1:length(sp1$genus_species)) {  
  if (!sp1$id[i] %in% syn$id) {
    scopus_out1[[i]] <- FetchSpTAK(genus = str_split(sp1$genus_species[i], pattern = " ")[[1]][1],
                                   species = str_split(sp1$genus_species[i], pattern = " ")[[1]][2],
                                   additionalkeywords = "conserv*",
                                   APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  } else {
    syns <- syn$synonyms[match(sp1$id[i], syn$id)]
    scopus_out1[[i]] <- FetchSpTAK(genus = str_split(sp1$genus_species[i], pattern = " ")[[1]][1],
                                   species = str_split(sp1$genus_species[i], pattern = " ")[[1]][2],
                                   synonyms = syns,
                                   additionalkeywords = "conserv*",
                                   APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  }
}

scopus_out2 <- list() #initializing empty list 2
for (i in 1:length(sp2$genus_species)) {  
  if (!sp2$id[i] %in% syn$id) {
    scopus_out2[[i]] <- FetchSpTAK(genus = str_split(sp2$genus_species[i], pattern = " ")[[1]][1],
                                   species = str_split(sp2$genus_species[i], pattern = " ")[[1]][2],
                                   additionalkeywords = "conserv*",
                                   APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  } else {
    syns <- syn$synonyms[match(sp2$id[i], syn$id)]
    scopus_out2[[i]] <- FetchSpTAK(genus = str_split(sp2$genus_species[i], pattern = " ")[[1]][1],
                                   species = str_split(sp2$genus_species[i], pattern = " ")[[1]][2],
                                   synonyms = syns,
                                   additionalkeywords = "conserv*",
                                   APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  }
}

indices1 <- list()
for (i in 1:length(scopus_out1)){
  indices1[[i]] <- Allindices(scopus_out1[[i]],
                              genus = str_split(sp1$genus_species[i], pattern = " ")[[1]][1],
                              species = str_split(sp1$genus_species[i], pattern = " ")[[1]][2])
}

indices2 <- list()
for (i in 1:length(scopus_out2)){
  indices2[[i]] <- Allindices(scopus_out2[[i]],
                              genus = str_split(sp2$genus_species[i], pattern = " ")[[1]][1],
                              species = str_split(sp2$genus_species[i], pattern = " ")[[1]][2])
}

indices1_df <- bind_rows(indices1)
indices2_df <- bind_rows(indices2)
conserv_indices_df <- rbind(indices1_df, indices2_df)

write.csv(conserv_indices_df, file = "outputs/hindex_conserv.csv")

#list to get citation records for "conservation"
scopus_out1 <- list() #initializing empty list 1
for (i in 1:length(sp1$genus_species)) {  
  if (!sp1$id[i] %in% syn$id) {
    scopus_out1[[i]] <- FetchSpTAK(genus = str_split(sp1$genus_species[i], pattern = " ")[[1]][1],
                                   species = str_split(sp1$genus_species[i], pattern = " ")[[1]][2],
                                   additionalkeywords = "conservation",
                                   APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  } else {
    syns <- syn$synonyms[match(sp1$id[i], syn$id)]
    scopus_out1[[i]] <- FetchSpTAK(genus = str_split(sp1$genus_species[i], pattern = " ")[[1]][1],
                                   species = str_split(sp1$genus_species[i], pattern = " ")[[1]][2],
                                   synonyms = syns,
                                   additionalkeywords = "conservation",
                                   APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  }
}

scopus_out2 <- list() #initializing empty list 2
for (i in 1:length(sp2$genus_species)) {  
  if (!sp2$id[i] %in% syn$id) {
    scopus_out2[[i]] <- FetchSpTAK(genus = str_split(sp2$genus_species[i], pattern = " ")[[1]][1],
                                   species = str_split(sp2$genus_species[i], pattern = " ")[[1]][2],
                                   additionalkeywords = "conservation",
                                   APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  } else {
    syns <- syn$synonyms[match(sp2$id[i], syn$id)]
    scopus_out2[[i]] <- FetchSpTAK(genus = str_split(sp2$genus_species[i], pattern = " ")[[1]][1],
                                   species = str_split(sp2$genus_species[i], pattern = " ")[[1]][2],
                                   synonyms = syns,
                                   additionalkeywords = "conservation",
                                   APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  }
}

indices1 <- list()
for (i in 1:length(scopus_out1)){
  indices1[[i]] <- Allindices(scopus_out1[[i]],
                              genus = str_split(sp1$genus_species[i], pattern = " ")[[1]][1],
                              species = str_split(sp1$genus_species[i], pattern = " ")[[1]][2])
}

indices2 <- list()
for (i in 1:length(scopus_out2)){
  indices2[[i]] <- Allindices(scopus_out2[[i]],
                              genus = str_split(sp2$genus_species[i], pattern = " ")[[1]][1],
                              species = str_split(sp2$genus_species[i], pattern = " ")[[1]][2])
}

indices1_df <- bind_rows(indices1)
indices2_df <- bind_rows(indices2)
conservation_indices_df <- rbind(indices1_df, indices2_df)

write.csv(conservation_indices_df, file = "outputs/hindex_conservation.csv")


#LOG TRANSFORM

conserv_indices_df <- conserv_indices_df %>%
  mutate(logh1 = log10(h+1))

conservation_indices_df <- conservation_indices_df %>%
  mutate(logh1 = log10(h+1))

#plots

ggplot() +
  geom_density(data = conservation_indices_df,
               mapping = aes(x = logh1,
                             fill = 'with keyword "conservation"'),
               alpha = 0.5)+
  geom_density(data = conserv_indices_df,
             mapping = aes(x = logh1,
                           fill = 'with keyword "conserv*"'),
             alpha = 0.5) +
  geom_density(data = includeh,
             mapping = aes(x = logh1,
                           fill = "without keywords"),
             alpha = 0.5) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0.3, 0.6, 1, 1.4),
                     labels = c(1, 3, 9, 24)) 

#reading and saving

includeh <- read_csv("outputs/includeh.csv")[-c(1)]
conserv_indices_df <- read_csv("outputs/hindex_conserv.csv")[-c(1)]
conservation_indices_df <- read_csv("outputs/hindex_conservation.csv")[-c(1)]

write.csv(conservation_indices_df, file = "outputs/hindex_conservation.csv")

#testing

