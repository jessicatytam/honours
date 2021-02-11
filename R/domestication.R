library(tidyverse)
library(rvest)
library(rotl)
library(specieshindex)

#get the data

wiki_url <- read_html("https://en.wikipedia.org/wiki/List_of_domesticated_animals")

domesticated <- wiki_url %>% 
  html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>%
  html_table()

partially_domesticated <- wiki_url %>% 
  html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>%
  html_table()

#remove non-mammals

domesticated <- domesticated %>%
  filter(str_extract(domesticated$`Taxon group`, "^1") == 1)

partially_domesticated <- partially_domesticated %>%
  filter(str_extract(partially_domesticated$`Taxon group`, "^1") == 1)

#get binomial names 

domesticated <- domesticated %>% #extract names using the brackets
  mutate(species = str_extract(domesticated$`Species and subspecies`, "(?<=\\().+(?=\\))")) 

partially_domesticated <- partially_domesticated %>% #extract names using the brackets
  mutate(species = str_extract_all(partially_domesticated$`Species and subspecies`, "\\(([^\\)]+)")) 

#cleaning binomial names

partially_domesticated$species <- str_replace_all(partially_domesticated$species, "\\(", "") #removing mismatched brackets
partially_domesticated$species <- str_replace_all(partially_domesticated$species, "\\)", "")

for (i in 1:length(partially_domesticated$species)) { #removing "c"
  if (grepl('"', partially_domesticated$species[i])) {
    partially_domesticated$species[i] <- str_replace(partially_domesticated$species[i], "c", "")
  }
}

partially_domesticated$species <- str_replace_all(partially_domesticated$species, '"', "") #removing quotation marks

partially_domesticated <- partially_domesticated %>% #separate the names using ","
  separate(species, c("species1", "species2", "species3", "species4", "species5",
                      "species6", "species7", "species8", "species9", "species10"), sep = "[,]+")

#fill in genus names

for (j in 11:ncol(partially_domesticated)) {
  for (i in 1:nrow(partially_domesticated)) {
    if (!is.na(partially_domesticated[,j][i]) & (lengths(str_split(partially_domesticated[,j][i], " ")) == 3)) {
      partially_domesticated[,j][i] <- str_replace(partially_domesticated[,j][i], ".+(?=[:space:])", word(partially_domesticated$species1[i], 1))
    } else if (!is.na(partially_domesticated[,j][i]) & (lengths(str_split(partially_domesticated[,j][i], " ")) == 4)) {
      partially_domesticated[,j][i] <- str_replace(partially_domesticated[,j][i], ".+(?=[:space:])", word(partially_domesticated$species1[i], 1, 2))
    } 
  }
}

partially_domesticated[46, 13] <- "Osphranter rufus"

#check for and fix spelling mistakes

for (i in 1:length(domesticated$species)) {
  table(tnrs_match_names(names = domesticated$species[i])$approximate_match)
}

for (i in 1:length(partially_domesticated$species1)) {
  table(tnrs_match_names(names = partially_domesticated$species1[i])$approximate_match)
} 

for (i in 1:length(partially_domesticated$species10)) {
  if (!is.na(partially_domesticated$species10[i])) {
    table(tnrs_match_names(names = partially_domesticated$species10[i])$approximate_match)
  }
}

#pivot longer

partially_domesticated <- partially_domesticated %>%
  rename("spp and subspp" = "Species and subspecies")

partially_domesticated <- partially_domesticated %>%
  pivot_longer(cols = starts_with("species"),
               values_to = "species",
               values_drop_na = TRUE)

partially_domesticated <- partially_domesticated %>%
  select(!name)

#combine

domesticated <- domesticated %>%
  rename("spp and subspp" = "Species and subspecies")

partially_domesticated <- partially_domesticated %>%
  rename(Purposes = Purpose)

domestication <- rbind(domesticated, partially_domesticated)

#remove unwanted data

domestication <- domestication[c(10, 5, 7:8)] 
domestication <- domestication %>%
  arrange(species)

#get id for domestication

domestication <- domestication %>%
  mutate(id = tnrs_match_names(names = domestication$species)$ott_id, .after = species)

#remove subspecies

domestication[28, 1] <- "Canis lupus" #add Canis lupus
domestication[28, 2] <- tnrs_match_names(names = "Canis lupus")$ott_id

domestication <- domestication %>%
  filter(lengths(str_split(domestication$species, " "))==2)

#check for synonyms

tnrs_match_names(names = domestication$species)

#h-index

syn <- read_csv("intermediate_data/synonyms.csv")
syn$synonyms <- shQuote(syn$synonyms, "cmd") #add quotation marks around synonyms

scopus_out <- list() #initializing empty list 
for (i in 1:length(domestication$species)) {  
  if (!domestication$id[i] %in% syn$id) {
    scopus_out[[i]] <- FetchSpTAK(genus = str_split(domestication$species[i], pattern = " ")[[1]][1],
                                  species = str_split(domestication$species[i], pattern = " ")[[1]][2],
                                  APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  } else {
    syns <- syn$synonyms[match(domestication$id[i], syn$id)]
    scopus_out[[i]] <- FetchSpTAK(genus = str_split(domestication$species[i], pattern = " ")[[1]][1],
                                  species = str_split(domestication$species[i], pattern = " ")[[1]][2],
                                  synonyms = syns,
                                  APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  }
}

indices <- list()
for (i in 1:length(scopus_out)){
  indices[[i]] <- Allindices(scopus_out[[i]],
                              genus = str_split(domestication$species[i], pattern = " ")[[1]][1],
                              species = str_split(domestication$species[i], pattern = " ")[[1]][2])
}

#save and load

write.csv(domestication, file = "intermediate_data/domestication.csv")
domestication <- read.csv(file = "intermediate_data/domestication.csv", header = T)[-c(1),]

write.csv(indices, file = "intermediate_data/domestication_h.csv")

#testing

test <- partially_domesticated[c(91:100),]

grepl('"', test$species[1]) #if this is true

str_replace(test$species, "c", "")

for (i in 1:length(test$species)) {
  if (grepl('"', test$species[i])) {
    test$species[i] <- str_replace(test$species[i], "c", "")
  }
}

