library(tidyverse)
library(rvest)
library(rotl)
library(specieshindex)
library(letsR)
library(geosphere)

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

#fix synonyms

domestication <- left_join(domestication, synonyms_check, by = "id")

for (i in 1:length(domestication$species)) {
  if (!is.na(domestication$unique_name[i])) {
    domestication$species[i] <- domestication$unique_name[i]
  }
}

domestication[148,1] <- "Tragelaphus scriptus"
domestication[148,2] <- as.integer(561121)
domestication <- domestication[-c(6,7)]
 
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

indices_df <- bind_rows(indices)

#check for synonyms

synonyms_check <- tnrs_match_names(names = domestication$species)
synonyms_check <- synonyms_check %>%
  filter(is_synonym == TRUE)
synonyms_check <- synonyms_check[c(1,2)]

synonyms_check$search_string <- str_to_sentence(synonyms_check$search_string)
synonyms_check <- synonyms_check %>%
  rename(genus_species = search_string)
indices_df$genus_species <- str_replace(indices_df$genus_species, "_", " ")

for (i in 1:length(indices_df$genus_species)) {
  if (indices_df$genus_species[i] %in% synonyms_check$genus_species) {
    indices_df <- indices_df[-i,]
  }
}

indices_df <- indices_df %>%
  filter(genus_species != "Macropus parma")

#get h-index again

synonyms_check <- synonyms_check %>%
  mutate(id = tnrs_match_names(names = synonyms_check$unique_name)$ott_id)

scopus_out_more <- list() #initializing empty list 
for (i in 1:length(synonyms_check$unique_name)) {  
  if (!synonyms_check$id[i] %in% syn$id) {
    scopus_out_more[[i]] <- FetchSpTAK(genus = str_split(synonyms_check$unique_name[i], pattern = " ")[[1]][1],
                                       species = str_split(synonyms_check$unique_name[i], pattern = " ")[[1]][2],
                                       APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  } else {
    syns <- syn$synonyms[match(synonyms_check$id[i], syn$id)]
    scopus_out_more[[i]] <- FetchSpTAK(genus = str_split(synonyms_check$unique_name[i], pattern = " ")[[1]][1],
                                       species = str_split(synonyms_check$unique_name[i], pattern = " ")[[1]][2],
                                       synonyms = syns,
                                       APIkey = "442b9048417ef20cf680a0ae26ee4d86")
  }
}

indices_more <- list()
for (i in 1:length(scopus_out_more)){
  indices_more[[i]] <- Allindices(scopus_out_more[[i]],
                             genus = str_split(synonyms_check$unique_name[i], pattern = " ")[[1]][1],
                             species = str_split(synonyms_check$unique_name[i], pattern = " ")[[1]][2])
}

indices_more_df <- bind_rows(indices_more)

#replace synonyms

indices_df <- rbind(indices_df, indices_more_df)
indices_df$genus_species <- str_replace(indices_df$genus_species, "_", " ")
indices_df <- indices_df %>%
  arrange()

#add missing species

african_elephant <- Allindices(FetchSpTAK(genus = "Loxodonta", species = "africana", APIkey = "442b9048417ef20cf680a0ae26ee4d86"),
                               genus = "Loxodonta", species = "africana")

#make it like includeh.csv

combinedf <- combinedf %>%
  rename(genus_species = species)

indices_df <- left_join(indices_df, combinedf, by = "genus_species")

indices_df <- indices_df %>%
  mutate(logh = log10(h),
         logh1 = log10(h+1),
         logmass = log10(BodyMass.Value))

sbs <- read.csv(file = "intermediate_data/gbif_processed.csv", header = T)

PAM <- lets.presab.points(cbind(sbs$decimalLongitude,sbs$decimalLatitude), sbs$species,
                          xmn = -180, xmx = 180, 
                          ymn = -90, ymx = 90,resol = 2)
#lets.midpoint.fixed() in file "R/geo_plotting"
mid <- lets.midpoint.fixed(PAM)
mid$x<-as.numeric(mid$x)
mid$y<-as.numeric(mid$y)
mid <- mid %>%
  rename(genus_species = Species)
indices_df <- list(indices_df, mid) %>%
  reduce(left_join, by = "genus_species")

indices_df$redlistCategory2 <- na_if(indices_df$redlistCategory2, "Not Applicable")

for (i in 1:nrow(indices_df)) {
  if(!is.na(indices_df$redlistCategory2[i])) {
    indices_df$redlistCategory1[i] <- indices_df$redlistCategory2[i]
    indices_df$redlistCategory2[i] <- NA
  }
}

indices_df <- indices_df[-c(22,23)]
indices_df <- indices_df %>%
  rename(redlistCategory = redlistCategory1)

status <- data.frame(tnrs_match_names(names = indices_df$genus_species)$flags)
status <- status %>%
  rename(status = tnrs_match_names.names...indices_df.genus_species..flags)
indices_df <- cbind(indices_df, status)

indices_df <- indices_df %>%
  rename(id = ID)

#add african elephant

indices_df <- bind_rows(indices_df, african_elephant)

indices_df[160, "ID"] <- 541936
indices_df[160, "BodyMass.Value"] <- 3940034.28
indices_df[160, "BodyMass.SpecLevel"] <- 1
indices_df[160, "phylogeny"] <- "No"
indices_df[160, "redlistCategory"] <- "Vulnerable"

indices_df[160, "logh"] <- log10(indices_df$h[160])
indices_df[160, "logh1"] <- log10(indices_df$h[160]+1)
indices_df[160, "logmass"] <- log10(indices_df$BodyMass.Value[160])

indices_df <- indices_df %>%
  arrange(genus_species)

#assign domestication category

indices_df <- indices_df %>%
  mutate(domestication = NA)

for (i in 1:length(indices_df$genus_species)) {
  if (indices_df$genus_species[i] %in% domesticated$species) {
    indices_df$domestication[i] <- "Domesticated"
  } else if (indices_df$genus_species[i] %in% partially_domesticated$species) {
  indices_df$domestication[i] <- "Partially-domesticated"
  }
}

indices_df$domestication[is.na(indices_df$domestication)] <- "Partially-domesticated"
indices_df[78, "domestication"] <- "Wild"

#save and load

write.csv(domestication, file = "intermediate_data/domestication.csv")
domestication <- read.csv(file = "intermediate_data/domestication.csv", header = T)[-c(1)]

write.csv(indices_df, file = "intermediate_data/domestication_h.csv")
indices_df <- read.csv(file = "intermediate_data/domestication_h.csv", header = T)[-c(1)]

combinedf <- read.csv(file = "outputs/combinedf.csv", header = T)[-c(1)]

#testing

test <- partially_domesticated[c(91:100),]

grepl('"', test$species[1]) #if this is true

str_replace(test$species, "c", "")

for (i in 1:length(test$species)) {
  if (grepl('"', test$species[i])) {
    test$species[i] <- str_replace(test$species[i], "c", "")
  }
}

