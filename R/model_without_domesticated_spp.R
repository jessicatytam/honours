library(tidyverse)
library(ggExtra)
library(taxize)
library(rotl)
library(ape)
library(rredlist)
library(rvest)

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(letsR)
library(geoshpere)
library(wesanderson)
library(ggtree)
library(ggtreeExtra)
library(treeio)
library(scales)
library(ggbeeswarm)
library(ggridges)
library(ggnewscale)
library(gghalves)
library(ggpol)
library(plotly)
library(sysfonts)
library(ggstream)
library(reshape2)
library(ggrepel)
library(ggpubr)

#load data
includeh <- read.csv(file = "outputs/data/includeh.csv")[-c(1)]

includeh[includeh$domestication_bin==1,] #domesticated spp
sum(includeh$domestication_bin==1) #12
View(includeh[includeh$domestication_bin==2,]) #partially-domesticated spp)
sum(includeh$domestication_bin==2) #136
View(includeh[includeh$humanuse_bin==1,])

includeh <- includeh %>% 
  filter(domestication_bin==3)

#domestication 
##get the data

wiki_url <- read_html("https://en.wikipedia.org/wiki/List_of_domesticated_animals")

domesticated <- wiki_url %>% 
  html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>%
  html_table()

partially_domesticated <- wiki_url %>% 
  html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>%
  html_table()

##remove non-mammals

domesticated <- domesticated %>%
  filter(str_extract(domesticated$`Taxon group`, "^1") == 1)

partially_domesticated <- partially_domesticated %>%
  filter(str_extract(partially_domesticated$`Taxon group`, "^1") == 1)

##get binomial names 

domesticated <- domesticated %>% #extract names using the brackets
  mutate(species = str_extract(domesticated$`Species and subspecies`, "(?<=\\().+(?=\\))")) 

partially_domesticated <- partially_domesticated %>% #extract names using the brackets
  mutate(species = str_extract_all(partially_domesticated$`Species and subspecies`, "\\(([^\\)]+)")) 

##cleaning binomial names

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
domesticated <- domesticated %>% #separate the names using ","
  separate(species, c("species1", "species2", "species3"), sep = "[,]+")
domesticated <- domesticated %>% #separate the names using ","
  separate(species3, c("species3", "species4"), sep = " and ")

##fill in genus names

domesticated[,"species2"]==" A. algirus" #find row with "C. l. lupus"
domesticated[26,"species2"] <- "Atelerix algirus" #change to full name
domesticated[,"species4"]=="H. collaris" #find row with "C. l. lupus"
domesticated[26,"species4"] <- "Hemiechinus algirus" #change to full name
domesticated[26,"species2"] #check
domesticated[26,"species4"] #check

partially_domesticated[,"species1"] #find row with "C. l. lupus"
partially_domesticated[92,"species1"] <- "Canis lupus lupus" #change to full name
partially_domesticated[92,"species1"] #check

for (j in 11:ncol(partially_domesticated)) {
  for (i in 1:nrow(partially_domesticated)) {
    if (!is.na(partially_domesticated[i,j]) & (lengths(str_split(partially_domesticated[i,j], " ")) == 3)) {
      partially_domesticated[i,j] <- str_replace(partially_domesticated[i,j], ".+(?=[:space:])", word(partially_domesticated$species1[i], 1))
    } else if (!is.na(partially_domesticated[i,j]) & (lengths(str_split(partially_domesticated[i,j], " ")) == 4)) {
      partially_domesticated[i,j] <- str_replace(partially_domesticated[i,j], ".+(?=[:space:])", word(partially_domesticated$species1[i], 1, 2))
    } 
  }
}

# partially_domesticated[46, 13] <- "Osphranter rufus"

##check for and fix spelling mistakes

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

##pivot longer

partially_domesticated <- partially_domesticated %>%
  rename("spp and subspp" = "Species and subspecies")

partially_domesticated <- partially_domesticated %>%
  pivot_longer(cols = starts_with("species"),
               values_to = "species",
               values_drop_na = TRUE)

partially_domesticated <- partially_domesticated %>%
  select(!name)

domesticated <- domesticated %>%
  rename("spp and subspp" = "Species and subspecies")

domesticated <- domesticated %>%
  pivot_longer(cols = starts_with("species"),
               values_to = "species",
               values_drop_na = TRUE)

domesticated <- domesticated %>%
  select(!name)

##combine

partially_domesticated <- partially_domesticated %>%
  rename(Purposes = Purpose)

domestication <- rbind(domesticated, partially_domesticated)

##remove unwanted data

domestication <- domestication[c(10, 5, 7:8)] 
domestication <- domestication %>%
  arrange(species)

##get id for domestication

domestication <- domestication %>%
  mutate(id = tnrs_match_names(names = domestication$species)$ott_id, .after = species)

#check if spp are in the domestication list

includeh_dom <- data.frame()
for (i in 1:length(includeh$genus_species)) {
  if (length(grep(includeh$genus_species[i], domestication$species))!=0) {
    includeh_dom <- rbind(includeh_dom, includeh$genus_species[i])
    print(paste(i, includeh$genus_species[i]))
  }
}

includeh_wo_dom <- includeh[!includeh$genus_species %in% includeh_dom$X.Acinonyx.jubatus.,]
includeh_wo_dom$genus_species <- str_replace(includeh_wo_dom$genus_species, " ", "_")

# new imputation list

imp_list <- readRDS("data/intermediate_data/MCMCglmm/imp_list.rds")

table(imp_list[[1]]$animal %in% includeh_wo_dom$genus_species) #5343 
table(!imp_list[[1]]$animal %in% includeh_wo_dom$genus_species) #155

imp_list_new <- imp_list
for (j in 1:length(imp_list)) {
  for (i in 1:length(imp_list[[j]]$animal)) {
    if (!imp_list[[j]]$animal[i] %in% includeh_wo_dom$genus_species) {
      imp_list_new[[j]] <- imp_list_new[[j]][-i,]
    }
  }
}

saveRDS(imp_list_new, "data/intermediate_data/MCMCglmm/imp_list_new.rds")

#new tree list

random_trees <- readRDS("data/intermediate_data/random_trees.rds")
length(random_trees$tree_9522$tip.label) #5498

random_trees_new <- random_trees
for (j in 1:length(random_trees_new)) { 
  for (i in 1:length(random_trees_new[[j]]$tip.label)) {
    if (random_trees_new[[j]]$tip.label[i] %in% imp_list_new[[1]]$animal) {
      random_trees_new[[j]] <- keep.tip(random_trees_new[[j]], imp_list_new[[1]]$animal)
      print(paste("tree", j, "trimmed"))
    }
  } 
}

saveRDS(random_trees_new, "data/intermediate_data/random_trees_new.rds")

