library(ape)
library(rotl)
library(tidyverse)

tree100 <- read.nexus("data/raw_data/trees/tree_pruner_tip_dated/output.nex")
includeh <- read.csv("outputs/data/includeh.csv", header = T)[-1]

#get species names

names <- tree100$tree_6061$tip.label 
names <- str_replace_all(names, "_", " ")
names <- as.data.frame(names)

#get synonyms

names$synonyms <- NA #create a new variable
for (i in 1:length(names$names)) {
  names$synonyms[i] <- tnrs_match_names(names$names[i])$unique_name
  print(paste("Done:", names$names[i]))
}

#checking

names$synonyms <- str_replace_all(names$synonyms, " ", "_")
table(names$synonyms %in% includeh$genus_species) #5,111 matches

#replace old list of names with new list

for (j in 1:length(tree100)) {
  for (i in 1:length(tree100[[j]]$tip.label)) {
    if (!tree100[[j]]$tip.label[i] %in% names$synonyms[i]) {
      tree100[[j]]$tip.label[i] <- names$synonyms[i]
      print(paste("Done: tree", j, "species", i))
    }
  }
}

#checking

table(tree100$tree_6061$tip.label %in% includeh$genus_species) #1 tree; 5,111 matches

for (i in 1:length(tree100)) {
  print(table(tree100[[i]]$tip.label %in% includeh$genus_species)) #all trees; 5,111 matches
}

#save new tree

saveRDS(tree100, "data/intermediate_data/trees/tree100.rds")
tree100 <- readRDS("data/intermediate_data/trees/tree100.rds") #to read the tree
