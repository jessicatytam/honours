library(dplyr)
library(tidytree)
library(ape)
library(phytools)
library(phylosignal)
library(phylobase)

#loading datasets

tree_node_dated <- read.nexus("trees/tree_pruner_node_dated/output.nex")
tree_tip_dated <- read.nexus("trees/tree_pruner_tip_dated/output.nex")
includeh <- read.csv(file = "outputs/includeh.csv")[-c(1)]

#match trees

table(tree_node_dated$tree_7415$tip.label %in% tree_node_dated$tree_6783$tip.label) #they should all be the same
table(tree_node_dated$tree_8943$tip.label %in% tree_tip_dated$tree_3732$tip.label) #both files the same
table(tree_node_dated$tree_4616$tip.label %in% includeh$genus_species) #4881 true
table(includeh$genus_species %in% tree_node_dated$tree_1640$tip.label)

#create a matching list

includeh_tree <- includeh %>%
  filter(includeh$genus_species %in% tree_node_dated$tree_7415$tip.label)

tree_test <- tree_node_dated$tree_7415
tree_test <- as_tibble(tree_test)

for (i in 1:length(tree_test$label)) { #run this until FALSE = 0
  if (!tree_test$label[i] %in% includeh_tree$genus_species) {
    tree_test <- tree_test[-i,]
  }
}
table(tree_test$label %in% includeh_tree$genus_species)

tree_test <- as.phylo(tree_test)

#check again

table(tree_test$tip.label %in% includeh_tree$genus_species)

#phylo signal

psignal <- phylosig(tree_test,
                    x = includeh_tree$h) #this keeps crashing

p4d <- phylo4d(tree_test, includeh_tree$h, includeh_tree$BodyMass.Value)








