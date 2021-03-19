library(dplyr)
library(tidytree)
library(ape)
library(phytools)
#library(phylosignal)
#library(phylobase)
library(formattable)

#loading datasets

tree_node_dated <- read.nexus("trees/tree_pruner_node_dated/output.nex")
tree_tip_dated <- read.nexus("trees/tree_pruner_tip_dated/output.nex")
includeh <- read.csv(file = "outputs/includeh.csv")[-c(1)]

#check for matches

table(tree_node_dated$tree_7415$tip.label %in% tree_node_dated$tree_6783$tip.label) #they should all be the same
table(tree_node_dated$tree_8943$tip.label %in% tree_tip_dated$tree_3732$tip.label) #both files the same
table(tree_node_dated$tree_4616$tip.label %in% includeh$genus_species) #4881 true
table(includeh$genus_species %in% tree_node_dated$tree_1640$tip.label)

#create a matching list

includeh_tree <- includeh %>%
  filter(includeh$genus_species %in% tree_node_dated$tree_7415$tip.label)

tree_test <- tree_node_dated$tree_7415
#tree_test <- as_tibble(tree_test)

#tree_test <- tree_test %>%
#  filter(label %in% includeh$genus_species) %>%
#  arrange(by = label)

#table(tree_test$label %in% includeh_tree$genus_species)

#tree_test <- as.phylo(tree_test)

#check again

table(tree_test$tip.label %in% includeh_tree$genus_species)

table(includeh_tree$genus_species %in% tree_test$tip.label)

#prepare the data for the test

h <- as.array(includeh_tree$logh1) #h-index
row.names(h) <- includeh_tree$genus_species

body_mass <- as.array(includeh_tree$logmass) #mass
row.names(body_mass) <- includeh_tree$genus_species

lat <- as.array(includeh_tree$median_lat) #latitude
row.names(lat) <- includeh_tree$genus_species

iucn <- as.array(includeh_tree$iucn_bin) #iucn
row.names(iucn) <- includeh_tree$genus_species

human_use <- as.array(includeh_tree$humanuse_bin) #human use
row.names(human_use) <- includeh_tree$genus_species

domestication <- as.array(includeh_tree$domestication_bin) #domestication
row.names(domestication) <- includeh_tree$genus_species

gtrends <- as.array(includeh_tree$gtrends_bin) #gtrends
row.names(gtrends) <- includeh_tree$genus_species

#check for na

sum(is.na(body_mass))

#phylo signal

psignal_h <- phylosig(tree_test,
                      x = h,
                      test = TRUE) #k = 0.01398515

psignal_mass <- phylosig(tree_test,
                         x = body_mass,
                         test = TRUE) #k = 0.2739998

psignal_lat <- phylosig(tree_test,
                        x = lat,
                        test = TRUE) #k = 0.03703145

psignal_iucn <- phylosig(tree_test,
                         x = iucn,
                         test = TRUE) #k = 0.01600188

psignal_use <- phylosig(tree_test,
                        x = human_use,
                        test = TRUE) #k = 0.02266098

psignal_domestication <- phylosig(tree_test,
                                  x = domestication,
                                  test = TRUE) #k = 0.0109628

psignal_gtrends <- phylosig(tree_test,
                            x = gtrends,
                            test = TRUE) #k = 0.02473557


#save the data

saveRDS(psignal_h, file = "outputs/stats/psignal_h.R")
saveRDS(psignal_mass, file = "outputs/stats/psignal_mass.R")
saveRDS(psignal_lat, file = "outputs/stats/psignal_lat.R")
saveRDS(psignal_iucn, file = "outputs/stats/psignal_iucn.R")
saveRDS(psignal_use, file = "outputs/stats/psignal_use.R")
saveRDS(psignal_domestication, file = "outputs/stats/psignal_domestication.R")
saveRDS(psignal_gtrends, file = "outputs/stats/psignal_gtrends.R")

#make a table

psignal_df <- data.frame("variable" = c("log(h+1)", "log(mass)", "latitude", "IUCN status", "human use", "domestication", "gtrends"),
                         "Blomberg's k" = c(0.01398515, 0.2739998, 0.03703145, 0.01600188, 0.02266098, 0.0109628, 0.02473557))

formattable(psignal_df,
            align = c("l", "r"),
            list("Blomberg.s.k" = color_bar("#71CA97")))
