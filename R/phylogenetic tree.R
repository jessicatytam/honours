library(rotl)
library(ape)
library(dplyr)
library(rlist)

taxa <- tnrs_match_names("Mammalia") #find iTOL record for Mammalia
res <- tol_subtree(ott_id = taxa$ott_id, label_format = "name") #extract subtree of mammals
str(res)

#9351 tip labels correspond to species, e.g.:
res$tip.label[1000:2000]
res$tip.label <- gsub("_"," ", res$tip.label) #get rid of the underscores
res$tip.label[1000:2000]

#Note that some further cleaning will required, esp. for lineages and subspecies:

hist(lengths(gregexpr("\\W+", res$tip.label)) + 1, xlab = "number of words in species names", main="mammalian tree tip labels") #histogram of how many words are in the species names from the tree
table((lengths(gregexpr("\\W+", res$tip.label)) + 1)) #table for the above

#We have 6952 "clean binomial" names and approx. 2000 non-binomial ones, many of the latter could potentially be collapsed to a higher taxonomic level?
#we will only use the "clean binomials"

#Plotting the tree (its too large, takes a while!):
#plot(res, labels=FALSE)

#Once we have a cleaned list of species on the tree, we can clean the tree and also use tnrs_match_names() to get their ott_id and then get other id types (NCBI, worms, gbif, irmng) using taxon_external_IDs() function.
 
res$tip.label

specieslist <- unlist(res$tip.label)
specieslist <- data.frame(specieslist)
table((lengths(gregexpr("\\W+", specieslist$specieslist)) + 1)) 

newspecieslist <- specieslist %>%
  mutate(numberofwords = lengths(gregexpr("\\W+", specieslist))+1)

finalspecieslist <- newspecieslist %>%
  subset(numberofwords == 2) %>%
  select(specieslist) %>%
  write_csv("intermediate_data/species_list_from_phylo.csv")
