library(tidyverse)
library(rotl)
library(rredlist)

#saving and loading
write.csv(combinedf2, file = "outputs/combinedf2.csv")
combinedf <- read.csv(file = "outputs/combinedf.csv", header = T)[-c(1)]
domestication_h <- read.csv(file = "intermediate_data/domestication_h.csv", header = T)

#datasets
mass <- read.csv(file = "intermediate_data/EltonTraits.csv", header = T)
phylogeny <- read.csv(file = "intermediate_data/species_list_from_phylo.csv", header = T)
location <- read.csv(file = "intermediate_data/gbif_lat_medians.csv", header = T)
humanuse <- read.csv(file = "intermediate_data/IUCN_use.csv", header = T)
redlist <- read.csv(file = "intermediate_data/IUCN_redlist.csv", header = T)
domestication <- read.csv(file = "intermediate_data/domestication_h.csv", header = T)

#synonym matching

##mass

for (i in 1:length(mass$Scientific)) {
  mass$species[i] <- tnrs_match_names(mass$Scientific[i])$unique_name
  print(paste("Done:", mass$Scientific[i]))
} 

for (i in 1:length(mass$species)) {
  if (is.na(mass$species[i])) {
    mass$species[i] <- mass$Scientific[i]
  }
}

##location

location$genus_species <- location$species
  
for (i in 1:length(location$genus_species)) {
  location$species[i] <- tnrs_match_names(location$genus_species[i])$unique_name
  print(paste("Done:", location$genus_species[i]))
} 

for (i in 1:length(location$species)) {
  if (is.na(location$species[i])) {
    location$species[i] <- location$genus_species[i]
  }
}

##phylogeny

phylogeny <- phylogeny %>%
  mutate(phylogeny = "Yes")

##human use

for (i in 1:length(humanuse$usescientificName)) {
  humanuse$species[i] <- tnrs_match_names(humanuse$usescientificName[i])$unique_name
  print(paste("Done:", humanuse$usescientificName[i]))
}

for (i in 1:length(humanuse$species)) {
  if (is.na(humanuse$species[i])) {
    humanuse$species[i] <- humanuse$usescientificName[i]
  }
}

##iucn red list

redlist <- redlist[-c(1)] %>%
  unique() 

redlist <- redlist %>%
  mutate(sppcount = sequence(rle(redlist$scientificName)$lengths)) #new column to count the occurrences of each species

redlist <- pivot_wider(redlist, names_from = sppcount, values_from = redlistCategory)
colnames(redlist) <- paste0("redlistCategory", colnames(redlist))

for (i in 1:length(redlist$redlistCategoryscientificName)) {
  redlist$species[i] <- tnrs_match_names(redlist$redlistCategoryscientificName[i])$unique_name
  print(paste("Done:", redlist$redlistCategoryscientificName[i]))
}

for (i in 1:length(redlist$species)) {
  if (is.na(redlist$species[i])) {
    redlist$species[i] <- redlist$redlistCategoryscientificName[i]
  }
}

##domestication

domestication <- domestication[c("genus_species", "domestication")]
domestication <- domestication %>%
  rename(species = "genus_species")


#combine

combinedf2 <- list(mass, phylogeny, location, redlist, humanuse, domestication) %>% 
  reduce(full_join, by = "species") %>%
  arrange(species) %>% #7955
  unique() 

#remove unwanted variables

combinedf2 <- combinedf2[-c(1, 2, 7, 12, 13, 17, 18)]

#renaming

combinedf2 <- combinedf2 %>%
  rename(genus_species = species,
         family = MSWFamilyLatin)

#reordering

combinedf2 <- combinedf2[c(4, 5, 1, 8, 2:3, 6:7, 9:28)]

#remove duplicates

combinedf2$genus_species[duplicated(combinedf2$genus_species)]
combinedf2 <- unique(combinedf2)

#manually remove duplicates



#fill in ids

for (i in 1:length(combinedf2$genus_species)) {
  if (is.na(combinedf2$id[i])) {
    print(i)
    combinedf2$id[i] <- tnrs_match_names(combinedf2$genus_species[i])$ott_id
  }
}

#clean red list status

for (i in 1:nrow(combinedf2)) {
  if (!is.na(combinedf2$redlistCategory1[i]) & combinedf2$redlistCategory1[i] == "Not Applicable") {
    combinedf2$redlistCategory1[i] <- combinedf2$redlistCategory2[i]
    combinedf2$redlistCategory2[i] <- NA
  }
}

for (i in 1:nrow(combinedf2)) {
  if (is.na(combinedf2$redlistCategory1[i]) & !is.na(combinedf2$redlistCategory2[i])) {
    combinedf2$redlistCategory1[i] <- combinedf2$redlistCategory2[i]
    combinedf2$redlistCategory2[i] <- NA
  }
}

for (i in 1:nrow(combinedf2)) {
  if (is.na(combinedf2$redlistCategory2[i]) & !is.na(combinedf2$redlistCategory3[i])) {
    combinedf2$redlistCategory2[i] <- combinedf2$redlistCategory3[i]
    combinedf2$redlistCategory3[i] <- NA
  }
}

combinedf2$redlistCategory2 <- na_if(combinedf2$redlistCategory2, "Not Applicable")

for (i in 1:nrow(combinedf2)) {
  if(!is.na(combinedf2$redlistCategory3[i])) {
    combinedf2$redlistCategory2[i] <- combinedf2$redlistCategory3[i]
    combinedf2$redlistCategory3[i] <- NA
  }
}

for (i in 1:nrow(combinedf2)) {
  if(!is.na(combinedf2$redlistCategory2[i])) {
    combinedf2$redlistCategory1[i] <- combinedf2$redlistCategory2[i]
    combinedf2$redlistCategory2[i] <- NA
  }
}

combinedf2 <- combinedf2[-c(10, 11)] %>%
  rename(redlistCategory = redlistCategory1)

for (i in 1:length(combinedf2$genus_species)) {
  if (is.na(combinedf2$redlistCategory[i])) {
    print(paste(i, combinedf2$genus_species[i], "missing IUCN status."))
    iucn_query <- rl_history(name = combinedf2$genus_species[i], key = "4eacf586ea255313b1646429c0f5b566cfa6f789cfb634f9704a8050a6123933")
    iucn_df <- data.frame(iucn_query$result)
    combinedf2$redlistCategory <- as.character(combinedf2$redlistCategory)
    if (nrow(iucn_df > 0)) {
      print(paste("filling in missing data."))
      combinedf2$redlistCategory[i] <- iucn_df[1,"category"]
    }
  }
}

#fill in domestication

for (i in 1:length(combinedf2$genus_species)) {
  if (is.na(combinedf2$domestication[i])) {
    combinedf2$domestication[i] <- "Wild"
  }
}

#add lat long

sbs <- read.csv(file = "intermediate_data/gbif_processed.csv", header = T)
PAM <- lets.presab.points(cbind(sbs$decimalLongitude,sbs$decimalLatitude), sbs$species,
                          xmn = -180, xmx = 180, 
                          ymn = -90, ymx = 90,resol = 2)
##lets.midpoint.fixed() in file "R/geo_plotting"
mid <- lets.midpoint.fixed(PAM)
mid$x <- as.numeric(mid$x)
mid$y<-as.numeric(mid$y)
for (i in 1:length(mid$Species)) {
  mid$genus_species[i] <- tnrs_match_names(mid$Species[i])$unique_name
  print(paste("Done:", mid$Species[i]))
}
combinedf2 <- list(combinedf2, mid) %>%
  reduce(left_join, by = "genus_species")






#fill in orders and families

for (i in 1382:length(combinedf2$id)) { #will do this later when there is more time
  if (is.na(combinedf2$order[i])) {
    print(i)
    tax_df <- reshape::melt(tax_lineage(taxonomy_taxon_info(ott_ids = combinedf2$id[i], include_lineage = TRUE)))
    tax_filter <- tax_df %>%
      filter(rank == "order")
    order <- tax_filter$unique_name
    combinedf2$order[i] <- as.character(order)
  }
} #209, 230, 231, 232, 419, 421, 507, 581, 721, 722, 1107, 1223, 1251:1254, 1256:1257, 1278, 1288, 1331, 1381, 1425

for (i in 1:length(combinedf2$id)) { #will do this later when there is more time
  if (is.na(combinedf2$family[i])) {
    print(i)
    tax_df <- reshape::melt(tax_lineage(taxonomy_taxon_info(ott_ids = combinedf2$id[i], include_lineage = TRUE)))
    tax_filter <- tax_df %>%
      filter(rank == "family")
    family <- tax_filter$unique_name
    icombinedf2$family[i] <- as.character(family)
  }
}

#add clade

for (i in 1:length(includeh$order)) {
  if (includeh$order[i] == "Pilosa"|
      includeh$order[i] == "Cingulata") {
    includeh$clade[i] <- "Xenarthra"
  } else if (includeh$order[i] == "Macroscelidea"|
             includeh$order[i] == "Afrosoricida"|
             includeh$order[i] == "Proboscidea"|
             includeh$order[i] == "Sirenia"|
             includeh$order[i] == "Hyracoidea") {
    includeh$clade[i] <- "Afrotheria"
  } else if (includeh$order[i] == "Chiroptera"|
             includeh$order[i] == "Perissodactyla"|
             includeh$order[i] == "Artiodactyla"|
             includeh$order[i] == "Cetacea"|
             includeh$order[i] == "Pholidota"|
             includeh$order[i] == "Carnivora"|
             includeh$order[i] == "Eulipotyphla"|
             includeh$order[i] == "Soricomorpha"|
             includeh$order[i] == "Erinaceomorpha") {
    includeh$clade[i] <- "Laurasiatheria"
  } else if (includeh$order[i] == "Primates"|
             includeh$order[i] == "Scandentia"|
             includeh$order[i] == "Lagomorpha"|
             includeh$order[i] == "Rodentia"|
             includeh$order[i] == "Dermoptera") {
    includeh$clade[i] <- "Euarchontoglires"
  } else if (includeh$order[i] == "Diprotodontia"|
             includeh$order[i] == "Dasyuromorphia"|
             includeh$order[i] == "Microbiotheria"|
             includeh$order[i] == "Peramelemorphia"|
             includeh$order[i] == "Notoryctemorphia"|
             includeh$order[i] == "Monotremata"|
             includeh$order[i] == "Paucituberculata"|
             includeh$order[i] == "Didelphimorphia") {
    includeh$clade[i] <- "Marsupials & monotremes"
  }
}

#remove extinct spp

status <- data.frame(tnrs_match_names(names = combinedf2$genus_species)$flags)
status <- status %>%
  rename(status = tnrs_match_names.names...combinedf2.genus_species..flags)
combinedf2 <- cbind(combinedf2, status, .after = use16)

combinedf2 <- combinedf2[!(combinedf2$genus_species == "Mylodon darwinii"),] #ground sloth
combinedf2 <- combinedf2[!(combinedf2$genus_species == "Orrorin tugenensis"),] #early human
combinedf2 <- combinedf2[!(combinedf2$genus_species == "Brandtocetus chongulek"),] #miocene whale

combinedf2 <- combinedf2[!(combinedf2$status == "extinct"),] #status from rotl
combinedf2 <- combinedf2[!(combinedf2$order == "Ascaridida"),] #worm
combinedf2 <- combinedf2[!(combinedf2$genus == "Homo"),] #humans
combinedf2 <- combinedf2[!(combinedf2$genus == "Mammuthus"),] #woolly mammoth





#h-index

#google trends



#testing

tax_df <- reshape::melt(tax_lineage(taxonomy_taxon_info(ott_ids = combinedf2$id[1257], include_lineage = TRUE)))
filter(rank == "order")
order <- tax_filter$unique_name
combinedf2$order[i] <- as.character(order)