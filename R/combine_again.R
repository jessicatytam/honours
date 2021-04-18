library(dplyr)
library(stringr)
library(rotl)
library(rredlist)
library(letsR)
library(geosphere)

#saving and loading
write.csv(combinedf2, file = "outputs/combinedf2.csv")
combinedf2 <- read.csv(file = "outputs/combinedf2.csv", header = T)[-c(1)]
includeh <- read.csv(file = "outputs/includeh.csv")[-c(1)]
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
combinedf2 <- combinedf2[!duplicated(combinedf2$genus_species),] #keeping the first record

#subset phylogeny = yes

combinedf2 <- subset(combinedf2, phylogeny == "Yes")

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
PAM <- letsR::lets.presab.points(cbind(sbs$decimalLongitude,sbs$decimalLatitude), sbs$species,
                          xmn = -180, xmx = 180, 
                          ymn = -90, ymx = 90,resol = 2)
##lets.midpoint.fixed() in file "R/geo_plotting"
mid <- geosphere::lets.midpoint.fixed(PAM)
mid$x <- as.numeric(mid$x)
mid$y<-as.numeric(mid$y)

for (i in 1:length(mid$Species)) {
  mid$genus_species[i] <- tnrs_match_names(mid$Species[i])$unique_name
  print(paste("Done:", mid$Species[i]))
}

combinedf2 <- left_join(combinedf2, mid, by = "genus_species")

combinedf2 <- combinedf2[-c(27)]
combinedf2 <- combinedf2[c(1:25, 27:28, 26)]

#fill in orders and families

for (i in 4631:length(combinedf2$id)) { #using ott_id
  if (is.na(combinedf2$order[i])) {
    print(i)
    tax_df <- reshape::melt(tax_lineage(taxonomy_taxon_info(ott_ids = combinedf2$id[i], include_lineage = TRUE)))
    tax_filter <- tax_df %>%
      filter(rank == "order")
    order <- tax_filter$unique_name
    combinedf2$order[i] <- as.character(order)
  }
} #188, 624, 1081:1084, 1086:1087, 2661, 3575, 3577:3578, 3581, 3588, 3590, 4232, 4325:4326, 4630

for (i in 5156:length(combinedf2$id)) { #using ott_id
  if (is.na(combinedf2$family[i])) {
    print(i)
    tax_df <- reshape::melt(tax_lineage(taxonomy_taxon_info(ott_ids = combinedf2$id[i], include_lineage = TRUE)))
    tax_filter <- tax_df %>%
      filter(rank == "family")
    family <- tax_filter$unique_name
    combinedf2$family[i] <- as.character(family)
  }
} #573, 860, 928:929, 3515, 4200, 5155

#fill in orders and families manually

for (i in 1:length(combinedf2$order)) { #looks like they are all Afrosoricida
  if (is.na(combinedf2$order[i])) {
    combinedf2$order[i] <- "Afrosoricida"
  }
}

#remove extinct spp; looks like the NA in families are extinct cetaceans

status <- data.frame(tnrs_match_names(names = combinedf2$genus_species)$flags)
status <- status %>%
  rename(status = tnrs_match_names.names...combinedf2.genus_species..flags)
combinedf2 <- cbind(combinedf2, status)
combinedf2 <- combinedf2[!(combinedf2$status == "extinct"),] #status from rotl
combinedf2 <- combinedf2[!grepl("extinct", combinedf2$status),]

#check orders

table(combinedf2$order) #check
for (i in 1:length(combinedf2$order)) {
  if (combinedf2$order[i] == "Cingulata (order in Deuterostomia)") {
    combinedf2$order[i] <- "Cingulata"
  } else if (combinedf2$order[i] == "Pilosa (order in Deuterostomia)") {
    combinedf2$order[i] <- "Pilosa"
  } else if (combinedf2$order[i] == "Proboscidea (order in Deuterostomia)") {
    combinedf2$order[i] <- "Proboscidea"
  }
}
table(combinedf2$order) #check

#add clade

for (i in 1:length(combinedf2$order)) {
  if (combinedf2$order[i] == "Pilosa"|
      combinedf2$order[i] == "Cingulata") {
    combinedf2$clade[i] <- "Xenarthra"
  } else if (combinedf2$order[i] == "Macroscelidea"|
             combinedf2$order[i] == "Afrosoricida"|
             combinedf2$order[i] == "Proboscidea"|
             combinedf2$order[i] == "Sirenia"|
             combinedf2$order[i] == "Hyracoidea") {
    combinedf2$clade[i] <- "Afrotheria"
  } else if (combinedf2$order[i] == "Chiroptera"|
             combinedf2$order[i] == "Perissodactyla"|
             combinedf2$order[i] == "Artiodactyla"|
             combinedf2$order[i] == "Cetacea"|
             combinedf2$order[i] == "Pholidota"|
             combinedf2$order[i] == "Carnivora"|
             combinedf2$order[i] == "Eulipotyphla"|
             combinedf2$order[i] == "Soricomorpha"|
             combinedf2$order[i] == "Erinaceomorpha") {
    combinedf2$clade[i] <- "Laurasiatheria"
  } else if (combinedf2$order[i] == "Primates"|
             combinedf2$order[i] == "Scandentia"|
             combinedf2$order[i] == "Lagomorpha"|
             combinedf2$order[i] == "Rodentia"|
             combinedf2$order[i] == "Dermoptera") {
    combinedf2$clade[i] <- "Euarchontoglires"
  } else if (combinedf2$order[i] == "Diprotodontia"|
             combinedf2$order[i] == "Dasyuromorphia"|
             combinedf2$order[i] == "Microbiotheria"|
             combinedf2$order[i] == "Peramelemorphia"|
             combinedf2$order[i] == "Notoryctemorphia"|
             combinedf2$order[i] == "Monotremata"|
             combinedf2$order[i] == "Paucituberculata"|
             combinedf2$order[i] == "Didelphimorphia") {
    combinedf2$clade[i] <- "Marsupials & monotremes"
  }
}

table(combinedf2$clade) #check
table(is.na(combinedf2$clade)) #check

combinedf2 <- combinedf2[!(combinedf2$genus_species == "Musserakis sulawesiensis"),] #remove the nematode

combinedf2$genus_species <- str_replace(combinedf2$genus_species, " ", "_")
combinedf2$genus_species[!combinedf2$genus_species %in% includeh$genus_species]

#looks like the entire list is already in the other one
#attach h-index & google trends

h_gtrends <- includeh[,c(1:13, 39:40, 46:53)]
combinedf2 <- left_join(combinedf2, h_gtrends)
