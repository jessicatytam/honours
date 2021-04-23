library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(taxize)
library(rinat)
library(rotl)
library(geosphere)

#REMEMBER TO SAVE

write.csv(combinedf, file = "outputs/data/combinedf.csv")
write.csv(combinedf_dup, file = "outputs/data/combinedf_dup.csv")
combinedf <- read.csv(file = "outputs/data/combinedf.csv", header = T)[-c(1)]

#load datasets

mass <- read.csv(file = "data/intermediate_data/EltonTraits.csv", header = T)
phylogeny <- read.csv(file = "data/intermediate_data/species_list_from_phylo.csv", header = T)
location <- read.csv(file = "data/intermediate_data/gbif_lat_medians.csv", header = T)
humanuse <- read.csv(file = "data/intermediate_data/IUCN_use.csv", header = T)
redlist <- read.csv(file = "data/intermediate_data/IUCN_redlist.csv", header = T)
domestication <- read.csv(file = "data/intermediate_data/domestication_h.csv", header = T)

#renaming and cleaning for species matching

mass <- mass[-c(1)] %>%
  rename(species = Scientific,
         family = MSWFamilyLatin)

phylogeny <- phylogeny[-c(1)] %>%
  mutate(phylogeny = "Yes")

humanuse <- humanuse[-c(1)] %>%
  rename(species = usescientificName)

redlist <- redlist[-c(1)] %>%
  unique() #6093

redlist <- redlist %>%
  mutate(sppcount = sequence(rle(redlist$scientificName)$lengths)) #new column to count the occurrences of each species

redlist <- pivot_wider(redlist, names_from = sppcount, values_from = redlistCategory)
colnames(redlist) <- paste0("redlistCategory", colnames(redlist))
redlist <- redlist %>%
  rename(species = redlistCategoryscientificName)

domestication <- domestication[c("genus_species", "domestication")]
domestication <- domestication %>%
  rename(species = "genus_species")

#combining the datasets

combinedf <- list(mass, phylogeny, location, redlist, humanuse, domestication) %>% 
  reduce(full_join, by = "species") %>%
  arrange(species) %>% #8308
  unique() 

combinedf <- combinedf[c(1:2,8,5,3:4,6:7,9:28)] #reordering

length(unique(combinedf$species)) #8308
count(unique(combinedf)) #8308

#remove extinct spp

status <- data.frame(tnrs_match_names(names = combinedf$species)$flags)
status <- status %>%
  rename(status = tnrs_match_names.names...combinedf.species..flags)
combinedf <- cbind(combinedf, status)
combinedf <- combinedf[!(combinedf$status == "extinct"),] #status from rotl
combinedf <- combinedf[!grepl("extinct", combinedf$status),] #8081

#add id

for (i in 1:length(combinedf$species)) {
  tryCatch (if (is.na(combinedf$id[i])) {
    print(i)
    combinedf$id[i] <- tnrs_match_names(combinedf$species[i])$ott_id
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
} 

table(is.na(combinedf$species)) #7981 false, 100 true

#add families and orders

for (i in 1:length(combinedf$id)) { #using ott_id
  tryCatch (if (is.na(combinedf$order[i])) {
    print(i)
    tax_df <- reshape::melt(tax_lineage(taxonomy_taxon_info(ott_ids = combinedf$id[i], include_lineage = TRUE)))
    tax_filter <- tax_df %>%
      filter(rank == "order")
    order <- tax_filter$unique_name
    combinedf$order[i] <- as.character(order)
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
} 

for (i in 1:length(combinedf$id)) { #using ott_id
  tryCatch (if (is.na(combinedf$family[i])) {
    print(i)
    tax_df <- reshape::melt(tax_lineage(taxonomy_taxon_info(ott_ids = combinedf$id[i], include_lineage = TRUE)))
    tax_filter <- tax_df %>%
      filter(rank == "family")
    family <- tax_filter$unique_name
    combinedf$family[i] <- as.character(family)
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

#checking the number of NA values

table(is.na(combinedf$order)) #7955 false, 126 true
table(is.na(combinedf$family)) #7981 false, 100 true

#replacing NA in phylogeny

#combinedf <- combinedf %>%
#  replace_na(list(phylogeny = "No"))

#remove species with 1 word in the string

sum(lengths(gregexpr("\\w+", combinedf$species))==1) #100 

for (i in 1:nrow(combinedf)) { #do this twice
  if (lengths(gregexpr("\\w+", combinedf$species[i]))==1) {
    combinedf <- combinedf[-i,]
  }
} #7981 species

#more checking

table(is.na(combinedf$id)) #all here
table(is.na(combinedf$order)) #26 missing
table(is.na(combinedf$family)) #all here 

#fill in missing orders manually

for (i in 1:length(combinedf$order)) { 
  if (combinedf$family[i] == "Chrysochloridae" | combinedf$family[i] == "Tenrecidae") {
    combinedf$order[i] <- "Afrosoricida"
  }
}
table(is.na(combinedf$order)) #all here

#remove weird worm

combinedf <- combinedf[!(combinedf$species == "Musserakis sulawesiensis"),] #7980

#clean red list status

for (i in 1:nrow(combinedf)) {
  if (!is.na(combinedf$redlistCategory1[i]) & combinedf$redlistCategory1[i] == "Not Applicable") {
    combinedf$redlistCategory1[i] <- combinedf$redlistCategory2[i]
    combinedf$redlistCategory2[i] <- NA
  }
}

for (i in 1:nrow(combinedf)) {
  if (is.na(combinedf$redlistCategory1[i]) & !is.na(combinedf$redlistCategory2[i])) {
    combinedf$redlistCategory1[i] <- combinedf$redlistCategory2[i]
    combinedf$redlistCategory2[i] <- NA
  }
}

for (i in 1:nrow(combinedf)) {
  if (is.na(combinedf$redlistCategory2[i]) & !is.na(combinedf$redlistCategory3[i])) {
    combinedf$redlistCategory2[i] <- combinedf$redlistCategory3[i]
    combinedf$redlistCategory3[i] <- NA
  }
}

combinedf$redlistCategory2 <- na_if(combinedf$redlistCategory2, "Not Applicable")

for (i in 1:nrow(combinedf)) {
  if(!is.na(combinedf$redlistCategory3[i])) {
    combinedf$redlistCategory2[i] <- combinedf$redlistCategory3[i]
    combinedf$redlistCategory3[i] <- NA
  }
}

for (i in 1:nrow(combinedf)) {
  if(!is.na(combinedf$redlistCategory2[i])) {
    combinedf$redlistCategory1[i] <- combinedf$redlistCategory2[i]
    combinedf$redlistCategory2[i] <- NA
  }
}

combinedf <- combinedf[-c(10, 11)] %>%
  rename(redlistCategory = redlistCategory1)

#fill in red list status

for (i in 1:length(combinedf$species)) {
  if (is.na(combinedf$redlistCategory[i])) {
    print(paste(i, combinedf$species[i], "missing IUCN status."))
    iucn_query <- rl_history(name = combinedf$species[i], key = "4eacf586ea255313b1646429c0f5b566cfa6f789cfb634f9704a8050a6123933")
    iucn_df <- data.frame(iucn_query$result)
    combinedf$redlistCategory <- as.character(combinedf$redlistCategory)
    if (nrow(iucn_df > 0)) {
      print(paste("filling in missing data."))
      combinedf$redlistCategory[i] <- iucn_df[1,"category"]
    }
  }
}

table(is.na(combinedf$redlistCategory)) #5825 matches

#fill in domestication

for (i in 1:length(combinedf$species)) {
  if (is.na(combinedf$domestication[i])) {
    combinedf$domestication[i] <- "Wild"
  }
}

table(combinedf$domestication)

#add lat long

sbs <- read.csv(file = "data/intermediate_data/gbif_processed.csv", header = T)
PAM <- letsR::lets.presab.points(cbind(sbs$decimalLongitude, sbs$decimalLatitude), sbs$species,
                                 xmn = -180, xmx = 180, 
                                 ymn = -90, ymx = 90,resol = 2)
##lets.midpoint.fixed() in file "R/geo_plotting"
mid <- lets.midpoint.fixed(PAM)
mid$x <- as.numeric(mid$x)
mid$y <- as.numeric(mid$y)

names(mid)[names(mid) == "Species"] <- "species"

combinedf <- left_join(combinedf, mid, by = "species")

combinedf <- combinedf[-c(27)]
combinedf <- combinedf[c(1:25, 27:28, 26)]


#synonym matching

length(combinedf$id) #7980
length(unique(combinedf$id)) #7522
dim(combinedf[duplicated(combinedf$id),])[1] #458
unique(combinedf$id) #7522

combinedf_dup <- combinedf[!duplicated(combinedf$id),] #remove duplicated id

#update phylogeny list; DONE

#phylo_list <- combinedf %>%
#  filter(phylogeny=="Yes") %>%
#  select(species) %>%
#  write.csv("intermediate_data/species_list_from_phylo.csv")

#some testing

test <- combinedf[11:20,]

test <- test %>%
  group_by(ID) %>%
  fill(c(3:27), .direction = "downup") %>%
  ungroup()

