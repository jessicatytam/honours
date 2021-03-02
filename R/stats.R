library(tidyverse)
library(GGally)

#adding coloums

includeh <- includeh %>% 
  arrange(genus_species) %>%
  mutate(iucn_bin = NA,
         humanuse_bin = NA,
         domestication_bin = NA)

#1:5 for iucn
for (i in 1:length(includeh$redlistCategory)) {
  if (!is.na(includeh$redlistCategory[i]) & includeh$redlistCategory[i] == "Least Concern") {
    includeh$iucn_bin[i] <- 1
  } else if (!is.na(includeh$redlistCategory[i]) & includeh$redlistCategory[i] == "Vulnerable") {
    includeh$iucn_bin[i] <- 2
  } else if (!is.na(includeh$redlistCategory[i]) & includeh$redlistCategory[i] == "Endangered") {
    includeh$iucn_bin[i] <- 3
  } else if (!is.na(includeh$redlistCategory[i]) & includeh$redlistCategory[i] == "Critically Endangered") {
    includeh$iucn_bin[i] <- 4
  } else if (!is.na(includeh$redlistCategory[i]) & includeh$redlistCategory[i] == "Extinct in the Wild") {
    includeh$iucn_bin[i] <- 5
  }
}

#binary for human use
for (i in 1:length(includeh$use1)) {
  if (is.na(includeh$use1[i])) {
    includeh$humanuse_bin[i] <- 0
  } else {
    includeh$humanuse_bin[i] <- 1
  }
}

#1:3 for domestication
for (i in 1:length(includeh$domestication)) {
  if (includeh$domestication[i] == "Domesticated") {
    includeh$domestication_bin[i] <- 1
  } else if (includeh$domestication[i] == "Partially-domesticated") {
    includeh$domestication_bin[i] <- 2
  } else if (includeh$domestication[i] == "Wild") {
    includeh$domestication_bin[i] <- 3
  }
}

#count complete cases

complete_list <- data.frame() #3393 records
for (i in 1:length(includeh$genus_species)) {
  if (!is.na(includeh$BodyMass.Value[i]) & !is.na(includeh$median_lat[i]) & !is.na(includeh$redlistCategory[i])) {
    complete_list <- rbind(complete_list, includeh[i,])
  }
}

#correlation matrix of complete cases

ggpairs(complete_list, columns = c(39, 40, 20, 21))

#save and read

write.csv(includeh, file = "outputs/includeh.csv")
includeh <- read.csv(file = "outputs/includeh.csv")[-c(1)]


#testing


