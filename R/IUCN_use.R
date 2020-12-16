library(reshape2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

#import the data

IUCNlist <- list.files(path = "IUCN_use", pattern = ".csv", full.names = TRUE) #show names of all the files in the folder

uselist <- list() #need to create list first so R will know where to store the data
for(i in 1:length(IUCNlist)){
  uselist[[i]] <- read.csv(file = paste0(IUCNlist[i]), header = T)[c(3,4)] #load all of the csv files
}

str(uselist[[1]]) #view the items in the list

meltedList <- melt(uselist) #converts list to df with list element number

subset(meltedList, L1 == 1) #view each group

#remove duplicates

length(unique(meltedList$scientificName)) #number of unique species; 1472

newlist <- meltedList[c(1,3)] %>%
  rename(use = L1) %>%
  arrange(scientificName) %>%
  unique() #remove duplicates; 2390

#trying pivot_wider

newlistaddcol <- newlist %>%
  mutate(sppcount = sequence(rle(newlist$scientificName)$lengths))

newlistnum <- newlistaddcol %>% #1472
  pivot_wider(names_from = sppcount,
              values_from = use)

#replace numbers with uses

Uses <- list.files(path = "IUCN_use", full.names = FALSE)
Uses <- data.frame(gsub("\\..*","", Uses)) #remove ".csv"

str_replace_all(newlistnum, "\\d+", Uses)

