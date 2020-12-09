library(reshape2)
library(dplyr)

#import the data

IUCNlist <- list.files(path = "IUCN_use_and_redlistcategory", pattern = ".csv", full.names = TRUE) #show names of all the files in the folder

uselist <- list() #need to create list first so R will know where to store the data
for(i in 1:length(IUCNlist)){
  uselist[[i]] <- read.csv(file = paste0(IUCNlist[i]), header = T)[c(3,4)] #load all of the csv files
}

str(uselist[[1]]) #view the items in the list

meltedList <- melt(uselist) #converts list to df with list element number

subset(meltedList, L1 == 1) #view each group

#remove duplicates

length(unique(meltedList$scientificName)) #number of unique species

newlist <- meltedList %>%
  rename(use1 = L1) %>%
  arrange(scientificName) %>%
  unique() #remove duplicates
 
#red list category columns

