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

redlist <- newlist %>%
  mutate(redlistCategory2 = NA) #add extra red list column
redlist <- redlist[c(1,2,4,3)] #rearrange columns

for (i in 1:count(redlist)-1){
  if(redlist[i,][1]==redlist[i+1,][1] & redlist[i,][4]==redlist[i+1,][4]){ #if species name and use1 are the same for 2 rows
    replace(redlist, redlist[i,][3], redlist[i+1,][3]) #replace NA with the 2nd redlist value
    redlist <- redlist[-c(i+1),] #remove the 2nd row
  }
}



#use columns





test <- matrix(c(1,1,2,2,3,3,NA,NA,NA,4,4,4),
               nrow = 3)
test
test[-1,]

for (i in 1:count(test)-1){
  n <- i+1
  if(test[i,][1]==test[n,][1] && test[i,][4]==test[n,][4]){
    replace(test, test[i,][3], test[n,][3])
  }
}

