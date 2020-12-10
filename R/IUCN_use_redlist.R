library(reshape2)
library(dplyr)
library(tidyr)

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
  rename(use = L1) %>%
  arrange(scientificName) %>%
  unique() #remove duplicates
 
#red list category columns

redlist <- newlist %>%
  mutate(redlistCategory2 = NA) #add extra red list column
redlist <- redlist[c(1,2,4,3)] #rearrange columns

for(i in 1:2466){
  for (j in 2:2465) {
    if(redlist[i,][1] %in% redlist[j,][1] & redlist[i,][4] %in% redlist[j,][4]){ #if species name & use are the same for 2 rows
      replace(redlist, redlist$redlistCategory2[i], redlist$redlistCategory[j]) #replace NA of the 1st row with the 2nd row's red list value
      redlist <- redlist[-j,] #remove the 2nd row
    }
  }
}


class(redlist$redlistCategory)



test <- matrix(c(1,1,2,2,3,3,NA,NA,NA,4,4,4),
               nrow = 3)
test
nrow(test)
test[-1,]

#for loop
for(i in 1:(nrow(test)-1)){
  for(j in 2:(nrow(test)-2)){
    if(identical(test[i,][1], test[j,][1]) & identical(test[i,][4], test[j,][4])){
    test[test[i,][3] == "NA"] <- test[j][2] #replacing not working
    test <- test[-j,] #removing works
    }
  }
}

#while loop
i <- 1
while(i < nrow(test)){
  if(test[i,][1] %in% test[i+1,][1] & test[i,][4] %in% test[i+1,][4]){
    replace(test, test[i,][3], test[i+1,][2])
    test <- test[-(i+1),]
  }
  i <- i+1
}

