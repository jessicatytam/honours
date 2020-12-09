library(dplyr)

#load data

IUCNlist <- list.files(path = "IUCN_use_and_redlistcategory", pattern = ".csv", full.names = TRUE) #show names of all the files in the folder


uselist <- list() #need to create list first so R will know where to store the data
for(i in 1:length(IUCNlist)){
  uselist[[i]] <- read.csv(file = paste0(IUCNlist[i]), header = T)[c(3,4)] #load all of the csv files
}

str(uselist[[1]]) #view the items in the list

usedf <- data.frame() #create df
for(i in length(uselist)){
  usedf <- data.frame(uselist[[i]]) #convert list to df
}

test <- data.frame(uselist[1])

#add 'human use' column

#combine into 1 dataset